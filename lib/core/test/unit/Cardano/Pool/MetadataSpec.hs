{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Pool.MetadataSpec
    ( spec
    ) where

import Prelude

import Cardano.Pool.Metadata
    ( Api
    , BaseUrl (..)
    , Client (..)
    , ClientCallbacks (..)
    , ClientConfig (..)
    , MetadataRegistryLog (..)
    , Scheme (..)
    , defaultManagerSettings
    , newClient
    , newManager
    )
import Cardano.Wallet.Api.Server
    ( Listen (..), withListeningSocket )
import Cardano.Wallet.Api.Types
    ( ApiT (..) )
import Cardano.Wallet.Primitive.Types
    ( PoolId (..), StakePoolOffChainMetadata (..), StakePoolTicker (..) )
import Control.Concurrent.Async
    ( async, cancel )
import Control.Concurrent.MVar
    ( newEmptyMVar, putMVar, takeMVar )
import Control.Exception
    ( bracket )
import Control.Lens
    ( at, (.~), (?~) )
import Control.Tracer
    ( Tracer, nullTracer )
import Data.Aeson
    ( toJSON )
import Data.Function
    ( (&) )
import Data.Maybe
    ( isNothing )
import Data.Proxy
    ( Proxy (..) )
import Data.String
    ( fromString )
import Data.Swagger
    ( Referenced (..)
    , Schema
    , SwaggerType (..)
    , maxLength
    , minLength
    , paramSchema
    , properties
    , required
    , type_
    )
import Data.Swagger.Schema.Validation
    ( validateJSON )
import Data.Text
    ( Text )
import Data.Text.Class
    ( ToText (..) )
import Data.Vector.Shuffle
    ( mkSeed )
import Network.Wai.Handler.Warp
    ( runSettingsSocket, setBeforeMainLoop )
import Servant
    ( Server, err404, serve, throwError )
import Test.Hspec
    ( Spec, around, describe, it )
import Test.QuickCheck
    ( ASCIIString (..)
    , Arbitrary (..)
    , PrintableString (..)
    , choose
    , counterexample
    , label
    , property
    , vector
    , vectorOf
    )
import Test.QuickCheck.Gen
    ( Gen (..) )
import Test.QuickCheck.Monadic
    ( assert, monadicIO, monitor, run )
import Test.QuickCheck.Random
    ( mkQCGen )
import Test.Utils.Trace
    ( captureLogging, withLogging )

import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.Text as T
import qualified Network.Wai.Handler.Warp as Warp

spec :: Spec
spec = describe "Metadata - MockServer" $ do

    around (withMockServer noCaching)
    $ it "Mock Server works as intended"
    $ \mkClient -> property $ \pid -> monadicIO $ do
        let Client{getStakePoolMetadata} = mkClient nullTracer
        run (getStakePoolMetadata pid) >>= \case
            Nothing -> do
                monitor $ label "No Corresponding Metadata"
                res <- run (getStakePoolMetadata pid)
                monitor $ counterexample $ unwords
                    [ "request isn't deterministic:"
                    , show res
                    ]
                assert (isNothing res)

            Just metadata -> do
                monitor $ label "Got Valid Metadata"
                let json = toJSON $ ApiT metadata
                let errs = validateJSON mempty metadataSchema json
                monitor $ counterexample $ BL8.unpack $ Aeson.encode json
                monitor $ counterexample $ show errs
                assert (null errs)


--
-- Mock Storage
--

noCaching :: ClientCallbacks IO
noCaching =
    ClientCallbacks
        { saveMetadata      = \_ _ -> pure ()
        , getCachedMetadata = \_   -> pure Nothing
        }

--
-- Mock Server
--

-- | Run a server in a separate thread. Block until the server is ready, and
-- returns the TCP port on which the server is listening, and a handle to the
-- server thread.
withMockServer
    :: ClientCallbacks IO
    -> ((Tracer IO MetadataRegistryLog -> Client Api IO) -> IO ())
    -> IO ()
withMockServer callbacks action = do
    bracket acquire release inBetween
  where
    host = "127.0.0.1"
    listen = ListenOnRandomPort
    acquire = do
        mvar <- newEmptyMVar
        thread <- async $ withListeningSocket (fromString host) listen $ \case
            Left e -> putMVar mvar (Left e)
            Right (p, socket) -> do
                let settings = Warp.defaultSettings
                        & setBeforeMainLoop (putMVar mvar (Right p))
                let application = serve (Proxy @Api) server
                runSettingsSocket settings socket application
        takeMVar mvar >>= \case
            Left e -> error (show e)
            Right port -> do
                manager <- newManager defaultManagerSettings
                let baseUrl  = BaseUrl Http host port ""
                let cacheTTL = 0
                let config = ClientConfig{manager,baseUrl,cacheTTL}
                pure (\tr -> newClient tr config callbacks, thread)
    release = cancel . snd
    inBetween = action . fst

server :: Server Api
server = hGetMetadata
  where
    -- A mock metadata server. Returns either a 404 not found, or, some
    -- arbitrary metadata. It uses the pool's id as an random seed such that
    -- results are consistent between calls.
    hGetMetadata (ApiT pid) =
        if generateWith seed arbitrary
            then throwError err404
            else return $ ApiT $ StakePoolOffChainMetadata
                { ticker =
                    generateWith seed arbitrary
                , name =
                    "_" <> generateWith seed (getPrintableText <$> arbitrary)
                , description =
                    "_" <> generateWith seed (getPrintableText <$> arbitrary)
                , homepage = mconcat
                    [ "https://"
                    , generateWith seed (getASCIIText <$> arbitrary)
                    , ".io"
                    ]
                }
      where
        seed = toText pid

metadataSchema :: Schema
metadataSchema = mempty
    & required .~
        ["ticker", "name", "description", "homepage" ]
    & properties .~ (mempty
        & at "ticker"
            ?~ Inline (mempty & paramSchema .~ tickerSchema)
        & at "name"
            ?~ Inline (mempty & paramSchema .~ nameSchema)
        & at "description"
            ?~ Inline (mempty & paramSchema .~ descriptionSchema)
        & at "homepage"
            ?~ Inline (mempty & paramSchema .~ homepageSchema)
    )
  where
    tickerSchema = mempty
        & type_ ?~ SwaggerString
        & minLength ?~ 3
        & maxLength ?~ 5
    nameSchema = mempty
        & type_ ?~ SwaggerString
        & minLength ?~ 1
        & maxLength ?~ 50
    descriptionSchema = mempty
        & type_ ?~ SwaggerString
        & minLength ?~ 1
        & maxLength ?~ 255
    homepageSchema = mempty
        & type_ ?~ SwaggerString

--
-- Internals
--

instance Arbitrary StakePoolTicker where
    arbitrary = StakePoolTicker . T.pack <$>
        (choose (3,5) >>= \n -> vectorOf n (choose ('A', 'Z')))

instance Arbitrary PoolId where
    arbitrary = PoolId . BS.pack <$> vector 32

getPrintableText :: PrintableString -> Text
getPrintableText = T.pack . getPrintableString

getASCIIText :: ASCIIString -> Text
getASCIIText = T.pack . getASCIIString

-- | Like QuickCheck 'generate', but allow using an explicit string as seed.
generateWith
    :: Text -- ^ A 'Text' seed
    -> Gen a -- ^ Quickcheck generator
    -> a
generateWith seed (MkGen gen) =
    gen (mkQCGen $ mkSeed seed) size
  where
    -- The size passed to the generator is always 30; See also:
    --
    -- https://hackage.haskell.org/package/QuickCheck-2.13.2/docs/src/Test.QuickCheck.Gen.html#generate
    size = 30
