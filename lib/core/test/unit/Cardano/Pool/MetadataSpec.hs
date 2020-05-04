{-# LANGUAGE BangPatterns #-}
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
    , Scheme (..)
    , defaultManagerSettings
    , mkClient
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
import Data.Aeson
    ( toJSON )
import Data.Function
    ( (&) )
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
import Network.HTTP.Types
    ( status404 )
import Network.Wai.Handler.Warp
    ( runSettingsSocket, setBeforeMainLoop )
import Servant
    ( Server, err404, serve, throwError )
import Servant.Client
    ( ClientError (..), responseStatusCode )
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

import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.Text as T
import qualified Network.Wai.Handler.Warp as Warp

spec :: Spec
spec = describe "Metadata - MockServer" $
    around withMockServer $ it "Mock Server works as intended" $
        \Client{getStakePoolMetadata} -> property $ \pid -> monadicIO $ do
            run (getStakePoolMetadata pid) >>= \case
                Left (FailureResponse _ res) -> do
                    monitor $ label "No Corresponding Metadata"
                    monitor $ counterexample $ mconcat
                        [ show (responseStatusCode res)
                        , " =/= "
                        , show status404
                        ]
                    assert (responseStatusCode res == status404)
                Left e -> do
                    monitor $ counterexample $
                        "unexpected response from server: " <> show e
                    assert False
                Right !metadata -> do
                    monitor $ label "Got Valid Metadata"
                    let json = toJSON $ ApiT metadata
                    let errs = validateJSON mempty metadataSchema json
                    monitor $ counterexample $ BL8.unpack $ Aeson.encode json
                    monitor $ counterexample $ show errs
                    assert (null errs)

--
-- Mock Server
--

-- | Run a server in a separate thread. Block until the server is ready, and
-- returns the TCP port on which the server is listening, and a handle to the
-- server thread.
withMockServer :: (Client Api -> IO ()) -> IO ()
withMockServer action = do
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
                mngr <- newManager defaultManagerSettings
                let baseUrl = BaseUrl Http host port ""
                pure (mkClient mngr baseUrl, thread)
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
