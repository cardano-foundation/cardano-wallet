{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Integration.Framework.Request
    ( request
    , unsafeRequest
    , Headers(..)
    , Payload(..)
    , RequestException(..)
    , Context(..)
    , TxDescription(..)
    ) where

import Prelude

import Cardano.CLI
    ( Port (..) )
import Cardano.Wallet.Primitive.Types
    ( NetworkParameters )
import Cardano.Wallet.Transaction
    ( DelegationAction )
import Control.Applicative
    ( (<|>) )
import Control.Monad.Catch
    ( Exception (..), MonadCatch (..), finally, throwM )
import Control.Monad.IO.Class
    ( MonadIO, liftIO )
import Data.Aeson
    ( FromJSON )
import Data.ByteString.Lazy
    ( ByteString )
import Data.Generics.Internal.VL.Lens
    ( (^.) )
import Data.Generics.Product.Typed
    ( HasType, typed )
import Data.Proxy
    ( Proxy (..) )
import Data.Text
    ( Text )
import GHC.Generics
    ( Generic )
import Network.HTTP.Client
    ( HttpException (..)
    , HttpExceptionContent
    , Manager
    , RequestBody (..)
    , brConsume
    , method
    , parseRequest
    , requestBody
    , requestHeaders
    , responseBody
    , responseClose
    , responseOpen
    , responseStatus
    )
import Network.HTTP.Types.Header
    ( RequestHeaders )
import Network.HTTP.Types.Method
    ( Method )
import Network.HTTP.Types.Status
    ( status500 )
import Numeric.Natural
    ( Natural )
import Test.Integration.Faucet
    ( Faucet )

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.Text as T
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Types.Status as HTTP


-- | Running Context for our integration test
data Context t = Context
    { _cleanup
        :: IO ()
        -- ^ Cleanup action for the context
    , _manager
        :: (Text, Manager)
        -- ^ The underlying BaseUrl and Manager used by the Wallet Client
    , _walletPort
        :: Port "wallet"
        -- ^ Server TCP port
    , _faucet
        :: Faucet
        -- ^ A 'Faucet' handle in to have access to funded wallets in
        -- integration tests.
    , _feeEstimator :: TxDescription -> (Natural, Natural)
        -- ^ A fee estimator for the integration tests
    , _networkParameters :: NetworkParameters
        -- ^ Blockchain parameters for the underlying chain
    , _target
        :: Proxy t
    } deriving Generic

-- | Describe a transaction in terms of its inputs and outputs
data TxDescription
    = DelegDescription DelegationAction
    | PaymentDescription
        { nInputs :: Int
        , nOutputs :: Int
        , nChanges :: Int
        }
    deriving (Show)

-- | The result when 'request' fails.
data RequestException
    = DecodeFailure ByteString
      -- ^ JSON decoding the given response data failed.
    | ClientError Aeson.Value
      -- ^ The HTTP response status code indicated failure.
    | HttpException HttpExceptionContent
      -- ^ A wild exception upon sending the request
    deriving (Show)

instance Exception RequestException

-- | The payload of the request
data Payload
    = Json Aeson.Value
    | NonJson ByteString
    | Empty
    deriving (Show)

-- | The headers of the request
data Headers
    = Headers RequestHeaders
    | Default
    | None
    deriving (Show)

-- | Makes a request to the API and decodes the response.
request
    :: forall a m s.
        ( FromJSON a
        , MonadIO m
        , MonadCatch m
        , HasType (Text, Manager) s
        )
    => s
    -> (Method, Text)
        -- ^ HTTP method and request path
    -> Headers
        -- ^ Request headers
    -> Payload
        -- ^ Request body
    -> m (HTTP.Status, Either RequestException a)
request ctx (verb, path) reqHeaders body = do
    let (base, manager) = ctx ^. typed @(Text, Manager)
    req <- parseRequest $ T.unpack $ base <> path
    let io = do
            res <- liftIO $ responseOpen (prepareReq req) manager
            liftIO (handleResponse res `finally` responseClose res)
    catch io handleException
  where
    prepareReq :: HTTP.Request -> HTTP.Request
    prepareReq req = req
        { method = verb
        , requestBody = payload
        , requestHeaders = headers
        }
        where
            headers = case reqHeaders of
                Headers x -> x
                Default -> [ ("Content-Type", "application/json")
                           , ("Accept", "application/json")
                           ]
                None -> mempty

            payload = case body of
                Json x -> (RequestBodyLBS . Aeson.encode) x
                NonJson x -> RequestBodyLBS x
                Empty -> mempty

    handleResponse res = do
        chunks <- brConsume (responseBody res)
        pure $ case responseStatus res of
            s | s < status500 ->
                case decodeOne chunks <|> decodeStream chunks of
                    Nothing -> decodeFailure s chunks
                    Just a  -> (s, Right a)
            -- TODO: decode API error responses into ClientError
            s -> decodeFailure s chunks

    -- NOTE
    -- The API now returns responses either as one chunk, or, as a list of chunks
    -- in case it's streaming responses. In practice, a smart client would process
    -- chunk one by one, but within our integration framework, it is easier to
    -- consider streams as a list.
    --
    -- Therefore, in case where the response is stream, we simply decode each
    -- chunk as a generic JSON, re-encode them as a list (trimming the '\n'
    -- separator), and decoding them as a JSON list.
    decodeOne = Aeson.decodeStrict . mconcat
    decodeStream chunks = do
        xs <- traverse (Aeson.decodeStrict @Aeson.Value . B8.init) chunks
        Aeson.decode (Aeson.encode xs)

    handleException = \case
        e@InvalidUrlException{} ->
            throwM e
        HttpExceptionRequest _ e ->
            return (status500, Left (HttpException e))

    decodeFailure s chunks =
        (s, Left $ DecodeFailure $ BL8.fromStrict $ mconcat chunks)

-- | Makes a request to the API, but throws if it fails.
unsafeRequest
    :: forall a m t.
        ( FromJSON a
        , MonadIO m
        , MonadCatch m
        )
    => Context t
    -> (Method, Text)
    -> Payload
    -> m (HTTP.Status, a)
unsafeRequest ctx req body = do
    (s, res) <- request ctx req Default body
    either throwM (pure . (s,)) res
