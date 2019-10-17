{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

-- |
-- Copyright: © 2018-2019 IOHK
-- License: Apache-2.0
--
-- API handlers and server using the underlying wallet layer to provide
-- endpoints reachable through HTTP.

module Cardano.Wallet.Api.Server
    ( Listen (..)
    , start
    , newApiLayer
    , withListeningSocket
    ) where

import Prelude

import Cardano.BM.Trace
    ( Trace, logError, logNotice )
import Cardano.Wallet
    ( ErrAdjustForFee (..)
    , ErrCoinSelection (..)
    , ErrCreateUnsignedTx (..)
    , ErrDecodeSignedTx (..)
    , ErrEstimateTxFee (..)
    , ErrForgetPendingTx (..)
    , ErrListTransactions (..)
    , ErrListUTxOStatistics (..)
    , ErrMkStdTx (..)
    , ErrNoSuchWallet (..)
    , ErrPostTx (..)
    , ErrSignTx (..)
    , ErrStartTimeLaterThanEndTime (..)
    , ErrSubmitExternalTx (..)
    , ErrSubmitTx (..)
    , ErrUpdatePassphrase (..)
    , ErrValidateSelection
    , ErrWalletAlreadyExists (..)
    , ErrWithRootKey (..)
    , ErrWrongPassphrase (..)
    , HasGenesisData
    , HasLogger
    , HasNetworkLayer
    , genesisData
    , networkLayer
    )
import Cardano.Wallet.Api
    ( Addresses
    , Api
    , ApiLayer (..)
    , CompatibilityApi
    , CoreApi
    , HasDBFactory
    , HasWorkerRegistry
    , StakePools
    , Transactions
    , Wallets
    , dbFactory
    , workerRegistry
    )
import Cardano.Wallet.Api.Types
    ( AddressAmount (..)
    , ApiAddress (..)
    , ApiBlockReference (..)
    , ApiByronWallet (..)
    , ApiByronWalletMigrationInfo (..)
    , ApiErrorCode (..)
    , ApiFee (..)
    , ApiMigrateByronWalletData (..)
    , ApiNetworkInformation (..)
    , ApiNetworkTip (..)
    , ApiStakePool
    , ApiT (..)
    , ApiTimeReference (..)
    , ApiTransaction (..)
    , ApiTxId (..)
    , ApiTxInput (..)
    , ApiUtxoStatistics (..)
    , ApiWallet (..)
    , ByronWalletPostData (..)
    , Iso8601Time (..)
    , PostExternalTransactionData (..)
    , PostTransactionData
    , PostTransactionFeeData
    , WalletBalance (..)
    , WalletPostData (..)
    , WalletPutData (..)
    , WalletPutPassphraseData (..)
    , getApiMnemonicT
    )
import Cardano.Wallet.DB
    ( DBFactory )
import Cardano.Wallet.Network
    ( ErrNetworkTip (..), ErrNetworkUnavailable (..), NetworkLayer )
import Cardano.Wallet.Primitive.AddressDerivation
    ( KeyToAddress (..), WalletKey (..), digest, publicKey )
import Cardano.Wallet.Primitive.AddressDerivation.Random
    ( RndKey )
import Cardano.Wallet.Primitive.AddressDerivation.Sequential
    ( SeqKey (..) )
import Cardano.Wallet.Primitive.AddressDiscovery
    ( IsOurs )
import Cardano.Wallet.Primitive.AddressDiscovery.Random
    ( RndState, mkRndState )
import Cardano.Wallet.Primitive.AddressDiscovery.Sequential
    ( SeqState (..), defaultAddressPoolGap, mkSeqState )
import Cardano.Wallet.Primitive.Fee
    ( Fee (..) )
import Cardano.Wallet.Primitive.Model
    ( BlockchainParameters
    , Wallet
    , availableBalance
    , currentTip
    , getState
    , totalBalance
    )
import Cardano.Wallet.Primitive.Types
    ( Address
    , AddressState
    , Block
    , Coin (..)
    , DecodeAddress (..)
    , DefineTx (..)
    , EncodeAddress (..)
    , Hash (..)
    , HistogramBar (..)
    , SortOrder (..)
    , TransactionInfo (TransactionInfo)
    , TxIn
    , TxOut (..)
    , UTxOStatistics (..)
    , WalletId (..)
    , WalletMetadata (..)
    , WalletName
    , slotAt
    , slotMinBound
    , syncProgressRelativeToTime
    )
import Cardano.Wallet.Registry
    ( HasWorkerCtx (..), MkWorker (..), newWorker, workerResource )
import Cardano.Wallet.Transaction
    ( TransactionLayer )
import Cardano.Wallet.Unsafe
    ( unsafeRunExceptT )
import Control.DeepSeq
    ( NFData )
import Control.Exception
    ( AsyncException (..)
    , SomeException
    , asyncExceptionFromException
    , bracket
    )
import Control.Monad
    ( forM_, void )
import Control.Monad.IO.Class
    ( MonadIO, liftIO )
import Control.Monad.Trans.Except
    ( ExceptT, throwE, withExceptT )
import Data.Aeson
    ( (.=) )
import Data.Function
    ( (&) )
import Data.Functor
    ( ($>), (<&>) )
import Data.Generics.Internal.VL.Lens
    ( Lens', (.~), (^.) )
import Data.Generics.Labels
    ()
import Data.List
    ( isSubsequenceOf, sortOn )
import Data.Maybe
    ( fromMaybe, isJust )
import Data.Proxy
    ( Proxy (..) )
import Data.Quantity
    ( Quantity (..) )
import Data.Set
    ( Set )
import Data.Streaming.Network
    ( bindPortTCP, bindRandomPortTCP )
import Data.Text
    ( Text )
import Data.Text.Class
    ( toText )
import Data.Time
    ( UTCTime )
import Data.Time.Clock
    ( getCurrentTime )
import Data.Word
    ( Word32 )
import Fmt
    ( Buildable, pretty )
import Network.HTTP.Media.RenderHeader
    ( renderHeader )
import Network.HTTP.Types.Header
    ( hContentType )
import Network.Socket
    ( Socket, close )
import Network.Wai
    ( Request, pathInfo )
import Network.Wai.Handler.Warp
    ( Port )
import Network.Wai.Middleware.Logging
    ( newApiLoggerSettings, obfuscateKeys, withApiLogger )
import Network.Wai.Middleware.ServantError
    ( handleRawError )
import Numeric.Natural
    ( Natural )
import Servant
    ( (:<|>) (..)
    , (:>)
    , Application
    , JSON
    , NoContent (..)
    , Server
    , contentType
    , err400
    , err403
    , err404
    , err409
    , err410
    , err500
    , err501
    , err503
    , serve
    , throwError
    )
import Servant.Server
    ( Handler (..), ServantErr (..) )
import System.Random
    ( getStdRandom, random )

import qualified Cardano.Wallet as W
import qualified Cardano.Wallet.Network as NW
import qualified Cardano.Wallet.Primitive.AddressDerivation.Random as Rnd
import qualified Cardano.Wallet.Primitive.AddressDerivation.Sequential as Seq
import qualified Cardano.Wallet.Primitive.Types as W
import qualified Cardano.Wallet.Registry as Registry
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Network.Wai.Handler.Warp as Warp

-- | How the server should listen for incoming requests.
data Listen
    = ListenOnPort Port
      -- ^ Listen on given TCP port
    | ListenOnRandomPort
      -- ^ Listen on an unused TCP port, selected at random
    deriving (Show, Eq)

-- | Start the application server, using the given settings and a bound socket.
start
    :: forall t .
        ( Buildable (ErrValidateSelection t)
        , DefineTx t
        , DecodeAddress t
        , EncodeAddress t
        , KeyToAddress t RndKey
        , KeyToAddress t SeqKey
        )
    => Warp.Settings
    -> Trace IO Text
    -> Socket
    -> ApiLayer (RndState t) t RndKey
    -> ApiLayer (SeqState t) t SeqKey
    -> IO ()
start settings trace socket rndCtx seqCtx = do
    logSettings <- newApiLoggerSettings <&> obfuscateKeys (const sensitive)
    Warp.runSettingsSocket settings socket
        $ handleRawError (curry handler)
        $ withApiLogger trace logSettings
        application
  where
    -- | A Servant server for our wallet API
    server :: Server (Api t)
    server = coreApiServer seqCtx :<|> compatibilityApiServer rndCtx seqCtx

    application :: Application
    application = serve (Proxy @("v2" :> Api t)) server

    sensitive :: [Text]
    sensitive =
        [ "passphrase"
        , "old_passphrase"
        , "new_passphrase"
        , "mnemonic_sentence"
        , "mnemonic_second_factor"
        ]

-- | Run an action with a TCP socket bound to a port specified by the `Listen`
-- parameter.
withListeningSocket
    :: Listen
    -- ^ Whether to listen on a given port, or random port.
    -> ((Port, Socket) -> IO ())
    -- ^ Action to run with listening socket.
    -> IO ()
withListeningSocket portOpt = bracket acquire release
  where
    acquire = case portOpt of
        ListenOnPort port -> (port,) <$> bindPortTCP port hostPreference
        ListenOnRandomPort -> bindRandomPortTCP hostPreference
    release (_, socket) = liftIO $ close socket
    -- TODO: make configurable, default to secure for now.
    hostPreference = "127.0.0.1"

{-------------------------------------------------------------------------------
                                   Core API
-------------------------------------------------------------------------------}

coreApiServer
    :: forall ctx s t k.
        ( DefineTx t
        , KeyToAddress t k
        , Buildable (ErrValidateSelection t)
        , k ~ SeqKey
        , s ~ SeqState t
        , ctx ~ ApiLayer s t k
        )
    => ctx
    -> Server (CoreApi t)
coreApiServer ctx =
    addresses ctx
    :<|> wallets ctx
    :<|> transactions ctx
    :<|> pools ctx
    :<|> network ctx

{-------------------------------------------------------------------------------
                                    Wallets
-------------------------------------------------------------------------------}

wallets
    :: forall ctx s t k.
        ( DefineTx t
        , KeyToAddress t k
        , k ~ SeqKey
        , s ~ SeqState t
        , ctx ~ ApiLayer s t k
        )
    => ctx
    -> Server Wallets
wallets ctx =
    deleteWallet ctx
    :<|> getWallet ctx
    :<|> listWallets ctx
    :<|> postWallet ctx
    :<|> putWallet ctx
    :<|> putWalletPassphrase ctx
    :<|> getUTxOsStatistics ctx

deleteWallet
    :: forall ctx s t k.
        ( s ~ SeqState t
        , ctx ~ ApiLayer s t k
        )
    => ctx
    -> ApiT WalletId
    -> Handler NoContent
deleteWallet ctx (ApiT wid) = do
    liftHandler $ withWorkerCtx ctx wid throwE $ \wrk -> W.removeWallet wrk wid
    liftIO $ Registry.remove re wid
    liftIO $ (df ^. #removeDatabase) wid
    return NoContent
  where
    re = ctx ^. workerRegistry @s @t @k
    df = ctx ^. dbFactory @s @t @k

getWallet
    :: forall ctx s t k.
        ( DefineTx t
        , s ~ SeqState t
        , ctx ~ ApiLayer s t k
        )
    => ctx
    -> ApiT WalletId
    -> Handler ApiWallet
getWallet ctx wid =
    fst <$> getWalletWithCreationTime mkApiWallet ctx wid

listWallets
    :: forall ctx s t k.
        ( DefineTx t
        , s ~ SeqState t
        , ctx ~ ApiLayer s t k
        )
    => ctx
    -> Handler [ApiWallet]
listWallets ctx = do
    wids <- liftIO $ Registry.keys re
    fmap fst . sortOn snd <$>
        mapM (getWalletWithCreationTime mkApiWallet ctx) (ApiT <$> wids)
  where
    re = ctx ^. workerRegistry @s @t @k

postWallet
    :: forall ctx s t k.
        ( DefineTx t
        , KeyToAddress t k
        , s ~ SeqState t
        , k ~ SeqKey
        , ctx ~ ApiLayer s t k
        )
    => ctx
    -> WalletPostData
    -> Handler ApiWallet
postWallet ctx body = do
    let seed = getApiMnemonicT (body ^. #mnemonicSentence)
    let secondFactor =
            maybe mempty getApiMnemonicT (body ^. #mnemonicSecondFactor)
    let pwd = getApiT (body ^. #passphrase)
    let rootXPrv = Seq.generateKeyFromSeed (seed, secondFactor) pwd
    let g = maybe defaultAddressPoolGap getApiT (body ^. #addressPoolGap)
    let s = mkSeqState (rootXPrv, pwd) g
    let wid = WalletId $ digest $ publicKey rootXPrv
    let wName = getApiT (body ^. #name)
    void $ liftHandler $ createWallet ctx wid wName s
    liftHandler $ withWorkerCtx ctx wid throwE $ \wrk ->
        W.attachPrivateKey wrk wid (rootXPrv, pwd)
    getWallet ctx (ApiT wid)

putWallet
    :: forall ctx s t k.
        ( DefineTx t
        , s ~ SeqState t
        , ctx ~ ApiLayer s t k
        )
    => ctx
    -> ApiT WalletId
    -> WalletPutData
    -> Handler ApiWallet
putWallet ctx (ApiT wid) body = do
    case body ^. #name of
        Nothing ->
            return ()
        Just (ApiT wName) -> liftHandler $ withWorkerCtx ctx wid throwE $ \wrk ->
            W.updateWallet wrk wid (modify wName)
    getWallet ctx (ApiT wid)
  where
    modify :: W.WalletName -> WalletMetadata -> WalletMetadata
    modify wName meta = meta { name = wName }

putWalletPassphrase
    :: forall ctx s t k.
        ( WalletKey k
        , s ~ SeqState t
        , ctx ~ ApiLayer s t k
        )
    => ctx
    -> ApiT WalletId
    -> WalletPutPassphraseData
    -> Handler NoContent
putWalletPassphrase ctx (ApiT wid) body = do
    let (WalletPutPassphraseData (ApiT old) (ApiT new)) = body
    liftHandler $ withWorkerCtx ctx wid liftE $ \wrk ->
        W.updateWalletPassphrase wrk wid (old, new)
    return NoContent
  where
    liftE = throwE . ErrUpdatePassphraseNoSuchWallet

getUTxOsStatistics
    :: forall ctx s t k.
        ( DefineTx t
        , s ~ SeqState t
        , ctx ~ ApiLayer s t k
        )
    => ctx
    -> ApiT WalletId
    -> Handler ApiUtxoStatistics
getUTxOsStatistics ctx (ApiT wid) = do
    stats <- liftHandler $ withWorkerCtx ctx wid liftE $ \wrk ->
        W.listUtxoStatistics wrk wid
    let (UTxOStatistics histo totalStakes bType) = stats
    return ApiUtxoStatistics
        { total = Quantity (fromIntegral totalStakes)
        , scale = ApiT bType
        , distribution = Map.fromList $ map (\(HistogramBar k v)-> (k,v)) histo
        }
  where
    liftE = throwE . ErrListUTxOStatisticsNoSuchWallet

{-------------------------------------------------------------------------------
                                    Addresses
-------------------------------------------------------------------------------}

addresses
    :: forall ctx s t k.
        ( DefineTx t
        , KeyToAddress t k
        , k ~ SeqKey
        , s ~ SeqState t
        , ctx ~ ApiLayer s t k
        )
    => ctx
    -> Server (Addresses t)
addresses = listAddresses

listAddresses
    :: forall ctx s t k.
        ( DefineTx t
        , KeyToAddress t k
        , k ~ SeqKey
        , s ~ SeqState t
        , ctx ~ ApiLayer s t k
        )
    => ctx
    -> ApiT WalletId
    -> Maybe (ApiT AddressState)
    -> Handler [ApiAddress t]
listAddresses ctx (ApiT wid) stateFilter = do
    addrs <- liftHandler $ withWorkerCtx ctx wid throwE $ \wrk ->
        W.listAddresses wrk wid
    return $ coerceAddress <$> filter filterCondition addrs
  where
    filterCondition :: (Address, AddressState) -> Bool
    filterCondition = case stateFilter of
        Nothing -> const True
        Just (ApiT s) -> (== s) . snd
    coerceAddress (a, s) = ApiAddress (ApiT a, Proxy @t) (ApiT s)

{-------------------------------------------------------------------------------
                                    Transactions
-------------------------------------------------------------------------------}

transactions
    :: forall ctx s t k.
        ( DefineTx t
        , KeyToAddress t k
        , Buildable (ErrValidateSelection t)
        , s ~ SeqState t
        , k ~ SeqKey
        , ctx ~ ApiLayer s t k
        )
    => ctx
    -> Server (Transactions t)
transactions ctx =
    postTransaction ctx
    :<|> listTransactions ctx
    :<|> postTransactionFee ctx
    :<|> postExternalTransaction ctx
    :<|> deleteTransaction ctx

postTransaction
    :: forall ctx s t k.
        ( DefineTx t
        , Buildable (ErrValidateSelection t)
        , KeyToAddress t k
        , k ~ SeqKey
        , s ~ SeqState t
        , ctx ~ ApiLayer s t k
        )
    => ctx
    -> ApiT WalletId
    -> PostTransactionData t
    -> Handler (ApiTransaction t)
postTransaction ctx (ApiT wid) body = do
    let outs = coerceCoin <$> (body ^. #payments)
    let pwd = getApiT $ body ^. #passphrase

    selection <- liftHandler $ withWorkerCtx ctx wid liftE1 $ \wrk ->
        W.createUnsignedTx wrk wid outs

    (tx, meta, wit) <- liftHandler $ withWorkerCtx ctx wid liftE2 $ \wrk ->
        W.signTx wrk wid pwd selection

    liftHandler $ withWorkerCtx ctx wid liftE3 $ \wrk ->
        W.submitTx wrk wid (tx, meta, wit)

    now <- liftIO getCurrentTime
    pure $ mkApiTransaction
        (txId @t tx)
        (fmap Just <$> selection ^. #inputs)
        (selection ^. #outputs)
        (meta, now)
        #pendingSince
  where
    liftE1 = throwE . ErrCreateUnsignedTxNoSuchWallet
    liftE2 = throwE . ErrSignTxNoSuchWallet
    liftE3 = throwE . ErrSubmitTxNoSuchWallet

postExternalTransaction
    :: forall ctx s t k.
        ( s ~ SeqState t
        , ctx ~ ApiLayer s t k
        , DefineTx t
        )
    => ctx
    -> PostExternalTransactionData
    -> Handler ApiTxId
postExternalTransaction ctx (PostExternalTransactionData load) = do
    tx <- liftHandler $ W.submitExternalTx @ctx @t @k ctx load
    return $ ApiTxId (ApiT (txId @t tx))

deleteTransaction
    :: forall ctx s t k.
        ( ctx ~ ApiLayer s t k
        , DefineTx t
        )
    => ctx
    -> ApiT WalletId
    -> ApiTxId
    -> Handler NoContent
deleteTransaction ctx (ApiT wid) (ApiTxId (ApiT (tid))) = do
    liftHandler $ withWorkerCtx ctx wid liftE $ \wrk ->
        W.forgetPendingTx wrk wid tid
    return NoContent
  where
    liftE = throwE . ErrForgetPendingTxNoSuchWallet

listTransactions
    :: forall ctx s t k.
        ( DefineTx t
        , s ~ SeqState t
        , ctx ~ ApiLayer s t k
        )
    => ctx
    -> ApiT WalletId
    -> Maybe Iso8601Time
    -> Maybe Iso8601Time
    -> Maybe (ApiT SortOrder)
    -> Handler [ApiTransaction t]
listTransactions ctx (ApiT wid) mStart mEnd mOrder = do
    txs <- liftHandler $ withWorkerCtx ctx wid liftE $ \wrk ->
        W.listTransactions wrk wid
            (getIso8601Time <$> mStart)
            (getIso8601Time <$> mEnd)
            (maybe defaultSortOrder getApiT mOrder)
    return $ map mkApiTransactionFromInfo txs
  where
    liftE = throwE . ErrListTransactionsNoSuchWallet
    defaultSortOrder :: SortOrder
    defaultSortOrder = Descending

    -- Populate an API transaction record with 'TransactionInfo' from the wallet
    -- layer.
    mkApiTransactionFromInfo :: TransactionInfo -> ApiTransaction t
    mkApiTransactionFromInfo (TransactionInfo txid ins outs meta depth txtime) =
        apiTx { depth  }
      where
        apiTx = mkApiTransaction txid ins outs (meta, txtime) #insertedAt

postTransactionFee
    :: forall ctx s t k.
        ( DefineTx t
        , Buildable (ErrValidateSelection t)
        , s ~ SeqState t
        , ctx ~ ApiLayer s t k
        )
    => ctx
    -> ApiT WalletId
    -> PostTransactionFeeData t
    -> Handler ApiFee
postTransactionFee ctx (ApiT wid) body = do
    let outs = coerceCoin <$> (body ^. #payments)
    (Fee fee) <- liftHandler $ withWorkerCtx ctx wid liftE $ \wrk ->
        W.estimateTxFee wrk wid outs
    return ApiFee
        { amount = Quantity (fromIntegral fee)
        }
  where
    liftE = throwE . ErrEstimateTxFeeNoSuchWallet

{-------------------------------------------------------------------------------
                                    Stake Pools
-------------------------------------------------------------------------------}

pools
    :: ctx
    -> Server StakePools
pools = listPools

listPools
    :: ctx
    -> Handler [ApiStakePool]
listPools _ctx = throwError err501

{-------------------------------------------------------------------------------
                                    Network
-------------------------------------------------------------------------------}

network
    :: forall s t k. ()
    => ApiLayer s t k
    -> Handler ApiNetworkInformation
network ctx = do
    now <- liftIO getCurrentTime
    nodeTip <- liftHandler (NW.networkTip nl)
    let ntrkTip = fromMaybe slotMinBound (slotAt sp now)
    pure $ ApiNetworkInformation
        { syncProgress =
            ApiT $ syncProgressRelativeToTime sp nodeTip now
        , nodeTip =
            ApiBlockReference
                { epochNumber = ApiT $ nodeTip ^. (#slotId . #epochNumber)
                , slotNumber  = ApiT $ nodeTip ^. (#slotId . #slotNumber)
                , height = natural (nodeTip ^. #blockHeight)
                }
        , networkTip =
            ApiNetworkTip
                { epochNumber = ApiT $ ntrkTip ^. #epochNumber
                , slotNumber = ApiT $ ntrkTip ^. #slotNumber
                }
        }
  where
    nl = ctx ^. networkLayer @t
    (_, bp) = ctx ^. genesisData @t
    sp :: W.SlotParameters
    sp = W.SlotParameters
        (bp ^. #getEpochLength)
        (bp ^. #getSlotLength)
        (bp ^. #getGenesisBlockDate)

{-------------------------------------------------------------------------------
                               Compatibility API
-------------------------------------------------------------------------------}

compatibilityApiServer
    :: forall t .
        ( Buildable (ErrValidateSelection t)
        , DefineTx t
        , KeyToAddress t RndKey
        , KeyToAddress t SeqKey
        )
    => ApiLayer (RndState t) t RndKey
    -> ApiLayer (SeqState t) t SeqKey
    -> Server (CompatibilityApi t)
compatibilityApiServer rndCtx seqCtx =
    deleteByronWallet rndCtx
    :<|> getByronWallet rndCtx
    :<|> getByronWalletMigrationInfo rndCtx
    :<|> listByronWallets rndCtx
    :<|> migrateByronWallet rndCtx seqCtx
    :<|> postByronWallet rndCtx

deleteByronWallet
    :: forall s t k. (s ~ RndState t)
    => ApiLayer s t k
    -> ApiT WalletId
    -> Handler NoContent
deleteByronWallet ctx (ApiT wid) = do
    liftHandler $ withWorkerCtx ctx wid throwE $
        \worker -> W.removeWallet worker wid
    liftIO $ Registry.remove re wid
    liftIO $ (df ^. #removeDatabase) wid
    return NoContent
  where
    re = ctx ^. workerRegistry @s @t @k
    df = ctx ^. dbFactory @s @t @k

getByronWallet
    :: forall t k. DefineTx t
    => ApiLayer (RndState t) t k
    -> ApiT WalletId
    -> Handler ApiByronWallet
getByronWallet ctx wid =
    fst <$> getWalletWithCreationTime mkApiByronWallet ctx wid

getByronWalletMigrationInfo
    :: ctx
    -> ApiT WalletId
    -> Handler ApiByronWalletMigrationInfo
getByronWalletMigrationInfo _ _ = throwError err501

migrateByronWallet
    :: rndCtx
    -> seqCtx
    -> ApiT WalletId
       -- ^ Source wallet (Byron)
    -> ApiT WalletId
       -- ^ Target wallet (new-style)
    -> ApiMigrateByronWalletData
    -> Handler [ApiTransaction t]
migrateByronWallet _rndCtx _seqCtx _sourceWid _targetWid _migrateData =
    throwError err501

listByronWallets
    :: forall s t k. (DefineTx t, s ~ RndState t)
    => ApiLayer s t k
    -> Handler [ApiByronWallet]
listByronWallets ctx = do
    wids <- liftIO $ Registry.keys re
    fmap fst . sortOn snd <$>
        mapM (getWalletWithCreationTime mkApiByronWallet ctx) (ApiT <$> wids)
  where
    re = ctx ^. workerRegistry @s @t @k

postByronWallet
    :: forall t. (DefineTx t, KeyToAddress t RndKey)
    => ApiLayer (RndState t) t RndKey
    -> ByronWalletPostData
    -> Handler ApiByronWallet
postByronWallet ctx body = do
    void
        . liftHandler
        . createWallet ctx wid name
        . mkRndState rootXPrv
        =<< liftIO (getStdRandom random)
    liftHandler $ withWorkerCtx ctx wid throwE $ \worker ->
        W.attachPrivateKey worker wid (rootXPrv, passphrase)
    getByronWallet ctx (ApiT wid)
  where
    mnemonicSentence = getApiMnemonicT (body ^. #mnemonicSentence)
    name = getApiT (body ^. #name)
    passphrase = getApiT (body ^. #passphrase)
    rootXPrv = Rnd.generateKeyFromSeed mnemonicSentence passphrase
    wid = WalletId $ digest $ publicKey rootXPrv

{-------------------------------------------------------------------------------
                                Helpers
-------------------------------------------------------------------------------}

-- | see 'Cardano.Wallet#createWallet'
createWallet
    :: forall ctx s t k.
        ( HasWorkerRegistry s t k ctx
        , HasDBFactory s t k ctx
        , HasLogger ctx
        , HasGenesisData t (WorkerCtx ctx)
        , HasNetworkLayer t (WorkerCtx ctx)
        , HasLogger (WorkerCtx ctx)
        , Show s
        , NFData s
        , IsOurs s
        , DefineTx t
        )
    => ctx
    -> WalletId
    -> WalletName
    -> s
    -> ExceptT ErrCreateWallet IO WalletId
createWallet ctx wid a0 a1 =
    liftIO (Registry.lookup re wid) >>= \case
        Just _ ->
            throwE $ ErrCreateWalletAlreadyExists $ ErrWalletAlreadyExists wid
        Nothing ->
            liftIO (newWorker @_ @_ @ctx ctx wid config) >>= \case
                Nothing ->
                    throwE ErrCreateWalletFailedToCreateWorker
                Just worker ->
                    liftIO (Registry.insert re worker) $> wid
  where
    config = MkWorker
        { workerBefore = \ctx' _ -> do
            -- FIXME:
            -- Review error handling here
            void $ unsafeRunExceptT $
                W.createWallet @(WorkerCtx ctx) @s @t @k ctx' wid a0 a1

        , workerMain = \ctx' _ -> do
            -- FIXME:
            -- Review error handling here
            unsafeRunExceptT $
                W.restoreWallet @(WorkerCtx ctx) @s @t @k ctx' wid

        , workerAfter =
            defaultWorkerAfter

        , workerAcquire =
            (df ^. #withDatabase) wid
        }
    re = ctx ^. workerRegistry @s @t @k
    df = ctx ^. dbFactory @s @t @k

mkApiTransaction
    :: forall t.
       Hash "Tx"
    -> [(TxIn, Maybe TxOut)]
    -> [TxOut]
    -> (W.TxMeta, UTCTime)
    -> Lens' (ApiTransaction t) (Maybe ApiTimeReference)
    -> ApiTransaction t
mkApiTransaction txid ins outs (meta, timestamp) setTimeReference =
    tx & setTimeReference .~ Just timeReference
  where
    tx :: ApiTransaction t
    tx = ApiTransaction
        { id = ApiT txid
        , amount = meta ^. #amount
        , insertedAt = Nothing
        , pendingSince = Nothing
        , depth = Quantity 0
        , direction = ApiT (meta ^. #direction)
        , inputs = [ApiTxInput (fmap toAddressAmount o) (ApiT i) | (i, o) <- ins]
        , outputs = NE.fromList (toAddressAmount <$> outs)
        , status = ApiT (meta ^. #status)
        }

    timeReference :: ApiTimeReference
    timeReference = ApiTimeReference
        { time = timestamp
        , block = ApiBlockReference
            { slotNumber = ApiT $ meta ^. (#slotId . #slotNumber )
            , epochNumber = ApiT $ meta ^. (#slotId . #epochNumber)
            , height = natural (meta ^. #blockHeight)
            }
        }

    toAddressAmount :: TxOut -> AddressAmount t
    toAddressAmount (TxOut addr (Coin c)) =
        AddressAmount (ApiT addr, Proxy @t) (Quantity $ fromIntegral c)

coerceCoin :: AddressAmount t -> TxOut
coerceCoin (AddressAmount (ApiT addr, _) (Quantity c)) =
    TxOut addr (Coin $ fromIntegral c)

natural :: Quantity q Word32 -> Quantity q Natural
natural = Quantity . fromIntegral . getQuantity

getWalletWithCreationTime
    :: forall s t k w. DefineTx t
    => (WalletId -> Wallet s t -> WalletMetadata -> Set (Tx t) -> w)
    -> ApiLayer s t k
    -> ApiT WalletId
    -> Handler (w, UTCTime)
getWalletWithCreationTime mk ctx (ApiT wid) = do
    (wallet, meta, pending) <-
        liftHandler $ withWorkerCtx ctx wid throwE $
            \wrk -> W.readWallet wrk wid
    return (mk wid wallet meta pending, meta ^. #creationTime)

mkApiWallet
    :: forall s t. (DefineTx t, s ~ SeqState t)
    => WalletId -> Wallet s t -> WalletMetadata -> Set (Tx t) -> ApiWallet
mkApiWallet wid wallet meta pending = ApiWallet
    { addressPoolGap = ApiT $ getState wallet ^. #externalPool . #gap
    , balance = getWalletBalance wallet pending
    , delegation = ApiT $ ApiT <$> meta ^. #delegation
    , id = ApiT wid
    , name = ApiT $ meta ^. #name
    , passphrase = ApiT <$> meta ^. #passphraseInfo
    , state = ApiT $ meta ^. #status
    , tip = getWalletTip wallet
    }

mkApiByronWallet
    :: forall s t. (DefineTx t, s ~ RndState t)
    => WalletId -> Wallet s t -> WalletMetadata -> Set (Tx t) -> ApiByronWallet
mkApiByronWallet wid wallet meta pending = ApiByronWallet
    { balance = getWalletBalance wallet pending
    , id = ApiT wid
    , name = ApiT $ meta ^. #name
    , passphrase = ApiT <$> meta ^. #passphraseInfo
    , state = ApiT $ meta ^. #status
    , tip = getWalletTip wallet
    }

getWalletBalance :: DefineTx t => Wallet s t -> Set (Tx t) -> ApiT WalletBalance
getWalletBalance wallet pending = ApiT $ WalletBalance
    { available = Quantity $ availableBalance pending wallet
    , total = Quantity $ totalBalance pending wallet
    }

getWalletTip :: Wallet s t -> ApiBlockReference
getWalletTip wallet = ApiBlockReference
    { epochNumber = ApiT $ (currentTip wallet) ^. #slotId . #epochNumber
    , slotNumber = ApiT $ (currentTip wallet) ^. #slotId . #slotNumber
    , height = natural $ (currentTip wallet) ^. #blockHeight
    }

{-------------------------------------------------------------------------------
                                Api Layer
-------------------------------------------------------------------------------}

-- | Create a new instance of the wallet layer.
newApiLayer
    :: forall ctx s t k. (ctx ~ ApiLayer s t k, DefineTx t)
    => Trace IO Text
    -> (Block (Tx t), BlockchainParameters)
    -> NetworkLayer IO t (Block (Tx t))
    -> TransactionLayer t k
    -> DBFactory IO s t k
    -> [WalletId]
    -> IO ctx
newApiLayer tr g0 nw tl df wids = do
    re <- Registry.empty
    let ctx = ApiLayer tr g0 nw tl df re
    forM_ wids (registerWorker re ctx)
    return ctx
  where
    registerWorker re ctx wid = do
        let config = MkWorker
                { workerBefore =
                    \_ _ -> return ()

                , workerMain = \ctx' _ -> do
                    -- FIXME:
                    -- Review error handling here
                    unsafeRunExceptT $
                        W.restoreWallet @(WorkerCtx ctx) @s @t @k ctx' wid

                , workerAfter =
                    defaultWorkerAfter

                , workerAcquire =
                    (df ^. #withDatabase) wid
                }
        newWorker @_ @_ @ctx ctx wid config >>= \case
            Nothing ->
                return ()
            Just worker ->
                Registry.insert re worker

-- | Run an action in a particular worker context. Fails if there's no worker
-- for a given id.
withWorkerCtx
    :: forall ctx s t k m a.
        ( HasWorkerRegistry s t k ctx
        , MonadIO m
        )
    => ctx
        -- ^ A context that has a registry
    -> WalletId
        -- ^ Wallet to look for
    -> (ErrNoSuchWallet -> m a)
        -- ^ Wallet not present, handle error
    -> (WorkerCtx ctx -> m a)
        -- ^ Do something with the wallet
    -> m a
withWorkerCtx ctx wid onMissing action =
    Registry.lookup re wid >>= \case
        Nothing ->
            onMissing (ErrNoSuchWallet wid)
        Just wrk ->
            action $ hoistResource (workerResource wrk) ctx
  where
    re = ctx ^. workerRegistry @s @t @k

defaultWorkerAfter :: Trace IO Text -> Either SomeException a -> IO ()
defaultWorkerAfter tr = \case
    Right _ ->
        logNotice tr "Worker has exited: main action is over"
    Left e -> case asyncExceptionFromException e of
        Just ThreadKilled ->
            logNotice tr "Worker has exited: killed by parent."
        Just UserInterrupt ->
            logNotice tr "Worker has exited: killed by user."
        _ ->
            logError tr $ "Worker has exited unexpectedly: " <> pretty (show e)

{-------------------------------------------------------------------------------
                                Error Handling
-------------------------------------------------------------------------------}

-- | Lift our wallet layer into servant 'Handler', by mapping each error to a
-- corresponding servant error.
class LiftHandler e where
    liftHandler :: ExceptT e IO a -> Handler a
    liftHandler action = Handler (withExceptT handler action)
    handler :: e -> ServantErr

apiError :: ServantErr -> ApiErrorCode -> Text -> ServantErr
apiError err code message = err
    { errBody = Aeson.encode $ Aeson.object
        [ "code" .= code
        , "message" .= T.replace "\n" " " message
        ]
    }

data ErrCreateWallet
    = ErrCreateWalletAlreadyExists ErrWalletAlreadyExists
        -- ^ Wallet already exists
    | ErrCreateWalletFailedToCreateWorker
        -- ^ Somehow, we couldn't create a worker or open a db connection
    deriving (Eq, Show)

-- | Small helper to easy show things to Text
showT :: Show a => a -> Text
showT = T.pack . show

instance LiftHandler ErrNoSuchWallet where
    handler = \case
        ErrNoSuchWallet wid ->
            apiError err404 NoSuchWallet $ mconcat
                [ "I couldn't find a wallet with the given id: "
                , toText wid
                ]

instance LiftHandler ErrWalletAlreadyExists where
    handler = \case
        ErrWalletAlreadyExists wid ->
            apiError err409 WalletAlreadyExists $ mconcat
                [ "This operation would yield a wallet with the following id: "
                , toText wid
                , " However, I already know of a wallet with this id."
                ]

instance LiftHandler ErrCreateWallet where
    handler = \case
        ErrCreateWalletAlreadyExists e -> handler e
        ErrCreateWalletFailedToCreateWorker ->
            apiError err500 UnexpectedError $ mconcat
                [ "That's embarassing. Your wallet looks good, but I couldn't "
                , "open a new database to store its data. This is unexpected "
                , "and likely not your fault. Perhaps, check your filesystem's "
                , "permissions or available space?"
                ]

instance LiftHandler ErrWithRootKey where
    handler = \case
        ErrWithRootKeyNoRootKey wid ->
            apiError err404 NoRootKey $ mconcat
                [ "I couldn't find a root private key for the given wallet: "
                , toText wid, ". However, this operation requires that I do "
                , "have such a key. Either there's no such wallet, or I don't "
                , "fully own it."
                ]
        ErrWithRootKeyWrongPassphrase wid ErrWrongPassphrase ->
            apiError err403 WrongEncryptionPassphrase $ mconcat
                [ "The given encryption passphrase doesn't match the one I use "
                , "to encrypt the root private key of the given wallet: "
                , toText wid
                ]

instance Buildable e => LiftHandler (ErrCoinSelection e) where
    handler = \case
        ErrNotEnoughMoney utxo payment ->
            apiError err403 NotEnoughMoney $ mconcat
                [ "I can't process this payment because there's not enough "
                , "UTxO available in the wallet. The total UTxO sums up to "
                , showT utxo, " Lovelace, but I need ", showT payment
                , " Lovelace (excluding fee amount) in order to proceed "
                , " with the payment."
                ]
        ErrUtxoNotEnoughFragmented nUtxo nOuts ->
            apiError err403 UtxoNotEnoughFragmented $ mconcat
                [ "When creating new transactions, I'm not able to re-use "
                , "the same UTxO for different outputs. Here, I only have "
                , showT nUtxo, " available, but there are ", showT nOuts
                , " outputs."
                ]
        ErrMaximumInputsReached n ->
            apiError err403 TransactionIsTooBig $ mconcat
                [ "I had to select ", showT n, " inputs to construct the "
                , "requested transaction. Unfortunately, this would create a "
                , "transaction that is too big, and this would consequently "
                , "be rejected by a core node. Try sending a smaller amount."
                ]
        ErrInputsDepleted ->
            apiError err403 InputsDepleted $ mconcat
                [ "I had to select inputs to construct the "
                , "requested transaction. Unfortunately, one output of the "
                , "transaction depleted all available inputs. "
                , "Try sending a smaller amount."
                ]
        ErrInvalidSelection e ->
            apiError err403 InvalidCoinSelection $ pretty e

instance LiftHandler ErrAdjustForFee where
    handler = \case
        ErrCannotCoverFee missing ->
            apiError err403 CannotCoverFee $ mconcat
                [ "I'm unable to adjust the given transaction to cover the "
                , "associated fee! In order to do so, I'd have to select one "
                , "or more additional inputs, but I can't do that without "
                , "increasing the size of the transaction beyond the "
                , "acceptable limit. Note that I am only missing "
                , showT missing, " Lovelace."
                ]

instance Buildable e => LiftHandler (ErrCreateUnsignedTx e) where
    handler = \case
        ErrCreateUnsignedTxNoSuchWallet e -> handler e
        ErrCreateUnsignedTxCoinSelection e -> handler e
        ErrCreateUnsignedTxFee e -> handler e

instance Buildable e => LiftHandler (ErrEstimateTxFee e) where
    handler = \case
        ErrEstimateTxFeeNoSuchWallet e -> handler e
        ErrEstimateTxFeeCoinSelection e -> handler e

instance LiftHandler ErrListUTxOStatistics where
    handler = \case
        ErrListUTxOStatisticsNoSuchWallet e -> handler e

instance LiftHandler ErrSignTx where
    handler = \case
        ErrSignTx (ErrKeyNotFoundForAddress addr) ->
            apiError err403 KeyNotFoundForAddress $ mconcat
                [ "I couldn't sign the given transaction: I haven't found the "
                , "corresponding private key for the following output address: "
                , pretty addr, ". Are you sure this address belongs to a known "
                , "wallet?"
                ]
        ErrSignTxNoSuchWallet e -> (handler e)
            { errHTTPCode = 410
            , errReasonPhrase = errReasonPhrase err410
            }
        ErrSignTxWithRootKey e@ErrWithRootKeyNoRootKey{} -> (handler e)
            { errHTTPCode = 410
            , errReasonPhrase = errReasonPhrase err410
            }
        ErrSignTxWithRootKey e@ErrWithRootKeyWrongPassphrase{} -> handler e

instance LiftHandler ErrDecodeSignedTx where
    handler = \case
        ErrDecodeSignedTxWrongPayload _ ->
            apiError err400 MalformedTxPayload $ mconcat
                [ "I couldn't verify that the payload has the correct binary "
                , "format. Therefore I couldn't send it to the node. Please "
                , "check the format and try again."
                ]
        ErrDecodeSignedTxNotSupported ->
            apiError err404 UnexpectedError $ mconcat
                [ "This endpoint is not supported by the backend currently "
                , "in use. Please try a different backend."
                ]

instance LiftHandler ErrSubmitExternalTx where
    handler = \case
        ErrSubmitExternalTxNetwork e -> case e of
            ErrPostTxNetworkUnreachable e' ->
                handler e'
            ErrPostTxBadRequest err ->
                apiError err500 CreatedInvalidTransaction $ mconcat
                    [ "That's embarrassing. It looks like I've created an "
                    , "invalid transaction that could not be parsed by the "
                    , "node. Here's an error message that may help with "
                    , "debugging: ", pretty err
                    ]
            ErrPostTxProtocolFailure err ->
                apiError err500 RejectedByCoreNode $ mconcat
                    [ "I successfully submitted a transaction, but "
                    , "unfortunately it was rejected by a relay. This could be "
                    , "because the fee was not large enough, or because the "
                    , "transaction conflicts with another transaction that "
                    , "uses one or more of the same inputs, or it may be due "
                    , "to some other reason. Here's an error message that may "
                    , "help with debugging: ", pretty err
                    ]
        ErrSubmitExternalTxDecode e -> (handler e)
            { errHTTPCode = 400
            , errReasonPhrase = errReasonPhrase err400
            }

instance LiftHandler ErrForgetPendingTx where
    handler = \case
        ErrForgetPendingTxNoSuchWallet e -> handler e
        ErrForgetPendingTxNoSuchTransaction tid ->
            apiError err404 NoSuchTransaction $ mconcat
                [ "I couldn't find a transaction with the given id: "
                , toText tid
                ]
        ErrForgetPendingTxTransactionIsNotPending tid ->
            apiError err404 TransactionNotPending $ mconcat
                [ "The transaction with id : ", toText tid,
                  " cannot be forgotten as it is not pending anymore."
                ]

instance LiftHandler ErrSubmitTx where
    handler = \case
        ErrSubmitTxNetwork e -> case e of
            ErrPostTxNetworkUnreachable e' ->
                handler e'
            ErrPostTxBadRequest err ->
                apiError err500 CreatedInvalidTransaction $ mconcat
                    [ "That's embarrassing. It looks like I've created an "
                    , "invalid transaction that could not be parsed by the "
                    , "node. Here's an error message that may help with "
                    , "debugging: ", pretty err
                    ]
            ErrPostTxProtocolFailure err ->
                apiError err500 RejectedByCoreNode $ mconcat
                    [ "I successfully submitted a transaction, but "
                    , "unfortunately it was rejected by a relay. This could be "
                    , "because the fee was not large enough, or because the "
                    , "transaction conflicts with another transaction that "
                    , "uses one or more of the same inputs, or it may be due "
                    , "to some other reason. Here's an error message that may "
                    , "help with debugging: ", pretty err
                    ]
        ErrSubmitTxNoSuchWallet e@ErrNoSuchWallet{} -> (handler e)
            { errHTTPCode = 410
            , errReasonPhrase = errReasonPhrase err410
            }

instance LiftHandler ErrNetworkUnavailable where
    handler = \case
        ErrNetworkUnreachable _err ->
            apiError err503 NetworkUnreachable $ mconcat
                [ "The node backend is unreachable at the moment. Trying again "
                , "in a bit might work."
                ]
        ErrNetworkInvalid n ->
            apiError err503 NetworkMisconfigured $ mconcat
                [ "The node backend is configured for the wrong network: "
                , n, "."
                ]

instance LiftHandler ErrUpdatePassphrase where
    handler = \case
        ErrUpdatePassphraseNoSuchWallet e -> handler e
        ErrUpdatePassphraseWithRootKey e  -> handler e

instance LiftHandler ErrListTransactions where
    handler = \case
        ErrListTransactionsNoSuchWallet e -> handler e
        ErrListTransactionsStartTimeLaterThanEndTime e -> handler e

instance LiftHandler ErrStartTimeLaterThanEndTime where
    handler err = apiError err400 StartTimeLaterThanEndTime $ mconcat
        [ "The specified start time '"
        , toText $ Iso8601Time $ errStartTime err
        , "' is later than the specified end time '"
        , toText $ Iso8601Time $ errEndTime err
        , "'."
        ]

instance LiftHandler ErrNetworkTip where
    handler = \case
        ErrNetworkTipNetworkUnreachable e -> handler e
        ErrNetworkTipNotFound -> apiError err503 NetworkTipNotFound $ mconcat
            [ "I couldn't get the current network tip at the moment. It's "
            , "probably because the node is down or not started yet. Retrying "
            , "in a bit might give better results!"
            ]

instance LiftHandler (Request, ServantErr) where
    handler (req, err@(ServantErr code _ body headers))
      | not (isJSON body) = case code of
        400 -> apiError err' BadRequest (utf8 body)
        404 -> apiError err' NotFound $ mconcat
            [ "I couldn't find the requested endpoint. If the endpoint "
            , "contains path parameters, please ensure they are well-formed, "
            , "otherwise I won't be able to route them correctly."
            ]
        405 -> apiError err' MethodNotAllowed $ mconcat
            [ "You've reached a known endpoint but I don't know how to handle "
            , "the HTTP method specified. Please double-check both the "
            , "endpoint and the method: one of them is likely to be incorrect "
            , "(for example: POST instead of PUT, or GET instead of POST...)."
            ]
        406 -> apiError err' NotAcceptable $ mconcat
            [ "It seems as though you don't accept 'application/json', but "
            , "unfortunately I only speak 'application/json'! Please "
            , "double-check your 'Accept' request header and make sure it's "
            , "set to 'application/json'."
            ]
        415 ->
            let cType =
                    if ["proxy", "transactions"] `isSubsequenceOf` pathInfo req
                        then "application/octet-stream"
                        else "application/json"
            in apiError err' UnsupportedMediaType $ mconcat
            [ "I'm really sorry but I only understand '", cType, "'. I need you "
            , "to tell me what language you're speaking in order for me to "
            , "understand your message. Please double-check your 'Content-Type' "
            , "request header and make sure it's set to '", cType, "'."
            ]
        _ -> apiError err' UnexpectedError $ mconcat
            [ "It looks like something unexpected went wrong. Unfortunately I "
            , "don't yet know how to handle this type of situation. Here's "
            , "some information about what happened: ", utf8 body
            ]
      | otherwise = err
      where
        utf8 = T.replace "\"" "'" . T.decodeUtf8 . BL.toStrict
        isJSON = isJust . Aeson.decode @Aeson.Value
        err' = err
            { errHeaders =
                ( hContentType
                , renderHeader $ contentType $ Proxy @JSON
                ) : headers
            }
