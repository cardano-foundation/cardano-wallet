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

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- API handlers and server using the underlying wallet layer to provide
-- endpoints reachable through HTTP.

module Cardano.Wallet.Api.Server
    (
    -- * Server Configuration
      Listen (..)
    , ListenError (..)
    , HostPreference
    , TlsConfiguration (..)

    -- * Server Setup
    , start
    , serve
    , withListeningSocket

    -- * ApiLayer
    , newApiLayer

    -- * Handlers
    , delegationFee
    , deleteTransaction
    , deleteWallet
    , getMigrationInfo
    , getNetworkClock
    , getNetworkInformation
    , getNetworkParameters
    , getUTxOsStatistics
    , getWallet
    , joinStakePool
    , listAddresses
    , listTransactions
    , getTransaction
    , listWallets
    , migrateWallet
    , postExternalTransaction
    , postIcarusWallet
    , postLedgerWallet
    , postRandomAddress
    , postRandomWallet
    , postRandomWalletFromXPrv
    , postTransaction
    , postTransactionFee
    , postTrezorWallet
    , postWallet
    , postShelleyWallet
    , postAccountWallet
    , putByronWalletPassphrase
    , putRandomAddress
    , putRandomAddresses
    , putWallet
    , putWalletPassphrase
    , quitStakePool
    , selectCoins

    -- * Internals
    , LiftHandler(..)
    , apiError
    , mkShelleyWallet
    , mkLegacyWallet
    , withLegacyLayer
    , withLegacyLayer'
    , rndStateChange
    , assignMigrationAddresses
    , withWorkerCtx
    , getCurrentEpoch

    -- * Workers
    , manageRewardBalance
    , idleWorker
    ) where

import Prelude

import Cardano.Address.Derivation
    ( XPrv, XPub )
import Cardano.Mnemonic
    ( SomeMnemonic )
import Cardano.Wallet
    ( ErrAdjustForFee (..)
    , ErrCannotJoin (..)
    , ErrCannotQuit (..)
    , ErrCoinSelection (..)
    , ErrCreateRandomAddress (..)
    , ErrDecodeSignedTx (..)
    , ErrFetchRewards (..)
    , ErrGetTransaction (..)
    , ErrImportAddress (..)
    , ErrImportRandomAddress (..)
    , ErrJoinStakePool (..)
    , ErrListPools (..)
    , ErrListTransactions (..)
    , ErrListUTxOStatistics (..)
    , ErrMkTx (..)
    , ErrNoSuchTransaction (..)
    , ErrNoSuchWallet (..)
    , ErrNotASequentialWallet (..)
    , ErrPostTx (..)
    , ErrQuitStakePool (..)
    , ErrReadChimericAccount (..)
    , ErrRemovePendingTx (..)
    , ErrSelectCoinsExternal (..)
    , ErrSelectForDelegation (..)
    , ErrSelectForMigration (..)
    , ErrSelectForPayment (..)
    , ErrSignDelegation (..)
    , ErrSignPayment (..)
    , ErrStartTimeLaterThanEndTime (..)
    , ErrSubmitExternalTx (..)
    , ErrSubmitTx (..)
    , ErrUTxOTooSmall (..)
    , ErrUpdatePassphrase (..)
    , ErrValidateSelection
    , ErrWalletAlreadyExists (..)
    , ErrWalletNotResponding (..)
    , ErrWithRootKey (..)
    , ErrWithdrawalNotWorth (..)
    , ErrWrongPassphrase (..)
    , FeeEstimation (..)
    , HasLogger
    , HasNetworkLayer
    , WalletLog
    , genesisData
    , manageRewardBalance
    , networkLayer
    )
import Cardano.Wallet.Api
    ( ApiLayer (..)
    , HasDBFactory
    , HasWorkerRegistry
    , dbFactory
    , workerRegistry
    )
import Cardano.Wallet.Api.Server.Tls
    ( TlsConfiguration (..), requireClientAuth )
import Cardano.Wallet.Api.Types
    ( AccountPostData (..)
    , AddressAmount (..)
    , ApiAccountPublicKey (..)
    , ApiAddress (..)
    , ApiBlockReference (..)
    , ApiByronWallet (..)
    , ApiByronWalletBalance (..)
    , ApiCoinSelection (..)
    , ApiCoinSelectionInput (..)
    , ApiEpochInfo (ApiEpochInfo)
    , ApiErrorCode (..)
    , ApiFee (..)
    , ApiMnemonicT (..)
    , ApiNetworkClock (..)
    , ApiNetworkInformation
    , ApiNetworkParameters (..)
    , ApiNetworkTip (..)
    , ApiPoolId (..)
    , ApiPostRandomAddressData (..)
    , ApiPutAddressesData (..)
    , ApiSelectCoinsData (..)
    , ApiT (..)
    , ApiTimeReference (..)
    , ApiTransaction (..)
    , ApiTxId (..)
    , ApiTxInput (..)
    , ApiTxMetadata (..)
    , ApiUtxoStatistics (..)
    , ApiWallet (..)
    , ApiWalletDelegation (..)
    , ApiWalletDelegationNext (..)
    , ApiWalletDelegationStatus (..)
    , ApiWalletMigrationInfo (..)
    , ApiWalletMigrationPostData (..)
    , ApiWalletPassphrase (..)
    , ApiWalletPassphraseInfo (..)
    , ApiWithdrawal (..)
    , ApiWithdrawalPostData (..)
    , ByronWalletFromXPrvPostData
    , ByronWalletPostData (..)
    , ByronWalletPutPassphraseData (..)
    , Iso8601Time (..)
    , KnownDiscovery (..)
    , MinWithdrawal (..)
    , PostExternalTransactionData (..)
    , PostTransactionData (..)
    , PostTransactionFeeData (..)
    , WalletBalance (..)
    , WalletOrAccountPostData (..)
    , WalletPostData (..)
    , WalletPutData (..)
    , WalletPutPassphraseData (..)
    , getApiMnemonicT
    , toApiNetworkParameters
    , toApiUtxoStatistics
    )
import Cardano.Wallet.DB
    ( DBFactory (..) )
import Cardano.Wallet.Network
    ( ErrCurrentNodeTip (..)
    , ErrNetworkUnavailable (..)
    , NetworkLayer
    , timeInterpreter
    )
import Cardano.Wallet.Primitive.AddressDerivation
    ( ChimericAccount (..)
    , DelegationAddress (..)
    , Depth (..)
    , DerivationType (..)
    , HardDerivation (..)
    , Index (..)
    , MkKeyFingerprint
    , NetworkDiscriminant (..)
    , Passphrase (..)
    , PaymentAddress (..)
    , SoftDerivation (..)
    , WalletKey (..)
    , deriveRewardAccount
    , digest
    , preparePassphrase
    , publicKey
    )
import Cardano.Wallet.Primitive.AddressDerivation.Byron
    ( ByronKey, mkByronKeyFromMasterKey )
import Cardano.Wallet.Primitive.AddressDerivation.Icarus
    ( IcarusKey )
import Cardano.Wallet.Primitive.AddressDerivation.Shelley
    ( ShelleyKey )
import Cardano.Wallet.Primitive.AddressDiscovery
    ( CompareDiscovery
    , GenChange (ArgGenChange)
    , IsOurs
    , IsOwned
    , KnownAddresses
    )
import Cardano.Wallet.Primitive.AddressDiscovery.Random
    ( RndState, mkRndState )
import Cardano.Wallet.Primitive.AddressDiscovery.Sequential
    ( SeqState (..)
    , defaultAddressPoolGap
    , mkSeqStateFromAccountXPub
    , mkSeqStateFromRootXPrv
    )
import Cardano.Wallet.Primitive.CoinSelection
    ( CoinSelection (..), changeBalance, inputBalance )
import Cardano.Wallet.Primitive.Model
    ( Wallet, availableBalance, currentTip, getState, totalBalance )
import Cardano.Wallet.Primitive.Slotting
    ( PastHorizonException
    , TimeInterpreter
    , currentEpoch
    , endTimeOfEpoch
    , epochSucc
    , firstSlotInEpoch
    , ongoingSlotAt
    , startTime
    , toSlotId
    )
import Cardano.Wallet.Primitive.SyncProgress
    ( SyncProgress (..), SyncTolerance, syncProgress )
import Cardano.Wallet.Primitive.Types
    ( Address
    , AddressState (..)
    , Block
    , BlockHeader (..)
    , Coin (..)
    , Hash (..)
    , NetworkParameters (..)
    , PassphraseScheme (..)
    , PoolId
    , PoolLifeCycleStatus (..)
    , SortOrder (..)
    , TransactionInfo (TransactionInfo)
    , Tx (..)
    , TxIn (..)
    , TxOut (..)
    , TxStatus (..)
    , UnsignedTx (..)
    , WalletId (..)
    , WalletMetadata (..)
    )
import Cardano.Wallet.Registry
    ( HasWorkerCtx (..)
    , MkWorker (..)
    , WorkerLog (..)
    , defaultWorkerAfter
    , workerResource
    )
import Cardano.Wallet.Transaction
    ( TransactionLayer )
import Cardano.Wallet.Unsafe
    ( unsafeRunExceptT )
import Control.Arrow
    ( second )
import Control.Concurrent
    ( threadDelay )
import Control.Concurrent.Async
    ( race_ )
import Control.Exception
    ( IOException, bracket, throwIO, try, tryJust )
import Control.Monad
    ( forM, forever, void, when, (>=>) )
import Control.Monad.Catch
    ( handle )
import Control.Monad.IO.Class
    ( MonadIO, liftIO )
import Control.Monad.Trans.Class
    ( lift )
import Control.Monad.Trans.Except
    ( ExceptT (..), runExceptT, throwE, withExceptT )
import Control.Monad.Trans.Maybe
    ( MaybeT (..) )
import Control.Tracer
    ( Tracer )
import Data.Aeson
    ( (.=) )
import Data.Bifunctor
    ( first )
import Data.Coerce
    ( coerce )
import Data.Function
    ( (&) )
import Data.Functor
    ( (<&>) )
import Data.Generics.Internal.VL.Lens
    ( Lens', view, (.~), (^.) )
import Data.Generics.Labels
    ()
import Data.List
    ( isInfixOf, isSubsequenceOf, sortOn )
import Data.Map.Strict
    ( Map )
import Data.Maybe
    ( fromMaybe, isJust )
import Data.Proxy
    ( Proxy (..) )
import Data.Quantity
    ( Quantity (..) )
import Data.Set
    ( Set )
import Data.Streaming.Network
    ( HostPreference, bindPortTCP, bindRandomPortTCP )
import Data.Text
    ( Text )
import Data.Text.Class
    ( FromText (..), ToText (..) )
import Data.Time
    ( UTCTime )
import Data.Time.Clock
    ( getCurrentTime )
import Data.Word
    ( Word32, Word64 )
import Fmt
    ( Buildable, pretty )
import GHC.Stack
    ( HasCallStack )
import Network.HTTP.Media.RenderHeader
    ( renderHeader )
import Network.HTTP.Types.Header
    ( hContentType )
import Network.Ntp
    ( NtpClient, getNtpStatus )
import Network.Socket
    ( Socket, close )
import Network.Wai
    ( Request, pathInfo )
import Network.Wai.Handler.Warp
    ( Port )
import Network.Wai.Middleware.Logging
    ( ApiLog (..), newApiLoggerSettings, obfuscateKeys, withApiLogger )
import Network.Wai.Middleware.ServerError
    ( handleRawError )
import Numeric.Natural
    ( Natural )
import Servant
    ( Application
    , JSON
    , NoContent (..)
    , contentType
    , err400
    , err403
    , err404
    , err409
    , err410
    , err500
    , err503
    , serve
    )
import Servant.Server
    ( Handler (..), ServerError (..), runHandler )
import System.IO.Error
    ( ioeGetErrorType
    , isAlreadyInUseError
    , isDoesNotExistError
    , isPermissionError
    , isUserError
    )
import System.Random
    ( getStdRandom, random )
import Type.Reflection
    ( Typeable )

import qualified Cardano.Wallet as W
import qualified Cardano.Wallet.Api.Types as Api
import qualified Cardano.Wallet.Network as NW
import qualified Cardano.Wallet.Primitive.AddressDerivation.Byron as Byron
import qualified Cardano.Wallet.Primitive.AddressDerivation.Icarus as Icarus
import qualified Cardano.Wallet.Primitive.Slotting as S
import qualified Cardano.Wallet.Primitive.Types as W
import qualified Cardano.Wallet.Registry as Registry
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WarpTLS as Warp

-- | How the server should listen for incoming requests.
data Listen
    = ListenOnPort Port
      -- ^ Listen on given TCP port
    | ListenOnRandomPort
      -- ^ Listen on an unused TCP port, selected at random
    deriving (Show, Eq)

-- | Start the application server, using the given settings and a bound socket.
start
    :: Warp.Settings
    -> Tracer IO ApiLog
    -> Maybe TlsConfiguration
    -> Socket
    -> Application
    -> IO ()
start settings tr tlsConfig socket application = do
    logSettings <- newApiLoggerSettings <&> obfuscateKeys (const sensitive)
    runSocket
        $ handleRawError (curry handler)
        $ withApiLogger tr logSettings
        application
  where
    runSocket :: Application -> IO ()
    runSocket = case tlsConfig of
        Nothing  -> Warp.runSettingsSocket settings socket
        Just tls -> Warp.runTLSSocket (requireClientAuth tls) settings socket

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
    :: HostPreference
    -- ^ Which host to bind.
    -> Listen
    -- ^ Whether to listen on a given port, or random port.
    -> (Either ListenError (Port, Socket) -> IO a)
    -- ^ Action to run with listening socket.
    -> IO a
withListeningSocket hostPreference portOpt = bracket acquire release
  where
    acquire = tryJust handleErr $ case portOpt of
        ListenOnPort port -> (port,) <$> bindPortTCP port hostPreference
        ListenOnRandomPort -> bindRandomPortTCP hostPreference
    release (Right (_, socket)) = liftIO $ close socket
    release (Left _) = pure ()
    handleErr = ioToListenError hostPreference portOpt

data ListenError
    = ListenErrorAddressAlreadyInUse (Maybe Port)
    | ListenErrorOperationNotPermitted
    | ListenErrorHostDoesNotExist HostPreference
    | ListenErrorInvalidAddress HostPreference
    deriving (Show, Eq)

ioToListenError :: HostPreference -> Listen -> IOException -> Maybe ListenError
ioToListenError hostPreference portOpt e
    -- A socket is already listening on that address and port
    | isAlreadyInUseError e =
        Just (ListenErrorAddressAlreadyInUse (listenPort portOpt))
    -- Usually caused by trying to listen on a privileged port
    | isPermissionError e =
        Just ListenErrorOperationNotPermitted
    -- Bad hostname -- Linux and Darwin
    | isDoesNotExistError e =
        Just (ListenErrorHostDoesNotExist hostPreference)
    -- Bad hostname (bind: WSAEOPNOTSUPP) -- Windows
    | isUserError e && (hasDescription "11001" || hasDescription "10045") =
        Just (ListenErrorHostDoesNotExist hostPreference)
    -- Address is valid, but can't be used for listening -- Linux
    | show (ioeGetErrorType e) == "invalid argument" =
        Just (ListenErrorInvalidAddress hostPreference)
    -- Address is valid, but can't be used for listening -- Darwin
    | show (ioeGetErrorType e) == "unsupported operation" =
        Just (ListenErrorInvalidAddress hostPreference)
    -- Address is valid, but can't be used for listening -- Windows
    | isOtherError e &&
      (hasDescription "WSAEINVAL" || hasDescription "WSAEADDRNOTAVAIL") =
        Just (ListenErrorInvalidAddress hostPreference)
    -- Listening on an unavailable or privileged port -- Windows
    | isOtherError e && hasDescription "WSAEACCESS" =
        Just (ListenErrorAddressAlreadyInUse (listenPort portOpt))
    | otherwise =
        Nothing
  where
    listenPort (ListenOnPort port) = Just port
    listenPort ListenOnRandomPort = Nothing

    isOtherError ex = show (ioeGetErrorType ex) == "failed"
    hasDescription text = text `isInfixOf` show e

{-------------------------------------------------------------------------------
                              Wallet Constructors
-------------------------------------------------------------------------------}

type MkApiWallet ctx s w
    =  ctx
    -> WalletId
    -> Wallet s
    -> WalletMetadata
    -> Set Tx
    -> SyncProgress
    -> Handler w

--------------------- Shelley
postWallet
    :: forall ctx s t k n.
        ( s ~ SeqState n k
        , ctx ~ ApiLayer s t k
        , SoftDerivation k
        , MkKeyFingerprint k (Proxy n, k 'AddressK XPub)
        , MkKeyFingerprint k Address
        , WalletKey k
        , Bounded (Index (AddressIndexDerivationType k) 'AddressK)
        , HasDBFactory s k ctx
        , HasWorkerRegistry s k ctx
        , IsOurs s ChimericAccount
        , Typeable s
        , Typeable n
        )
    => ctx
    -> ((SomeMnemonic, Maybe SomeMnemonic) -> Passphrase "encryption" -> k 'RootK XPrv)
    -> (XPub -> k 'AccountK XPub)
    -> WalletOrAccountPostData
    -> Handler ApiWallet
postWallet ctx generateKey liftKey (WalletOrAccountPostData body) = case body of
    Left  body' ->
        postShelleyWallet ctx generateKey body'
    Right body' ->
        postAccountWallet ctx mkShelleyWallet liftKey
            (W.manageRewardBalance @_ @_ @_ @_ (Proxy @n)) body'

postShelleyWallet
    :: forall ctx s t k n.
        ( s ~ SeqState n k
        , ctx ~ ApiLayer s t k
        , SoftDerivation k
        , MkKeyFingerprint k (Proxy n, k 'AddressK XPub)
        , MkKeyFingerprint k Address
        , WalletKey k
        , Bounded (Index (AddressIndexDerivationType k) 'AddressK)
        , HasDBFactory s k ctx
        , HasWorkerRegistry s k ctx
        , IsOurs s ChimericAccount
        , Typeable s
        , Typeable n
        )
    => ctx
    -> ((SomeMnemonic, Maybe SomeMnemonic) -> Passphrase "encryption" -> k 'RootK XPrv)
    -> WalletPostData
    -> Handler ApiWallet
postShelleyWallet ctx generateKey body = do
    let state = mkSeqStateFromRootXPrv (rootXPrv, pwd) g
    void $ liftHandler $ initWorker @_ @s @k ctx wid
        (\wrk -> W.createWallet  @(WorkerCtx ctx) @s @k wrk wid wName state)
        (\wrk -> W.restoreWallet @(WorkerCtx ctx) @s @t @k wrk wid)
        (\wrk -> W.manageRewardBalance @(WorkerCtx ctx) @s @t @k (Proxy @n) wrk wid)
    withWorkerCtx @_ @s @k ctx wid liftE liftE $ \wrk -> liftHandler $
        W.attachPrivateKeyFromPwd @_ @s @k wrk wid (rootXPrv, pwd)
    fst <$> getWallet ctx (mkShelleyWallet @_ @s @t @k) (ApiT wid)
  where
    seed = getApiMnemonicT (body ^. #mnemonicSentence)
    secondFactor = getApiMnemonicT <$> (body ^. #mnemonicSecondFactor)
    pwd = preparePassphrase EncryptWithPBKDF2 $ getApiT (body ^. #passphrase)
    rootXPrv = generateKey (seed, secondFactor) pwd
    g = maybe defaultAddressPoolGap getApiT (body ^. #addressPoolGap)
    wid = WalletId $ digest $ publicKey rootXPrv
    wName = getApiT (body ^. #name)

postAccountWallet
    :: forall ctx s t k n w.
        ( s ~ SeqState n k
        , ctx ~ ApiLayer s t k
        , SoftDerivation k
        , MkKeyFingerprint k (Proxy n, k 'AddressK XPub)
        , MkKeyFingerprint k Address
        , WalletKey k
        , HasWorkerRegistry s k ctx
        , IsOurs s ChimericAccount
        )
    => ctx
    -> MkApiWallet ctx s w
    -> (XPub -> k 'AccountK XPub)
    -> (WorkerCtx ctx -> WalletId -> IO ())
        -- ^ Action to run concurrently with restore action
    -> AccountPostData
    -> Handler w
postAccountWallet ctx mkWallet liftKey coworker body = do
    let state = mkSeqStateFromAccountXPub (liftKey accXPub) g
    void $ liftHandler $ initWorker @_ @s @k ctx wid
        (\wrk -> W.createWallet  @(WorkerCtx ctx) @s @k wrk wid wName state)
        (\wrk -> W.restoreWallet @(WorkerCtx ctx) @s @t @k wrk wid)
        (`coworker` wid)
    fst <$> getWallet ctx mkWallet (ApiT wid)
  where
    g = maybe defaultAddressPoolGap getApiT (body ^. #addressPoolGap)
    wName = getApiT (body ^. #name)
    (ApiAccountPublicKey accXPubApiT) =  body ^. #accountPublicKey
    accXPub = getApiT accXPubApiT
    wid = WalletId $ digest (liftKey accXPub)

mkShelleyWallet
    :: forall ctx s t k n.
        ( ctx ~ ApiLayer s t k
        , s ~ SeqState n k
        , IsOurs s Address
        , IsOurs s ChimericAccount
        , HasWorkerRegistry s k ctx
        )
    => MkApiWallet ctx s ApiWallet
mkShelleyWallet ctx wid cp meta pending progress = do
    reward <- withWorkerCtx @_ @s @k ctx wid liftE liftE $ \wrk ->
        -- never fails - returns zero if balance not found
        liftIO $ fmap fromIntegral <$> W.fetchRewardBalance @_ @s @k wrk wid


    apiDelegation <- liftIO $ toApiWalletDelegation (meta ^. #delegation)
    tip' <- liftIO $ getWalletTip ti cp
    pure ApiWallet
        { addressPoolGap = ApiT $ getState cp ^. #externalPool . #gap
        , balance = ApiT $ WalletBalance
            { available = Quantity $ availableBalance pending cp
            , total = Quantity $ totalBalance pending reward cp
            , reward
            }
        , delegation = apiDelegation
        , id = ApiT wid
        , name = ApiT $ meta ^. #name
        , passphrase = ApiWalletPassphraseInfo
            <$> fmap (view #lastUpdatedAt) (meta ^. #passphraseInfo)
        , state = ApiT progress
        , tip = tip'
        }
  where
    ti :: TimeInterpreter IO
    ti = timeInterpreter (ctx ^. networkLayer @t)

    -- This may fail
    -- 1. In Byron when running Byron;Shelley
    -- 2. In Shelley when running Byron;Shelley;SomethingElse
    --
    -- But:
    -- 1. We should never see a delegation certificate in Byron (unless under
    -- very intricate and rare conditions involving roll-backs and
    -- raceconditions right at the fork?)
    -- 2. We are currently only targeting Byron;Shelley.
    --
    -- so it shouldn't be a problem.
    toApiEpochInfo ep = do
        time <- ti (firstSlotInEpoch ep >>= startTime)
        return $ ApiEpochInfo (ApiT ep) time

    toApiWalletDelegation W.WalletDelegation{active,next} = do
        apiNext <- forM next $ \W.WalletDelegationNext{status,changesAt} -> do
            info <- toApiEpochInfo changesAt
            return $ toApiWalletDelegationNext (Just info) status

        return $ ApiWalletDelegation
            { active = toApiWalletDelegationNext Nothing active
            , next = apiNext
            }

    toApiWalletDelegationNext mepochInfo = \case
        W.Delegating pid -> ApiWalletDelegationNext
            { status = Delegating
            , target = Just (ApiT pid)
            , changesAt = mepochInfo
            }

        W.NotDelegating -> ApiWalletDelegationNext
            { status = NotDelegating
            , target = Nothing
            , changesAt = mepochInfo
            }


--------------------- Legacy

postLegacyWallet
    :: forall ctx s t k.
        ( ctx ~ ApiLayer s t k
        , KnownDiscovery s
        , IsOurs s ChimericAccount
        , IsOurs s Address
        , HasNetworkLayer t ctx
        , WalletKey k
        )
    => ctx
        -- ^ Surrounding Context
    -> (k 'RootK XPrv, Passphrase "encryption")
        -- ^ Root key
    -> (  WorkerCtx ctx
       -> WalletId
       -> ExceptT ErrWalletAlreadyExists IO WalletId
       )
        -- ^ How to create this legacy wallet
    -> Handler ApiByronWallet
postLegacyWallet ctx (rootXPrv, pwd) createWallet = do
    void $ liftHandler $ initWorker @_ @s @k ctx wid (`createWallet` wid)
        (\wrk -> W.restoreWallet @(WorkerCtx ctx) @s @t @k wrk wid)
        (`idleWorker` wid)
    withWorkerCtx ctx wid liftE liftE $ \wrk -> liftHandler $
        W.attachPrivateKeyFromPwd wrk wid (rootXPrv, pwd)
    fst <$> getWallet ctx (mkLegacyWallet @_ @_ @_ @t) (ApiT wid)
  where
    wid = WalletId $ digest $ publicKey rootXPrv

mkLegacyWallet
    :: forall ctx s k t.
        ( HasWorkerRegistry s k ctx
        , HasDBFactory s k ctx
        , KnownDiscovery s
        , HasNetworkLayer t ctx
        , IsOurs s Address
        , IsOurs s ChimericAccount
        )
    => ctx
    -> WalletId
    -> Wallet s
    -> WalletMetadata
    -> Set Tx
    -> SyncProgress
    -> Handler ApiByronWallet
mkLegacyWallet ctx wid cp meta pending progress = do
    -- NOTE
    -- Legacy wallets imported through via XPrv might have an empty passphrase
    -- set. The passphrase is empty from a client perspective, but in practice
    -- it still exists (it is a CBOR-serialized empty bytestring!).
    --
    -- Therefore, if we detect an empty passphrase, we choose to return the
    -- metadata as if no passphrase was set, so that clients can react
    -- appropriately.
    pwdInfo <- case meta ^. #passphraseInfo of
        Nothing ->
            pure Nothing
        Just (W.WalletPassphraseInfo time EncryptWithPBKDF2) ->
            pure $ Just $ ApiWalletPassphraseInfo time
        Just (W.WalletPassphraseInfo time EncryptWithScrypt) -> do
            withWorkerCtx @_ @s @k ctx wid liftE liftE $
                matchEmptyPassphrase >=> \case
                    Right{} -> pure Nothing
                    Left{} -> pure $ Just $ ApiWalletPassphraseInfo time

    tip' <- liftIO $ getWalletTip ti cp
    pure ApiByronWallet
        { balance = ApiByronWalletBalance
            { available = Quantity $ availableBalance pending cp
            , total = Quantity $ totalBalance pending (Quantity 0) cp
            }
        , id = ApiT wid
        , name = ApiT $ meta ^. #name
        , passphrase = pwdInfo
        , state = ApiT progress
        , tip = tip'
        , discovery = knownDiscovery @s
        }
  where
    ti :: TimeInterpreter IO
    ti = timeInterpreter (ctx ^. networkLayer @t)

    matchEmptyPassphrase
        :: WorkerCtx ctx
        -> Handler (Either ErrWithRootKey ())
    matchEmptyPassphrase wrk = liftIO $ runExceptT $
        W.withRootKey @_ @s @k wrk wid mempty Prelude.id (\_ _ -> pure ())

postRandomWallet
    :: forall ctx s t k n.
        ( ctx ~ ApiLayer s t k
        , s ~ RndState n
        , k ~ ByronKey
        )
    => ctx
    -> ByronWalletPostData '[12,15,18,21,24]
    -> Handler ApiByronWallet
postRandomWallet ctx body = do
    s <- liftIO $ mkRndState rootXPrv <$> getStdRandom random
    postLegacyWallet ctx (rootXPrv, pwd) $ \wrk wid ->
        W.createWallet @(WorkerCtx ctx) @s @k wrk wid wName s
  where
    wName = getApiT (body ^. #name)
    pwd   = preparePassphrase EncryptWithPBKDF2 $ getApiT (body ^. #passphrase)
    rootXPrv = Byron.generateKeyFromSeed seed pwd
      where seed = getApiMnemonicT (body ^. #mnemonicSentence)

postRandomWalletFromXPrv
    :: forall ctx s t k n.
        ( ctx ~ ApiLayer s t k
        , s ~ RndState n
        , k ~ ByronKey
        , HasNetworkLayer t ctx
        )
    => ctx
    -> ByronWalletFromXPrvPostData
    -> Handler ApiByronWallet
postRandomWalletFromXPrv ctx body = do
    s <- liftIO $ mkRndState byronKey <$> getStdRandom random
    void $ liftHandler $ initWorker @_ @s @k ctx wid
        (\wrk -> W.createWallet @(WorkerCtx ctx) @s @k wrk wid wName s)
        (\wrk -> W.restoreWallet @(WorkerCtx ctx) @s @t @k wrk wid)
        (`idleWorker` wid)
    withWorkerCtx ctx wid liftE liftE $ \wrk -> liftHandler $
        W.attachPrivateKeyFromPwdHash wrk wid (byronKey, pwd)
    fst <$> getWallet ctx (mkLegacyWallet @_ @_ @_ @t) (ApiT wid)
  where
    wName = getApiT (body ^. #name)
    pwd   = getApiT (body ^. #passphraseHash)
    masterKey = getApiT (body ^. #encryptedRootPrivateKey)
    byronKey = mkByronKeyFromMasterKey masterKey
    wid = WalletId $ digest $ publicKey byronKey

postIcarusWallet
    :: forall ctx s t k n.
        ( ctx ~ ApiLayer s t k
        , s ~ SeqState n k
        , k ~ IcarusKey
        , HasWorkerRegistry s k ctx
        , PaymentAddress n IcarusKey
        )
    => ctx
    -> ByronWalletPostData '[12,15,18,21,24]
    -> Handler ApiByronWallet
postIcarusWallet ctx body = do
    postLegacyWallet ctx (rootXPrv, pwd) $ \wrk wid ->
        W.createIcarusWallet @(WorkerCtx ctx) @s @k wrk wid wName (rootXPrv, pwd)
  where
    wName = getApiT (body ^. #name)
    pwd   = preparePassphrase EncryptWithPBKDF2 $ getApiT (body ^. #passphrase)
    rootXPrv = Icarus.generateKeyFromSeed seed pwd
      where seed = getApiMnemonicT (body ^. #mnemonicSentence)

postTrezorWallet
    :: forall ctx s t k n.
        ( ctx ~ ApiLayer s t k
        , s ~ SeqState n k
        , k ~ IcarusKey
        , HasWorkerRegistry s k ctx
        , PaymentAddress n IcarusKey
        )
    => ctx
    -> ByronWalletPostData '[12,15,18,21,24]
    -> Handler ApiByronWallet
postTrezorWallet ctx body = do
    postLegacyWallet ctx (rootXPrv, pwd) $ \wrk wid ->
        W.createIcarusWallet @(WorkerCtx ctx) @s @k wrk wid wName (rootXPrv, pwd)
  where
    wName = getApiT (body ^. #name)
    pwd   = preparePassphrase EncryptWithPBKDF2 $ getApiT (body ^. #passphrase)
    rootXPrv = Icarus.generateKeyFromSeed seed pwd
      where seed = getApiMnemonicT (body ^. #mnemonicSentence)

postLedgerWallet
    :: forall ctx s t k n.
        ( ctx ~ ApiLayer s t k
        , s ~ SeqState n k
        , k ~ IcarusKey
        , HasWorkerRegistry s k ctx
        , PaymentAddress n IcarusKey
        )
    => ctx
    -> ByronWalletPostData '[12,15,18,21,24]
    -> Handler ApiByronWallet
postLedgerWallet ctx body = do
    postLegacyWallet ctx (rootXPrv, pwd) $ \wrk wid ->
        W.createIcarusWallet @(WorkerCtx ctx) @s @k wrk wid wName (rootXPrv, pwd)
  where
    wName = getApiT (body ^. #name)
    pwd   = preparePassphrase EncryptWithPBKDF2 $ getApiT (body ^. #passphrase)
    rootXPrv = Icarus.generateKeyFromHardwareLedger mw pwd
      where mw = getApiMnemonicT (body ^. #mnemonicSentence)

{-------------------------------------------------------------------------------
                             ApiLayer Discrimination
-------------------------------------------------------------------------------}

-- Legacy wallets like 'Byron Random' and 'Icarus Sequential' are handled
-- through the same API endpoints. However, they rely on different contexts.
-- Since they have identical ids, we actually lookup both contexts in sequence.
withLegacyLayer
    :: forall byron icarus t n a.
        ( byron ~ ApiLayer (RndState n) t ByronKey
        , icarus ~ ApiLayer (SeqState n IcarusKey) t IcarusKey
        )
    => ApiT WalletId
    -> (byron, Handler a)
    -> (icarus, Handler a)
    -> Handler a
withLegacyLayer (ApiT wid) (byron, withByron) (icarus, withIcarus) =
    withLegacyLayer' (ApiT wid)
        (byron, withByron, liftE)
        (icarus, withIcarus, liftE)

-- | Like 'withLegacyLayer' but allow passing a custom handler for handling dead
-- workers.
withLegacyLayer'
    :: forall byron icarus t n a.
        ( byron ~ ApiLayer (RndState n) t ByronKey
        , icarus ~ ApiLayer (SeqState n IcarusKey) t IcarusKey
        )
    => ApiT WalletId
    -> (byron, Handler a, ErrWalletNotResponding -> Handler a)
    -> (icarus, Handler a, ErrWalletNotResponding -> Handler a)
    -> Handler a
withLegacyLayer' (ApiT wid)
  (byron, withByron, deadByron)
  (icarus, withIcarus, deadIcarus)
    = tryByron (const $ tryIcarus liftE deadIcarus) deadByron
  where
    tryIcarus onMissing onNotResponding = withWorkerCtx @_
        @(SeqState n IcarusKey)
        @IcarusKey
        icarus
        wid
        onMissing
        onNotResponding
        (const withIcarus)

    tryByron onMissing onNotResponding = withWorkerCtx @_
        @(RndState n)
        @ByronKey
        byron
        wid
        onMissing
        onNotResponding
        (const withByron)

{-------------------------------------------------------------------------------
                                   Wallets
-------------------------------------------------------------------------------}

deleteWallet
    :: forall ctx s t k.
        ( ctx ~ ApiLayer s t k
        )
    => ctx
    -> ApiT WalletId
    -> Handler NoContent
deleteWallet ctx (ApiT wid) =
    withWorkerCtx @_ @s @k ctx wid liftE liftE $ \_ -> do
        liftIO $ Registry.unregister re wid
        liftIO $ removeDatabase df wid
        return NoContent
  where
    re = ctx ^. workerRegistry @s @k
    df = ctx ^. dbFactory @s @k

getWallet
    :: forall ctx s t k apiWallet.
        ( ctx ~ ApiLayer s t k
        , HasWorkerRegistry s k ctx
        , HasDBFactory s k ctx
        )
    => ctx
    -> MkApiWallet ctx s apiWallet
    -> ApiT WalletId
    -> Handler (apiWallet, UTCTime)
getWallet ctx mkApiWallet (ApiT wid) = do
    withWorkerCtx @_ @s @k ctx wid liftE whenNotResponding whenAlive
  where
    df = ctx ^. dbFactory @s @k

    whenAlive wrk = do
        (cp, meta, pending) <- liftHandler $ W.readWallet @_ @s @k wrk wid
        progress <- liftIO $ W.walletSyncProgress @_ @_ @t ctx cp
        (, meta ^. #creationTime) <$> mkApiWallet ctx wid cp meta pending progress

    whenNotResponding _ = Handler $ ExceptT $ withDatabase df wid $ \db -> runHandler $ do
        let wrk = hoistResource db (MsgFromWorker wid) ctx
        (cp, meta, pending) <- liftHandler $ W.readWallet @_ @s @k wrk wid
        (, meta ^. #creationTime) <$> mkApiWallet ctx wid cp meta pending NotResponding

listWallets
    :: forall ctx s t k apiWallet.
        ( ctx ~ ApiLayer s t k
        )
    => ctx
    -> MkApiWallet ctx s apiWallet
    -> Handler [(apiWallet, UTCTime)]
listWallets ctx mkApiWallet = do
    wids <- liftIO $ listDatabases df
    sortOn snd <$> mapM (getWallet ctx mkApiWallet) (ApiT <$> wids)
  where
    df = ctx ^. dbFactory @s @k

putWallet
    :: forall ctx s t k apiWallet.
        ( ctx ~ ApiLayer s t k
        )
    => ctx
    -> MkApiWallet ctx s apiWallet
    -> ApiT WalletId
    -> WalletPutData
    -> Handler apiWallet
putWallet ctx mkApiWallet (ApiT wid) body = do
    case body ^. #name of
        Nothing ->
            return ()
        Just (ApiT wName) -> withWorkerCtx ctx wid liftE liftE $ \wrk -> do
            liftHandler $ W.updateWallet wrk wid (modify wName)
    fst <$> getWallet ctx mkApiWallet (ApiT wid)
  where
    modify :: W.WalletName -> WalletMetadata -> WalletMetadata
    modify wName meta = meta { name = wName }

putWalletPassphrase
    :: forall ctx s t k.
        ( WalletKey k
        , ctx ~ ApiLayer s t k
        )
    => ctx
    -> ApiT WalletId
    -> WalletPutPassphraseData
    -> Handler NoContent
putWalletPassphrase ctx (ApiT wid) body = do
    let (WalletPutPassphraseData (ApiT old) (ApiT new)) = body
    withWorkerCtx ctx wid liftE liftE $ \wrk -> liftHandler $
        W.updateWalletPassphrase wrk wid (old, new)
    return NoContent

putByronWalletPassphrase
    :: forall ctx s t k.
        ( WalletKey k
        , ctx ~ ApiLayer s t k
        )
    => ctx
    -> ApiT WalletId
    -> ByronWalletPutPassphraseData
    -> Handler NoContent
putByronWalletPassphrase ctx (ApiT wid) body = do
    let (ByronWalletPutPassphraseData oldM (ApiT new)) = body
    withWorkerCtx ctx wid liftE liftE $ \wrk -> liftHandler $ do
        let old = maybe mempty (coerce . getApiT) oldM
        W.updateWalletPassphrase wrk wid (old, new)
    return NoContent

getUTxOsStatistics
    :: forall ctx s t k.
        ( ctx ~ ApiLayer s t k
        )
    => ctx
    -> ApiT WalletId
    -> Handler ApiUtxoStatistics
getUTxOsStatistics ctx (ApiT wid) = do
    stats <- withWorkerCtx ctx wid liftE liftE $ \wrk -> liftHandler $
        W.listUtxoStatistics wrk wid
    return $ toApiUtxoStatistics stats

{-------------------------------------------------------------------------------
                                  Coin Selections
-------------------------------------------------------------------------------}

selectCoins
    :: forall ctx s t k n.
        ( Buildable (ErrValidateSelection t)
        , s ~ SeqState n k
        , SoftDerivation k
        , ctx ~ ApiLayer s t k
        )
    => ctx
    -> ArgGenChange s
    -> ApiT WalletId
    -> ApiSelectCoinsData n
    -> Handler (ApiCoinSelection n)
selectCoins ctx gen (ApiT wid) body =
    fmap mkApiCoinSelection
    $ withWorkerCtx ctx wid liftE liftE
    $ \wrk -> do
        -- TODO:
        -- Allow representing withdrawals as part of external coin selections.
        let withdrawal = Quantity 0
        let outs = coerceCoin <$> body ^. #payments
        liftHandler $ W.selectCoinsExternal @_ @s @t @k wrk wid gen outs withdrawal Nothing

{-------------------------------------------------------------------------------
                                    Addresses
-------------------------------------------------------------------------------}

postRandomAddress
    :: forall ctx s t k n.
        ( s ~ RndState n
        , k ~ ByronKey
        , ctx ~ ApiLayer s t k
        , PaymentAddress n ByronKey
        )
    => ctx
    -> ApiT WalletId
    -> ApiPostRandomAddressData
    -> Handler (ApiAddress n)
postRandomAddress ctx (ApiT wid) body = do
    let pwd = coerce $ getApiT $ body ^. #passphrase
    let mix = getApiT <$> (body ^. #addressIndex)
    addr <- withWorkerCtx ctx wid liftE liftE
        $ \wrk -> liftHandler $ W.createRandomAddress @_ @s @k @n wrk wid pwd mix
    pure $ coerceAddress (addr, Unused)
  where
    coerceAddress (a, s) = ApiAddress (ApiT a, Proxy @n) (ApiT s)

putRandomAddress
    :: forall ctx s t k n.
        ( s ~ RndState n
        , k ~ ByronKey
        , ctx ~ ApiLayer s t k
        )
    => ctx
    -> ApiT WalletId
    -> (ApiT Address, Proxy n)
    -> Handler NoContent
putRandomAddress ctx (ApiT wid) (ApiT addr, _proxy)  = do
    withWorkerCtx ctx wid liftE liftE
        $ \wrk -> liftHandler $ W.importRandomAddresses @_ @s @k wrk wid [addr]
    pure NoContent

putRandomAddresses
    :: forall ctx s t k n.
        ( s ~ RndState n
        , k ~ ByronKey
        , ctx ~ ApiLayer s t k
        )
    => ctx
    -> ApiT WalletId
    -> ApiPutAddressesData n
    -> Handler NoContent
putRandomAddresses ctx (ApiT wid) (ApiPutAddressesData addrs)  = do
    withWorkerCtx ctx wid liftE liftE
        $ \wrk -> liftHandler $ W.importRandomAddresses @_ @s @k wrk wid addrs'
    pure NoContent
  where
    addrs' = map (getApiT . fst) addrs

listAddresses
    :: forall ctx s t k n.
        ( ctx ~ ApiLayer s t k
        , CompareDiscovery s
        , KnownAddresses s
        )
    => ctx
    -> (s -> Address -> Maybe Address)
    -> ApiT WalletId
    -> Maybe (ApiT AddressState)
    -> Handler [ApiAddress n]
listAddresses ctx normalize (ApiT wid) stateFilter = do
    addrs <- withWorkerCtx ctx wid liftE liftE $ \wrk -> liftHandler $
        W.listAddresses @_ @s @k wrk wid normalize
    return $ coerceAddress <$> filter filterCondition addrs
  where
    filterCondition :: (Address, AddressState) -> Bool
    filterCondition = case stateFilter of
        Nothing -> const True
        Just (ApiT s) -> (== s) . snd
    coerceAddress (a, s) = ApiAddress (ApiT a, Proxy @n) (ApiT s)


{-------------------------------------------------------------------------------
                                    Transactions
-------------------------------------------------------------------------------}

postTransaction
    :: forall ctx s t k n.
        ( Buildable (ErrValidateSelection t)
        , GenChange s
        , HasNetworkLayer t ctx
        , IsOurs s ChimericAccount
        , IsOwned s k
        , ctx ~ ApiLayer s t k
        , HardDerivation k
        , Bounded (Index (AddressIndexDerivationType k) 'AddressK)
        , WalletKey k
        , Typeable s
        , Typeable n
        )
    => ctx
    -> ArgGenChange s
    -> ApiT WalletId
    -> PostTransactionData n
    -> Handler (ApiTransaction n)
postTransaction ctx genChange (ApiT wid) body = do
    let pwd = coerce $ getApiT $ body ^. #passphrase
    let outs = coerceCoin <$> (body ^. #payments)
    let md = getApiT <$> body ^. #metadata

    let selfRewardCredentials (rootK, pwdP) =
            (getRawKey $ deriveRewardAccount @k pwdP rootK, pwdP)

    (selection, credentials) <- withWorkerCtx ctx wid liftE liftE $ \wrk -> do
        (wdrl, credentials) <- case body ^. #withdrawal of
            Nothing ->
                pure (Quantity 0, selfRewardCredentials)

            Just SelfWithdrawal -> do
                acct <- liftHandler $ W.readChimericAccount @_ @s @k @n wrk wid
                wdrl <- liftHandler $ W.queryRewardBalance @_ @t wrk acct
                (, selfRewardCredentials)
                    <$> liftIO (W.readNextWithdrawal @_ @s @t @k wrk wid wdrl)

            Just (ExternalWithdrawal (ApiMnemonicT mw)) -> do
                let (xprv, acct) = W.someChimericAccount @ShelleyKey mw
                wdrl <- liftHandler (W.queryRewardBalance @_ @t wrk acct)
                    >>= liftIO . W.readNextWithdrawal @_ @s @t @k wrk wid
                when (wdrl == Quantity 0) $ do
                    liftHandler $ throwE ErrWithdrawalNotWorth
                pure (wdrl, const (xprv, mempty))

        selection <- liftHandler $ W.selectCoinsForPayment @_ @s @t wrk wid outs wdrl md
        pure (selection, credentials)

    (tx, meta, time, wit) <- withWorkerCtx ctx wid liftE liftE $ \wrk -> liftHandler $
        W.signPayment @_ @s @t @k wrk wid genChange credentials pwd md selection

    withWorkerCtx ctx wid liftE liftE $ \wrk -> liftHandler $
        W.submitTx @_ @s @t @k wrk wid (tx, meta, wit)

    liftIO $ mkApiTransaction
        ti
        (txId tx)
        (fmap Just <$> selection ^. #inputs)
        (tx ^. #outputs)
        (tx ^. #withdrawals)
        (meta, time)
        (tx ^. #metadata)
        #pendingSince
  where
    ti :: TimeInterpreter IO
    ti = timeInterpreter (ctx ^. networkLayer @t)

deleteTransaction
    :: forall ctx s t k. ctx ~ ApiLayer s t k
    => ctx
    -> ApiT WalletId
    -> ApiTxId
    -> Handler NoContent
deleteTransaction ctx (ApiT wid) (ApiTxId (ApiT (tid))) = do
    withWorkerCtx ctx wid liftE liftE $ \wrk -> liftHandler $
        W.forgetPendingTx wrk wid tid
    return NoContent

listTransactions
    :: forall ctx s t k n. (ctx ~ ApiLayer s t k)
    => ctx
    -> ApiT WalletId
    -> Maybe MinWithdrawal
    -> Maybe Iso8601Time
    -> Maybe Iso8601Time
    -> Maybe (ApiT SortOrder)
    -> Handler [ApiTransaction n]
listTransactions ctx (ApiT wid) mMinWithdrawal mStart mEnd mOrder = do
    txs <- withWorkerCtx ctx wid liftE liftE $ \wrk -> liftHandler $
        W.listTransactions @_ @_ @_ @t wrk wid
            (Quantity . getMinWithdrawal <$> mMinWithdrawal)
            (getIso8601Time <$> mStart)
            (getIso8601Time <$> mEnd)
            (maybe defaultSortOrder getApiT mOrder)
    liftIO $ mapM (mkApiTransactionFromInfo ti) txs
  where
    defaultSortOrder :: SortOrder
    defaultSortOrder = Descending

    -- In the Shelley of Byron;Shelley this is fine, but otherwise,
    -- supplying a time range in the future may cause a err500.
    ti :: TimeInterpreter IO
    ti = timeInterpreter (ctx ^. networkLayer @t)

getTransaction
    :: forall ctx s t k n. (ctx ~ ApiLayer s t k)
    => ctx
    -> ApiT WalletId
    -> ApiTxId
    -> Handler (ApiTransaction n)
getTransaction ctx (ApiT wid) (ApiTxId (ApiT (tid))) = do
    tx <- withWorkerCtx ctx wid liftE liftE $ \wrk -> liftHandler $
        W.getTransaction wrk wid tid
    liftIO $ mkApiTransactionFromInfo ti tx
  where
    ti :: TimeInterpreter IO
    ti = timeInterpreter (ctx ^. networkLayer @t)

-- Populate an API transaction record with 'TransactionInfo' from the wallet
-- layer.
mkApiTransactionFromInfo
    :: Monad m
    => TimeInterpreter m
    -> TransactionInfo
    -> m (ApiTransaction n)
mkApiTransactionFromInfo ti (TransactionInfo txid ins outs ws meta depth txtime txmeta) = do
    apiTx <- mkApiTransaction ti txid (drop2nd <$> ins) outs ws (meta, txtime) txmeta $
        case meta ^. #status of
            Pending  -> #pendingSince
            InLedger -> #insertedAt
    return $ case meta ^. #status of
        Pending  -> apiTx
        InLedger -> apiTx { depth = Just depth  }
  where
      drop2nd (a,_,c) = (a,c)

apiFee :: FeeEstimation -> ApiFee
apiFee (FeeEstimation estMin estMax) = ApiFee (qty estMin) (qty estMax)
    where qty = Quantity . fromIntegral

postTransactionFee
    :: forall ctx s t k n.
        ( Buildable (ErrValidateSelection t)
        , ctx ~ ApiLayer s t k
        , Typeable s
        , Typeable n
        )
    => ctx
    -> ApiT WalletId
    -> PostTransactionFeeData n
    -> Handler ApiFee
postTransactionFee ctx (ApiT wid) body = do
    let outs = coerceCoin <$> body ^. #payments
    let md = getApiT <$> body ^. #metadata

    withWorkerCtx ctx wid liftE liftE $ \wrk -> do
        wdrl <- case body ^. #withdrawal of
            Nothing ->
                pure (Quantity 0)

            Just SelfWithdrawal -> do
                acct <- liftHandler $ W.readChimericAccount @_ @s @k @n wrk wid
                wdrl <- liftHandler $ W.queryRewardBalance @_ @t wrk acct
                liftIO $ W.readNextWithdrawal @_ @s @t @k wrk wid wdrl

            Just (ExternalWithdrawal (ApiMnemonicT mw)) -> do
                let (_, acct) = W.someChimericAccount @ShelleyKey mw
                wdrl <- liftHandler $ W.queryRewardBalance @_ @t wrk acct
                liftIO $ W.readNextWithdrawal @_ @s @t @k wrk wid wdrl

        fee <- liftHandler $ W.estimateFeeForPayment @_ @s @t @k wrk wid outs wdrl md
        pure $ apiFee fee

joinStakePool
    :: forall ctx s t n k.
        ( DelegationAddress n k
        , s ~ SeqState n k
        , IsOurs s ChimericAccount
        , IsOwned s k
        , GenChange s
        , HardDerivation k
        , AddressIndexDerivationType k ~ 'Soft
        , WalletKey k
        , ctx ~ ApiLayer s t k
        )
    => ctx
    -> IO (Set PoolId)
       -- ^ Known pools
       -- We could maybe replace this with a @IO (PoolId -> Bool)@
    -> (PoolId -> IO PoolLifeCycleStatus)
    -> ApiPoolId
    -> ApiT WalletId
    -> ApiWalletPassphrase
    -> Handler (ApiTransaction n)
joinStakePool ctx knownPools getPoolStatus apiPoolId (ApiT wid) body = do
    let pwd = coerce $ getApiT $ body ^. #passphrase

    pid <- case apiPoolId of
        ApiPoolIdPlaceholder -> liftE ErrUnexpectedPoolIdPlaceholder
        ApiPoolId pid -> pure pid

    poolStatus <- liftIO (getPoolStatus pid)
    pools <- liftIO knownPools
    curEpoch <- getCurrentEpoch ctx

    (tx, txMeta, txTime) <- withWorkerCtx ctx wid liftE liftE $
        \wrk -> liftHandler $
            W.joinStakePool
                @_ @s @t @k wrk
                curEpoch pools pid poolStatus wid (delegationAddress @n) pwd

    liftIO $ mkApiTransaction
        ti
        (txId tx)
        (second (const Nothing) <$> tx ^. #resolvedInputs)
        (tx ^. #outputs)
        (tx ^. #withdrawals)
        (txMeta, txTime)
        Nothing
        #pendingSince
  where
    -- Not forecasting into the future. Should be safe.
    ti :: TimeInterpreter IO
    ti = timeInterpreter (ctx ^. networkLayer @t)

delegationFee
    :: forall ctx s t n k.
        ( s ~ SeqState n k
        , ctx ~ ApiLayer s t k
        )
    => ctx
    -> ApiT WalletId
    -> Handler ApiFee
delegationFee ctx (ApiT wid) = do
    withWorkerCtx ctx wid liftE liftE $ \wrk -> liftHandler $
         apiFee <$> W.estimateFeeForDelegation @_ @s @t @k wrk wid

quitStakePool
    :: forall ctx s t n k.
        ( DelegationAddress n k
        , s ~ SeqState n k
        , IsOurs s ChimericAccount
        , IsOwned s k
        , GenChange s
        , HasNetworkLayer t ctx
        , HardDerivation k
        , AddressIndexDerivationType k ~ 'Soft
        , WalletKey k
        , ctx ~ ApiLayer s t k
        )
    => ctx
    -> ApiT WalletId
    -> ApiWalletPassphrase
    -> Handler (ApiTransaction n)
quitStakePool ctx (ApiT wid) body = do
    let pwd = coerce $ getApiT $ body ^. #passphrase

    (tx, txMeta, txTime) <- withWorkerCtx ctx wid liftE liftE $ \wrk -> liftHandler $
        W.quitStakePool @_ @s @t @k wrk wid (delegationAddress @n) pwd

    liftIO $ mkApiTransaction
        ti
        (txId tx)
        (second (const Nothing) <$> tx ^. #resolvedInputs)
        (tx ^. #outputs)
        (tx ^. #withdrawals)
        (txMeta, txTime)
        Nothing
        #pendingSince
  where
    -- Not forecasting into the future. Should be safe.
    ti :: TimeInterpreter IO
    ti = timeInterpreter (ctx ^. networkLayer @t)

{-------------------------------------------------------------------------------
                                Migrations
-------------------------------------------------------------------------------}

getMigrationInfo
    :: forall s t k n.
        ( PaymentAddress n ByronKey
        )
    => ApiLayer s t k
        -- ^ Source wallet context
    -> ApiT WalletId
        -- ^ Source wallet
    -> Handler ApiWalletMigrationInfo
getMigrationInfo ctx (ApiT wid) = do
    (cs, leftovers) <- getSelections
    let migrationCost = costFromSelections cs
    pure $ ApiWalletMigrationInfo{migrationCost,leftovers}
  where
    costFromSelections :: [CoinSelection] -> Quantity "lovelace" Natural
    costFromSelections = Quantity
        . fromIntegral
        . sum
        . fmap selectionFee

    selectionFee :: CoinSelection -> Word64
    selectionFee s = inputBalance s - changeBalance s

    getSelections :: Handler ([CoinSelection], Quantity "lovelace" Natural)
    getSelections = withWorkerCtx ctx wid liftE liftE $ \wrk -> liftHandler $
        W.selectCoinsForMigration @_ @s @t @k @n wrk wid

migrateWallet
    :: forall s t k n p.
        ( IsOurs s ChimericAccount
        , IsOwned s k
        , HardDerivation k
        , Bounded (Index (AddressIndexDerivationType k) 'AddressK)
        , PaymentAddress n ByronKey
        , WalletKey k
        )
    => ApiLayer s t k
        -- ^ Source wallet context
    -> ApiT WalletId
        -- ^ Source wallet
    -> ApiWalletMigrationPostData n p
    -> Handler [ApiTransaction n]
migrateWallet ctx (ApiT wid) migrateData = do
    -- TODO: check if addrs are not empty

    migration <- do
        withWorkerCtx ctx wid liftE liftE $ \wrk -> liftHandler $ do
            (cs, _) <- W.selectCoinsForMigration @_ @_ @t @_ @n wrk wid
            pure $ assignMigrationAddresses addrs cs

    forM migration $ \cs -> do
        (tx, meta, time, wit) <- withWorkerCtx ctx wid liftE liftE
            $ \wrk -> liftHandler $ W.signTx @_ @s @t @k wrk wid pwd Nothing cs
        withWorkerCtx ctx wid liftE liftE
            $ \wrk -> liftHandler $ W.submitTx @_ @_ @t wrk wid (tx, meta, wit)
        liftIO $ mkApiTransaction
            ti
            (txId tx)
            (fmap Just <$> NE.toList (W.unsignedInputs cs))
            (NE.toList (W.unsignedOutputs cs))
            (tx ^. #withdrawals)
            (meta, time)
            Nothing
            #pendingSince
  where
    pwd = coerce $ getApiT $ migrateData ^. #passphrase
    addrs = getApiT . fst <$> migrateData ^. #addresses
    -- Not forecasting into the future. Should be safe.
    ti :: TimeInterpreter IO
    ti = timeInterpreter (ctx ^. networkLayer @t)


-- | Transform the given set of migration coin selections (for a source wallet)
--   into a set of coin selections that will migrate funds to the specified
--   target addresses.
--
-- Each change entry in the specified set of coin selections is replaced with a
-- corresponding output entry in the returned set, where the output entry has a
-- address from specified addresses.
--
-- If the number of outputs in the specified coin selection is greater than
-- the number of addresses in the specified address list, addresses will be
-- recycled in order of their appearance in the original list.
assignMigrationAddresses
    :: [Address]
    -- ^ Target addresses
    -> [CoinSelection]
    -- ^ Migration data for the source wallet.
    -> [UnsignedTx]
assignMigrationAddresses addrs selections =
    fst $ foldr accumulate ([], cycle addrs) selections
  where
    accumulate sel (txs, addrsAvailable) = first
        (\addrsSelected -> makeTx sel addrsSelected : txs)
        (splitAt (length $ change sel) addrsAvailable)

    makeTx :: CoinSelection -> [Address] -> UnsignedTx
    makeTx sel addrsSelected = UnsignedTx
        (NE.fromList (sel ^. #inputs))
        (NE.fromList (zipWith TxOut addrsSelected (sel ^. #change)))

{-------------------------------------------------------------------------------
                                    Network
-------------------------------------------------------------------------------}

data ErrCurrentEpoch
    = ErrUnableToDetermineCurrentEpoch
    | ErrCurrentEpochPastHorizonException PastHorizonException

getCurrentEpoch
    :: forall ctx s t k . (ctx ~ ApiLayer s t k)
    => ctx
    -> Handler W.EpochNo
getCurrentEpoch ctx = do
    res <- liftIO $ try $ currentEpoch ti
    case res of
        Right Nothing  -> liftE ErrUnableToDetermineCurrentEpoch
        Right (Just x) -> pure x
        Left e@(S.PastHorizon{}) -> liftE (ErrCurrentEpochPastHorizonException e)
  where
    ti :: TimeInterpreter IO
    ti = timeInterpreter (ctx ^. networkLayer @t)

getNetworkInformation
    :: forall t. (HasCallStack)
    => SyncTolerance
    -> NetworkLayer IO t Block
    -> Handler ApiNetworkInformation
getNetworkInformation st nl = do
    now <- liftIO getCurrentTime
    nodeTip <-  liftHandler (NW.currentNodeTip nl)
    apiNodeTip <- liftIO $ mkApiBlockReference ti nodeTip
    nowInfo <- liftIO $ runMaybeT $ networkTipInfo now
    progress <- handle (\(_ :: PastHorizonException) -> pure NotResponding)
        $ liftIO (syncProgress st ti nodeTip now)
    pure $ Api.ApiNetworkInformation
        { Api.syncProgress = ApiT progress
        , Api.nextEpoch = snd <$> nowInfo
        , Api.nodeTip = apiNodeTip
        , Api.networkTip = fst <$> nowInfo
        }
  where
    ti :: TimeInterpreter IO
    ti = timeInterpreter nl

    -- (network tip, next epoch)
    -- May be unavailible if the node is still syncing.
    networkTipInfo :: UTCTime -> MaybeT IO (ApiNetworkTip, ApiEpochInfo)
    networkTipInfo now = handle handlePastHorizonException $ do
        networkTipSlot <- MaybeT (ti $ ongoingSlotAt now)
        networkTip <- lift $ ti $ toSlotId networkTipSlot
        let curEpoch = networkTip ^. #epochNumber
        nextEpochStart <- lift $ ti $ endTimeOfEpoch curEpoch
        let tip = ApiNetworkTip
                (ApiT $ networkTip ^. #epochNumber)
                (ApiT $ networkTip ^. #slotNumber)
                (ApiT networkTipSlot)
        let nextEpoch = ApiEpochInfo
                (ApiT $ unsafeEpochSucc curEpoch)
                (nextEpochStart)
        return (tip, nextEpoch)
      where
        handlePastHorizonException
            :: PastHorizonException
            -> MaybeT IO (ApiNetworkTip, ApiEpochInfo)
        handlePastHorizonException _ =
            MaybeT (pure Nothing)

    -- Unsafe constructor for the next epoch. Chances to reach the last epoch
    -- are quite unlikely in this context :)
    unsafeEpochSucc :: HasCallStack => W.EpochNo -> W.EpochNo
    unsafeEpochSucc = fromMaybe bomb . epochSucc
      where bomb = error "reached final epoch of the Blockchain!?"

getNetworkParameters
    :: (Block, NetworkParameters, SyncTolerance)
    -> NetworkLayer IO t Block
    -> Handler ApiNetworkParameters
getNetworkParameters (_block0, np, _st) nl = do
    pp <- liftIO $ NW.getProtocolParameters nl
    let (apiNetworkParams, epochNoM) =
            toApiNetworkParameters np { protocolParameters = pp }
    case epochNoM of
        Just epochNo -> do
            epochStartTime <-
                liftIO $ timeInterpreter nl
                (firstSlotInEpoch epochNo >>= startTime)
            pure $ apiNetworkParams
                { hardforkAt = Just $
                    ApiEpochInfo (ApiT epochNo) epochStartTime }
        Nothing ->
            pure apiNetworkParams

getNetworkClock :: NtpClient -> Bool -> Handler ApiNetworkClock
getNetworkClock client = liftIO . getNtpStatus client

{-------------------------------------------------------------------------------
                                   Proxy
-------------------------------------------------------------------------------}

postExternalTransaction
    :: forall ctx s t k.
        ( ctx ~ ApiLayer s t k
        )
    => ctx
    -> PostExternalTransactionData
    -> Handler ApiTxId
postExternalTransaction ctx (PostExternalTransactionData load) = do
    tx <- liftHandler $ W.submitExternalTx @ctx @t @k ctx load
    return $ ApiTxId (ApiT (txId tx))

{-------------------------------------------------------------------------------
                                  Helpers
-------------------------------------------------------------------------------}

-- | see 'Cardano.Wallet#createWallet'
initWorker
    :: forall ctx s k.
        ( HasWorkerRegistry s k ctx
        , HasDBFactory s k ctx
        , HasLogger (WorkerLog WalletId WalletLog) ctx
        )
    => ctx
        -- ^ Surrounding API context
    -> WalletId
        -- ^ Wallet Id
    -> (WorkerCtx ctx -> ExceptT ErrWalletAlreadyExists IO WalletId)
        -- ^ Create action
    -> (WorkerCtx ctx -> ExceptT ErrNoSuchWallet IO ())
        -- ^ Restore action
    -> (WorkerCtx ctx -> IO ())
        -- ^ Action to run concurrently with restore action
    -> ExceptT ErrCreateWallet IO WalletId
initWorker ctx wid createWallet restoreWallet coworker =
    liftIO (Registry.lookup re wid) >>= \case
        Just _ ->
            throwE $ ErrCreateWalletAlreadyExists $ ErrWalletAlreadyExists wid
        Nothing ->
            liftIO (Registry.register @_ @ctx re ctx wid config) >>= \case
                Nothing ->
                    throwE ErrCreateWalletFailedToCreateWorker
                Just _ ->
                    pure wid
  where
    config = MkWorker
        { workerBefore = \ctx' _ -> do
            -- FIXME:
            -- Review error handling here
            void $ unsafeRunExceptT $ createWallet ctx'

        , workerMain = \ctx' _ -> do
            -- FIXME:
            -- Review error handling here
            race_
                (unsafeRunExceptT $ restoreWallet ctx')
                (coworker ctx')

        , workerAfter =
            defaultWorkerAfter

        , workerAcquire =
            withDatabase df wid
        }
    re = ctx ^. workerRegistry @s @k
    df = ctx ^. dbFactory @s @k

-- | Something to pass as the coworker action to 'newApiLayer', which does
-- nothing, and never exits.
idleWorker :: ctx -> wid -> IO ()
idleWorker _ _ = forever $ threadDelay maxBound

-- | Handler for fetching the 'ArgGenChange' for the 'RndState' (i.e. the root
-- XPrv), necessary to derive new change addresses.
rndStateChange
    :: forall ctx s t k n.
        ( ctx ~ ApiLayer s t k
        , s ~ RndState n
        , k ~ ByronKey
        )
    => ctx
    -> ApiT WalletId
    -> Passphrase "raw"
    -> Handler (ArgGenChange s)
rndStateChange ctx (ApiT wid) pwd =
    withWorkerCtx @_ @s @k ctx wid liftE liftE $ \wrk -> liftHandler $
        W.withRootKey @_ @s @k wrk wid pwd ErrSignPaymentWithRootKey $ \xprv scheme ->
            pure (xprv, preparePassphrase scheme pwd)

-- | Makes an 'ApiCoinSelection' from the given 'UnsignedTx'.
mkApiCoinSelection :: forall n. UnsignedTx -> ApiCoinSelection n
mkApiCoinSelection (UnsignedTx inputs outputs) =
    ApiCoinSelection
        (mkApiCoinSelectionInput <$> inputs)
        (mkAddressAmount <$> outputs)
  where
    mkAddressAmount :: TxOut -> AddressAmount (ApiT Address, Proxy n)
    mkAddressAmount (TxOut addr (Coin c)) =
        AddressAmount (ApiT addr, Proxy @n) (Quantity $ fromIntegral c)

    mkApiCoinSelectionInput :: (TxIn, TxOut) -> ApiCoinSelectionInput n
    mkApiCoinSelectionInput (TxIn txid index, TxOut addr (Coin c)) =
        ApiCoinSelectionInput
            { id = ApiT txid
            , index = index
            , address = (ApiT addr, Proxy @n)
            , amount = Quantity $ fromIntegral c
            }

mkApiTransaction
    :: forall n m. Monad m
    => TimeInterpreter m
    -> Hash "Tx"
    -> [(TxIn, Maybe TxOut)]
    -> [TxOut]
    -> Map ChimericAccount Coin
    -> (W.TxMeta, UTCTime)
    -> Maybe W.TxMetadata
    -> Lens' (ApiTransaction n) (Maybe ApiTimeReference)
    -> m (ApiTransaction n)
mkApiTransaction ti txid ins outs ws (meta, timestamp) txMeta setTimeReference = do
    timeRef <- timeReference
    return $ tx & setTimeReference .~ Just timeRef
  where
    tx :: ApiTransaction n
    tx = ApiTransaction
        { id = ApiT txid
        , amount = meta ^. #amount
        , insertedAt = Nothing
        , pendingSince = Nothing
        , depth = Nothing
        , direction = ApiT (meta ^. #direction)
        , inputs = [ApiTxInput (fmap toAddressAmount o) (ApiT i) | (i, o) <- ins]
        , outputs = toAddressAmount <$> outs
        , withdrawals = mkApiWithdrawal @n <$> Map.toList ws
        , status = ApiT (meta ^. #status)
        , metadata = apiTxMetadata txMeta
        }

    timeReference :: m ApiTimeReference
    timeReference = do
        slotId <- ti (toSlotId $ meta ^. #slotNo)
        -- TODO: We get passed the timestamp, but still have to do additional
        -- time-conversions here. We should probably do both
        --  SlotNo -> SlotId
        --  SlotNo -> UTCTime
        -- conversions in the same place.
        return $
            ApiTimeReference
                timestamp
                ApiBlockReference
                    { epochNumber = ApiT $ slotId ^. #epochNumber
                    , slotNumber = ApiT $ slotId ^. #slotNumber
                    , height = natural (meta ^. #blockHeight)
                    , slot = ApiT $ meta ^. #slotNo
                    }

    toAddressAmount :: TxOut -> AddressAmount (ApiT Address, Proxy n)
    toAddressAmount (TxOut addr c) =
        AddressAmount (ApiT addr, Proxy @n) (mkApiCoin c)

    apiTxMetadata :: Maybe W.TxMetadata -> ApiTxMetadata
    apiTxMetadata = ApiTxMetadata . fmap ApiT

mkApiCoin
    :: Coin
    -> Quantity "lovelace" Natural
mkApiCoin (Coin c) = Quantity $ fromIntegral c

mkApiWithdrawal
    :: forall (n :: NetworkDiscriminant). ()
    => (ChimericAccount, Coin)
    -> ApiWithdrawal n
mkApiWithdrawal (acct, c) =
    ApiWithdrawal (ApiT acct, Proxy @n) (mkApiCoin c)

coerceCoin :: forall (n :: NetworkDiscriminant). AddressAmount (ApiT Address, Proxy n) -> TxOut
coerceCoin (AddressAmount (ApiT addr, _) (Quantity c)) =
    TxOut addr (Coin $ fromIntegral c)

natural :: Quantity q Word32 -> Quantity q Natural
natural = Quantity . fromIntegral . getQuantity

mkApiBlockReference
    :: Monad m
    => TimeInterpreter m
    -> BlockHeader
    -> m ApiBlockReference
mkApiBlockReference ti tip = do
    slotId <- ti (toSlotId $ slotNo tip)
    return $ ApiBlockReference
        { epochNumber = ApiT $ slotId ^. #epochNumber
        , slotNumber = ApiT $ slotId ^. #slotNumber
        , height = natural $ tip ^. #blockHeight
        , slot = ApiT $ slotNo tip
        }

getWalletTip
    :: Monad m
    => TimeInterpreter m
    -> Wallet s
    -> m ApiBlockReference
getWalletTip ti wallet = mkApiBlockReference ti (currentTip wallet)

{-------------------------------------------------------------------------------
                                Api Layer
-------------------------------------------------------------------------------}

-- | Create a new instance of the wallet layer.
newApiLayer
    :: forall ctx s t k.
        ( ctx ~ ApiLayer s t k
        , IsOurs s ChimericAccount
        , IsOurs s Address
        )
    => Tracer IO (WorkerLog WalletId WalletLog)
    -> (Block, NetworkParameters, SyncTolerance)
    -> NetworkLayer IO t Block
    -> TransactionLayer t k
    -> DBFactory IO s k
    -> (WorkerCtx ctx -> WalletId -> IO ())
        -- ^ Action to run concurrently with wallet restore
    -> IO ctx
newApiLayer tr g0 nw tl df coworker = do
    re <- Registry.empty
    let ctx = ApiLayer tr g0 nw tl df re
    listDatabases df >>= mapM_ (registerWorker ctx coworker)
    return ctx

-- | Register a restoration worker to the registry.
registerWorker
    :: forall ctx s t k.
        ( ctx ~ ApiLayer s t k
        , IsOurs s ChimericAccount
        , IsOurs s Address
        )
    => ApiLayer s t k
    -> (WorkerCtx ctx -> WalletId -> IO ())
    -> WalletId
    -> IO ()
registerWorker ctx coworker wid =
    void $ Registry.register @_ @ctx re ctx wid config
  where
    (_, NetworkParameters gp _, _) = ctx ^. genesisData
    re = ctx ^. workerRegistry
    df = ctx ^. dbFactory
    config = MkWorker
        { workerBefore = \ctx' _ ->
            runExceptT (W.checkWalletIntegrity ctx' wid gp)
                >>= either throwIO pure

        , workerMain = \ctx' _ -> do
            -- FIXME:
            -- Review error handling here
            race_
                (unsafeRunExceptT $ W.restoreWallet @(WorkerCtx ctx) @s @t ctx' wid)
                (coworker ctx' wid)

        , workerAfter =
            defaultWorkerAfter

        , workerAcquire =
            withDatabase df wid
        }

-- | Run an action in a particular worker context. Fails if there's no worker
-- for a given id.
withWorkerCtx
    :: forall ctx s k m a.
        ( HasWorkerRegistry s k ctx
        , HasDBFactory s k ctx
        , MonadIO m
        )
    => ctx
        -- ^ A context that has a registry
    -> WalletId
        -- ^ Wallet to look for
    -> (ErrNoSuchWallet -> m a)
        -- ^ Wallet not present, handle error
    -> (ErrWalletNotResponding -> m a)
        -- ^ Wallet worker is dead, handle error
    -> (WorkerCtx ctx -> m a)
        -- ^ Do something with the wallet
    -> m a
withWorkerCtx ctx wid onMissing onNotResponding action =
    Registry.lookup re wid >>= \case
        Nothing -> do
            wids <- liftIO $ listDatabases df
            if wid `elem` wids then
                onNotResponding (ErrWalletNotResponding wid)
            else
                onMissing (ErrNoSuchWallet wid)
        Just wrk ->
            action $ hoistResource (workerResource wrk) (MsgFromWorker wid) ctx
  where
    re = ctx ^. workerRegistry @s @k
    df = ctx ^. dbFactory @s @k

{-------------------------------------------------------------------------------
                                Error Handling
-------------------------------------------------------------------------------}

-- | Lift our wallet layer into servant 'Handler', by mapping each error to a
-- corresponding servant error.
class LiftHandler e where
    liftHandler :: ExceptT e IO a -> Handler a
    liftHandler action = Handler (withExceptT handler action)

    liftE :: e -> Handler a
    liftE = liftHandler . throwE

    handler :: e -> ServerError

apiError :: ServerError -> ApiErrorCode -> Text -> ServerError
apiError err code message = err
    { errBody = Aeson.encode $ Aeson.object
        [ "code" .= code
        , "message" .= T.replace "\n" " " message
        ]
    }

data ErrUnexpectedPoolIdPlaceholder = ErrUnexpectedPoolIdPlaceholder
    deriving (Eq, Show)

data ErrCreateWallet
    = ErrCreateWalletAlreadyExists ErrWalletAlreadyExists
        -- ^ Wallet already exists
    | ErrCreateWalletFailedToCreateWorker
        -- ^ Somehow, we couldn't create a worker or open a db connection
    deriving (Eq, Show)

newtype ErrRejectedTip = ErrRejectedTip ApiNetworkTip
    deriving (Eq, Show)

-- | Small helper to easy show things to Text
showT :: Show a => a -> Text
showT = T.pack . show

instance LiftHandler ErrCurrentEpoch where
    handler = \case
        ErrUnableToDetermineCurrentEpoch ->
            apiError err500 UnableToDetermineCurrentEpoch $ mconcat
                [ "I'm unable to determine the current epoch. "
                , "Please wait a while for the node to sync and try again."
                ]
        ErrCurrentEpochPastHorizonException e -> handler e

instance LiftHandler ErrUnexpectedPoolIdPlaceholder where
    handler = \case
        ErrUnexpectedPoolIdPlaceholder ->
            apiError err400 BadRequest (pretty msg)
      where
        Left msg = fromText @PoolId "INVALID"

instance LiftHandler ErrRejectedTip where
    handler = \case
        ErrRejectedTip {} ->
            apiError err403 RejectedTip $ mconcat
                [ "I am sorry but I refuse to rollback to the given point. "
                , "Notwithstanding I'll willingly rollback to the genesis point "
                , "(0, 0) should you demand it."
                ]

instance LiftHandler ErrSelectForMigration where
    handler = \case
        ErrSelectForMigrationNoSuchWallet e -> handler e
        ErrSelectForMigrationEmptyWallet wid ->
            apiError err403 NothingToMigrate $ mconcat
                [ "I can't migrate the wallet with the given id: "
                , toText wid
                , ", because it's either empty or full of small coins "
                , "which wouldn't be worth migrating."
                ]

instance LiftHandler ErrNoSuchWallet where
    handler = \case
        ErrNoSuchWallet wid ->
            apiError err404 NoSuchWallet $ mconcat
                [ "I couldn't find a wallet with the given id: "
                , toText wid
                ]

instance LiftHandler ErrWalletNotResponding where
    handler = \case
        ErrWalletNotResponding wid ->
            apiError err500 WalletNotResponding $ T.unwords
                [ "That's embarassing. My associated worker for", toText wid
                , "is no longer responding. This is not something that is supposed"
                , "to happen. The worker must have left a trace in the logs of"
                , "severity 'Error' when it died which might explain the cause."
                , "Said differently, this wallet won't be accessible until the"
                , "server is restarted but there are good chances it'll recover"
                , "itself upon restart."
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
            apiError err403 NoRootKey $ mconcat
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

instance Buildable e => LiftHandler (ErrSelectCoinsExternal e) where
    handler = \case
        ErrSelectCoinsExternalNoSuchWallet e ->
            handler e
        ErrSelectCoinsExternalUnableToMakeSelection e ->
            handler e
        ErrSelectCoinsExternalUnableToAssignInputs wid ->
            apiError err500 UnexpectedError $ mconcat
                [ "I was unable to assign inputs while generating a coin "
                , "selection for the specified wallet: "
                , toText wid
                ]
        ErrSelectCoinsExternalUnableToAssignOutputs wid ->
            apiError err500 UnexpectedError $ mconcat
                [ "I was unable to assign outputs while generating a coin "
                , "selection for the specified wallet: "
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
        ErrMaximumInputsReached n ->
            apiError err403 TransactionIsTooBig $ mconcat
                [ "I had to select ", showT n, " inputs to construct the "
                , "requested transaction. Unfortunately, this would create a "
                , "transaction that is too big, and this would consequently "
                , "be rejected by a core node. Try sending a smaller amount."
                ]
        ErrInputsDepleted ->
            apiError err403 InputsDepleted $ mconcat
                [ "I cannot select enough UTxO from your wallet to construct "
                , "an adequate transaction. Try sending a smaller amount or "
                , "increasing the number of available UTxO."
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

instance LiftHandler ErrUTxOTooSmall where
    handler = \case
        ErrUTxOTooSmall minUtxoValue invalidUTxO ->
            apiError err403 UtxoTooSmall $ mconcat
                [ "I'm unable to construct the given transaction as some "
                , "outputs or changes are too small! Each output and change is "
                , "expected to be >= ", showT minUtxoValue, " Lovelace. "
                , "In the current transaction the following pieces are not "
                , "satisfying this condition : ", showT invalidUTxO, " ."
                ]

instance Buildable e => LiftHandler (ErrSelectForPayment e) where
    handler = \case
        ErrSelectForPaymentNoSuchWallet e -> handler e
        ErrSelectForPaymentCoinSelection e -> handler e
        ErrSelectForPaymentFee e -> handler e
        ErrSelectForPaymentMinimumUTxOValue e -> handler e
        ErrSelectForPaymentAlreadyWithdrawing tx ->
            apiError err403 AlreadyWithdrawing $ mconcat
                [ "I already know of a pending transaction with withdrawals: "
                , toText (txId tx), ". Note that when I withdraw rewards, I "
                , "need to withdraw them fully for the Ledger to accept it. "
                , "There's therefore no point creating another conflicting "
                , "transaction; if, for some reason, you really want a new "
                , "transaction, then cancel the previous one first."
                ]
        ErrSelectForPaymentTxTooLarge maxSize maxN  ->
            apiError err400 TransactionIsTooBig $ mconcat
                [ "I am afraid that the transaction you're trying to submit is "
                , "too large! The network allows transactions only as large as "
                , pretty maxSize, "s! As it stands, the current transaction only "
                , "allows me to select up to ", showT maxN, " inputs. Note "
                , "that I am selecting inputs randomly, so retrying *may work* "
                , "provided I end up choosing bigger inputs sufficient to cover "
                , "the transaction cost. Alternatively, try sending to less "
                , "recipients or with smaller metadata."
                ]

instance LiftHandler ErrListUTxOStatistics where
    handler = \case
        ErrListUTxOStatisticsNoSuchWallet e -> handler e

instance LiftHandler ErrMkTx where
    handler = \case
        ErrKeyNotFoundForAddress addr ->
            apiError err500 KeyNotFoundForAddress $ mconcat
                [ "That's embarassing. I couldn't sign the given transaction: "
                , "I haven't found the corresponding private key for a known "
                , "input address I should keep track of: ", showT addr, ". "
                , "Retrying may work, but something really went wrong..."
                ]

instance LiftHandler ErrSignPayment where
    handler = \case
        ErrSignPaymentMkTx e -> handler e
        ErrSignPaymentNetwork e -> handler e
        ErrSignPaymentNoSuchWallet e -> (handler e)
            { errHTTPCode = 410
            , errReasonPhrase = errReasonPhrase err410
            }
        ErrSignPaymentWithRootKey e@ErrWithRootKeyNoRootKey{} -> (handler e)
            { errHTTPCode = 403
            , errReasonPhrase = errReasonPhrase err403
            }
        ErrSignPaymentWithRootKey e@ErrWithRootKeyWrongPassphrase{} -> handler e

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

instance LiftHandler ErrRemovePendingTx where
    handler = \case
        ErrRemovePendingTxNoSuchWallet wid -> handler wid
        ErrRemovePendingTxNoSuchTransaction tid ->
            apiError err404 NoSuchTransaction $ mconcat
                [ "I couldn't find a transaction with the given id: "
                , toText tid
                ]
        ErrRemovePendingTxTransactionNoMorePending tid ->
            apiError err403 TransactionNotPending $ mconcat
                [ "The transaction with id: ", toText tid,
                  " cannot be forgotten as it is not pending anymore."
                ]

instance LiftHandler ErrPostTx where
    handler = \case
        ErrPostTxNetworkUnreachable e -> handler e
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

instance LiftHandler ErrSubmitTx where
    handler = \case
        ErrSubmitTxNetwork e -> handler e
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
        ErrListTransactionsMinWithdrawalWrong ->
            apiError err400 MinWithdrawalWrong
            "The minimum withdrawal value must be at least 1 Lovelace."
        ErrListTransactionsPastHorizonException e -> handler e

instance LiftHandler ErrStartTimeLaterThanEndTime where
    handler err = apiError err400 StartTimeLaterThanEndTime $ mconcat
        [ "The specified start time '"
        , toText $ Iso8601Time $ errStartTime err
        , "' is later than the specified end time '"
        , toText $ Iso8601Time $ errEndTime err
        , "'."
        ]

instance LiftHandler PastHorizonException where
    handler _ = apiError err503 PastHorizon $ mconcat
        [ "Tried to convert something that is past the horizon"
        , " (due to uncertainty about the next hard fork)."
        , " Wait for the node to finish syncing to the hard fork."
        , " Depending on the blockchain, this process can take an"
        , " unknown amount of time."
        ]

instance LiftHandler ErrGetTransaction where
    handler = \case
        ErrGetTransactionNoSuchWallet e -> handler e
        ErrGetTransactionNoSuchTransaction e -> handler e

instance LiftHandler ErrNoSuchTransaction where
    handler = \case
        ErrNoSuchTransaction tid ->
            apiError err404 NoSuchTransaction $ mconcat
                [ "I couldn't find a transaction with the given id: "
                , toText tid
                ]

instance LiftHandler ErrCurrentNodeTip where
    handler = \case
        ErrCurrentNodeTipNetworkUnreachable e -> handler e
        ErrCurrentNodeTipNotFound -> apiError err503 NetworkTipNotFound $ mconcat
            [ "I couldn't get the current network tip at the moment. It's "
            , "probably because the node is down or not started yet. Retrying "
            , "in a bit might give better results!"
            ]

instance LiftHandler ErrSelectForDelegation where
    handler = \case
        ErrSelectForDelegationNoSuchWallet e -> handler e
        ErrSelectForDelegationFee (ErrCannotCoverFee cost) ->
            apiError err403 CannotCoverFee $ mconcat
                [ "I'm unable to select enough coins to pay for a "
                , "delegation certificate. I need: ", showT cost, " Lovelace."
                ]

instance LiftHandler ErrSignDelegation where
    handler = \case
        ErrSignDelegationMkTx e -> handler e
        ErrSignDelegationNetwork e -> handler e
        ErrSignDelegationNoSuchWallet e -> (handler e)
            { errHTTPCode = 410
            , errReasonPhrase = errReasonPhrase err410
            }
        ErrSignDelegationWithRootKey e@ErrWithRootKeyNoRootKey{} -> (handler e)
            { errHTTPCode = 403
            , errReasonPhrase = errReasonPhrase err403
            }
        ErrSignDelegationWithRootKey e@ErrWithRootKeyWrongPassphrase{} -> handler e

instance LiftHandler ErrJoinStakePool where
    handler = \case
        ErrJoinStakePoolNoSuchWallet e -> handler e
        ErrJoinStakePoolSubmitTx e -> handler e
        ErrJoinStakePoolSignDelegation e -> handler e
        ErrJoinStakePoolSelectCoin e -> handler e
        ErrJoinStakePoolCannotJoin e -> case e of
            ErrAlreadyDelegating pid ->
                apiError err403 PoolAlreadyJoined $ mconcat
                    [ "I couldn't join a stake pool with the given id: "
                    , toText pid
                    , ". I have already joined this pool;"
                    , " joining again would incur an unnecessary fee!"
                    ]
            ErrNoSuchPool pid ->
                apiError err404 NoSuchPool $ mconcat
                    [ "I couldn't find any stake pool with the given id: "
                    , toText pid
                    ]

instance LiftHandler ErrFetchRewards where
    handler = \case
        ErrFetchRewardsReadChimericAccount e -> handler e
        ErrFetchRewardsNetworkUnreachable e -> handler e

instance LiftHandler ErrReadChimericAccount where
    handler = \case
        ErrReadChimericAccountNoSuchWallet e -> handler e
        ErrReadChimericAccountNotAShelleyWallet ->
            apiError err403 InvalidWalletType $ mconcat
                [ "It is regrettable but you've just attempted an operation "
                , "that is invalid for this type of wallet. Only new 'Shelley' "
                , "wallets can do something with rewards and this one isn't."
                ]

instance LiftHandler ErrQuitStakePool where
    handler = \case
        ErrQuitStakePoolNoSuchWallet e -> handler e
        ErrQuitStakePoolSelectCoin e -> handler e
        ErrQuitStakePoolSignDelegation e -> handler e
        ErrQuitStakePoolSubmitTx e -> handler e
        ErrQuitStakePoolCannotQuit e -> case e of
            ErrNotDelegatingOrAboutTo ->
                apiError err403 NotDelegatingTo $ mconcat
                    [ "It seems that you're trying to retire from delegation "
                    , "although you're not even delegating, nor won't be in an "
                    , "immediate future."
                    ]
            ErrNonNullRewards (Quantity rewards) ->
                apiError err403 NonNullRewards $ mconcat
                    [ "It seems that you're trying to retire from delegation "
                    , "although you've unspoiled rewards in your rewards "
                    , "account! Make sure to withdraw your ", pretty rewards
                    , " lovelace first."
                    ]

instance LiftHandler ErrCreateRandomAddress where
    handler = \case
        ErrCreateAddrNoSuchWallet e -> handler e
        ErrCreateAddrWithRootKey  e -> handler e
        ErrIndexAlreadyExists ix ->
            apiError err409 AddressAlreadyExists $ mconcat
                [ "I cannot derive a new unused address #", pretty (fromEnum ix)
                , " because I already know of such address."
                ]
        ErrCreateAddressNotAByronWallet ->
            apiError err403 InvalidWalletType $ mconcat
                [ "I cannot derive new address for this wallet type."
                , " Make sure to use Byron random wallet id."
                ]

instance LiftHandler ErrImportRandomAddress where
    handler = \case
        ErrImportAddrNoSuchWallet e -> handler e
        ErrImportAddressNotAByronWallet ->
            apiError err403 InvalidWalletType $ mconcat
                [ "I cannot derive new address for this wallet type."
                , " Make sure to use Byron random wallet id."
                ]
        ErrImportAddr ErrAddrDoesNotBelong{} ->
            apiError err403 KeyNotFoundForAddress $ mconcat
                [ "I couldn't identify this address as one of mine. It likely "
                , "belongs to another wallet and I will therefore not import it."
                ]

instance LiftHandler ErrNotASequentialWallet where
    handler = \case
        ErrNotASequentialWallet ->
            apiError err403 InvalidWalletType $ mconcat
                [ "I cannot derive new address for this wallet type. "
                , "Make sure to use a sequential wallet style, like Icarus."
                ]

instance LiftHandler ErrWithdrawalNotWorth where
    handler = \case
        ErrWithdrawalNotWorth ->
            apiError err403 WithdrawalNotWorth $ mconcat
                [ "I've noticed that you're requesting a withdrawal from an "
                , "account that is either empty or doesn't have a balance big "
                , "enough to deserve being withdrawn. I won't proceed with that "
                , "request."
                ]

instance LiftHandler ErrListPools where
    handler = \case
        ErrListPoolsNetworkError e -> handler e
        ErrListPoolsPastHorizonException e -> handler e

instance LiftHandler (Request, ServerError) where
    handler (req, err@(ServerError code _ body headers))
      | not (isJSON body) = case code of
        400 | "Failed reading" `BS.isInfixOf` BL.toStrict body ->
            apiError err' BadRequest $ mconcat
                [ "I couldn't understand the content of your message. If your "
                , "message is intended to be in JSON format, please check that "
                , "the JSON is valid."
                ]
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
        501 -> apiError err' NotImplemented
            "I'm really sorry but this endpoint is not implemented yet."
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
