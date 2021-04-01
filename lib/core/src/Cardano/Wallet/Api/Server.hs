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
    , derivePublicKey
    , getMigrationInfo
    , getNetworkClock
    , getNetworkInformation
    , getNetworkParameters
    , getUTxOsStatistics
    , getWallet
    , joinStakePool
    , listAssets
    , getAsset
    , getAssetDefault
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
    , selectCoinsForJoin
    , selectCoinsForQuit
    , signMetadata
    , postAccountPublicKey

    -- * Internals
    , LiftHandler(..)
    , apiError
    , mkShelleyWallet
    , mkLegacyWallet
    , withLegacyLayer
    , withLegacyLayer'
    , rndStateChange
    , withWorkerCtx
    , getCurrentEpoch

    -- * Workers
    , manageRewardBalance
    , idleWorker

    -- * Logging
    , WalletEngineLog (..)
    ) where

import Prelude

import Cardano.Address.Derivation
    ( XPrv, XPub, xpubPublicKey, xpubToBytes )
import Cardano.Api.Typed
    ( AnyCardanoEra (..), CardanoEra (..), SerialiseAsCBOR (..) )
import Cardano.BM.Tracing
    ( HasPrivacyAnnotation (..), HasSeverityAnnotation (..) )
import Cardano.Mnemonic
    ( SomeMnemonic )
import Cardano.Wallet
    ( ErrCannotJoin (..)
    , ErrCannotQuit (..)
    , ErrCreateRandomAddress (..)
    , ErrDecodeSignedTx (..)
    , ErrDerivePublicKey (..)
    , ErrFetchRewards (..)
    , ErrGetTransaction (..)
    , ErrImportAddress (..)
    , ErrImportRandomAddress (..)
    , ErrInvalidDerivationIndex (..)
    , ErrJoinStakePool (..)
    , ErrListTransactions (..)
    , ErrListUTxOStatistics (..)
    , ErrMkTx (..)
    , ErrNoSuchTransaction (..)
    , ErrNoSuchWallet (..)
    , ErrNotASequentialWallet (..)
    , ErrPostTx (..)
    , ErrQuitStakePool (..)
    , ErrReadAccountPublicKey (..)
    , ErrReadRewardAccount (..)
    , ErrRemoveTx (..)
    , ErrSelectAssets (..)
    , ErrSignMetadataWith (..)
    , ErrSignPayment (..)
    , ErrStartTimeLaterThanEndTime (..)
    , ErrSubmitExternalTx (..)
    , ErrSubmitTx (..)
    , ErrUpdatePassphrase (..)
    , ErrWalletAlreadyExists (..)
    , ErrWalletNotResponding (..)
    , ErrWithRootKey (..)
    , ErrWithdrawalNotWorth (..)
    , ErrWrongPassphrase (..)
    , FeeEstimation (..)
    , HasNetworkLayer
    , TxSubmitLog
    , WalletLog
    , genesisData
    , manageRewardBalance
    , networkLayer
    )
import Cardano.Wallet.Api
    ( ApiLayer (..)
    , HasDBFactory
    , HasTokenMetadataClient
    , HasWorkerRegistry
    , dbFactory
    , tokenMetadataClient
    , workerRegistry
    )
import Cardano.Wallet.Api.Server.Tls
    ( TlsConfiguration (..), requireClientAuth )
import Cardano.Wallet.Api.Types
    ( AccountPostData (..)
    , AddressAmount (..)
    , ApiAccountKey (..)
    , ApiAccountPublicKey (..)
    , ApiAddress (..)
    , ApiAddressInfo (..)
    , ApiAsset (..)
    , ApiBlockInfo (..)
    , ApiBlockReference (..)
    , ApiByronWallet (..)
    , ApiByronWalletBalance (..)
    , ApiCoinSelection (..)
    , ApiCoinSelectionChange (..)
    , ApiCoinSelectionInput (..)
    , ApiCoinSelectionOutput (..)
    , ApiCoinSelectionWithdrawal (..)
    , ApiEpochInfo (ApiEpochInfo)
    , ApiEra (..)
    , ApiErrorCode (..)
    , ApiFee (..)
    , ApiMnemonicT (..)
    , ApiNetworkClock (..)
    , ApiNetworkInformation
    , ApiNetworkParameters (..)
    , ApiPoolId (..)
    , ApiPostAccountKeyData (..)
    , ApiPostRandomAddressData (..)
    , ApiPutAddressesData (..)
    , ApiRawMetadata (..)
    , ApiSelectCoinsPayments
    , ApiSlotId (..)
    , ApiSlotReference (..)
    , ApiT (..)
    , ApiTransaction (..)
    , ApiTxId (..)
    , ApiTxInput (..)
    , ApiTxMetadata (..)
    , ApiUtxoStatistics (..)
    , ApiVerificationKey (..)
    , ApiWallet (..)
    , ApiWalletAssetsBalance (..)
    , ApiWalletBalance (..)
    , ApiWalletDelegation (..)
    , ApiWalletDelegationNext (..)
    , ApiWalletDelegationStatus (..)
    , ApiWalletMigrationInfo (..)
    , ApiWalletMigrationPostData (..)
    , ApiWalletPassphrase (..)
    , ApiWalletPassphraseInfo (..)
    , ApiWalletSignData (..)
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
    , WalletOrAccountPostData (..)
    , WalletPostData (..)
    , WalletPutData (..)
    , WalletPutPassphraseData (..)
    , coinFromQuantity
    , coinToQuantity
    , getApiMnemonicT
    , toApiAsset
    , toApiEpochInfo
    , toApiNetworkParameters
    , toApiUtxoStatistics
    )
import Cardano.Wallet.Compat
    ( (^?) )
import Cardano.Wallet.DB
    ( DBFactory (..) )
import Cardano.Wallet.Network
    ( NetworkLayer, timeInterpreter )
import Cardano.Wallet.Primitive.AddressDerivation
    ( DelegationAddress (..)
    , Depth (..)
    , DerivationIndex (..)
    , DerivationType (..)
    , HardDerivation (..)
    , Index (..)
    , MkKeyFingerprint
    , NetworkDiscriminant (..)
    , Passphrase (..)
    , PaymentAddress (..)
    , RewardAccount (..)
    , Role
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
    , purposeCIP1852
    )
import Cardano.Wallet.Primitive.CoinSelection.MA.RoundRobin
    ( SelectionError (..)
    , SelectionInsufficientError (..)
    , UnableToConstructChangeError (..)
    , balanceMissing
    , selectionDelta
    )
import Cardano.Wallet.Primitive.Model
    ( Wallet, availableBalance, currentTip, getState, totalBalance )
import Cardano.Wallet.Primitive.Slotting
    ( PastHorizonException
    , RelativeTime
    , TimeInterpreter
    , currentEpoch
    , currentRelativeTime
    , expectAndThrowFailures
    , hoistTimeInterpreter
    , interpretQuery
    , neverFails
    , ongoingSlotAt
    , slotToUTCTime
    , timeOfEpoch
    , toSlotId
    , unsafeExtendSafeZone
    )
import Cardano.Wallet.Primitive.SyncProgress
    ( SyncProgress (..), SyncTolerance, syncProgress )
import Cardano.Wallet.Primitive.Types
    ( Block
    , BlockHeader (..)
    , NetworkParameters (..)
    , PassphraseScheme (..)
    , PoolId
    , PoolLifeCycleStatus (..)
    , Signature (..)
    , SlotId
    , SlotNo
    , SortOrder (..)
    , WalletId (..)
    , WalletMetadata (..)
    )
import Cardano.Wallet.Primitive.Types.Address
    ( Address (..), AddressState (..) )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..), coinQuantity )
import Cardano.Wallet.Primitive.Types.Hash
    ( Hash (..) )
import Cardano.Wallet.Primitive.Types.TokenBundle
    ( Flat (..), TokenBundle (..) )
import Cardano.Wallet.Primitive.Types.TokenMap
    ( AssetId (..) )
import Cardano.Wallet.Primitive.Types.TokenPolicy
    ( TokenName (..), TokenPolicyId (..), nullTokenName )
import Cardano.Wallet.Primitive.Types.Tx
    ( TransactionInfo (TransactionInfo)
    , Tx (..)
    , TxChange (..)
    , TxIn (..)
    , TxOut (..)
    , TxStatus (..)
    , UnsignedTx (..)
    , txOutCoin
    )
import Cardano.Wallet.Registry
    ( HasWorkerCtx (..)
    , MkWorker (..)
    , WorkerLog (..)
    , defaultWorkerAfter
    , workerResource
    )
import Cardano.Wallet.TokenMetadata
    ( TokenMetadataClient, fillMetadata )
import Cardano.Wallet.Transaction
    ( DelegationAction (..)
    , ErrOutputTokenBundleSizeExceedsLimit (..)
    , ErrOutputTokenQuantityExceedsLimit (..)
    , ErrSelectionCriteria (..)
    , TransactionCtx (..)
    , TransactionLayer
    , Withdrawal (..)
    , defaultTransactionCtx
    )
import Cardano.Wallet.Unsafe
    ( unsafeRunExceptT )
import Control.Arrow
    ( second )
import Control.DeepSeq
    ( NFData )
import Control.Monad
    ( forM, forever, join, void, when, (>=>) )
import Control.Monad.IO.Class
    ( MonadIO, liftIO )
import Control.Monad.Trans.Except
    ( ExceptT (..), runExceptT, throwE, withExceptT )
import Control.Monad.Trans.Maybe
    ( MaybeT (..), exceptToMaybeT )
import Control.Tracer
    ( Tracer, contramap )
import Data.Aeson
    ( (.=) )
import Data.ByteString
    ( ByteString )
import Data.Coerce
    ( coerce )
import Data.Either.Extra
    ( eitherToMaybe )
import Data.Function
    ( (&) )
import Data.Functor
    ( (<&>) )
import Data.Functor.Identity
    ( Identity (..) )
import Data.Generics.Internal.VL.Lens
    ( Lens', view, (.~), (^.) )
import Data.Generics.Labels
    ()
import Data.List
    ( isInfixOf, isPrefixOf, isSubsequenceOf, sortOn )
import Data.List.NonEmpty
    ( NonEmpty (..) )
import Data.Map.Strict
    ( Map )
import Data.Maybe
    ( catMaybes, fromMaybe, isJust, mapMaybe, maybeToList )
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
import Data.Type.Equality
    ( (:~:) (..), testEquality )
import Data.Word
    ( Word32 )
import Fmt
    ( blockListF, indentF, pretty )
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
    , err500
    , err501
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
    ( Typeable, typeRep )
import UnliftIO.Async
    ( race_ )
import UnliftIO.Concurrent
    ( threadDelay )
import UnliftIO.Exception
    ( IOException, bracket, throwIO, tryAnyDeep, tryJust )

import qualified Cardano.Wallet as W
import qualified Cardano.Wallet.Api.Types as Api
import qualified Cardano.Wallet.Network as NW
import qualified Cardano.Wallet.Primitive.AddressDerivation.Byron as Byron
import qualified Cardano.Wallet.Primitive.AddressDerivation.Icarus as Icarus
import qualified Cardano.Wallet.Primitive.Types as W
import qualified Cardano.Wallet.Primitive.Types.Coin as Coin
import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle
import qualified Cardano.Wallet.Primitive.Types.Tx as W
import qualified Cardano.Wallet.Registry as Registry
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Foldable as F
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
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
    acquire = tryJust handleErr bindAndListen
    -- Note: These Data.Streaming.Network functions also listen on the socket,
    -- even though their name just says "bind".
    bindAndListen = case portOpt of
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
    :: forall ctx s k n.
        ( s ~ SeqState n k
        , ctx ~ ApiLayer s k
        , SoftDerivation k
        , MkKeyFingerprint k (Proxy n, k 'AddressK XPub)
        , MkKeyFingerprint k Address
        , WalletKey k
        , Bounded (Index (AddressIndexDerivationType k) 'AddressK)
        , HasDBFactory s k ctx
        , HasWorkerRegistry s k ctx
        , IsOurs s RewardAccount
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
            (W.manageRewardBalance @_ @s @k (Proxy @n)) body'

postShelleyWallet
    :: forall ctx s k n.
        ( s ~ SeqState n k
        , ctx ~ ApiLayer s k
        , SoftDerivation k
        , MkKeyFingerprint k (Proxy n, k 'AddressK XPub)
        , MkKeyFingerprint k Address
        , WalletKey k
        , Bounded (Index (AddressIndexDerivationType k) 'AddressK)
        , HasDBFactory s k ctx
        , HasWorkerRegistry s k ctx
        , IsOurs s RewardAccount
        , Typeable s
        , Typeable n
        )
    => ctx
    -> ((SomeMnemonic, Maybe SomeMnemonic) -> Passphrase "encryption" -> k 'RootK XPrv)
    -> WalletPostData
    -> Handler ApiWallet
postShelleyWallet ctx generateKey body = do
    let state = mkSeqStateFromRootXPrv (rootXPrv, pwd) purposeCIP1852 g
    void $ liftHandler $ createWalletWorker @_ @s @k ctx wid
        (\wrk -> W.createWallet  @(WorkerCtx ctx) @s @k wrk wid wName state)
        (\wrk _ -> W.manageRewardBalance @(WorkerCtx ctx) @s @k (Proxy @n) wrk wid)
    withWorkerCtx @_ @s @k ctx wid liftE liftE $ \wrk -> liftHandler $
        W.attachPrivateKeyFromPwd @_ @s @k wrk wid (rootXPrv, pwd)
    fst <$> getWallet ctx (mkShelleyWallet @_ @s @k) (ApiT wid)
  where
    seed = getApiMnemonicT (body ^. #mnemonicSentence)
    secondFactor = getApiMnemonicT <$> (body ^. #mnemonicSecondFactor)
    pwd = preparePassphrase EncryptWithPBKDF2 $ getApiT (body ^. #passphrase)
    rootXPrv = generateKey (seed, secondFactor) pwd
    g = maybe defaultAddressPoolGap getApiT (body ^. #addressPoolGap)
    wid = WalletId $ digest $ publicKey rootXPrv
    wName = getApiT (body ^. #name)

postAccountWallet
    :: forall ctx s k n w.
        ( s ~ SeqState n k
        , ctx ~ ApiLayer s k
        , SoftDerivation k
        , MkKeyFingerprint k (Proxy n, k 'AddressK XPub)
        , MkKeyFingerprint k Address
        , WalletKey k
        , HasWorkerRegistry s k ctx
        , IsOurs s RewardAccount
        , Typeable n
        )
    => ctx
    -> MkApiWallet ctx s w
    -> (XPub -> k 'AccountK XPub)
    -> (WorkerCtx ctx -> WalletId -> IO ())
        -- ^ Action to run concurrently with restore action
    -> AccountPostData
    -> Handler w
postAccountWallet ctx mkWallet liftKey coworker body = do
    let state = mkSeqStateFromAccountXPub (liftKey accXPub) purposeCIP1852 g
    void $ liftHandler $ createWalletWorker @_ @s @k ctx wid
        (\wrk -> W.createWallet  @(WorkerCtx ctx) @s @k wrk wid wName state)
        coworker
    fst <$> getWallet ctx mkWallet (ApiT wid)
  where
    g = maybe defaultAddressPoolGap getApiT (body ^. #addressPoolGap)
    wName = getApiT (body ^. #name)
    (ApiAccountPublicKey accXPubApiT) =  body ^. #accountPublicKey
    accXPub = getApiT accXPubApiT
    wid = WalletId $ digest (liftKey accXPub)

mkShelleyWallet
    :: forall ctx s k n.
        ( ctx ~ ApiLayer s k
        , s ~ SeqState n k
        , IsOurs s Address
        , IsOurs s RewardAccount
        , HasWorkerRegistry s k ctx
        )
    => MkApiWallet ctx s ApiWallet
mkShelleyWallet ctx wid cp meta pending progress = do
    reward <- withWorkerCtx @_ @s @k ctx wid liftE liftE $ \wrk ->
        -- never fails - returns zero if balance not found
        liftIO $ W.fetchRewardBalance @_ @s @k wrk wid

    let ti = timeInterpreter $ ctx ^. networkLayer

    -- In the Shelley era of Byron;Shelley;Allegra toApiWalletDelegation using
    -- an unextended @ti@ will simply fail because of uncertainty about the next
    -- fork.
    --
    -- @unsafeExtendSafeZone@ performs the calculation as if no fork will occur.
    -- This should be fine because:
    -- 1. We expect the next few eras to have the same epoch length as Shelley
    -- 2. It shouldn't be the end of the world to return the wrong time.
    --
    -- But ultimately, we might want to make the uncertainty transparent to API
    -- users. TODO: ADP-575
    apiDelegation <- liftIO $ toApiWalletDelegation (meta ^. #delegation)
        (unsafeExtendSafeZone ti)

    tip' <- liftIO $ getWalletTip
        (neverFails "getWalletTip wallet tip should be behind node tip" ti)
        cp
    let available = availableBalance pending cp
    let total = totalBalance pending reward cp
    pure ApiWallet
        { addressPoolGap = ApiT $ getState cp ^. #externalPool . #gap
        , balance = ApiWalletBalance
            { available = coinToQuantity (available ^. #coin)
            , total = coinToQuantity (total ^. #coin)
            , reward = coinToQuantity reward
            }
        , assets = ApiWalletAssetsBalance
            { available = ApiT (available ^. #tokens)
            , total = ApiT (total ^. #tokens)
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
    toApiWalletDelegation W.WalletDelegation{active,next} ti = do
        apiNext <- forM next $ \W.WalletDelegationNext{status,changesAt} -> do
            info <- interpretQuery ti $ toApiEpochInfo changesAt
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
    :: forall ctx s k.
        ( ctx ~ ApiLayer s k
        , KnownDiscovery s
        , IsOurs s RewardAccount
        , IsOurs s Address
        , HasNetworkLayer ctx
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
    void $ liftHandler $ createWalletWorker @_ @s @k ctx wid (`createWallet` wid)
        idleWorker
    withWorkerCtx ctx wid liftE liftE $ \wrk -> liftHandler $
        W.attachPrivateKeyFromPwd wrk wid (rootXPrv, pwd)
    fst <$> getWallet ctx mkLegacyWallet (ApiT wid)
  where
    wid = WalletId $ digest $ publicKey rootXPrv

mkLegacyWallet
    :: forall ctx s k.
        ( HasWorkerRegistry s k ctx
        , HasDBFactory s k ctx
        , KnownDiscovery s
        , HasNetworkLayer ctx
        , IsOurs s Address
        , IsOurs s RewardAccount
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

    tip' <- liftIO $ getWalletTip (expectAndThrowFailures ti) cp
    let available = availableBalance pending cp
    let total = totalBalance pending (Coin 0) cp
    pure ApiByronWallet
        { balance = ApiByronWalletBalance
            { available = coinToQuantity $ TokenBundle.getCoin available
            , total = coinToQuantity $ TokenBundle.getCoin total
            }
        , assets = ApiWalletAssetsBalance
            { available = ApiT (available ^. #tokens)
            , total = ApiT (total ^. #tokens)
            }
        , id = ApiT wid
        , name = ApiT $ meta ^. #name
        , passphrase = pwdInfo
        , state = ApiT progress
        , tip = tip'
        , discovery = knownDiscovery @s
        }
  where
    ti :: TimeInterpreter (ExceptT PastHorizonException IO)
    ti = timeInterpreter $ ctx ^. networkLayer

    matchEmptyPassphrase
        :: WorkerCtx ctx
        -> Handler (Either ErrWithRootKey ())
    matchEmptyPassphrase wrk = liftIO $ runExceptT $
        W.withRootKey @_ @s @k wrk wid mempty Prelude.id (\_ _ -> pure ())

postRandomWallet
    :: forall ctx s k n.
        ( ctx ~ ApiLayer s k
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
    :: forall ctx s k n.
        ( ctx ~ ApiLayer s k
        , s ~ RndState n
        , k ~ ByronKey
        , HasNetworkLayer ctx
        )
    => ctx
    -> ByronWalletFromXPrvPostData
    -> Handler ApiByronWallet
postRandomWalletFromXPrv ctx body = do
    s <- liftIO $ mkRndState byronKey <$> getStdRandom random
    void $ liftHandler $ createWalletWorker @_ @s @k ctx wid
        (\wrk -> W.createWallet @(WorkerCtx ctx) @s @k wrk wid wName s)
        idleWorker
    withWorkerCtx ctx wid liftE liftE $ \wrk -> liftHandler $
        W.attachPrivateKeyFromPwdHash wrk wid (byronKey, pwd)
    fst <$> getWallet ctx mkLegacyWallet (ApiT wid)
  where
    wName = getApiT (body ^. #name)
    pwd   = getApiT (body ^. #passphraseHash)
    masterKey = getApiT (body ^. #encryptedRootPrivateKey)
    byronKey = mkByronKeyFromMasterKey masterKey
    wid = WalletId $ digest $ publicKey byronKey

postIcarusWallet
    :: forall ctx s k n.
        ( ctx ~ ApiLayer s k
        , s ~ SeqState n k
        , k ~ IcarusKey
        , HasWorkerRegistry s k ctx
        , PaymentAddress n IcarusKey
        , Typeable n
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
    :: forall ctx s k n.
        ( ctx ~ ApiLayer s k
        , s ~ SeqState n k
        , k ~ IcarusKey
        , HasWorkerRegistry s k ctx
        , PaymentAddress n IcarusKey
        , Typeable n
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
    :: forall ctx s k n.
        ( ctx ~ ApiLayer s k
        , s ~ SeqState n k
        , k ~ IcarusKey
        , HasWorkerRegistry s k ctx
        , PaymentAddress n IcarusKey
        , Typeable n
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
    :: forall byron icarus n a.
        ( byron ~ ApiLayer (RndState n) ByronKey
        , icarus ~ ApiLayer (SeqState n IcarusKey) IcarusKey
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
    :: forall byron icarus n a.
        ( byron ~ ApiLayer (RndState n) ByronKey
        , icarus ~ ApiLayer (SeqState n IcarusKey) IcarusKey
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
    :: forall ctx s k.
        ( ctx ~ ApiLayer s k
        )
    => ctx
    -> ApiT WalletId
    -> Handler NoContent
deleteWallet ctx (ApiT wid) = do
    -- Start a context so that an error is throw if the wallet doesn't exist.
    withWorkerCtx @_ @s @k ctx wid liftE
        (const $ pure()) (const $ pure ())

    liftIO $ Registry.unregister re wid
    liftIO $ removeDatabase df wid

    return NoContent
  where
    re = ctx ^. workerRegistry @s @k
    df = ctx ^. dbFactory @s @k

getWallet
    :: forall ctx s k apiWallet.
        ( ctx ~ ApiLayer s k
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
        progress <- liftIO $ W.walletSyncProgress @_ @_ ctx cp
        (, meta ^. #creationTime) <$> mkApiWallet ctx wid cp meta pending progress

    whenNotResponding _ = Handler $ ExceptT $ withDatabase df wid $ \db -> runHandler $ do
        let wrk = hoistResource db (MsgFromWorker wid) ctx
        (cp, meta, pending) <- liftHandler $ W.readWallet @_ @s @k wrk wid
        (, meta ^. #creationTime) <$> mkApiWallet ctx wid cp meta pending NotResponding

listWallets
    :: forall ctx s k apiWallet.
        ( ctx ~ ApiLayer s k
        , NFData apiWallet
        )
    => ctx
    -> MkApiWallet ctx s apiWallet
    -> Handler [(apiWallet, UTCTime)]
listWallets ctx mkApiWallet = do
    wids <- liftIO $ listDatabases df
    liftIO $ sortOn snd . catMaybes <$> mapM maybeGetWallet (ApiT <$> wids)
  where
    df = ctx ^. dbFactory @s @k

    -- Under extreme circumstances (like integration tests running in parallel)
    -- there may be race conditions where the wallet is deleted just before we
    -- try to read it.
    --
    -- But.. why do we need to both runHandler and tryAnyDeep?
    maybeGetWallet :: ApiT WalletId -> IO (Maybe (apiWallet, UTCTime))
    maybeGetWallet =
        fmap (join . eitherToMaybe)
        . tryAnyDeep
        . fmap eitherToMaybe
        . runHandler
        . getWallet ctx mkApiWallet

putWallet
    :: forall ctx s k apiWallet.
        ( ctx ~ ApiLayer s k
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
    :: forall ctx s k.
        ( WalletKey k
        , ctx ~ ApiLayer s k
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
    :: forall ctx s k.
        ( WalletKey k
        , ctx ~ ApiLayer s k
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
    :: forall ctx s k.
        ( ctx ~ ApiLayer s k
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
    :: forall ctx s k n.
        ( s ~ SeqState n k
        , ctx ~ ApiLayer s k
        , SoftDerivation k
        , IsOurs s Address
        , Bounded (Index (AddressIndexDerivationType k) 'AddressK)
        , Typeable n
        , Typeable s
        , WalletKey k
        )
    => ctx
    -> ArgGenChange s
    -> ApiT WalletId
    -> ApiSelectCoinsPayments n
    -> Handler (ApiCoinSelection n)
selectCoins ctx genChange (ApiT wid) body = do
    let md = body ^? #metadata . traverse . #getApiT
    (wdrl, _) <-
        mkRewardAccountBuilder @_ @s @_ @n ctx wid (body ^. #withdrawal)

    withWorkerCtx ctx wid liftE liftE $ \wrk -> do
        let outs = addressAmountToTxOut <$> body ^. #payments
        let txCtx = defaultTransactionCtx
                { txWithdrawal = wdrl
                , txMetadata = getApiT <$> body ^. #metadata
                }
        let transform = \s sel ->
                W.assignChangeAddresses genChange sel s
                & uncurry (W.selectionToUnsignedTx (txWithdrawal txCtx))
        w <- liftHandler $ W.readWalletUTxOIndex @_ @s @k wrk wid
        utx <- liftHandler
            $ W.selectAssets  @_ @s @k wrk w txCtx outs transform

        pure $ mkApiCoinSelection [] Nothing md utx

selectCoinsForJoin
    :: forall ctx s n k.
        ( s ~ SeqState n k
        , ctx ~ ApiLayer s k
        , DelegationAddress n k
        , MkKeyFingerprint k (Proxy n, k 'AddressK XPub)
        , SoftDerivation k
        , Typeable n
        , Typeable s
        )
    => ctx
    -> IO (Set PoolId)
       -- ^ Known pools
       -- We could maybe replace this with a @IO (PoolId -> Bool)@
    -> (PoolId -> IO PoolLifeCycleStatus)
    -> PoolId
    -> WalletId
    -> Handler (Api.ApiCoinSelection n)
selectCoinsForJoin ctx knownPools getPoolStatus pid wid = do
    poolStatus <- liftIO (getPoolStatus pid)
    pools <- liftIO knownPools
    curEpoch <- getCurrentEpoch ctx

    withWorkerCtx ctx wid liftE liftE $ \wrk -> do
        (action, deposit) <- liftHandler
            $ W.joinStakePool @_ @s @k @n wrk curEpoch pools pid poolStatus wid

        let txCtx = defaultTransactionCtx
                { txDelegationAction = Just action
                }

        let transform = \s sel ->
                W.assignChangeAddresses (delegationAddress @n) sel s
                & uncurry (W.selectionToUnsignedTx (txWithdrawal txCtx))
        wal <- liftHandler $ W.readWalletUTxOIndex @_ @s @k wrk wid
        utx <- liftHandler
            $ W.selectAssetsNoOutputs @_ @s @k wrk wid wal txCtx transform
        (_, path) <- liftHandler
            $ W.readRewardAccount @_ @s @k @n wrk wid

        let deposits = maybeToList deposit

        pure $ mkApiCoinSelection deposits (Just (action, path)) Nothing utx

selectCoinsForQuit
    :: forall ctx s n k.
        ( s ~ SeqState n k
        , ctx ~ ApiLayer s k
        , DelegationAddress n k
        , MkKeyFingerprint k (Proxy n, k 'AddressK XPub)
        , SoftDerivation k
        , Typeable n
        , Typeable s
        )
    => ctx
    -> ApiT WalletId
    -> Handler (Api.ApiCoinSelection n)
selectCoinsForQuit ctx (ApiT wid) = do
    withWorkerCtx ctx wid liftE liftE $ \wrk -> do
        action <- liftHandler $ W.quitStakePool @_ @s @k @n wrk wid

        let txCtx = defaultTransactionCtx
                { txDelegationAction = Just action
                }

        let transform = \s sel ->
                W.assignChangeAddresses (delegationAddress @n) sel s
                & uncurry (W.selectionToUnsignedTx (txWithdrawal txCtx))
        wal <- liftHandler $ W.readWalletUTxOIndex @_ @s @k wrk wid
        utx <- liftHandler
            $ W.selectAssetsNoOutputs @_ @s @k wrk wid wal txCtx transform
        (_, path) <- liftHandler $ W.readRewardAccount @_ @s @k @n wrk wid

        pure $ mkApiCoinSelection [] (Just (action, path)) Nothing utx

{-------------------------------------------------------------------------------
                                     Assets
-------------------------------------------------------------------------------}

data ErrGetAsset
     = ErrGetAssetNoSuchWallet ErrNoSuchWallet
     | ErrGetAssetNotPresent
    deriving (Eq, Show)

newtype ErrListAssets = ErrListAssetsNoSuchWallet ErrNoSuchWallet
    deriving (Eq, Show)

-- | All assets associated with this wallet, and their metadata (if metadata is
-- available). This list may include assets which have already been spent.
listAssets
    :: forall ctx s k.
        ( ctx ~ ApiLayer s k
        , HasTokenMetadataClient ctx
        )
    => ctx
    -> ApiT WalletId
    -> Handler [ApiAsset]
listAssets ctx wid = do
    assets <- listAssetsBase ctx wid
    liftIO $ fillMetadata client assets toApiAsset
  where
    client = ctx ^. tokenMetadataClient

-- | Return a list of all AssetIds involved in the transaction history of this
-- wallet.
listAssetsBase
    :: forall ctx s k.
        ( ctx ~ ApiLayer s k
        )
    => ctx
    -> ApiT WalletId
    -> Handler [AssetId]
listAssetsBase ctx (ApiT wid) = withWorkerCtx ctx wid liftE liftE $ \wrk ->
    liftHandler $ allTxAssets <$>
    W.listTransactions @_ @_ @_ wrk wid Nothing Nothing Nothing Descending
  where
    allTxAssets = Set.toList . Set.unions . map txAssets
    txAssets = Set.unions
        . map (TokenBundle.getAssets . view #tokens)
        . W.txInfoOutputs

-- | Look up a single asset and its metadata.
--
-- NOTE: This is slightly inefficient because it greps through the transaction
-- history to check if the asset is associated with this wallet.
getAsset
    :: forall ctx s k.
        ( ctx ~ ApiLayer s k
        , HasTokenMetadataClient ctx
        )
    => ctx
    -> ApiT WalletId
    -> ApiT TokenPolicyId
    -> ApiT TokenName
    -> Handler ApiAsset
getAsset ctx wid (ApiT policyId) (ApiT assetName) = do
    assetId <- liftHandler . findAsset =<< listAssetsBase ctx wid
    liftIO $ runIdentity <$> fillMetadata client (Identity assetId) toApiAsset
  where
    findAsset = maybe (throwE ErrGetAssetNotPresent) pure
        . F.find (== (AssetId policyId assetName))
    client = ctx ^. tokenMetadataClient

-- | The handler for 'getAsset' when 'TokenName' is empty.
getAssetDefault
    :: forall ctx s k.
        ( ctx ~ ApiLayer s k
        , HasTokenMetadataClient ctx
        )
    => ctx
    -> ApiT WalletId
    -> ApiT TokenPolicyId
    -> Handler ApiAsset
getAssetDefault ctx wid pid = getAsset ctx wid pid (ApiT nullTokenName)

{-------------------------------------------------------------------------------
                                    Addresses
-------------------------------------------------------------------------------}

postRandomAddress
    :: forall ctx s k n.
        ( s ~ RndState n
        , k ~ ByronKey
        , ctx ~ ApiLayer s k
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
    :: forall ctx s k n.
        ( s ~ RndState n
        , k ~ ByronKey
        , ctx ~ ApiLayer s k
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
    :: forall ctx s k n.
        ( s ~ RndState n
        , k ~ ByronKey
        , ctx ~ ApiLayer s k
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
    :: forall ctx s k n.
        ( ctx ~ ApiLayer s k
        , CompareDiscovery s
        , KnownAddresses s
        )
    => ctx
    -> (s -> Address -> Maybe Address)
    -> ApiT WalletId
    -> Maybe (ApiT AddressState)
    -> Handler [ApiAddressInfo n]
listAddresses ctx normalize (ApiT wid) stateFilter = do
    addrs <- withWorkerCtx ctx wid liftE liftE $ \wrk -> liftHandler $
        W.listAddresses @_ @s @k wrk wid normalize
    return $ coerceAddress <$> filter filterCondition addrs
  where
    filterCondition :: (Address, AddressState, DerivationIndex, DerivationIndex) -> Bool
    filterCondition = case stateFilter of
        Nothing -> const True
        Just (ApiT s) -> \(_,state,_,_) -> state == s
    coerceAddress (a, s, ix1, ix2) =
        ApiAddressInfo
        (ApiAddress (ApiT a, Proxy @n) (ApiT s))
        (NE.fromList [ApiT ix1, ApiT ix2])

{-------------------------------------------------------------------------------
                                    Transactions
-------------------------------------------------------------------------------}

postTransaction
    :: forall ctx s k n.
        ( ctx ~ ApiLayer s k
        , Bounded (Index (AddressIndexDerivationType k) 'AddressK)
        , GenChange s
        , HardDerivation k
        , HasNetworkLayer ctx
        , IsOwned s k
        , Typeable n
        , Typeable s
        , WalletKey k
        )
    => ctx
    -> ArgGenChange s
    -> ApiT WalletId
    -> PostTransactionData n
    -> Handler (ApiTransaction n)
postTransaction ctx genChange (ApiT wid) body = do
    let pwd = coerce $ body ^. #passphrase . #getApiT
    let outs = addressAmountToTxOut <$> body ^. #payments
    let md = body ^? #metadata . traverse . #getApiT
    let mTTL = body ^? #timeToLive . traverse . #getQuantity

    (wdrl, mkRwdAcct) <-
        mkRewardAccountBuilder @_ @s @_ @n ctx wid (body ^. #withdrawal)

    ttl <- liftIO $ W.getTxExpiry ti mTTL
    let txCtx = defaultTransactionCtx
            { txWithdrawal = wdrl
            , txMetadata = md
            , txTimeToLive = ttl
            }

    (sel, tx, txMeta, txTime) <- withWorkerCtx ctx wid liftE liftE $ \wrk -> do
        w <- liftHandler $ W.readWalletUTxOIndex @_ @s @k wrk wid
        sel <- liftHandler
            $ W.selectAssets @_ @s @k wrk w txCtx outs (const Prelude.id)
        (tx, txMeta, txTime, sealedTx) <- liftHandler
            $ W.signTransaction @_ @s @k wrk wid genChange mkRwdAcct pwd txCtx sel
        liftHandler
            $ W.submitTx @_ @s @k wrk wid (tx, txMeta, sealedTx)
        pure (sel, tx, txMeta, txTime)

    liftIO $ mkApiTransaction
        (timeInterpreter $ ctx ^. networkLayer)
        (txId tx)
        (tx ^. #fee)
        (NE.toList $ second Just <$> sel ^. #inputsSelected)
        (tx ^. #outputs)
        (tx ^. #withdrawals)
        (txMeta, txTime)
        (tx ^. #metadata)
        #pendingSince
  where
    ti :: TimeInterpreter (ExceptT PastHorizonException IO)
    ti = timeInterpreter (ctx ^. networkLayer)

deleteTransaction
    :: forall ctx s k. ctx ~ ApiLayer s k
    => ctx
    -> ApiT WalletId
    -> ApiTxId
    -> Handler NoContent
deleteTransaction ctx (ApiT wid) (ApiTxId (ApiT (tid))) = do
    withWorkerCtx ctx wid liftE liftE $ \wrk -> liftHandler $
        W.forgetTx wrk wid tid
    return NoContent

listTransactions
    :: forall ctx s k n. (ctx ~ ApiLayer s k)
    => ctx
    -> ApiT WalletId
    -> Maybe MinWithdrawal
    -> Maybe Iso8601Time
    -> Maybe Iso8601Time
    -> Maybe (ApiT SortOrder)
    -> Handler [ApiTransaction n]
listTransactions ctx (ApiT wid) mMinWithdrawal mStart mEnd mOrder = do
    txs <- withWorkerCtx ctx wid liftE liftE $ \wrk -> liftHandler $
        W.listTransactions @_ @_ @_ wrk wid
            (Coin . fromIntegral . getMinWithdrawal <$> mMinWithdrawal)
            (getIso8601Time <$> mStart)
            (getIso8601Time <$> mEnd)
            (maybe defaultSortOrder getApiT mOrder)
    liftIO $ mapM (mkApiTransactionFromInfo (timeInterpreter (ctx ^. networkLayer))) txs
  where
    defaultSortOrder :: SortOrder
    defaultSortOrder = Descending

getTransaction
    :: forall ctx s k n. (ctx ~ ApiLayer s k)
    => ctx
    -> ApiT WalletId
    -> ApiTxId
    -> Handler (ApiTransaction n)
getTransaction ctx (ApiT wid) (ApiTxId (ApiT (tid))) = do
    tx <- withWorkerCtx ctx wid liftE liftE $ \wrk -> liftHandler $
        W.getTransaction wrk wid tid
    liftIO $ mkApiTransactionFromInfo (timeInterpreter (ctx ^. networkLayer)) tx

-- Populate an API transaction record with 'TransactionInfo' from the wallet
-- layer.
mkApiTransactionFromInfo
    :: MonadIO m
    => TimeInterpreter (ExceptT PastHorizonException IO)
    -> TransactionInfo
    -> m (ApiTransaction n)
mkApiTransactionFromInfo ti (TransactionInfo txid fee ins outs ws meta depth txtime txmeta) = do
    apiTx <- liftIO $ mkApiTransaction ti txid fee (drop2nd <$> ins) outs ws (meta, txtime) txmeta $
        case meta ^. #status of
            Pending  -> #pendingSince
            InLedger -> #insertedAt
            Expired  -> #pendingSince
    return $ case meta ^. #status of
        Pending  -> apiTx
        InLedger -> apiTx { depth = Just depth  }
        Expired  -> apiTx
  where
      drop2nd (a,_,c) = (a,c)

postTransactionFee
    :: forall ctx s k n.
        ( ctx ~ ApiLayer s k
        , Bounded (Index (AddressIndexDerivationType k) 'AddressK)
        , HardDerivation k
        , Typeable n
        , Typeable s
        , WalletKey k
        )
    => ctx
    -> ApiT WalletId
    -> PostTransactionFeeData n
    -> Handler ApiFee
postTransactionFee ctx (ApiT wid) body = do
    (wdrl, _) <- mkRewardAccountBuilder @_ @s @_ @n ctx wid (body ^. #withdrawal)
    let txCtx = defaultTransactionCtx
            { txWithdrawal = wdrl
            , txMetadata = getApiT <$> body ^. #metadata
            }
    withWorkerCtx ctx wid liftE liftE $ \wrk -> do
        w <- liftHandler $ W.readWalletUTxOIndex @_ @s @k wrk wid
        let outs = addressAmountToTxOut <$> body ^. #payments
        let runSelection = W.selectAssets @_ @s @k wrk w txCtx outs getFee
              where getFee = const (selectionDelta TokenBundle.getCoin)
        minCoins <- NE.toList <$> liftIO (W.calcMinimumCoinValues @_ @k wrk outs)
        liftHandler $ mkApiFee Nothing minCoins <$> W.estimateFee runSelection

joinStakePool
    :: forall ctx s n k.
        ( ctx ~ ApiLayer s k
        , s ~ SeqState n k
        , AddressIndexDerivationType k ~ 'Soft
        , DelegationAddress n k
        , GenChange s
        , IsOwned s k
        , SoftDerivation k
        , Typeable n
        , Typeable s
        , WalletKey k
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

    (sel, tx, txMeta, txTime) <- withWorkerCtx ctx wid liftE liftE $ \wrk -> do
        (action, _) <- liftHandler
            $ W.joinStakePool @_ @s @k @n wrk curEpoch pools pid poolStatus wid

        (wdrl, mkRwdAcct) <- mkRewardAccountBuilder @_ @s @_ @n ctx wid Nothing
        ttl <- liftIO $ W.getTxExpiry ti Nothing
        let txCtx = defaultTransactionCtx
                { txWithdrawal = wdrl
                , txTimeToLive = ttl
                , txDelegationAction = Just action
                }
        wal <- liftHandler $ W.readWalletUTxOIndex @_ @s @k wrk wid
        sel <- liftHandler
            $ W.selectAssetsNoOutputs @_ @s @k wrk wid wal txCtx
            $ const Prelude.id
        (tx, txMeta, txTime, sealedTx) <- liftHandler
            $ W.signTransaction @_ @s @k wrk wid genChange mkRwdAcct pwd txCtx sel
        liftHandler
            $ W.submitTx @_ @s @k wrk wid (tx, txMeta, sealedTx)

        pure (sel, tx, txMeta, txTime)

    liftIO $ mkApiTransaction
        (timeInterpreter (ctx ^. networkLayer))
        (txId tx)
        (tx ^. #fee)
        (NE.toList $ second Just <$> sel ^. #inputsSelected)
        (tx ^. #outputs)
        (tx ^. #withdrawals)
        (txMeta, txTime)
        Nothing
        #pendingSince
  where
    ti :: TimeInterpreter (ExceptT PastHorizonException IO)
    ti = timeInterpreter (ctx ^. networkLayer)

    genChange = delegationAddress @n

delegationFee
    :: forall ctx s n k.
        ( s ~ SeqState n k
        , ctx ~ ApiLayer s k
        )
    => ctx
    -> ApiT WalletId
    -> Handler ApiFee
delegationFee ctx (ApiT wid) = do
    withWorkerCtx ctx wid liftE liftE $ \wrk -> liftHandler $ do
        w <- withExceptT ErrSelectAssetsNoSuchWallet $
            W.readWalletUTxOIndex @_ @s @k wrk wid
        deposit <- W.calcMinimumDeposit @_ @s @k wrk wid
        mkApiFee (Just deposit) [] <$> W.estimateFee (runSelection wrk deposit w)
  where
    txCtx :: TransactionCtx
    txCtx = defaultTransactionCtx

    runSelection wrk deposit wal =
        W.selectAssetsNoOutputs @_ @s @k wrk wid wal txCtx calcFee
      where
        calcFee _ = Coin.distance deposit . selectionDelta TokenBundle.getCoin

quitStakePool
    :: forall ctx s n k.
        ( ctx ~ ApiLayer s k
        , s ~ SeqState n k
        , AddressIndexDerivationType k ~ 'Soft
        , DelegationAddress n k
        , GenChange s
        , HasNetworkLayer ctx
        , IsOwned s k
        , SoftDerivation k
        , Typeable n
        , Typeable s
        , WalletKey k
        )
    => ctx
    -> ApiT WalletId
    -> ApiWalletPassphrase
    -> Handler (ApiTransaction n)
quitStakePool ctx (ApiT wid) body = do
    let pwd = coerce $ getApiT $ body ^. #passphrase

    (sel, tx, txMeta, txTime) <- withWorkerCtx ctx wid liftE liftE $ \wrk -> do
        action <- liftHandler
            $ W.quitStakePool @_ @s @k @n wrk wid

        (wdrl, mkRwdAcct) <- mkRewardAccountBuilder @_ @s @_ @n ctx wid Nothing
        ttl <- liftIO $ W.getTxExpiry ti Nothing
        let txCtx = defaultTransactionCtx
                { txWithdrawal = wdrl
                , txTimeToLive = ttl
                , txDelegationAction = Just action
                }

        wal <- liftHandler $ W.readWalletUTxOIndex @_ @s @k wrk wid
        sel <- liftHandler
            $ W.selectAssetsNoOutputs @_ @s @k wrk wid wal txCtx
            $ const Prelude.id
        (tx, txMeta, txTime, sealedTx) <- liftHandler
            $ W.signTransaction @_ @s @k wrk wid genChange mkRwdAcct pwd txCtx sel
        liftHandler
            $ W.submitTx @_ @s @k wrk wid (tx, txMeta, sealedTx)

        pure (sel, tx, txMeta, txTime)

    liftIO $ mkApiTransaction
        (timeInterpreter (ctx ^. networkLayer))
        (txId tx)
        (tx ^. #fee)
        (NE.toList $ second Just <$> sel ^. #inputsSelected)
        (tx ^. #outputs)
        (tx ^. #withdrawals)
        (txMeta, txTime)
        Nothing
        #pendingSince
  where
    ti :: TimeInterpreter (ExceptT PastHorizonException IO)
    ti = timeInterpreter (ctx ^. networkLayer)

    genChange = delegationAddress @n

{-------------------------------------------------------------------------------
                                Migrations
-------------------------------------------------------------------------------}

getMigrationInfo
    :: forall s k. ()
    => ApiLayer s k
        -- ^ Source wallet context
    -> ApiT WalletId
        -- ^ Source wallet
    -> Handler ApiWalletMigrationInfo
getMigrationInfo _ctx _wid = do
    liftHandler $ throwE ErrTemporarilyDisabled

migrateWallet
    :: forall s k n p. ()
    => ApiLayer s k
        -- ^ Source wallet context
    -> ApiT WalletId
        -- ^ Source wallet
    -> ApiWalletMigrationPostData n p
    -> Handler [ApiTransaction n]
migrateWallet _ctx _wid _migrateData = do
    liftHandler $ throwE ErrTemporarilyDisabled

{-------------------------------------------------------------------------------
                                    Network
-------------------------------------------------------------------------------}

data ErrCurrentEpoch
    = ErrUnableToDetermineCurrentEpoch  -- fixme: unused
    | ErrCurrentEpochPastHorizonException PastHorizonException

getCurrentEpoch
    :: forall ctx s k . (ctx ~ ApiLayer s k)
    => ctx
    -> Handler W.EpochNo
getCurrentEpoch ctx = liftIO (runExceptT (currentEpoch ti)) >>= \case
    Left e -> liftE $ ErrCurrentEpochPastHorizonException e
    Right x -> pure x
  where
    ti :: TimeInterpreter (ExceptT PastHorizonException IO)
    ti = timeInterpreter (ctx ^. networkLayer)

getNetworkInformation
    :: HasCallStack
    => SyncTolerance
    -> NetworkLayer IO Block
    -> Handler ApiNetworkInformation
getNetworkInformation st nl = liftIO $ do
    now <- currentRelativeTime ti
    nodeTip <- NW.currentNodeTip nl
    nodeEra <- NW.currentNodeEra nl
    apiNodeTip <- makeApiBlockReferenceFromHeader
        (neverFails "node tip is within safe-zone" $ timeInterpreter nl)
        nodeTip
    nowInfo <- runMaybeT $ networkTipInfo now
    progress <- syncProgress
            st
            (neverFails "syncProgress" $ timeInterpreter nl)
            nodeTip
            now
    pure $ Api.ApiNetworkInformation
        { Api.syncProgress = ApiT progress
        , Api.nextEpoch = snd <$> nowInfo
        , Api.nodeTip = apiNodeTip
        , Api.networkTip = fst <$> nowInfo
        , Api.nodeEra = toApiEra nodeEra
        }
  where
    ti :: TimeInterpreter (MaybeT IO)
    ti = hoistTimeInterpreter exceptToMaybeT $ timeInterpreter nl

    toApiEra :: AnyCardanoEra -> ApiEra
    toApiEra (AnyCardanoEra ByronEra) = ApiByron
    toApiEra (AnyCardanoEra ShelleyEra) = ApiShelley
    toApiEra (AnyCardanoEra AllegraEra) = ApiAllegra
    toApiEra (AnyCardanoEra MaryEra) = ApiMary

    -- (network tip, next epoch)
    -- May be unavailible if the node is still syncing.
    networkTipInfo :: RelativeTime -> MaybeT IO (ApiSlotReference, ApiEpochInfo)
    networkTipInfo now = do
        networkTipSlot <- interpretQuery ti $ ongoingSlotAt now
        tip <- makeApiSlotReference ti networkTipSlot
        let curEpoch = tip ^. #slotId . #epochNumber . #getApiT
        (_, nextEpochStart) <- interpretQuery ti $ timeOfEpoch curEpoch
        let nextEpoch = ApiEpochInfo
                (ApiT $ succ curEpoch)
                nextEpochStart
        return (tip, nextEpoch)

getNetworkParameters
    :: (Block, NetworkParameters, SyncTolerance)
    -> NetworkLayer IO Block
    -> Handler ApiNetworkParameters
getNetworkParameters (_block0, genesisNp, _st) nl = do
    pp <- liftIO $ NW.currentProtocolParameters nl
    sp <- liftIO $ NW.currentSlottingParameters nl
    let np = genesisNp { protocolParameters = pp, slottingParameters = sp }
    liftIO $ toApiNetworkParameters np (interpretQuery ti . toApiEpochInfo)
  where
    ti :: TimeInterpreter IO
    ti = neverFails
        "PastHorizonException should never happen in getNetworkParameters \
        \because the ledger is being queried for slotting info about its own \
        \tip."
        (timeInterpreter nl)

getNetworkClock :: NtpClient -> Bool -> Handler ApiNetworkClock
getNetworkClock client = liftIO . getNtpStatus client

{-------------------------------------------------------------------------------
                               Miscellaneous
-------------------------------------------------------------------------------}

postExternalTransaction
    :: forall ctx s k.
        ( ctx ~ ApiLayer s k
        )
    => ctx
    -> PostExternalTransactionData
    -> Handler ApiTxId
postExternalTransaction ctx (PostExternalTransactionData load) = do
    tx <- liftHandler $ W.submitExternalTx @ctx @k ctx load
    return $ ApiTxId (ApiT (txId tx))

signMetadata
    :: forall ctx s k n.
        ( ctx ~ ApiLayer s k
        , s ~ SeqState n k
        , HardDerivation k
        , AddressIndexDerivationType k ~ 'Soft
        , WalletKey k
        )
    => ctx
    -> ApiT WalletId
    -> ApiT Role
    -> ApiT DerivationIndex
    -> ApiWalletSignData
    -> Handler ByteString
signMetadata ctx (ApiT wid) (ApiT role_) (ApiT ix) body = do
    let meta = body ^. #metadata . #getApiT
    let pwd  = body ^. #passphrase . #getApiT

    withWorkerCtx @_ @s @k ctx wid liftE liftE $ \wrk -> liftHandler $ do
        getSignature <$> W.signMetadataWith @_ @s @k @n
            wrk wid (coerce pwd) (role_, ix) meta

derivePublicKey
    :: forall ctx s k n.
        ( s ~ SeqState n k
        , ctx ~ ApiLayer s k
        , SoftDerivation k
        , WalletKey k
        )
    => ctx
    -> ApiT WalletId
    -> ApiT Role
    -> ApiT DerivationIndex
    -> Handler ApiVerificationKey
derivePublicKey ctx (ApiT wid) (ApiT role_) (ApiT ix) = do
    withWorkerCtx @_ @s @k ctx wid liftE liftE $ \wrk -> do
        k <- liftHandler $ W.derivePublicKey @_ @s @k @n wrk wid role_ ix
        pure $ ApiVerificationKey (xpubPublicKey $ getRawKey k, role_)

postAccountPublicKey
    :: forall ctx s k n.
        ( s ~ SeqState n k
        , ctx ~ ApiLayer s k
        , HardDerivation k
        , WalletKey k
        )
    => ctx
    -> ApiT WalletId
    -> ApiT DerivationIndex
    -> ApiPostAccountKeyData
    -> Handler ApiAccountKey
postAccountPublicKey ctx (ApiT wid) (ApiT ix) (ApiPostAccountKeyData (ApiT pwd) extd) = do
    withWorkerCtx @_ @s @k ctx wid liftE liftE $ \wrk -> do
        k <- liftHandler $ W.readPublicAccountKey @_ @s @k @n wrk wid pwd ix
        let toBytes = if extd then xpubToBytes else xpubPublicKey
        pure $ ApiAccountKey (toBytes $ getRawKey k) extd

{-------------------------------------------------------------------------------
                                  Helpers
-------------------------------------------------------------------------------}

-- | Handler for fetching the 'ArgGenChange' for the 'RndState' (i.e. the root
-- XPrv), necessary to derive new change addresses.
rndStateChange
    :: forall ctx s k n.
        ( ctx ~ ApiLayer s k
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

type RewardAccountBuilder k
        =  (k 'RootK XPrv, Passphrase "encryption")
        -> (XPrv, Passphrase "encryption")

mkRewardAccountBuilder
    :: forall ctx s k (n :: NetworkDiscriminant) shelley.
        ( ctx ~ ApiLayer s k
        , shelley ~ SeqState n ShelleyKey
        , HardDerivation k
        , Bounded (Index (AddressIndexDerivationType k) 'AddressK)
        , WalletKey k
        , Typeable s
        , Typeable n
        )
    => ctx
    -> WalletId
    -> Maybe ApiWithdrawalPostData
    -> Handler (Withdrawal, RewardAccountBuilder k)
mkRewardAccountBuilder ctx wid withdrawal = do
    let selfRewardCredentials (rootK, pwdP) =
            (getRawKey $ deriveRewardAccount @k pwdP rootK, pwdP)

    withWorkerCtx ctx wid liftE liftE $ \wrk -> do
        case (testEquality (typeRep @s) (typeRep @shelley), withdrawal) of
            (Nothing, Just{}) ->
                liftHandler $ throwE ErrReadRewardAccountNotAShelleyWallet

            (_, Nothing) ->
                pure (NoWithdrawal, selfRewardCredentials)

            (Just Refl, Just SelfWithdrawal) -> do
                (acct, path) <- liftHandler $ W.readRewardAccount @_ @s @k @n wrk wid
                wdrl <- liftHandler $ W.queryRewardBalance @_ wrk acct
                (, selfRewardCredentials) . WithdrawalSelf acct path
                    <$> liftIO (W.readNextWithdrawal @_ @k wrk wdrl)

            (Just Refl, Just (ExternalWithdrawal (ApiMnemonicT mw))) -> do
                let (xprv, acct, path) = W.someRewardAccount @ShelleyKey mw
                wdrl <- liftHandler (W.queryRewardBalance @_ wrk acct)
                    >>= liftIO . W.readNextWithdrawal @_ @k wrk
                when (wdrl == Coin 0) $ do
                    liftHandler $ throwE ErrWithdrawalNotWorth
                pure (WithdrawalExternal acct path wdrl, const (xprv, mempty))

-- | Makes an 'ApiCoinSelection' from the given 'UnsignedTx'.
mkApiCoinSelection
    :: forall n input output change withdrawal.
        ( input ~ (TxIn, TxOut, NonEmpty DerivationIndex)
        , output ~ TxOut
        , change ~ TxChange (NonEmpty DerivationIndex)
        , withdrawal ~ (RewardAccount, Coin, NonEmpty DerivationIndex)
        )
    => [Coin]
    -> Maybe (DelegationAction, NonEmpty DerivationIndex)
    -> Maybe W.TxMetadata
    -> UnsignedTx input output change withdrawal
    -> ApiCoinSelection n
mkApiCoinSelection deps mcerts meta (UnsignedTx inputs outputs change wdrls) =
    ApiCoinSelection
        (mkApiCoinSelectionInput <$> inputs)
        (mkApiCoinSelectionOutput <$> outputs)
        (mkApiCoinSelectionChange <$> change)
        (mkApiCoinSelectionWithdrawal <$> wdrls)
        (fmap (uncurry mkCertificates) mcerts)
        (fmap mkApiCoin deps)
        (ApiRawMetadata . serialiseToCBOR <$> meta)
  where
    mkCertificates
        :: DelegationAction
        -> NonEmpty DerivationIndex
        -> NonEmpty Api.ApiCertificate
    mkCertificates action xs =
        case action of
            Join pid -> NE.fromList
                [ Api.JoinPool apiStakePath (ApiT pid)
                ]

            RegisterKeyAndJoin pid -> NE.fromList
                [ Api.RegisterRewardAccount apiStakePath
                , Api.JoinPool apiStakePath (ApiT pid)
                ]

            Quit -> NE.fromList
                [ Api.QuitPool apiStakePath
                ]
      where
        apiStakePath = ApiT <$> xs

    mkApiCoinSelectionInput :: input -> ApiCoinSelectionInput n
    mkApiCoinSelectionInput
        (TxIn txid index, TxOut addr (TokenBundle amount assets), path) =
        ApiCoinSelectionInput
            { id = ApiT txid
            , index = index
            , address = (ApiT addr, Proxy @n)
            , amount = coinToQuantity amount
            , assets = ApiT assets
            , derivationPath = ApiT <$> path
            }

    mkApiCoinSelectionOutput :: output -> ApiCoinSelectionOutput n
    mkApiCoinSelectionOutput (TxOut addr (TokenBundle amount assets)) =
        ApiCoinSelectionOutput (ApiT addr, Proxy @n)
        (coinToQuantity amount)
        (ApiT assets)

    mkApiCoinSelectionChange :: change -> ApiCoinSelectionChange n
    mkApiCoinSelectionChange txChange =
        ApiCoinSelectionChange
            { address =
                (ApiT $ view #address txChange, Proxy @n)
            , amount =
                coinToQuantity $ view #amount txChange
            , assets =
                ApiT $ view #assets txChange
            , derivationPath =
                ApiT <$> view #derivationPath txChange
            }

    mkApiCoinSelectionWithdrawal :: withdrawal -> ApiCoinSelectionWithdrawal n
    mkApiCoinSelectionWithdrawal (rewardAcct, wdrl, path) =
        ApiCoinSelectionWithdrawal
            { stakeAddress =
                (ApiT rewardAcct, Proxy @n)
            , amount =
                coinToQuantity wdrl
            , derivationPath =
                ApiT <$> path
            }

mkApiTransaction
    :: forall n. ()
    => TimeInterpreter (ExceptT PastHorizonException IO)
    -> Hash "Tx"
    -> Maybe Coin
    -> [(TxIn, Maybe TxOut)]
    -> [TxOut]
    -> Map RewardAccount Coin
    -> (W.TxMeta, UTCTime)
    -> Maybe W.TxMetadata
    -> Lens' (ApiTransaction n) (Maybe ApiBlockReference)
    -> IO (ApiTransaction n)
mkApiTransaction ti txid mfee ins outs ws (meta, timestamp) txMeta setTimeReference = do
    timeRef <- (#time .~ timestamp) <$> makeApiBlockReference
        (neverFails "makeApiBlockReference shouldn't fail getting the time of \
            \transactions with slots in the past" ti)
        (meta ^. #slotNo)
        (natural $ meta ^. #blockHeight)

    expRef <- traverse makeApiSlotReference' (meta ^. #expiry)
    return $ tx & setTimeReference .~ Just timeRef & #expiresAt .~ expRef
  where
    -- Since tx expiry can be far in the future, we use unsafeExtendSafeZone for
    -- now.
    makeApiSlotReference' = makeApiSlotReference (unsafeExtendSafeZone ti)

    tx :: ApiTransaction n
    tx = ApiTransaction
        { id = ApiT txid
        , amount = Quantity . fromIntegral $ meta ^. #amount . #unCoin
        , fee = maybe (Quantity 0) (Quantity . fromIntegral . unCoin) mfee
        , deposit = Quantity depositIfAny
        , insertedAt = Nothing
        , pendingSince = Nothing
        , expiresAt = Nothing
        , depth = Nothing
        , direction = ApiT (meta ^. #direction)
        , inputs = [ApiTxInput (fmap toAddressAmount o) (ApiT i) | (i, o) <- ins]
        , outputs = toAddressAmount <$> outs
        , withdrawals = mkApiWithdrawal @n <$> Map.toList ws
        , mint = mempty  -- TODO: ADP-xxx
        , status = ApiT (meta ^. #status)
        , metadata = ApiTxMetadata $ ApiT <$> txMeta
        }

    depositIfAny :: Natural
    depositIfAny
        -- NOTE: totalIn will be zero for incoming transactions where inputs are
        -- unknown, in which case, we also give no visibility on the deposit.
        --
        -- For outgoing transactions however, the totalIn is guaranteed to be
        -- greater or equal to totalOut; any remainder is actually a
        -- deposit. Said differently, if totalIn > 0, then necessarily 'fee' on
        -- metadata should be 'Just{}'
        | meta ^. #direction == W.Outgoing =
            if totalIn < totalOut
            then 0 -- This should not be possible in practice. See FIXME below.
            else totalIn - totalOut
        | otherwise = 0
      where
        -- FIXME: ADP-460
        --
        -- In theory, the input side can't be smaller than the output side.
        -- However, since we do not yet track 'reclaims', we may end up in
        -- situation here where we no longer know that there's a reclaim on a
        -- transaction and numbers do not add up. Ideally, we would like to
        -- change the `then` clause above to be `invariantViolation` instead of
        -- `0` but we can't do that until we acknowledge for reclaims up until
        -- here too.
        --
        -- `0` is an okay-ish placeholder in the meantime because we know that
        -- (at least at the moment of writing this comment) the wallet never
        -- registers a key and deregister a key at the same time. Thus, if we
        -- are in the case where the apparent totalIn is smaller than the
        -- total out, then necessary the deposit is null.
        --
        -- invariantViolation :: HasCallStack => a
        -- invariantViolation = error $ unlines
        --     [ "invariant violated: outputs larger than inputs"
        --     , "direction:   " <> show (meta ^. #direction)
        --     , "fee:         " <> show (getQuantity <$> (meta ^. #fee))
        --     , "inputs:      " <> show (fmap (view (#coin . #unCoin)) . snd <$> ins)
        --     , "reclaims:    " <> ...
        --     , "withdrawals: " <> show (view #unCoin <$> Map.elems ws)
        --     , "outputs:     " <> show (view (#coin . #unCoin) <$> outs)
        --     ]
        totalIn :: Natural
        totalIn
            = sum (txOutValue <$> mapMaybe snd ins)
            + sum (fromIntegral . unCoin <$> Map.elems ws)
            -- FIXME: ADP-460 + reclaims.

        totalOut :: Natural
        totalOut
            = sum (txOutValue <$> outs)
            + maybe 0 (fromIntegral . unCoin) mfee

        txOutValue :: TxOut -> Natural
        txOutValue = fromIntegral . unCoin . txOutCoin

    toAddressAmount :: TxOut -> AddressAmount (ApiT Address, Proxy n)
    toAddressAmount (TxOut addr (TokenBundle.TokenBundle coin assets)) =
        AddressAmount (ApiT addr, Proxy @n) (mkApiCoin coin) (ApiT assets)

mkApiCoin
    :: Coin
    -> Quantity "lovelace" Natural
mkApiCoin (Coin c) = Quantity $ fromIntegral c

mkApiFee :: Maybe Coin -> [Coin] -> FeeEstimation -> ApiFee
mkApiFee mDeposit minCoins (FeeEstimation estMin estMax) = ApiFee
    { estimatedMin = qty estMin
    , estimatedMax = qty estMax
    , minimumCoins = coinQuantity <$> minCoins
    , deposit = coinQuantity $ fromMaybe (Coin 0) mDeposit
    }
  where
    qty = Quantity . fromIntegral

mkApiWithdrawal
    :: forall (n :: NetworkDiscriminant). ()
    => (RewardAccount, Coin)
    -> ApiWithdrawal n
mkApiWithdrawal (acct, c) =
    ApiWithdrawal (ApiT acct, Proxy @n) (mkApiCoin c)

addressAmountToTxOut
    :: forall (n :: NetworkDiscriminant). AddressAmount (ApiT Address, Proxy n)
    -> TxOut
addressAmountToTxOut (AddressAmount (ApiT addr, _) c (ApiT assets)) =
    TxOut addr (TokenBundle.TokenBundle (coinFromQuantity c) assets)

natural :: Quantity q Word32 -> Quantity q Natural
natural = Quantity . fromIntegral . getQuantity

apiSlotId :: SlotId -> ApiSlotId
apiSlotId slotId = ApiSlotId
   (ApiT $ slotId ^. #epochNumber)
   (ApiT $ slotId ^. #slotNumber)

makeApiBlockReference
    :: Monad m
    => TimeInterpreter m
    -> SlotNo
    -> Quantity "block" Natural
    -> m ApiBlockReference
makeApiBlockReference ti sl height = do
    slotId <- interpretQuery ti (toSlotId sl)
    slotTime <- interpretQuery ti (slotToUTCTime sl)
    return $ ApiBlockReference
        { absoluteSlotNumber = ApiT sl
        , slotId = apiSlotId slotId
        , time = slotTime
        , block = ApiBlockInfo { height }
        }

makeApiBlockReferenceFromHeader
    :: Monad m
    => TimeInterpreter m
    -> BlockHeader
    -> m ApiBlockReference
makeApiBlockReferenceFromHeader ti tip =
    makeApiBlockReference ti (tip ^. #slotNo) (natural $ tip ^. #blockHeight)

makeApiSlotReference
    :: Monad m
    => TimeInterpreter m
    -> SlotNo
    -> m ApiSlotReference
makeApiSlotReference ti sl =
    ApiSlotReference (ApiT sl)
        <$> fmap apiSlotId (interpretQuery ti $ toSlotId sl)
        <*> interpretQuery ti (slotToUTCTime sl)

getWalletTip
    :: Monad m
    => TimeInterpreter m
    -> Wallet s
    -> m ApiBlockReference
getWalletTip ti = makeApiBlockReferenceFromHeader ti . currentTip

{-------------------------------------------------------------------------------
                                Api Layer
-------------------------------------------------------------------------------}

-- | Create a new instance of the wallet layer.
newApiLayer
    :: forall ctx s k.
        ( ctx ~ ApiLayer s k
        , IsOurs s RewardAccount
        , IsOurs s Address
        )
    => Tracer IO WalletEngineLog
    -> (Block, NetworkParameters, SyncTolerance)
    -> NetworkLayer IO Block
    -> TransactionLayer k
    -> DBFactory IO s k
    -> TokenMetadataClient IO
    -> (WorkerCtx ctx -> WalletId -> IO ())
        -- ^ Action to run concurrently with wallet restore
    -> IO ctx
newApiLayer tr g0 nw tl df tokenMeta coworker = do
    re <- Registry.empty
    let trTx = contramap MsgSubmitSealedTx tr
    let trW = contramap MsgWallet tr
    let ctx = ApiLayer trTx trW g0 nw tl df re tokenMeta
    listDatabases df >>= mapM_ (startWalletWorker ctx coworker)
    return ctx

-- | Register a wallet restoration thread with the worker registry.
startWalletWorker
    :: forall ctx s k.
        ( ctx ~ ApiLayer s k
        , IsOurs s RewardAccount
        , IsOurs s Address
        )
    => ctx
    -> (WorkerCtx ctx -> WalletId -> IO ())
        -- ^ Action to run concurrently with restore
    -> WalletId
    -> IO ()
startWalletWorker ctx coworker = void . registerWorker ctx before coworker
  where
    before ctx' wid =
        runExceptT (W.checkWalletIntegrity ctx' wid gp)
        >>= either throwIO pure
    (_, NetworkParameters gp _ _, _) = ctx ^. genesisData

-- | Register a wallet create and restore thread with the worker registry.
-- See 'Cardano.Wallet#createWallet'
createWalletWorker
    :: forall ctx s k.
        ( ctx ~ ApiLayer s k
        , IsOurs s RewardAccount
        , IsOurs s Address
        )
    => ctx
        -- ^ Surrounding API context
    -> WalletId
        -- ^ Wallet Id
    -> (WorkerCtx ctx -> ExceptT ErrWalletAlreadyExists IO WalletId)
        -- ^ Create action
    -> (WorkerCtx ctx -> WalletId -> IO ())
        -- ^ Action to run concurrently with restore
    -> ExceptT ErrCreateWallet IO WalletId
createWalletWorker ctx wid createWallet coworker =
    liftIO (Registry.lookup re wid) >>= \case
        Just _ ->
            throwE $ ErrCreateWalletAlreadyExists $ ErrWalletAlreadyExists wid
        Nothing ->
            liftIO (registerWorker ctx before coworker wid) >>= \case
                Nothing -> throwE ErrCreateWalletFailedToCreateWorker
                Just _ -> pure wid
  where
    before ctx' _ = void $ unsafeRunExceptT $ createWallet ctx'
    re = ctx ^. workerRegistry @s @k

-- | Create a worker for an existing wallet, register it, then start the worker
-- thread. This is used by 'startWalletWorker' and 'createWalletWorker'.
registerWorker
    :: forall ctx s k.
        ( ctx ~ ApiLayer s k
        , IsOurs s RewardAccount
        , IsOurs s Address
        )
    => ctx
    -> (WorkerCtx ctx -> WalletId -> IO ())
        -- ^ First action to run after starting the worker thread.
    -> (WorkerCtx ctx -> WalletId -> IO ())
        -- ^ Action to run concurrently with restore.
    -> WalletId
    -> IO (Maybe ctx)
registerWorker ctx before coworker wid =
    fmap (const ctx) <$> Registry.register @_ @ctx re ctx wid config
  where
    re = ctx ^. workerRegistry @s @k
    df = ctx ^. dbFactory
    config = MkWorker
        { workerAcquire = withDatabase df wid
        , workerBefore = before
        , workerAfter = defaultWorkerAfter
        -- fixme: ADP-641 Review error handling here
        , workerMain = \ctx' _ -> race_
            (unsafeRunExceptT $ W.restoreWallet ctx' wid)
            (coworker ctx' wid)
        }

-- | Something to pass as the coworker action to 'newApiLayer', which does
-- nothing, and never exits.
idleWorker :: ctx -> wid -> IO a
idleWorker _ _ = forever $ threadDelay maxBound

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

data ErrTemporarilyDisabled = ErrTemporarilyDisabled
    deriving (Eq, Show)

-- | Small helper to easy show things to Text
showT :: Show a => a -> Text
showT = T.pack . show

instance LiftHandler ErrTemporarilyDisabled where
    handler = \case
        ErrTemporarilyDisabled ->
            apiError err501 NotImplemented $ mconcat
                [ "This endpoint is temporarily disabled. It'll be made "
                , "accessible again in future releases."
                ]

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
                [ "That's embarrassing. My associated worker for", toText wid
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
                [ "That's embarrassing. Your wallet looks good, but I couldn't "
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

instance LiftHandler ErrListAssets where
    handler = \case
        ErrListAssetsNoSuchWallet e -> handler e

instance LiftHandler ErrGetAsset where
    handler = \case
        ErrGetAssetNoSuchWallet e -> handler e
        ErrGetAssetNotPresent ->
            apiError err404 AssetNotPresent $ mconcat
                [ "The requested asset is not associated with this wallet."
                ]

instance LiftHandler ErrListUTxOStatistics where
    handler = \case
        ErrListUTxOStatisticsNoSuchWallet e -> handler e

instance LiftHandler ErrMkTx where
    handler = \case
        ErrKeyNotFoundForAddress addr ->
            apiError err500 KeyNotFoundForAddress $ mconcat
                [ "That's embarrassing. I couldn't sign the given transaction: "
                , "I haven't found the corresponding private key for a known "
                , "input address I should keep track of: ", showT addr, ". "
                , "Retrying may work, but something really went wrong..."
                ]
        ErrConstructedInvalidTx hint ->
            apiError err500 CreatedInvalidTransaction hint
        ErrInvalidEra _era ->
            apiError err500 CreatedInvalidTransaction $ mconcat
                [ "Whoops, it seems like I just experienced a hard-fork in the "
                , "middle of other tasks. This is a pretty rare situation but "
                , "as a result, I must throw-away what I was doing. Please "
                , "retry whatever you were doing in a short delay."
                ]

instance LiftHandler ErrSignPayment where
    handler = \case
        ErrSignPaymentMkTx e -> handler e
        ErrSignPaymentNoSuchWallet e -> (handler e)
            { errHTTPCode = 404
            , errReasonPhrase = errReasonPhrase err404
            }
        ErrSignPaymentWithRootKey e@ErrWithRootKeyNoRootKey{} -> (handler e)
            { errHTTPCode = 403
            , errReasonPhrase = errReasonPhrase err403
            }
        ErrSignPaymentWithRootKey e@ErrWithRootKeyWrongPassphrase{} -> handler e
        ErrSignPaymentIncorrectTTL e -> handler e

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
            ErrPostTxBadRequest err ->
                apiError err500 CreatedInvalidTransaction $ mconcat
                    [ "That's embarrassing. It looks like I've created an "
                    , "invalid transaction that could not be parsed by the "
                    , "node. Here's an error message that may help with "
                    , "debugging: ", err
                    ]
            ErrPostTxProtocolFailure err ->
                apiError err500 RejectedByCoreNode $ mconcat
                    [ "I successfully submitted a transaction, but "
                    , "unfortunately it was rejected by a relay. This could be "
                    , "because the fee was not large enough, or because the "
                    , "transaction conflicts with another transaction that "
                    , "uses one or more of the same inputs, or it may be due "
                    , "to some other reason. Here's an error message that may "
                    , "help with debugging: ", err
                    ]
        ErrSubmitExternalTxDecode e -> (handler e)
            { errHTTPCode = 400
            , errReasonPhrase = errReasonPhrase err400
            }

instance LiftHandler ErrRemoveTx where
    handler = \case
        ErrRemoveTxNoSuchWallet wid -> handler wid
        ErrRemoveTxNoSuchTransaction tid ->
            apiError err404 NoSuchTransaction $ mconcat
                [ "I couldn't find a transaction with the given id: "
                , toText tid
                ]
        ErrRemoveTxAlreadyInLedger tid ->
            apiError err403 TransactionAlreadyInLedger $ mconcat
                [ "The transaction with id: ", toText tid,
                  " cannot be forgotten as it is already in the ledger."
                ]

instance LiftHandler ErrPostTx where
    handler = \case
        ErrPostTxBadRequest err ->
            apiError err500 CreatedInvalidTransaction $ mconcat
            [ "That's embarrassing. It looks like I've created an "
            , "invalid transaction that could not be parsed by the "
            , "node. Here's an error message that may help with "
            , "debugging: ", err
            ]
        ErrPostTxProtocolFailure err ->
            apiError err500 RejectedByCoreNode $ mconcat
            [ "I successfully submitted a transaction, but "
            , "unfortunately it was rejected by a relay. This could be "
            , "because the fee was not large enough, or because the "
            , "transaction conflicts with another transaction that "
            , "uses one or more of the same inputs, or it may be due "
            , "to some other reason. Here's an error message that may "
            , "help with debugging: ", err
            ]

instance LiftHandler ErrSubmitTx where
    handler = \case
        ErrSubmitTxNetwork e -> handler e
        ErrSubmitTxNoSuchWallet e@ErrNoSuchWallet{} -> (handler e)
            { errHTTPCode = 404
            , errReasonPhrase = errReasonPhrase err404
            }

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

instance LiftHandler ErrJoinStakePool where
    handler = \case
        ErrJoinStakePoolNoSuchWallet e -> handler e
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
        ErrFetchRewardsReadRewardAccount e -> handler e

instance LiftHandler ErrReadRewardAccount where
    handler = \case
        ErrReadRewardAccountNoSuchWallet e -> handler e
        ErrReadRewardAccountNotAShelleyWallet ->
            apiError err403 InvalidWalletType $ mconcat
                [ "It is regrettable but you've just attempted an operation "
                , "that is invalid for this type of wallet. Only new 'Shelley' "
                , "wallets can do something with rewards and this one isn't."
                ]

instance LiftHandler ErrQuitStakePool where
    handler = \case
        ErrQuitStakePoolNoSuchWallet e -> handler e
        ErrQuitStakePoolCannotQuit e -> case e of
            ErrNotDelegatingOrAboutTo ->
                apiError err403 NotDelegatingTo $ mconcat
                    [ "It seems that you're trying to retire from delegation "
                    , "although you're not even delegating, nor won't be in an "
                    , "immediate future."
                    ]
            ErrNonNullRewards (Coin rewards) ->
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

instance LiftHandler ErrSignMetadataWith where
    handler = \case
        ErrSignMetadataWithRootKey e -> handler e
        ErrSignMetadataWithNoSuchWallet e -> handler e
        ErrSignMetadataWithInvalidIndex e -> handler e

instance LiftHandler ErrReadAccountPublicKey where
    handler = \case
        ErrReadAccountPublicKeyRootKey e -> handler e
        ErrReadAccountPublicKeyNoSuchWallet e -> handler e
        ErrReadAccountPublicKeyInvalidIndex e -> handler e

instance LiftHandler ErrDerivePublicKey where
    handler = \case
        ErrDerivePublicKeyNoSuchWallet e -> handler e
        ErrDerivePublicKeyInvalidIndex e -> handler e

instance LiftHandler (ErrInvalidDerivationIndex 'Soft level) where
    handler = \case
        ErrIndexOutOfBound minIx maxIx _ix ->
            apiError err403 SoftDerivationRequired $ mconcat
                [ "It looks like you've provided a derivation index that is "
                , "out of bound. The index is well-formed, but I require "
                , "indexes valid for soft derivation only. That is, indexes "
                , "between ", pretty minIx, " and ", pretty maxIx, " without a suffix."
                ]

instance LiftHandler ErrSelectionCriteria where
    handler = \case
        ErrSelectionCriteriaOutputTokenBundleSizeExceedsLimit e ->
            handler e
        ErrSelectionCriteriaOutputTokenQuantityExceedsLimit e ->
            handler e

instance LiftHandler ErrOutputTokenBundleSizeExceedsLimit where
    handler e = apiError err403 OutputTokenBundleSizeExceedsLimit $ mconcat
        [ "One of the outputs you've specified contains too many assets. "
        , "Try splitting these assets across two or more outputs. "
        , "Destination address: "
        , pretty (view #address e)
        , ". Asset count: "
        , pretty (view #assetCount e)
        , "."
        ]

instance LiftHandler ErrOutputTokenQuantityExceedsLimit where
    handler e = apiError err403 OutputTokenQuantityExceedsLimit $ mconcat
        [ "One of the token quantities you've specified is greater than the "
        , "maximum quantity allowed in a single transaction output. Try "
        , "splitting this quantity across two or more outputs. "
        , "Destination address: "
        , pretty (view #address e)
        , ". Token policy identifier: "
        , pretty (view #tokenPolicyId $ asset e)
        , ". Asset name: "
        , pretty (view #tokenName $ asset e)
        , ". Token quantity specified: "
        , pretty (view #quantity e)
        , ". Maximum allowable token quantity: "
        , pretty (view #quantityMaxBound e)
        , "."
        ]

instance LiftHandler ErrSelectAssets where
    handler = \case
        ErrSelectAssetsCriteriaError e -> handler e
        ErrSelectAssetsNoSuchWallet e -> handler e
        ErrSelectAssetsAlreadyWithdrawing tx ->
            apiError err403 AlreadyWithdrawing $ mconcat
                [ "I already know of a pending transaction with withdrawals: "
                , toText (txId tx), ". Note that when I withdraw rewards, I "
                , "need to withdraw them fully for the Ledger to accept it. "
                , "There's therefore no point creating another conflicting "
                , "transaction; if, for some reason, you really want a new "
                , "transaction, then cancel the previous one first."
                ]
        ErrSelectAssetsSelectionError selectionError ->
            case selectionError of
                BalanceInsufficient e ->
                    apiError err403 NotEnoughMoney $ mconcat
                        [ "I can't process this payment as there are not "
                        , "enough funds available in the wallet. I am "
                        , "missing: ", pretty . Flat $ balanceMissing e
                        ]
                SelectionInsufficient e ->
                    apiError err403 TransactionIsTooBig $ mconcat
                        [ "I am not able to finalize the transaction "
                        , "because I need to select additional inputs and "
                        , "doing so will make the transaction too big. Try "
                        , "sending a smaller amount. I had already selected "
                        , showT (length $ inputsSelected e), " inputs."
                        ]
                InsufficientMinCoinValues xs ->
                    apiError err403 UtxoTooSmall $ mconcat
                        [ "Some outputs have ada values that are too small. "
                        , "There's a minimum ada value specified by the "
                        , "protocol that each output must satisfy. I'll handle "
                        , "that minimum value myself when you do not explicitly "
                        , "specify an ada value for an output. Otherwise, you "
                        , "must specify enough ada. Here are the problematic "
                        , "outputs:\n" <> pretty (indentF 2 $ blockListF xs)
                        ]
                UnableToConstructChange e ->
                    apiError err403 CannotCoverFee $ mconcat
                        [ "I am unable to finalize the transaction, as there is "
                        , "not enough ada I can use to pay for fees, or to "
                        , "satisfy the minimum ada quantities of change outputs. "
                        , "I need about pretty ", pretty (shortfall e), " ada to "
                        , "proceed; try increasing your wallet balance as such, "
                        , "or try sending a different, smaller payment."
                        ]

instance LiftHandler (ErrInvalidDerivationIndex 'Hardened level) where
    handler = \case
        ErrIndexOutOfBound minIx maxIx _ix ->
            apiError err403 HardenedDerivationRequired $ mconcat
                [ "It looks like you've provided a derivation index that is "
                , "out of bound. The index is well-formed, but I require "
                , "indexes valid for hardened derivation only. That is, indexes "
                , "between ", pretty minIx, " and ", pretty maxIx, " with a suffix 'H'."
                ]

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
        406 ->
            let cType =
                    -- FIXME: Ugly and not really scalable nor maintainable.
                    if ["wallets"] `isPrefixOf` pathInfo req
                    && ["signatures"] `isInfixOf` pathInfo req
                    then "application/octet-stream"
                    else "application/json"
            in apiError err' NotAcceptable $ mconcat
            [ "It seems as though you don't accept '", cType,"', but "
            , "unfortunately I only speak '", cType,"'! Please "
            , "double-check your 'Accept' request header and make sure it's "
            , "set to '", cType,"'."
            ]
        415 ->
            let cType =
                    -- FIXME: Ugly and not really scalable nor maintainable.
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

{-------------------------------------------------------------------------------
                               Logging
-------------------------------------------------------------------------------}

-- | The type of log messages coming from the server 'ApiLayer', which may or
-- may not be associated with a particular worker thread.
data WalletEngineLog
    = MsgWallet (WorkerLog WalletId WalletLog)
    | MsgSubmitSealedTx TxSubmitLog
    deriving (Show, Eq)

instance ToText WalletEngineLog where
    toText = \case
        MsgWallet msg -> toText msg
        MsgSubmitSealedTx msg -> toText msg

instance HasPrivacyAnnotation WalletEngineLog where
    getPrivacyAnnotation = \case
        MsgWallet msg -> getPrivacyAnnotation msg
        MsgSubmitSealedTx msg -> getPrivacyAnnotation msg

instance HasSeverityAnnotation WalletEngineLog where
    getSeverityAnnotation = \case
        MsgWallet msg -> getSeverityAnnotation msg
        MsgSubmitSealedTx msg -> getSeverityAnnotation msg
