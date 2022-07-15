{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
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
    , getNetworkClock
    , getNetworkInformation
    , getNetworkParameters
    , getUTxOsStatistics
    , getWalletUtxoSnapshot
    , getWallet
    , joinStakePool
    , listAssets
    , getAsset
    , getAssetDefault
    , listAddresses
    , listTransactions
    , getTransaction
    , constructTransaction
    , listWallets
    , listStakeKeys
    , createMigrationPlan
    , migrateWallet
    , postExternalTransaction
    , postIcarusWallet
    , postLedgerWallet
    , postRandomAddress
    , postRandomWallet
    , postRandomWalletFromXPrv
    , signTransaction
    , postTransactionOld
    , postTransactionFeeOld
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
    , getAccountPublicKey
    , postSharedWallet
    , patchSharedWallet
    , mkSharedWallet
    , balanceTransaction
    , decodeTransaction
    , submitTransaction
    , getPolicyKey
    , postPolicyKey
    , postPolicyId
    , constructSharedTransaction

    -- * Server error responses
    , IsServerError(..)
    , liftHandler
    , apiError

    -- * Internals
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
import Cardano.Address.Script
    ( Cosigner (..)
    , ScriptTemplate (..)
    , ValidationLevel (..)
    , foldScript
    , validateScriptOfTemplate
    )
import Cardano.Api
    ( AnyCardanoEra (..), CardanoEra (..), SerialiseAsCBOR (..) )
import Cardano.Api.Extra
    ( asAnyShelleyBasedEra, inAnyCardanoEra, withShelleyBasedTx )
import Cardano.BM.Tracing
    ( HasPrivacyAnnotation (..), HasSeverityAnnotation (..) )
import Cardano.Ledger.Alonzo.TxInfo
    ( TranslationError (TimeTranslationPastHorizon, TranslationLogicMissingInput)
    )
import Cardano.Mnemonic
    ( SomeMnemonic )
import Cardano.Wallet
    ( ErrAddCosignerKey (..)
    , ErrBalanceTx (..)
    , ErrBalanceTxInternalError (..)
    , ErrCannotJoin (..)
    , ErrCannotQuit (..)
    , ErrConstructSharedWallet (..)
    , ErrConstructTx (..)
    , ErrCreateMigrationPlan (..)
    , ErrCreateRandomAddress (..)
    , ErrDecodeTx (..)
    , ErrDerivePublicKey (..)
    , ErrFetchRewards (..)
    , ErrGetPolicyId (..)
    , ErrGetTransaction (..)
    , ErrImportAddress (..)
    , ErrImportRandomAddress (..)
    , ErrInvalidDerivationIndex (..)
    , ErrListTransactions (..)
    , ErrListUTxOStatistics (..)
    , ErrMkTransaction (..)
    , ErrNoSuchTransaction (..)
    , ErrNoSuchWallet (..)
    , ErrNotASequentialWallet (..)
    , ErrPostTx (..)
    , ErrReadAccountPublicKey (..)
    , ErrReadPolicyPublicKey (..)
    , ErrReadRewardAccount (..)
    , ErrRemoveTx (..)
    , ErrSelectAssets (..)
    , ErrSignMetadataWith (..)
    , ErrSignPayment (..)
    , ErrStakePoolDelegation (..)
    , ErrStartTimeLaterThanEndTime (..)
    , ErrSubmitTransaction (..)
    , ErrSubmitTx (..)
    , ErrUpdatePassphrase (..)
    , ErrUpdateSealedTx (..)
    , ErrWalletAlreadyExists (..)
    , ErrWalletNotResponding (..)
    , ErrWithRootKey (..)
    , ErrWithdrawalNotWorth (..)
    , ErrWitnessTx (..)
    , ErrWritePolicyPublicKey (..)
    , ErrWrongPassphrase (..)
    , FeeEstimation (..)
    , HasNetworkLayer
    , TxSubmitLog
    , genesisData
    , manageRewardBalance
    , networkLayer
    )
import Cardano.Wallet.Address.Book
    ( AddressBookIso )
import Cardano.Wallet.Api
    ( ApiLayer (..)
    , HasDBFactory
    , HasTokenMetadataClient
    , HasWorkerRegistry
    , WalletLock (..)
    , dbFactory
    , tokenMetadataClient
    , walletLocks
    , workerRegistry
    )
import Cardano.Wallet.Api.Server.Tls
    ( TlsConfiguration (..), requireClientAuth )
import Cardano.Wallet.Api.Types
    ( AccountPostData (..)
    , AddressAmount (..)
    , AddressAmountNoAssets (..)
    , ApiAccountPublicKey (..)
    , ApiActiveSharedWallet (..)
    , ApiAddress (..)
    , ApiAnyCertificate (..)
    , ApiAsArray (..)
    , ApiAsset (..)
    , ApiAssetMintBurn (..)
    , ApiBalanceTransactionPostData
    , ApiBlockInfo (..)
    , ApiBlockReference (..)
    , ApiBurnData (..)
    , ApiByronWallet (..)
    , ApiByronWalletBalance (..)
    , ApiBytesT (..)
    , ApiCertificate (..)
    , ApiCoinSelection (..)
    , ApiCoinSelectionChange (..)
    , ApiCoinSelectionCollateral (..)
    , ApiCoinSelectionOutput (..)
    , ApiCoinSelectionWithdrawal (..)
    , ApiConstructTransaction (..)
    , ApiConstructTransactionData (..)
    , ApiDecodedTransaction (..)
    , ApiDeregisterPool (..)
    , ApiEpochInfo (ApiEpochInfo)
    , ApiEra (..)
    , ApiErrorCode (..)
    , ApiExternalCertificate (..)
    , ApiExternalInput (..)
    , ApiFee (..)
    , ApiForeignStakeKey (..)
    , ApiMintBurnData (..)
    , ApiMintBurnOperation (..)
    , ApiMintData (..)
    , ApiMnemonicT (..)
    , ApiMultiDelegationAction (..)
    , ApiNetworkClock (..)
    , ApiNetworkInformation
    , ApiNetworkParameters (..)
    , ApiNullStakeKey (..)
    , ApiOurStakeKey (..)
    , ApiPaymentDestination (..)
    , ApiPendingSharedWallet (..)
    , ApiPolicyId (..)
    , ApiPolicyKey (..)
    , ApiPoolId (..)
    , ApiPostAccountKeyDataWithPurpose (..)
    , ApiPostPolicyIdData
    , ApiPostPolicyKeyData (..)
    , ApiPostRandomAddressData (..)
    , ApiPutAddressesData (..)
    , ApiRedeemer (..)
    , ApiRegisterPool (..)
    , ApiScriptTemplateEntry (..)
    , ApiSelectCoinsPayments
    , ApiSerialisedTransaction (..)
    , ApiSharedWallet (..)
    , ApiSharedWalletPatchData (..)
    , ApiSharedWalletPostData (..)
    , ApiSharedWalletPostDataFromAccountPubX (..)
    , ApiSharedWalletPostDataFromMnemonics (..)
    , ApiSignTransactionPostData (..)
    , ApiSlotId (..)
    , ApiSlotReference (..)
    , ApiStakeKeyIndex (..)
    , ApiStakeKeys (..)
    , ApiT (..)
    , ApiTokenAmountFingerprint (..)
    , ApiTokens (..)
    , ApiTransaction (..)
    , ApiTxCollateral (..)
    , ApiTxId (..)
    , ApiTxInput (..)
    , ApiTxInputGeneral (..)
    , ApiTxMetadata (..)
    , ApiTxOutputGeneral (..)
    , ApiUtxoStatistics (..)
    , ApiValidityBound (..)
    , ApiValidityInterval (..)
    , ApiWallet (..)
    , ApiWalletAssetsBalance (..)
    , ApiWalletBalance (..)
    , ApiWalletDelegation (..)
    , ApiWalletDelegationNext (..)
    , ApiWalletDelegationStatus (..)
    , ApiWalletInput (..)
    , ApiWalletMigrationBalance (..)
    , ApiWalletMigrationPlan (..)
    , ApiWalletMigrationPlanPostData (..)
    , ApiWalletMigrationPostData (..)
    , ApiWalletOutput (..)
    , ApiWalletPassphrase (..)
    , ApiWalletPassphraseInfo (..)
    , ApiWalletSignData (..)
    , ApiWalletUtxoSnapshot (..)
    , ApiWalletUtxoSnapshotEntry (..)
    , ApiWithdrawal (..)
    , ApiWithdrawalGeneral (..)
    , ApiWithdrawalPostData (..)
    , ByronWalletFromXPrvPostData
    , ByronWalletPostData (..)
    , ByronWalletPutPassphraseData (..)
    , Iso8601Time (..)
    , KeyFormat (..)
    , KnownDiscovery (..)
    , MinWithdrawal (..)
    , PostTransactionFeeOldData (..)
    , PostTransactionOldData (..)
    , ResourceContext (..)
    , VerificationKeyHashing (..)
    , WalletOrAccountPostData (..)
    , WalletPostData (..)
    , WalletPutData (..)
    , WalletPutPassphraseData (..)
    , XPubOrSelf (..)
    , coinFromQuantity
    , coinToQuantity
    , getApiMnemonicT
    , toApiAsset
    , toApiEpochInfo
    , toApiNetworkParameters
    , toApiUtxoStatistics
    )
import Cardano.Wallet.Api.Types.SchemaMetadata
    ( TxMetadataSchema (..), TxMetadataWithSchema (TxMetadataWithSchema) )
import Cardano.Wallet.CoinSelection
    ( SelectionBalanceError (..)
    , SelectionCollateralError
    , SelectionError (..)
    , SelectionOf (..)
    , SelectionOutputError (..)
    , SelectionOutputSizeExceedsLimitError (..)
    , SelectionOutputTokenQuantityExceedsLimitError (..)
    , SelectionStrategy (..)
    , WalletSelectionContext
    , balanceMissing
    , selectionDelta
    , shortfall
    )
import Cardano.Wallet.Compat
    ( (^?) )
import Cardano.Wallet.DB
    ( DBFactory (..) )
import Cardano.Wallet.Network
    ( NetworkLayer, fetchRewardAccountBalances, timeInterpreter )
import Cardano.Wallet.Primitive.AddressDerivation
    ( DelegationAddress (..)
    , Depth (..)
    , DerivationIndex (..)
    , DerivationType (..)
    , HardDerivation (..)
    , Index (..)
    , MkKeyFingerprint
    , NetworkDiscriminant (..)
    , PaymentAddress (..)
    , RewardAccount (..)
    , Role
    , SoftDerivation (..)
    , WalletKey (..)
    , deriveRewardAccount
    , digest
    , publicKey
    )
import Cardano.Wallet.Primitive.AddressDerivation.Byron
    ( ByronKey, mkByronKeyFromMasterKey )
import Cardano.Wallet.Primitive.AddressDerivation.Icarus
    ( IcarusKey )
import Cardano.Wallet.Primitive.AddressDerivation.MintBurn
    ( scriptSlotIntervals
    , toTokenMapAndScript
    , toTokenPolicyId
    , withinSlotInterval
    )
import Cardano.Wallet.Primitive.AddressDerivation.SharedKey
    ( SharedKey (..) )
import Cardano.Wallet.Primitive.AddressDerivation.Shelley
    ( ShelleyKey )
import Cardano.Wallet.Primitive.AddressDiscovery
    ( CompareDiscovery
    , GenChange (ArgGenChange)
    , GetAccount
    , GetPurpose (..)
    , IsOurs
    , IsOwned
    , KnownAddresses
    , MaybeLight
    , isOwned
    )
import Cardano.Wallet.Primitive.AddressDiscovery.Random
    ( RndState, mkRndState )
import Cardano.Wallet.Primitive.AddressDiscovery.Sequential
    ( DerivationPrefix (..)
    , SeqState (..)
    , defaultAddressPoolGap
    , getGap
    , mkSeqStateFromAccountXPub
    , mkSeqStateFromRootXPrv
    , purposeCIP1852
    )
import Cardano.Wallet.Primitive.AddressDiscovery.Shared
    ( CredentialType (..)
    , ErrAddCosigner (..)
    , ErrScriptTemplate (..)
    , SharedState (..)
    , mkSharedStateFromAccountXPub
    , mkSharedStateFromRootXPrv
    , toSharedWalletId
    , validateScriptTemplates
    )
import Cardano.Wallet.Primitive.Delegation.UTxO
    ( stakeKeyCoinDistr )
import Cardano.Wallet.Primitive.Migration
    ( MigrationPlan (..) )
import Cardano.Wallet.Primitive.Model
    ( Wallet
    , availableBalance
    , availableUTxO
    , currentTip
    , getState
    , totalBalance
    , totalUTxO
    )
import Cardano.Wallet.Primitive.Passphrase
    ( Passphrase (..)
    , PassphraseScheme (..)
    , WalletPassphraseInfo (..)
    , currentPassphraseScheme
    , preparePassphrase
    )
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
    , snapshot
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
    , PoolId
    , PoolLifeCycleStatus (..)
    , Signature (..)
    , SlotId
    , SlotNo (..)
    , SortOrder (..)
    , WalletId (..)
    , WalletMetadata (..)
    )
import Cardano.Wallet.Primitive.Types.Address
    ( Address (..), AddressState (..) )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..) )
import Cardano.Wallet.Primitive.Types.Hash
    ( Hash (..) )
import Cardano.Wallet.Primitive.Types.Redeemer
    ( Redeemer (..) )
import Cardano.Wallet.Primitive.Types.TokenBundle
    ( Flat (..), TokenBundle (..) )
import Cardano.Wallet.Primitive.Types.TokenMap
    ( AssetId (..), fromFlatList, toNestedList )
import Cardano.Wallet.Primitive.Types.TokenPolicy
    ( TokenName (..)
    , TokenPolicyId (..)
    , mkTokenFingerprint
    , nullTokenName
    , tokenNameMaxLength
    )
import Cardano.Wallet.Primitive.Types.TokenQuantity
    ( TokenQuantity (..) )
import Cardano.Wallet.Primitive.Types.Tx
    ( TransactionInfo
    , Tx (..)
    , TxChange (..)
    , TxIn (..)
    , TxOut (..)
    , TxStatus (..)
    , UnsignedTx (..)
    , cardanoTx
    , getSealedTxWitnesses
    , txMintBurnMaxTokenQuantity
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
    , ErrAssignRedeemers (..)
    , ErrSignTx (..)
    , TokenMapWithScripts (..)
    , TransactionCtx (..)
    , TransactionLayer (..)
    , Withdrawal (..)
    , defaultTransactionCtx
    )
import Cardano.Wallet.Unsafe
    ( unsafeRunExceptT )
import Cardano.Wallet.Util
    ( invariant )
import Control.Arrow
    ( second, (&&&) )
import Control.DeepSeq
    ( NFData )
import Control.Error.Util
    ( failWith )
import Control.Monad
    ( forM, forever, join, void, when, (>=>) )
import Control.Monad.IO.Class
    ( MonadIO, liftIO )
import Control.Monad.Trans.Except
    ( ExceptT (..), mapExceptT, runExceptT, throwE, withExceptT )
import Control.Monad.Trans.Maybe
    ( MaybeT (..), exceptToMaybeT )
import Control.Tracer
    ( Tracer, contramap )
import Crypto.Hash.Utils
    ( blake2b224 )
import Data.Aeson
    ( (.=) )
import Data.ByteString
    ( ByteString )
import Data.Coerce
    ( coerce )
import Data.Either
    ( isLeft )
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
    ( isInfixOf, isPrefixOf, isSubsequenceOf, sortOn, (\\) )
import Data.List.NonEmpty
    ( NonEmpty (..) )
import Data.Map.Strict
    ( Map )
import Data.Maybe
    ( catMaybes
    , fromJust
    , fromMaybe
    , isJust
    , isNothing
    , mapMaybe
    , maybeToList
    )
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
    ( (:~:) (..), type (==), testEquality )
import Data.Word
    ( Word32 )
import Fmt
    ( blockListF, indentF, listF, pretty )
import GHC.Generics
    ( Generic )
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

import qualified Cardano.Api as Cardano
import qualified Cardano.Wallet as W
import qualified Cardano.Wallet.Api.Types as Api
import qualified Cardano.Wallet.DB as W
import qualified Cardano.Wallet.Network as NW
import qualified Cardano.Wallet.Primitive.AddressDerivation.Byron as Byron
import qualified Cardano.Wallet.Primitive.AddressDerivation.Icarus as Icarus
import qualified Cardano.Wallet.Primitive.AddressDiscovery.Shared as Shared
import qualified Cardano.Wallet.Primitive.Types as W
import qualified Cardano.Wallet.Primitive.Types.Coin as Coin
import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle
import qualified Cardano.Wallet.Primitive.Types.TokenMap as TokenMap
import qualified Cardano.Wallet.Primitive.Types.Tx as W
import qualified Cardano.Wallet.Primitive.Types.UTxO as UTxO
import qualified Cardano.Wallet.Primitive.Types.UTxOIndex as UTxOIndex
import qualified Cardano.Wallet.Primitive.Types.UTxOSelection as UTxOSelection
import qualified Cardano.Wallet.Registry as Registry
import qualified Control.Concurrent.Concierge as Concierge
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Foldable as F
import qualified Data.List as L
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
        $ handleRawError (curry toServerError)
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
    -- Bad hostname -- Windows
    -- WSAHOST_NOT_FOUND, WSATRY_AGAIN, or bind: WSAEOPNOTSUPP
    | isUserError e && any hasDescription ["11001", "11002", "10045"] =
        Just (ListenErrorHostDoesNotExist hostPreference)
    -- Address is valid, but can't be used for listening -- Linux
    | show (ioeGetErrorType e) == "invalid argument" =
        Just (ListenErrorInvalidAddress hostPreference)
    -- Address is valid, but can't be used for listening -- Darwin
    | show (ioeGetErrorType e) == "unsupported operation" =
        Just (ListenErrorInvalidAddress hostPreference)
    -- Address is valid, but can't be used for listening -- Windows
    | isOtherError e && any hasDescription ["WSAEINVAL", "WSAEADDRNOTAVAIL"] =
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
        , HasDBFactory s k ctx
        , HasWorkerRegistry s k ctx
        , IsOurs s RewardAccount
        , Typeable s
        , Typeable n
        , (k == SharedKey) ~ 'False
        , AddressBookIso s
        , MaybeLight s
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
        , HasDBFactory s k ctx
        , HasWorkerRegistry s k ctx
        , IsOurs s RewardAccount
        , MaybeLight s
        , Typeable s
        , Typeable n
        , (k == SharedKey) ~ 'False
        , AddressBookIso s
        )
    => ctx
    -> ((SomeMnemonic, Maybe SomeMnemonic) -> Passphrase "encryption" -> k 'RootK XPrv)
    -> WalletPostData
    -> Handler ApiWallet
postShelleyWallet ctx generateKey body = do
    let state = mkSeqStateFromRootXPrv (rootXPrv, pwdP) purposeCIP1852 g
    void $ liftHandler $ createWalletWorker @_ @s @k ctx wid
        (\wrk -> W.createWallet @(WorkerCtx ctx) @_ @s @k wrk wid wName state)
        (\wrk _ -> W.manageRewardBalance @(WorkerCtx ctx) @s @k (Proxy @n) wrk wid)
    withWorkerCtx @_ @s @k ctx wid liftE liftE $ \wrk -> liftHandler $
        W.attachPrivateKeyFromPwd @_ @s @k wrk wid (rootXPrv, pwd)
    fst <$> getWallet ctx (mkShelleyWallet @_ @s @k) (ApiT wid)
  where
    seed = getApiMnemonicT (body ^. #mnemonicSentence)
    secondFactor = getApiMnemonicT <$> (body ^. #mnemonicSecondFactor)
    pwd = getApiT (body ^. #passphrase)
    pwdP = preparePassphrase currentPassphraseScheme pwd
    rootXPrv = generateKey (seed, secondFactor) pwdP
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
        , MaybeLight s
        , (k == SharedKey) ~ 'False
        , Typeable n
        , AddressBookIso s
        )
    => ctx
    -> MkApiWallet ctx s w
    -> (XPub -> k 'AccountK XPub)
    -> (WorkerCtx ctx -> WalletId -> IO ())
        -- ^ Action to run concurrently with restore action
    -> AccountPostData
    -> Handler w
postAccountWallet ctx mkWallet liftKey coworker body = do
    let state = mkSeqStateFromAccountXPub
            (liftKey accXPub) Nothing purposeCIP1852 g
    void $ liftHandler $ createWalletWorker @_ @s @k ctx wid
        (\wrk -> W.createWallet @(WorkerCtx ctx) @_ @s @k wrk wid wName state)
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
        { addressPoolGap = ApiT $ getGap $ getState cp ^. #externalPool
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

toApiWalletDelegation
    :: W.WalletDelegation
    -> TimeInterpreter IO
    -> IO ApiWalletDelegation
toApiWalletDelegation W.WalletDelegation{active,next} ti = do
    apiNext <- forM next $ \W.WalletDelegationNext{status,changesAt} -> do
        info <- interpretQuery ti $ toApiEpochInfo changesAt
        return $ toApiWalletDelegationNext (Just info) status

    return $ ApiWalletDelegation
        { active = toApiWalletDelegationNext Nothing active
        , next = apiNext
        }
  where
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

--------------------- Shared Wallet

postSharedWallet
    :: forall ctx s k n.
        ( s ~ SharedState n k
        , ctx ~ ApiLayer s k
        , SoftDerivation k
        , MkKeyFingerprint k (Proxy n, k 'AddressK XPub)
        , MkKeyFingerprint k Address
        , WalletKey k
        , HasDBFactory s k ctx
        , HasWorkerRegistry s k ctx
        , Typeable n
        , k ~ SharedKey
        )
    => ctx
    -> ((SomeMnemonic, Maybe SomeMnemonic) -> Passphrase "encryption" -> k 'RootK XPrv)
    -> (XPub -> k 'AccountK XPub)
    -> ApiSharedWalletPostData
    -> Handler ApiSharedWallet
postSharedWallet ctx generateKey liftKey postData =
    case postData of
        ApiSharedWalletPostData (Left body) ->
            postSharedWalletFromRootXPrv ctx generateKey body
        ApiSharedWalletPostData (Right body) ->
            postSharedWalletFromAccountXPub ctx liftKey body

postSharedWalletFromRootXPrv
    :: forall ctx s k n.
        ( s ~ SharedState n k
        , ctx ~ ApiLayer s k
        , SoftDerivation k
        , MkKeyFingerprint k (Proxy n, k 'AddressK XPub)
        , MkKeyFingerprint k Address
        , WalletKey k
        , HasDBFactory s k ctx
        , HasWorkerRegistry s k ctx
        , Typeable n
        , k ~ SharedKey
        )
    => ctx
    -> ((SomeMnemonic, Maybe SomeMnemonic) -> Passphrase "encryption" -> k 'RootK XPrv)
    -> ApiSharedWalletPostDataFromMnemonics
    -> Handler ApiSharedWallet
postSharedWalletFromRootXPrv ctx generateKey body = do
    case validateScriptTemplates accXPub scriptValidation pTemplate dTemplateM of
        Left err ->
            liftHandler $ throwE $ ErrConstructSharedWalletWrongScriptTemplate err
        Right _ -> pure ()
    ix' <- liftHandler $ withExceptT ErrConstructSharedWalletInvalidIndex $
        W.guardHardIndex ix
    let state = mkSharedStateFromRootXPrv (rootXPrv, pwdP) ix' g pTemplate dTemplateM
    void $ liftHandler $ createWalletWorker @_ @s @k ctx wid
        (\wrk -> W.createWallet @(WorkerCtx ctx) @_ @s @k wrk wid wName state)
        idleWorker
    withWorkerCtx @_ @s @k ctx wid liftE liftE $ \wrk -> liftHandler $
        W.attachPrivateKeyFromPwd @_ @s @k wrk wid (rootXPrv, pwd)
    fst <$> getWallet ctx (mkSharedWallet @_ @s @k) (ApiT wid)
  where
    seed = body ^. #mnemonicSentence . #getApiMnemonicT
    secondFactor = getApiMnemonicT <$> body ^. #mnemonicSecondFactor
    pwdP = preparePassphrase currentPassphraseScheme pwd
    pwd = body ^. #passphrase . #getApiT
    rootXPrv = generateKey (seed, secondFactor) pwdP
    g = defaultAddressPoolGap
    ix = getApiT (body ^. #accountIndex)
    pTemplate = scriptTemplateFromSelf (getRawKey accXPub) $ body ^. #paymentScriptTemplate
    dTemplateM = scriptTemplateFromSelf (getRawKey accXPub) <$> body ^. #delegationScriptTemplate
    wName = getApiT (body ^. #name)
    accXPub = publicKey $ deriveAccountPrivateKey pwdP rootXPrv (Index $ getDerivationIndex ix)
    wid = WalletId $ toSharedWalletId accXPub pTemplate dTemplateM
    scriptValidation = maybe RecommendedValidation getApiT (body ^. #scriptValidation)

postSharedWalletFromAccountXPub
    :: forall ctx s k n.
        ( s ~ SharedState n k
        , ctx ~ ApiLayer s k
        , SoftDerivation k
        , MkKeyFingerprint k (Proxy n, k 'AddressK XPub)
        , MkKeyFingerprint k Address
        , WalletKey k
        , HasDBFactory s k ctx
        , HasWorkerRegistry s k ctx
        , Typeable n
        , k ~ SharedKey
        )
    => ctx
    -> (XPub -> k 'AccountK XPub)
    -> ApiSharedWalletPostDataFromAccountPubX
    -> Handler ApiSharedWallet
postSharedWalletFromAccountXPub ctx liftKey body = do
    case validateScriptTemplates (liftKey accXPub) scriptValidation pTemplate dTemplateM of
        Left err ->
            liftHandler $ throwE $ ErrConstructSharedWalletWrongScriptTemplate err
        Right _ -> pure ()
    acctIx <- liftHandler $ withExceptT ErrConstructSharedWalletInvalidIndex $
        W.guardHardIndex ix
    let state = mkSharedStateFromAccountXPub (liftKey accXPub) acctIx g pTemplate dTemplateM
    void $ liftHandler $ createWalletWorker @_ @s @k ctx wid
        (\wrk -> W.createWallet @(WorkerCtx ctx) @_ @s @k wrk wid wName state)
        idleWorker
    fst <$> getWallet ctx (mkSharedWallet @_ @s @k) (ApiT wid)
  where
    g = defaultAddressPoolGap
    ix = getApiT (body ^. #accountIndex)
    pTemplate = scriptTemplateFromSelf accXPub $ body ^. #paymentScriptTemplate
    dTemplateM = scriptTemplateFromSelf accXPub <$> body ^. #delegationScriptTemplate
    wName = getApiT (body ^. #name)
    (ApiAccountPublicKey accXPubApiT) =  body ^. #accountPublicKey
    accXPub = getApiT accXPubApiT
    wid = WalletId $ toSharedWalletId (liftKey accXPub) pTemplate dTemplateM
    scriptValidation = maybe RecommendedValidation getApiT (body ^. #scriptValidation)

scriptTemplateFromSelf :: XPub -> ApiScriptTemplateEntry -> ScriptTemplate
scriptTemplateFromSelf xpub (ApiScriptTemplateEntry cosigners' template') =
    ScriptTemplate cosignersWithoutSelf template'
  where
    unSelf (SomeAccountKey xpub') = xpub'
    unSelf Self = xpub
    cosignersWithoutSelf = Map.map unSelf cosigners'

mkSharedWallet
    :: forall ctx s k n.
        ( ctx ~ ApiLayer s k
        , s ~ SharedState n k
        , HasWorkerRegistry s k ctx
        , Shared.SupportsDiscovery n k
        )
    => MkApiWallet ctx s ApiSharedWallet
mkSharedWallet ctx wid cp meta pending progress = case Shared.ready st of
    Shared.Pending -> pure $ ApiSharedWallet $ Left $ ApiPendingSharedWallet
        { id = ApiT wid
        , name = ApiT $ meta ^. #name
        , accountIndex = ApiT $ DerivationIndex $ getIndex accIx
        , addressPoolGap = ApiT $ Shared.poolGap st
        , paymentScriptTemplate = Shared.paymentTemplate st
        , delegationScriptTemplate = Shared.delegationTemplate st
        }
    Shared.Active _ -> do
        reward <- withWorkerCtx @_ @s @k ctx wid liftE liftE $ \wrk ->
            -- never fails - returns zero if balance not found
            liftIO $ W.fetchRewardBalance @_ @s @k wrk wid

        let ti = timeInterpreter $ ctx ^. networkLayer
        apiDelegation <- liftIO $ toApiWalletDelegation (meta ^. #delegation)
            (unsafeExtendSafeZone ti)

        tip' <- liftIO $ getWalletTip
            (neverFails "getWalletTip wallet tip should be behind node tip" ti)
            cp
        let available = availableBalance pending cp
        let total = totalBalance pending reward cp
        pure $ ApiSharedWallet $ Right $ ApiActiveSharedWallet
            { id = ApiT wid
            , name = ApiT $ meta ^. #name
            , accountIndex = ApiT $ DerivationIndex $ getIndex accIx
            , addressPoolGap = ApiT $ Shared.poolGap st
            , passphrase = ApiWalletPassphraseInfo
                <$> fmap (view #lastUpdatedAt) (meta ^. #passphraseInfo)
            , paymentScriptTemplate = Shared.paymentTemplate st
            , delegationScriptTemplate = Shared.delegationTemplate st
            , delegation = apiDelegation
            , balance = ApiWalletBalance
                { available = coinToQuantity (available ^. #coin)
                , total = coinToQuantity (total ^. #coin)
                , reward = coinToQuantity reward
                }
            , assets = ApiWalletAssetsBalance
                { available = ApiT (available ^. #tokens)
                , total = ApiT (total ^. #tokens)
                }
            , state = ApiT progress
            , tip = tip'
            }
  where
    st = getState cp
    DerivationPrefix (_,_,accIx) = Shared.derivationPrefix st

patchSharedWallet
    :: forall ctx s k n.
        ( s ~ SharedState n k
        , ctx ~ ApiLayer s k
        , SoftDerivation k
        , MkKeyFingerprint k (Proxy n, k 'AddressK XPub)
        , MkKeyFingerprint k Address
        , WalletKey k
        , HasDBFactory s k ctx
        , Typeable n
        , k ~ SharedKey
        )
    => ctx
    -> (XPub -> k 'AccountK XPub)
    -> CredentialType
    -> ApiT WalletId
    -> ApiSharedWalletPatchData
    -> Handler ApiSharedWallet
patchSharedWallet ctx liftKey cred (ApiT wid) body = do
    withWorkerCtx ctx wid liftE liftE $ \wrk -> do
        liftHandler $ W.updateCosigner wrk wid (liftKey accXPub) cosigner cred
    fst <$> getWallet ctx (mkSharedWallet @_ @s @k) (ApiT wid)
  where
      cosigner = getApiT (body ^. #cosigner)
      (ApiAccountPublicKey accXPubApiT) = (body ^. #accountPublicKey)
      accXPub = getApiT accXPubApiT

--------------------- Legacy

postLegacyWallet
    :: forall ctx s k.
        ( ctx ~ ApiLayer s k
        , KnownDiscovery s
        , IsOurs s RewardAccount
        , IsOurs s Address
        , MaybeLight s
        , HasNetworkLayer IO ctx
        , WalletKey k
        , AddressBookIso s
        )
    => ctx
        -- ^ Surrounding Context
    -> (k 'RootK XPrv, Passphrase "user")
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
        , HasNetworkLayer IO ctx
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
        Just (WalletPassphraseInfo time EncryptWithPBKDF2) ->
            pure $ Just $ ApiWalletPassphraseInfo time
        Just (WalletPassphraseInfo time EncryptWithScrypt) -> do
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
        W.createWallet @(WorkerCtx ctx) @_ @s @k wrk wid wName s
  where
    wName    = body ^. #name . #getApiT
    seed     = body ^. #mnemonicSentence . #getApiMnemonicT
    pwd      = body ^. #passphrase . #getApiT
    pwdP     = preparePassphrase currentPassphraseScheme pwd
    rootXPrv = Byron.generateKeyFromSeed seed pwdP

postRandomWalletFromXPrv
    :: forall ctx s k n.
        ( ctx ~ ApiLayer s k
        , s ~ RndState n
        , k ~ ByronKey
        , HasNetworkLayer IO ctx
        )
    => ctx
    -> ByronWalletFromXPrvPostData
    -> Handler ApiByronWallet
postRandomWalletFromXPrv ctx body = do
    s <- liftIO $ mkRndState byronKey <$> getStdRandom random
    void $ liftHandler $ createWalletWorker @_ @s @k ctx wid
        (\wrk -> W.createWallet @(WorkerCtx ctx) @_ @s @k wrk wid wName s)
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
        W.createIcarusWallet @(WorkerCtx ctx) @s @k wrk wid wName
            (rootXPrv, pwdP)
  where
    wName    = body ^. #name . #getApiT
    seed     = body ^. #mnemonicSentence . #getApiMnemonicT
    pwd      = body ^. #passphrase . #getApiT
    pwdP     = preparePassphrase currentPassphraseScheme pwd
    rootXPrv = Icarus.generateKeyFromSeed seed pwdP

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
        W.createIcarusWallet @(WorkerCtx ctx) @s @k wrk wid wName
            (rootXPrv, pwdP)
  where
    wName    = body ^. #name . #getApiT
    seed     = body ^. #mnemonicSentence . #getApiMnemonicT
    pwd      = body ^. #passphrase . #getApiT
    pwdP     = preparePassphrase currentPassphraseScheme pwd
    rootXPrv = Icarus.generateKeyFromSeed seed pwdP

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
        W.createIcarusWallet @(WorkerCtx ctx) @s @k wrk wid wName
            (rootXPrv, pwdP)
  where
    wName    = body ^. #name . #getApiT
    mw       = body ^. #mnemonicSentence . #getApiMnemonicT
    pwd      = body ^. #passphrase . #getApiT
    pwdP     = preparePassphrase currentPassphraseScheme pwd
    rootXPrv = Icarus.generateKeyFromHardwareLedger mw pwdP

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
        , GetAccount s k
        , HardDerivation k)
    => ctx
    -> ((SomeMnemonic, Maybe SomeMnemonic)
            -> Passphrase "encryption"
            -> k 'RootK XPrv
        )
    -> (k 'AccountK XPub -> XPub)
    -> ApiT WalletId
    -> WalletPutPassphraseData
    -> Handler NoContent
putWalletPassphrase ctx createKey getKey (ApiT wid)
    (WalletPutPassphraseData method) = withWrk $ \wrk ->
        NoContent <$ case method of
        Left
            (Api.WalletPutPassphraseOldPassphraseData
                (ApiT old)
                (ApiT new)
            ) -> liftHandler
                $ W.updateWalletPassphraseWithOldPassphrase wrk wid (old, new)
        Right
            (Api.WalletPutPassphraseMnemonicData
                    (ApiMnemonicT mnemonic) sndFactor (ApiT new)
            ) -> do
            let encrPass = preparePassphrase currentPassphraseScheme new
                challengeKey = createKey
                    (mnemonic, getApiMnemonicT <$> sndFactor) encrPass
                challengPubKey = publicKey
                    $ deriveAccountPrivateKey encrPass challengeKey minBound
            storedPubKey <- liftHandler
                $ W.readAccountPublicKey wrk wid
            if getKey challengPubKey == getKey storedPubKey
                then liftHandler
                    $ W.updateWalletPassphraseWithMnemonic
                        wrk wid (challengeKey, new)
                else liftHandler
                    $ throwE
                    $ ErrUpdatePassphraseWithRootKey
                    $ ErrWithRootKeyWrongMnemonic wid
    where
        withWrk :: (WorkerCtx (ApiLayer s k) -> Handler a) -> Handler a
        withWrk = withWorkerCtx ctx wid liftE liftE

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
        W.updateWalletPassphraseWithOldPassphrase wrk wid (old, new)
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

getWalletUtxoSnapshot
    :: forall ctx s k. (ctx ~ ApiLayer s k)
    => ctx
    -> ApiT WalletId
    -> Handler ApiWalletUtxoSnapshot
getWalletUtxoSnapshot ctx (ApiT wid) = do
    entries <- withWorkerCtx ctx wid liftE liftE $ \wrk -> liftHandler $
        W.getWalletUtxoSnapshot wrk wid
    return $ mkApiWalletUtxoSnapshot entries
  where
    mkApiWalletUtxoSnapshot :: [(TokenBundle, Coin)] -> ApiWalletUtxoSnapshot
    mkApiWalletUtxoSnapshot bundleMinCoins = ApiWalletUtxoSnapshot
        { entries = mkApiWalletUtxoSnapshotEntry <$> bundleMinCoins }

    mkApiWalletUtxoSnapshotEntry
        :: (TokenBundle, Coin) -> ApiWalletUtxoSnapshotEntry
    mkApiWalletUtxoSnapshotEntry (bundle, minCoin) = ApiWalletUtxoSnapshotEntry
        { ada = coinToQuantity $ view #coin bundle
        , adaMinimum = coinToQuantity minCoin
        , assets = ApiT $ view #tokens bundle
        }

{-------------------------------------------------------------------------------
                                  Coin Selections
-------------------------------------------------------------------------------}

selectCoins
    :: forall ctx s k n.
        ( ctx ~ ApiLayer s k
        , SoftDerivation k
        , IsOurs s Address
        , GenChange s
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

    -- FIXME [ADP-1489] mkRewardAccountBuilder does itself read
    -- @currentNodeEra@ which is not guaranteed with the era read here. This
    -- could cause problems under exceptional circumstances.
    (wdrl, _) <-
        mkRewardAccountBuilder @_ @s @_ @n ctx wid (body ^. #withdrawal)

    withWorkerCtx ctx wid liftE liftE $ \wrk -> do
        let outs = addressAmountToTxOut <$> body ^. #payments
        let txCtx = defaultTransactionCtx
                { txWithdrawal = wdrl
                , txMetadata = getApiT <$> body ^. #metadata
                }
        let transform s sel =
                W.assignChangeAddresses genChange sel s
                & uncurry (W.selectionToUnsignedTx (txWithdrawal txCtx))
        (utxoAvailable, wallet, pendingTxs) <-
            liftHandler $ W.readWalletUTxOIndex @_ @s @k wrk wid
        pp <- liftIO $ NW.currentProtocolParameters (wrk ^. networkLayer)
        era <- liftIO $ NW.currentNodeEra (wrk ^. networkLayer)
        let selectAssetsParams = W.SelectAssetsParams
                { outputs = F.toList outs
                , pendingTxs
                , randomSeed = Nothing
                , txContext = txCtx
                , utxoAvailableForInputs = UTxOSelection.fromIndex utxoAvailable
                , utxoAvailableForCollateral = UTxOIndex.toMap utxoAvailable
                , wallet
                , selectionStrategy = SelectionStrategyOptimal
                }
        utx <- liftHandler $
            W.selectAssets @_ @_ @s @k wrk era pp selectAssetsParams transform
        pure $ mkApiCoinSelection [] [] Nothing md utx

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
            $ W.joinStakePool @_ @s @k wrk curEpoch pools pid poolStatus wid

        let txCtx = defaultTransactionCtx
                { txDelegationAction = Just action
                }

        let transform s sel =
                W.assignChangeAddresses (delegationAddress @n) sel s
                & uncurry (W.selectionToUnsignedTx (txWithdrawal txCtx))
        (utxoAvailable, wallet, pendingTxs) <-
            liftHandler $ W.readWalletUTxOIndex @_ @s @k wrk wid
        pp <- liftIO $ NW.currentProtocolParameters (wrk ^. networkLayer)
        era <- liftIO $ NW.currentNodeEra (wrk ^. networkLayer)

        let selectAssetsParams = W.SelectAssetsParams
                { outputs = []
                , pendingTxs
                , randomSeed = Nothing
                , txContext = txCtx
                , utxoAvailableForInputs = UTxOSelection.fromIndex utxoAvailable
                , utxoAvailableForCollateral = UTxOIndex.toMap utxoAvailable
                , wallet
                , selectionStrategy = SelectionStrategyOptimal
                }
        utx <- liftHandler
            $ W.selectAssets @_ @_ @s @k wrk era pp selectAssetsParams transform
        (_, _, path) <- liftHandler
            $ W.readRewardAccount @_ @s @k @n wrk wid

        let deposits = maybeToList deposit

        pure $ mkApiCoinSelection deposits [] (Just (action, path)) Nothing utx

selectCoinsForQuit
    :: forall ctx s n k.
        ( s ~ SeqState n k
        , ctx ~ ApiLayer s k
        , DelegationAddress n k
        , MkKeyFingerprint k (Proxy n, k 'AddressK XPub)
        , Bounded (Index (AddressIndexDerivationType k) 'AddressK)
        , SoftDerivation k
        , Typeable n
        , Typeable s
        , WalletKey k
        )
    => ctx
    -> ApiT WalletId
    -> Handler (Api.ApiCoinSelection n)
selectCoinsForQuit ctx (ApiT wid) = do
    withWorkerCtx ctx wid liftE liftE $ \wrk -> do
        -- FIXME [ADP-1489] mkRewardAccountBuilder does itself read
        -- @currentNodeEra@ which is not guaranteed with the era read here. This
        -- could cause problems under exceptional circumstances.
        (wdrl, _mkRwdAcct) <-
            mkRewardAccountBuilder @_ @s @_ @n ctx wid (Just SelfWithdrawal)
        action <- liftHandler $ W.quitStakePool @_ @s @k wrk wid wdrl

        let txCtx = defaultTransactionCtx
                { txDelegationAction = Just action
                , txWithdrawal = wdrl
                }

        let transform s sel =
                W.assignChangeAddresses (delegationAddress @n) sel s
                & uncurry (W.selectionToUnsignedTx (txWithdrawal txCtx))
        (utxoAvailable, wallet, pendingTxs) <-
            liftHandler $ W.readWalletUTxOIndex @_ @s @k wrk wid
        pp <- liftIO $ NW.currentProtocolParameters (wrk ^. networkLayer)
        era <- liftIO $ NW.currentNodeEra (wrk ^. networkLayer)
        let refund = W.stakeKeyDeposit pp
        let selectAssetsParams = W.SelectAssetsParams
                { outputs = []
                , pendingTxs
                , randomSeed = Nothing
                , txContext = txCtx
                , utxoAvailableForInputs = UTxOSelection.fromIndex utxoAvailable
                , utxoAvailableForCollateral = UTxOIndex.toMap utxoAvailable
                , wallet
                , selectionStrategy = SelectionStrategyOptimal
                }
        utx <- liftHandler
            $ W.selectAssets @_ @_ @s @k wrk era pp selectAssetsParams transform
        (_, _, path) <- liftHandler $ W.readRewardAccount @_ @s @k @n wrk wid

        pure $ mkApiCoinSelection [] [refund] (Just (action, path)) Nothing utx

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
        , IsOurs s Address
        , HasTokenMetadataClient ctx
        )
    => ctx
    -> ApiT WalletId
    -> Handler [ApiAsset]
listAssets ctx wid = do
    assets <- listAssetsBase ctx wid
    liftIO $ fillMetadata client (Set.toList assets) toApiAsset
  where
    client = ctx ^. tokenMetadataClient

-- | Return a list of all AssetIds involved in the transaction history of this
-- wallet.
listAssetsBase
    :: forall s k. IsOurs s Address =>
    ApiLayer s k -> ApiT WalletId -> Handler (Set AssetId)
listAssetsBase ctx (ApiT wallet) =
    withWorkerCtx ctx wallet liftE liftE $ \wctx ->
        liftHandler $ W.listAssets wctx wallet

-- | Look up a single asset and its metadata.
--
-- NOTE: This is slightly inefficient because it greps through the transaction
-- history to check if the asset is associated with this wallet.
getAsset
    :: forall ctx s k.
        ( ctx ~ ApiLayer s k
        , IsOurs s Address
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
        , IsOurs s Address
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
    (addr, path) <- withWorkerCtx ctx wid liftE liftE
        $ \wrk -> liftHandler $ W.createRandomAddress @_ @s @k @n wrk wid pwd mix
    pure $ coerceAddress (addr, Unused, path)
  where
    coerceAddress (a, s, p) =
        ApiAddress (ApiT a, Proxy @n) (ApiT s) (NE.map ApiT p)

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
    -> Handler [ApiAddress n]
listAddresses ctx normalize (ApiT wid) stateFilter = do
    addrs <- withWorkerCtx ctx wid liftE liftE $ \wrk -> liftHandler $
        W.listAddresses @_ @s @k wrk wid normalize
    return $ coerceAddress <$> filter filterCondition addrs
  where
    filterCondition :: (Address, AddressState, NonEmpty DerivationIndex) -> Bool
    filterCondition = case stateFilter of
        Nothing -> const True
        Just (ApiT s) -> \(_,state,_) -> (state == s)
    coerceAddress (a, s, p) =
        ApiAddress (ApiT a, Proxy @n) (ApiT s) (NE.map ApiT p)

{-------------------------------------------------------------------------------
                                    Transactions
-------------------------------------------------------------------------------}

signTransaction
    :: forall ctx s k.
        ( ctx ~ ApiLayer s k
        , Bounded (Index (AddressIndexDerivationType k) 'AddressK)
        , WalletKey k
        , IsOwned s k
        , HardDerivation k
        )
    => ctx
    -> ApiT WalletId
    -> ApiSignTransactionPostData
    -> Handler ApiSerialisedTransaction
signTransaction ctx (ApiT wid) body = do
    let pwd = coerce $ body ^. #passphrase . #getApiT
    let sealedTx = body ^. #transaction . #getApiT
    sealedTx' <- withWorkerCtx ctx wid liftE liftE $ \wrk -> liftHandler $ do
        let
            db = wrk ^. W.dbLayer @IO @s @k
            tl = wrk ^. W.transactionLayer @k
            nl = wrk ^. W.networkLayer
        era <- liftIO $ NW.currentNodeEra nl
        db & \W.DBLayer{atomically, readCheckpoint} -> do
            W.withRootKey @_ @s wrk wid pwd ErrWitnessTxWithRootKey $ \rootK scheme -> do
                cp <- mapExceptT atomically
                    $ withExceptT ErrWitnessTxNoSuchWallet
                    $ W.withNoSuchWallet wid
                    $ readCheckpoint wid
                let
                    pwdP :: Passphrase "encryption"
                    pwdP = preparePassphrase scheme pwd

                    utxo :: UTxO.UTxO
                    utxo = totalUTxO mempty cp

                    keyLookup
                        :: Address
                        -> Maybe (k 'AddressK XPrv, Passphrase "encryption")
                    keyLookup = isOwned (getState cp) (rootK, pwdP)

                pure $ W.signTransaction tl era keyLookup (rootK, pwdP) utxo sealedTx

    -- TODO: The body+witnesses seem redundant with the sealedTx already. What's
    -- the use-case for having them provided separately? In the end, the client
    -- should be able to decouple them if they need to.
    pure $ Api.ApiSerialisedTransaction
        { transaction = ApiT sealedTx'
        }

postTransactionOld
    :: forall ctx s k n.
        ( ctx ~ ApiLayer s k
        , Bounded (Index (AddressIndexDerivationType k) 'AddressK)
        , GenChange s
        , HardDerivation k
        , HasNetworkLayer IO ctx
        , IsOwned s k
        , Typeable n
        , Typeable s
        , WalletKey k
        , AddressBookIso s
        )
    => ctx
    -> ArgGenChange s
    -> ApiT WalletId
    -> PostTransactionOldData n
    -> Handler (ApiTransaction n)
postTransactionOld ctx genChange (ApiT wid) body = do
    let pwd = coerce $ body ^. #passphrase . #getApiT
    let outs = addressAmountToTxOut <$> body ^. #payments
    let md = body ^? #metadata . traverse . #txMetadataWithSchema_metadata
    let mTTL = body ^? #timeToLive . traverse . #getQuantity

    -- FIXME [ADP-1489] mkRewardAccountBuilder does itself read
    -- @currentNodeEra@ which is not guaranteed with the era read here. This
    -- could cause problems under exceptional circumstances.
    (wdrl, mkRwdAcct) <-
        mkRewardAccountBuilder @_ @s @_ @n ctx wid (body ^. #withdrawal)

    ttl <- liftIO $ W.getTxExpiry ti mTTL
    let txCtx = defaultTransactionCtx
            { txWithdrawal = wdrl
            , txMetadata = md
            , txValidityInterval = (Nothing, ttl)
            }

    (sel, tx, txMeta, txTime, pp) <- withWorkerCtx ctx wid liftE liftE $ \wrk ->
        atomicallyWithHandler (ctx ^. walletLocks) (PostTransactionOld wid) $ do
            (utxoAvailable, wallet, pendingTxs) <-
                liftHandler $ W.readWalletUTxOIndex @_ @s @k wrk wid
            pp <- liftIO $ NW.currentProtocolParameters (wrk ^. networkLayer)
            era <- liftIO $ NW.currentNodeEra (wrk ^. networkLayer)
            let selectAssetsParams = W.SelectAssetsParams
                    { outputs = F.toList outs
                    , pendingTxs
                    , randomSeed = Nothing
                    , txContext = txCtx
                    , utxoAvailableForInputs =
                        UTxOSelection.fromIndex utxoAvailable
                    , utxoAvailableForCollateral =
                        UTxOIndex.toMap utxoAvailable
                    , wallet
                    , selectionStrategy = SelectionStrategyOptimal
                    }
            sel <- liftHandler
                $ W.selectAssets @_ @_ @s @k wrk era pp selectAssetsParams
                $ const Prelude.id
            sel' <- liftHandler
                $ W.assignChangeAddressesAndUpdateDb wrk wid genChange sel
            (tx, txMeta, txTime, sealedTx) <- liftHandler
                $ W.buildAndSignTransaction
                    @_ @s @k wrk wid era mkRwdAcct pwd txCtx sel'
            liftHandler
                $ W.submitTx @_ @s @k wrk wid (tx, txMeta, sealedTx)
            pure (sel, tx, txMeta, txTime, pp)

    liftIO $ mkApiTransaction
        (timeInterpreter $ ctx ^. networkLayer)
        #pendingSince
        $ MkApiTransactionParams
            { txId = tx ^. #txId
            , txFee = tx ^. #fee
            , txInputs = NE.toList $ second Just <$> sel ^. #inputs
              -- TODO: ADP-957:
            , txCollateralInputs = []
            , txOutputs = tx ^. #outputs
            , txCollateralOutput = tx ^. #collateralOutput
            , txWithdrawals = tx ^. #withdrawals
            , txMeta
            , txMetadata = tx ^. #metadata
            , txTime
            , txScriptValidity = tx ^. #scriptValidity
            , txDeposit = W.stakeKeyDeposit pp
            , txMetadataSchema = TxMetadataDetailedSchema
            }
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
    -> TxMetadataSchema
    -> Handler [ApiTransaction n]
listTransactions
    ctx (ApiT wid) mMinWithdrawal mStart mEnd mOrder metadataSchema = do
        (txs, depo) <- withWorkerCtx ctx wid liftE liftE $ \wrk -> do
            txs <- liftHandler $
                W.listTransactions @_ @_ @_ wrk wid
                (Coin . fromIntegral . getMinWithdrawal <$> mMinWithdrawal)
                (getIso8601Time <$> mStart)
                (getIso8601Time <$> mEnd)
                (maybe defaultSortOrder getApiT mOrder)
            depo <- liftIO $ W.stakeKeyDeposit <$>
                NW.currentProtocolParameters (wrk ^. networkLayer)
            pure (txs, depo)
        liftIO $ forM txs $ \tx ->
            mkApiTransactionFromInfo
                (timeInterpreter (ctx ^. networkLayer))
                depo
                tx
                metadataSchema
  where
    defaultSortOrder :: SortOrder
    defaultSortOrder = Descending

getTransaction
    :: forall ctx s k n. (ctx ~ ApiLayer s k)
    => ctx
    -> ApiT WalletId
    -> ApiTxId
    -> TxMetadataSchema
    -> Handler (ApiTransaction n)
getTransaction ctx (ApiT wid) (ApiTxId (ApiT (tid))) metadataSchema = do
    (tx, depo) <- withWorkerCtx ctx wid liftE liftE $ \wrk -> do
        tx <- liftHandler $ W.getTransaction wrk wid tid
        depo <- liftIO $ W.stakeKeyDeposit <$>
            NW.currentProtocolParameters (wrk ^. networkLayer)
        pure (tx, depo)
    liftIO
        $ mkApiTransactionFromInfo
            (timeInterpreter (ctx ^. networkLayer)) depo tx
            metadataSchema

-- Populate an API transaction record with 'TransactionInfo' from the wallet
-- layer.
mkApiTransactionFromInfo
    :: MonadIO m
    => TimeInterpreter (ExceptT PastHorizonException IO)
    -> Coin
    -> TransactionInfo
    -> TxMetadataSchema
    -> m (ApiTransaction n)
mkApiTransactionFromInfo ti deposit info metadataSchema = do
    apiTx <- liftIO $ mkApiTransaction
        ti
        status
        MkApiTransactionParams
            { txId = info ^. #txInfoId
            , txFee = info ^. #txInfoFee
            , txInputs = info ^. #txInfoInputs <&> drop2nd
            , txCollateralInputs = info ^. #txInfoCollateralInputs <&> drop2nd
            , txOutputs = info ^. #txInfoOutputs
            , txCollateralOutput = info ^. #txInfoCollateralOutput
            , txWithdrawals = info ^. #txInfoWithdrawals
            , txMeta = info ^. #txInfoMeta
            , txMetadata = info ^. #txInfoMetadata
            , txTime = info ^. #txInfoTime
            , txScriptValidity = info ^. #txInfoScriptValidity
            , txDeposit = deposit
            , txMetadataSchema = metadataSchema
            }
    return $ case info ^. (#txInfoMeta . #status) of
        Pending  -> apiTx
        InLedger -> apiTx {depth = Just $ info ^. #txInfoDepth}
        Expired  -> apiTx
  where
    drop2nd (a,_,c) = (a,c)
    status :: Lens' (ApiTransaction n) (Maybe ApiBlockReference)
    status = case info ^. (#txInfoMeta . #status) of
        Pending  -> #pendingSince
        InLedger -> #insertedAt
        Expired  -> #pendingSince

postTransactionFeeOld
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
    -> PostTransactionFeeOldData n
    -> Handler ApiFee
postTransactionFeeOld ctx (ApiT wid) body = do

    -- FIXME [ADP-1489] mkRewardAccountBuilder does itself read
    -- @currentNodeEra@ which is not guaranteed with the era read here. This
    -- could cause problems under exceptional circumstances.
    (wdrl, _) <- mkRewardAccountBuilder @_ @s @_ @n ctx wid (body ^. #withdrawal)
    let txCtx = defaultTransactionCtx
            { txWithdrawal = wdrl
            , txMetadata
                = body ^? #metadata
                . traverse
                . #txMetadataWithSchema_metadata
            }
    withWorkerCtx ctx wid liftE liftE $ \wrk -> do
        (utxoAvailable, wallet, pendingTxs) <-
            liftHandler $ W.readWalletUTxOIndex @_ @s @k wrk wid
        let outs = addressAmountToTxOut <$> body ^. #payments
        pp <- liftIO $ NW.currentProtocolParameters (wrk ^. networkLayer)
        era <- liftIO $ NW.currentNodeEra (wrk ^. networkLayer)
        let getFee = const (selectionDelta TokenBundle.getCoin)
        let selectAssetsParams = W.SelectAssetsParams
                { outputs = F.toList outs
                , pendingTxs
                , randomSeed = Nothing
                , txContext = txCtx
                , utxoAvailableForInputs = UTxOSelection.fromIndex utxoAvailable
                , utxoAvailableForCollateral = UTxOIndex.toMap utxoAvailable
                , wallet
                , selectionStrategy = SelectionStrategyOptimal
                }
        let runSelection =
                W.selectAssets @_ @_ @s @k wrk era pp selectAssetsParams getFee
        minCoins <- liftIO (W.calcMinimumCoinValues @_ @k wrk era (F.toList outs))
        liftHandler $ mkApiFee Nothing minCoins <$> W.estimateFee runSelection

constructTransaction
    :: forall ctx s k n.
        ( ctx ~ ApiLayer s k
        , Bounded (Index (AddressIndexDerivationType k) 'AddressK)
        , GenChange s
        , HardDerivation k
        , HasNetworkLayer IO ctx
        , IsOurs s Address
        , Typeable n
        , Typeable s
        , WalletKey k
        )
    => ctx
    -> ArgGenChange s
    -> IO (Set PoolId)
    -> (PoolId -> IO PoolLifeCycleStatus)
    -> ApiT WalletId
    -> ApiConstructTransactionData n
    -> Handler (ApiConstructTransaction n)
constructTransaction ctx genChange knownPools getPoolStatus (ApiT wid) body = do
    let isNoPayload =
            isNothing (body ^. #payments) &&
            isNothing (body ^. #withdrawal) &&
            isNothing (body ^. #metadata) &&
            isNothing (body ^. #mintBurn) &&
            isNothing (body ^. #delegations)
    when isNoPayload $
        liftHandler $ throwE ErrConstructTxWrongPayload

    let mintingBurning = body ^. #mintBurn
    let handleMissingAssetName :: ApiMintBurnData n -> ApiMintBurnData n
        handleMissingAssetName mb = case mb ^. #assetName of
            Nothing -> mb {assetName = Just $ ApiT nullTokenName}
            Just _ -> mb
    let mintingBurning' = fmap handleMissingAssetName <$> mintingBurning
    let retrieveAllCosigners = foldScript (:) []
    let wrongMintingTemplate (ApiMintBurnData (ApiT scriptTempl) _ _) =
            isLeft (validateScriptOfTemplate RecommendedValidation scriptTempl)
            || length (retrieveAllCosigners scriptTempl) /= 1
            || (L.any (/= Cosigner 0)) (retrieveAllCosigners scriptTempl)
    when
        ( isJust mintingBurning' &&
          L.any wrongMintingTemplate (NE.toList $ fromJust mintingBurning')
        ) $ liftHandler $ throwE ErrConstructTxWrongMintingBurningTemplate

    let assetNameTooLong = \case
            (ApiMintBurnData _ (Just (ApiT (UnsafeTokenName bs))) _) ->
                BS.length bs > tokenNameMaxLength
            _ ->
                error "tokenName should be nonempty at this step"
    when
        ( isJust mintingBurning' &&
          L.any assetNameTooLong (NE.toList $ fromJust mintingBurning')
        ) $ liftHandler $ throwE ErrConstructTxAssetNameTooLong

    let assetQuantityOutOfBounds
            (ApiMintBurnData _ _ (ApiMint (ApiMintData _ amt))) =
            amt <= 0 || amt > unTokenQuantity txMintBurnMaxTokenQuantity
        assetQuantityOutOfBounds
            (ApiMintBurnData _ _ (ApiBurn (ApiBurnData amt))) =
            amt <= 0 || amt > unTokenQuantity txMintBurnMaxTokenQuantity
    when
        ( isJust mintingBurning' &&
          L.any assetQuantityOutOfBounds (NE.toList $ fromJust mintingBurning')
        ) $ liftHandler $
            throwE ErrConstructTxMintOrBurnAssetQuantityOutOfBounds

    let checkIx (ApiStakeKeyIndex (ApiT derIndex)) =
            derIndex == DerivationIndex (getIndex @'Hardened minBound)
    let validApiDelAction = \case
            Joining _ stakeKeyIx -> checkIx stakeKeyIx
            Leaving stakeKeyIx -> checkIx stakeKeyIx
    let notall0Haccount = case body ^. #delegations of
            Nothing -> False
            Just delegs -> not . all validApiDelAction $ NE.toList delegs
    when notall0Haccount $
        liftHandler $ throwE ErrConstructTxMultiaccountNotSupported

    let md = body ^? #metadata . traverse . #txMetadataWithSchema_metadata

    (before, hereafter, isThereNegativeTime) <-
        handleValidityInterval ti (body ^. #validityInterval)

    when (hereafter < before || isThereNegativeTime) $
        liftHandler $ throwE ErrConstructTxWrongValidityBounds

    let notWithinValidityInterval (ApiMintBurnData (ApiT scriptTempl) _ _) =
            not $ withinSlotInterval before hereafter $
            scriptSlotIntervals scriptTempl
    when
        ( isJust mintingBurning' &&
          L.any notWithinValidityInterval (NE.toList $ fromJust mintingBurning')
        )
        $ liftHandler
        $ throwE ErrConstructTxValidityIntervalNotWithinScriptTimelock


    -- FIXME [ADP-1489] mkRewardAccountBuilder does itself read
    -- @currentNodeEra@ which is not guaranteed with the era read here. This
    -- could cause problems under exceptional circumstances.
    (wdrl, _) <-
        mkRewardAccountBuilder @_ @s @_ @n ctx wid (body ^. #withdrawal)

    withWorkerCtx ctx wid liftE liftE $ \wrk -> do
        pp <- liftIO $ NW.currentProtocolParameters (wrk ^. networkLayer)
        era <- liftIO $ NW.currentNodeEra (wrk ^. networkLayer)
        (deposit, refund, txCtx) <- case body ^. #delegations of
            Nothing -> pure (Nothing, Nothing, defaultTransactionCtx
                 { txWithdrawal = wdrl
                 , txMetadata = md
                 , txValidityInterval = (Just before, hereafter)
                 })
            Just delegs -> do
                -- TODO: Current limitation:
                -- at this moment we are handling just one delegation action:
                -- either joining pool, or rejoining or quitting
                -- When we support multi-account this should be lifted
                (action, deposit, refund) <- case NE.toList delegs of
                    [(Joining (ApiT pid) _)] -> do
                        poolStatus <- liftIO (getPoolStatus pid)
                        pools <- liftIO knownPools
                        curEpoch <- getCurrentEpoch ctx
                        (del, act) <- liftHandler $ W.joinStakePool
                            @_ @s @k wrk curEpoch pools pid poolStatus wid
                        pure (del, act, Nothing)
                    [(Leaving _)] -> do
                        del <- liftHandler $
                            W.quitStakePool @_ @s @k wrk wid wdrl
                        pure (del, Nothing, Just $ W.stakeKeyDeposit pp)
                    _ ->
                        liftHandler $
                            throwE ErrConstructTxMultidelegationNotSupported

                pure (deposit, refund, defaultTransactionCtx
                    { txWithdrawal = wdrl
                    , txMetadata = md
                    , txValidityInterval = (Just before, hereafter)
                    , txDelegationAction = Just action
                    })
        let transform s sel =
                ( W.assignChangeAddresses genChange sel s
                    & uncurry (W.selectionToUnsignedTx (txWithdrawal txCtx))
                , sel
                , selectionDelta TokenBundle.getCoin sel
                )

        (utxoAvailable, wallet, pendingTxs) <-
            liftHandler $ W.readWalletUTxOIndex @_ @s @k wrk wid
        (txCtx', policyXPubM) <-
            if isJust mintingBurning' then do
                (policyXPub, _) <-
                    liftHandler $ W.readPolicyPublicKey @_ @s @k @n wrk wid
                let isMinting (ApiMintBurnData _ _ (ApiMint _)) = True
                    isMinting _ = False
                let getMinting = \case
                        ApiMintBurnData
                            (ApiT scriptT)
                            (Just (ApiT tName))
                            (ApiMint (ApiMintData _ amt)) ->
                            toTokenMapAndScript @k
                                scriptT
                                (Map.singleton (Cosigner 0) policyXPub)
                                tName
                                amt
                        _ -> error "getMinting should not be used in this way"
                let getBurning = \case
                        ApiMintBurnData
                            (ApiT scriptT)
                            (Just (ApiT tName))
                            (ApiBurn (ApiBurnData amt)) ->
                            toTokenMapAndScript @k
                                scriptT
                                (Map.singleton (Cosigner 0) policyXPub)
                                tName
                                amt
                        _ -> error "getBurning should not be used in this way"
                let toTokenMap =
                        fromFlatList .
                        map (\(a,q,_) -> (a,q))
                let toScriptTemplateMap =
                        Map.fromList .
                        map (\(a,_,s) -> (a,s))
                let mintingData =
                        toTokenMap &&& toScriptTemplateMap $
                        map getMinting $
                        filter isMinting $
                        NE.toList $ fromJust mintingBurning'
                let burningData =
                        toTokenMap &&& toScriptTemplateMap $
                        map getBurning $
                        filter (not . isMinting) $
                        NE.toList $ fromJust mintingBurning'
                pure ( txCtx
                      { txAssetsToMint = mintingData
                      , txAssetsToBurn = burningData
                      }
                     , Just policyXPub)
            else
                pure (txCtx, Nothing)

        let runSelection outs =
                W.selectAssets @_ @_ @s @k wrk era pp selectAssetsParams transform
              where
                selectAssetsParams = W.SelectAssetsParams
                    { outputs = outs
                    , pendingTxs
                    , randomSeed = Nothing
                    , txContext = txCtx'
                    , utxoAvailableForInputs =
                        UTxOSelection.fromIndex utxoAvailable
                    , utxoAvailableForCollateral =
                        UTxOIndex.toMap utxoAvailable
                    , wallet
                    , selectionStrategy = SelectionStrategyOptimal
                    }

        (sel, sel', fee) <- do
            outs <- case (body ^. #payments) of
                Nothing -> pure []
                Just (ApiPaymentAddresses content) ->
                    pure $ F.toList (addressAmountToTxOut <$> content)
                Just (ApiPaymentAll _) -> do
                    liftHandler $
                        throwE $ ErrConstructTxNotImplemented "ADP-1189"

            let mintWithAddress
                    (ApiMintBurnData _ _ (ApiMint (ApiMintData (Just _) _)))
                    = True
                mintWithAddress _ = False
            let mintingOuts = case mintingBurning' of
                    Just mintBurns ->
                        coalesceTokensPerAddr $
                        map (toMintTxOut (fromJust policyXPubM)) $
                        filter mintWithAddress $
                        NE.toList mintBurns
                    Nothing -> []

            (sel', utx, fee') <- liftHandler $
                runSelection (outs ++ mintingOuts)
            sel <- liftHandler $
                W.assignChangeAddressesWithoutDbUpdate wrk wid genChange utx
            (FeeEstimation estMin _) <- liftHandler $ W.estimateFee (pure fee')
            pure (sel, sel', estMin)

        tx <- liftHandler
            $ W.constructTransaction @_ @s @k @n wrk wid era txCtx' sel

        pure $ ApiConstructTransaction
            { transaction = ApiT tx
            , coinSelection = mkApiCoinSelection
                (maybeToList deposit) (maybeToList refund) Nothing md sel'
            , fee = Quantity $ fromIntegral fee
            }
  where
    ti :: TimeInterpreter (ExceptT PastHorizonException IO)
    ti = timeInterpreter (ctx ^. networkLayer)

    toMintTxOut policyXPub
        (ApiMintBurnData (ApiT scriptT) (Just (ApiT tName))
            (ApiMint (ApiMintData (Just addr) amt))) =
                let (assetId, tokenQuantity, _) =
                        toTokenMapAndScript @k
                            scriptT (Map.singleton (Cosigner 0) policyXPub)
                            tName amt
                    assets = fromFlatList [(assetId, tokenQuantity)]
                in
                (addr, assets)
    toMintTxOut _ _ = error
        "toMintTxOut can only be used in the minting context with addr \
        \specified"

    coalesceTokensPerAddr =
        let toTxOut (addr, assets) =
                addressAmountToTxOut $
                AddressAmount addr (Quantity 0) (ApiT assets)
        in
        map toTxOut
            . Map.toList
            . foldr (uncurry (Map.insertWith (<>))) Map.empty

handleValidityInterval
    :: TimeInterpreter (ExceptT PastHorizonException IO)
    -> Maybe ApiValidityInterval
    -> Handler (SlotNo, SlotNo, Bool)
handleValidityInterval ti validityInterval = do
    let isValidityBoundTimeNegative
            (ApiValidityBoundAsTimeFromNow (Quantity sec)) = sec < 0
        isValidityBoundTimeNegative _ = False

    let isThereNegativeTime = case validityInterval of
            Just (ApiValidityInterval (Just before') Nothing) ->
                isValidityBoundTimeNegative before'
            Just (ApiValidityInterval Nothing (Just hereafter')) ->
                isValidityBoundTimeNegative hereafter'
            Just (ApiValidityInterval (Just before') (Just hereafter')) ->
                isValidityBoundTimeNegative before' ||
                isValidityBoundTimeNegative hereafter'
            _ -> False

    let fromValidityBound = liftIO . \case
            Left ApiValidityBoundUnspecified ->
                pure $ SlotNo 0
            Right ApiValidityBoundUnspecified ->
                W.getTxExpiry ti Nothing
            Right (ApiValidityBoundAsTimeFromNow (Quantity sec)) ->
                W.getTxExpiry ti (Just sec)
            Left (ApiValidityBoundAsTimeFromNow (Quantity sec)) ->
                W.getTxExpiry ti (Just sec)
            Right (ApiValidityBoundAsSlot (Quantity slot)) ->
                pure $ SlotNo slot
            Left (ApiValidityBoundAsSlot (Quantity slot)) ->
                pure $ SlotNo slot

    (before, hereafter) <- case validityInterval of
        Nothing -> do
            before' <-
                fromValidityBound (Left ApiValidityBoundUnspecified)
            hereafter' <-
                fromValidityBound (Right ApiValidityBoundUnspecified)
            pure (before', hereafter')
        Just (ApiValidityInterval before' hereafter') -> do
            before'' <- case before' of
                Nothing ->
                    fromValidityBound (Left ApiValidityBoundUnspecified)
                Just val ->
                    fromValidityBound (Left val)
            hereafter'' <- case hereafter' of
                Nothing ->
                    fromValidityBound (Right ApiValidityBoundUnspecified)
                Just val ->
                    fromValidityBound (Right val)
            pure (before'', hereafter'')

    pure $ (before, hereafter, isThereNegativeTime)

-- TO-DO delegations/withdrawals
-- TO-DO minting/burning
constructSharedTransaction
    :: forall ctx s k n.
        ( s ~ SharedState n k
        , ctx ~ ApiLayer s k
        , Bounded (Index (AddressIndexDerivationType k) 'AddressK)
        , GenChange s
        , HardDerivation k
        , HasNetworkLayer IO ctx
        , IsOurs s Address
        , Typeable n
        , Typeable s
        , WalletKey k
        )
    => ctx
    -> ArgGenChange s
    -> IO (Set PoolId)
    -> (PoolId -> IO PoolLifeCycleStatus)
    -> ApiT WalletId
    -> ApiConstructTransactionData n
    -> Handler (ApiConstructTransaction n)
constructSharedTransaction ctx genChange _knownPools _getPoolStatus (ApiT wid) body = do
    let isNoPayload =
            isNothing (body ^. #payments) &&
            isNothing (body ^. #withdrawal) &&
            isNothing (body ^. #metadata) &&
            isNothing (body ^. #mintBurn) &&
            isNothing (body ^. #delegations)
    when isNoPayload $
        liftHandler $ throwE ErrConstructTxWrongPayload

    let md = body ^? #metadata . traverse . #txMetadataWithSchema_metadata

    (wdrl, _) <-
        mkRewardAccountBuilder @_ @s @_ @n ctx wid (body ^. #withdrawal)

    (before, hereafter, isThereNegativeTime) <-
        handleValidityInterval ti (body ^. #validityInterval)

    when (hereafter < before || isThereNegativeTime) $
        liftHandler $ throwE ErrConstructTxWrongValidityBounds

    let txCtx = defaultTransactionCtx
            { txWithdrawal = wdrl
            , txMetadata = md
            , txValidityInterval = (Just before, hereafter)
            , txDelegationAction = Nothing
            }

    let transform s sel =
            ( W.assignChangeAddresses genChange sel s
                & uncurry (W.selectionToUnsignedTx (txWithdrawal txCtx))
            , sel
            , selectionDelta TokenBundle.getCoin sel
            )

    withWorkerCtx ctx wid liftE liftE $ \wrk -> do
        (cp, _, _) <- liftHandler $ withExceptT ErrConstructTxNoSuchWallet $
            W.readWallet @_ @s @k wrk wid
        case Shared.ready (getState cp) of
            Shared.Pending ->
                liftHandler $ throwE ErrConstructTxSharedWalletPending
            Shared.Active _ -> do
                pp <- liftIO $ NW.currentProtocolParameters (wrk ^. networkLayer)
                era <- liftIO $ NW.currentNodeEra (wrk ^. networkLayer)

                (utxoAvailable, wallet, pendingTxs) <-
                    liftHandler $ W.readWalletUTxOIndex @_ @s @k wrk wid

                let runSelection outs =
                        W.selectAssets @_ @_ @s @k wrk era pp selectAssetsParams transform
                      where
                        selectAssetsParams = W.SelectAssetsParams
                            { outputs = outs
                            , pendingTxs
                            , randomSeed = Nothing
                            , txContext = txCtx
                            , utxoAvailableForInputs =
                                UTxOSelection.fromIndex utxoAvailable
                            , utxoAvailableForCollateral =
                                UTxOIndex.toMap utxoAvailable
                            , wallet
                            , selectionStrategy = SelectionStrategyOptimal
                            }
                (sel, sel', fee) <- do
                    outs <- case (body ^. #payments) of
                        Nothing -> pure []
                        Just (ApiPaymentAddresses content) ->
                            pure $ F.toList (addressAmountToTxOut <$> content)
                        Just (ApiPaymentAll _) -> do
                            liftHandler $
                                throwE $ ErrConstructTxNotImplemented "ADP-1189"

                    (sel', utx, fee') <- liftHandler $ runSelection outs
                    sel <- liftHandler $
                        W.assignChangeAddressesWithoutDbUpdate wrk wid genChange utx
                    (FeeEstimation estMin _) <- liftHandler $ W.estimateFee (pure fee')
                    pure (sel, sel', estMin)

                tx <- liftHandler
                    $ W.constructTransaction @_ @s @k @n wrk wid era txCtx sel

                pure $ ApiConstructTransaction
                    { transaction = ApiT tx
                    , coinSelection = mkApiCoinSelection [] [] Nothing md sel'
                    , fee = Quantity $ fromIntegral fee
                    }
  where
    ti :: TimeInterpreter (ExceptT PastHorizonException IO)
    ti = timeInterpreter (ctx ^. networkLayer)


-- TODO: Most of the body of this function should really belong to
-- Cardano.Wallet to keep the Api.Server module free of business logic!
balanceTransaction
    :: forall ctx s k (n :: NetworkDiscriminant).
        ( ctx ~ ApiLayer s k
        , HasNetworkLayer IO ctx
        , GenChange s
        )
    => ctx
    -> ArgGenChange s
    -> ApiT WalletId
    -> ApiBalanceTransactionPostData n
    -> Handler ApiSerialisedTransaction
balanceTransaction ctx genChange (ApiT wid) body = do
    pp <- liftIO $ NW.currentProtocolParameters nl
    -- TODO: This throws when still in the Byron era.
    let nodePParams = fromJust $ W.currentNodeProtocolParameters pp
    withWorkerCtx ctx wid liftE liftE $ \wrk -> do
        wallet <- liftHandler $ W.readWalletUTxOIndex @_ @s @k wrk wid
        ti <- liftIO $ snapshot $ timeInterpreter $ ctx ^. networkLayer

        let mkPartialTx
                :: forall era. Cardano.Tx era
                -> W.PartialTx era
            mkPartialTx tx = W.PartialTx
                    tx
                    (fromExternalInput <$> body ^. #inputs)
                    (fromApiRedeemer <$> body ^. #redeemers)

        let balanceTx
                :: forall era. Cardano.IsShelleyBasedEra era
                => W.PartialTx era
                -> Handler (Cardano.Tx era)
            balanceTx partialTx =
                liftHandler $ W.balanceTransaction @_ @IO @s @k
                    wrk
                    genChange
                    (pp, nodePParams)
                    ti
                    wallet
                    partialTx

        anyShelleyTx <- maybeToHandler ErrByronTxNotSupported
            . asAnyShelleyBasedEra
            . cardanoTx
            . getApiT $ body ^. #transaction

        res <- withShelleyBasedTx anyShelleyTx
            (fmap inAnyCardanoEra . balanceTx . mkPartialTx)

        pure $ ApiSerialisedTransaction $ ApiT $ W.sealedTxFromCardano res
  where
    nl = ctx ^. networkLayer

    maybeToHandler :: IsServerError e => e -> Maybe a -> Handler a
    maybeToHandler _ (Just a) = pure a
    maybeToHandler e Nothing  = liftHandler $ throwE e

decodeTransaction
    :: forall ctx s k n.
        ( ctx ~ ApiLayer s k
        , IsOurs s Address
        , Typeable s
        , Typeable n
        )
    => ctx
    -> ApiT WalletId
    -> ApiSerialisedTransaction
    -> Handler (ApiDecodedTransaction n)
decodeTransaction ctx (ApiT wid) (ApiSerialisedTransaction (ApiT sealed)) = do
    let (decodedTx, toMint, toBurn, allCerts, interval) = decodeTx tl sealed
    let (Tx { txId
            , fee
            , resolvedInputs
            , resolvedCollateralInputs
            , outputs
            , withdrawals
            , metadata
            , scriptValidity
            }) = decodedTx
    (txinsOutsPaths, collateralInsOutsPaths, outsPath, acct, acctPath, pp, policyXPubM)
        <- withWorkerCtx ctx wid liftE liftE $ \wrk -> do
        (acct, _, acctPath) <-
            liftHandler $ W.readRewardAccount @_ @s @k @n wrk wid
        inputPaths <-
            liftHandler $ W.lookupTxIns @_ @s @k wrk wid $
            fst <$> resolvedInputs
        collateralInputPaths <-
            liftHandler $ W.lookupTxIns @_ @s @k wrk wid $
            fst <$> resolvedCollateralInputs
        outputPaths <-
            liftHandler $ W.lookupTxOuts @_ @s @k wrk wid outputs
        pp <- liftIO $ NW.currentProtocolParameters (wrk ^. networkLayer)
        policyXPubM <- fmap (fmap fst . eitherToMaybe)
            <$> liftIO . runExceptT $ W.readPolicyPublicKey @_ @s @k @n wrk wid
        pure
            ( inputPaths
            , collateralInputPaths
            , outputPaths
            , acct
            , acctPath
            , pp
            , policyXPubM
            )
    pure $ ApiDecodedTransaction
        { id = ApiT txId
        , fee = maybe (Quantity 0) (Quantity . fromIntegral . unCoin) fee
        , inputs = map toInp txinsOutsPaths
        , outputs = map toOut outsPath
        , collateral = map toInp collateralInsOutsPaths
        -- TODO: [ADP-1670]
        , collateralOutputs = ApiAsArray Nothing
        , withdrawals = map (toWrdl acct) $ Map.assocs withdrawals
        , mint = toApiAssetMintBurn policyXPubM toMint
        , burn = toApiAssetMintBurn policyXPubM toBurn
        , certificates = map (toApiAnyCert acct acctPath) allCerts
        , depositsTaken =
            (Quantity . fromIntegral . unCoin . W.stakeKeyDeposit $ pp)
                <$ filter ourRewardAccountRegistration
                    (toApiAnyCert acct acctPath <$> allCerts)
        , depositsReturned =
            (Quantity . fromIntegral . unCoin . W.stakeKeyDeposit $ pp)
                <$ filter ourRewardAccountDeregistration
                    (toApiAnyCert acct acctPath <$> allCerts)
        , metadata = ApiTxMetadata $ ApiT <$> metadata
        , scriptValidity = ApiT <$> scriptValidity
        , validityInterval = interval
        }
  where
    tl = ctx ^. W.transactionLayer @k
    policyIx = ApiT $ DerivationIndex $
        getIndex (minBound :: Index 'Hardened 'PolicyK)

    askForScript policyId scriptMap =
        case Map.lookup policyId scriptMap of
            Just script -> script
            Nothing -> error "askForScript: no minting/burning without either\
                             \ native or plutus script"

    toIdScriptAssets scriptmap tokenmap =
        [ (policy, askForScript policy scriptmap, tokenQuantities)
        | (policy, tokenQuantities) <- toNestedList tokenmap
        ]

    toTokenAmountFingerprint policy (name, tokenquantity) =
        ApiTokenAmountFingerprint
            { assetName = ApiT name
            , quantity = unTokenQuantity tokenquantity
            , fingerprint = ApiT $ mkTokenFingerprint policy name
            }

    fromIdScriptAssets (policy, script, tokens) = ApiTokens
        { policyId = ApiT policy
        , policyScript = ApiT script
        , assets = NE.map (toTokenAmountFingerprint policy) tokens
        }

    toApiTokens (TokenMapWithScripts tokenMap scriptMap) =
        map fromIdScriptAssets $
        toIdScriptAssets scriptMap tokenMap
    includePolicyKeyInfo (TokenMapWithScripts tokenMap _) xpubM =
        if tokenMap == TokenMap.empty then
            Nothing
        else
            xpubM
    toApiAssetMintBurn xpubM tokenWithScripts = ApiAssetMintBurn
        { tokens = toApiTokens tokenWithScripts
        , walletPolicyKeyHash =
            uncurry ApiPolicyKey . computeKeyPayload (Just True) <$>
            includePolicyKeyInfo tokenWithScripts xpubM
        , walletPolicyKeyIndex =
            policyIx <$ includePolicyKeyInfo tokenWithScripts xpubM
        }
    toOut (txoutIncoming, Nothing) =
        ExternalOutput $ toAddressAmount @n txoutIncoming
    toOut ((TxOut addr (TokenBundle (Coin c) tmap)), (Just path)) =
            WalletOutput $ ApiWalletOutput
                { address = (ApiT addr, Proxy @n)
                , amount = Quantity $ fromIntegral c
                , assets = ApiT tmap
                , derivationPath = NE.map ApiT path
                }
    toInp (txin@(TxIn txid ix), txoutPathM) =
        case txoutPathM of
            Nothing ->
                ExternalInput (ApiT txin)
            Just (TxOut addr (TokenBundle (Coin c) tmap), path) ->
                WalletInput $ ApiWalletInput
                    { id = ApiT txid
                    , index = ix
                    , address = (ApiT addr, Proxy @n)
                    , derivationPath = NE.map ApiT path
                    , amount = Quantity $ fromIntegral c
                    , assets = ApiT tmap
                    }
    toWrdl acct (rewardKey, (Coin c)) =
        if rewardKey == acct then
           ApiWithdrawalGeneral (ApiT rewardKey, Proxy @n) (Quantity $ fromIntegral c) Our
        else
           ApiWithdrawalGeneral (ApiT rewardKey, Proxy @n) (Quantity $ fromIntegral c) External

    toApiAnyCert acct acctPath = \case
        W.CertificateOfDelegation delCert -> toApiDelCert acct acctPath delCert
        W.CertificateOfPool poolCert -> toApiPoolCert poolCert
        W.CertificateOther otherCert -> toApiOtherCert otherCert

    toApiOtherCert = OtherCertificate . ApiT

    toApiPoolCert (W.Registration (W.PoolRegistrationCertificate poolId' poolOwners' poolMargin' poolCost' poolPledge' poolMetadata')) =
        let enrich (a, b) = (ApiT a, ApiT b)
        in StakePoolRegister $ ApiRegisterPool
           (ApiT poolId')
           (map ApiT poolOwners')
           (Quantity poolMargin')
           (Quantity $ unCoin poolCost')
           (Quantity $ unCoin poolPledge')
           (enrich <$> poolMetadata')
    toApiPoolCert (W.Retirement (W.PoolRetirementCertificate poolId' retirementEpoch')) =
        StakePoolDeregister $ ApiDeregisterPool
        (ApiT poolId')
        (ApiT retirementEpoch')

    toApiDelCert acct acctPath (W.CertDelegateNone rewardKey) =
        if rewardKey == acct then
            WalletDelegationCertificate $ QuitPool $ NE.map ApiT acctPath
        else
            DelegationCertificate $ QuitPoolExternal (ApiT rewardKey, Proxy @n)
    toApiDelCert acct acctPath (W.CertRegisterKey rewardKey) =
        if rewardKey == acct then
            WalletDelegationCertificate $
            RegisterRewardAccount $ NE.map ApiT acctPath
        else
            DelegationCertificate $
            RegisterRewardAccountExternal (ApiT rewardKey, Proxy @n)
    toApiDelCert acct acctPath (W.CertDelegateFull rewardKey poolId') =
        if rewardKey == acct then
            WalletDelegationCertificate $
            JoinPool (NE.map ApiT acctPath) (ApiT poolId')
        else
            DelegationCertificate $
            JoinPoolExternal (ApiT rewardKey, Proxy @n) (ApiT poolId')

    ourRewardAccountRegistration = \case
        WalletDelegationCertificate (RegisterRewardAccount _) -> True
        _ -> False
    ourRewardAccountDeregistration = \case
        WalletDelegationCertificate (QuitPool _) -> True
        _ -> False

submitTransaction
    :: forall ctx s k (n :: NetworkDiscriminant).
        ( ctx ~ ApiLayer s k
        , HasNetworkLayer IO ctx
        , IsOwned s k
        , Typeable s
        , Typeable n
        )
    => ctx
    -> ApiT WalletId
    -> ApiSerialisedTransaction
    -> Handler ApiTxId
submitTransaction ctx apiw@(ApiT wid) apitx@(ApiSerialisedTransaction (ApiT sealedTx)) = do
    --TODO: revisit/possibly set proper ttls in ADP-1193
    ttl <- liftIO $ W.getTxExpiry ti Nothing
    apiDecoded <- decodeTransaction @_ @s @k @n ctx apiw apitx
    when (isForeign apiDecoded) $
        liftHandler $ throwE ErrSubmitTransactionForeignWallet
    let ourOuts = getOurOuts apiDecoded
    let ourInps = getOurInps apiDecoded

    -- TODO: when partial signing is switched on we will need to revise this.
    -- The following needs to be taken into account. Wits could come from:
    -- (a) our wallet
    -- (b) other parties
    -- (c) script inputs
    -- With (b) not supported we can now filter our inputs and look for the unique payment keys
    -- Also with multisig switched on the input would need more than 1 wits
    let witsRequiredForInputs = length $ L.nubBy samePaymentKey $
            filter isInpOurs $
            (apiDecoded ^. #inputs) ++ (apiDecoded ^. #collateral)
    let totalNumberOfWits = length $ getSealedTxWitnesses sealedTx
    let ourDel =
            filter isJust $
            map isJoiningOrQuitting $ apiDecoded ^. #certificates
    when (length ourDel > 1) $
        liftHandler $ throwE ErrSubmitTransactionMultidelegationNotSupported

    let delAction = case ourDel of
            [Just del] -> Just del
            [] -> Nothing
            _ -> error "impossible to be here due to check above"

    when (witsRequiredForInputs > totalNumberOfWits) $
        liftHandler $ throwE $
        ErrSubmitTransactionPartiallySignedOrNoSignedTx witsRequiredForInputs totalNumberOfWits

    _ <- withWorkerCtx ctx wid liftE liftE $ \wrk -> do
        (acct, _, path) <- liftHandler $ W.readRewardAccount @_ @s @k @n wrk wid
        let wdrl = getOurWdrl acct path apiDecoded
        let txCtx = defaultTransactionCtx
                { -- TODO: [ADP-1193]
                  -- Get this from decodeTx:
                  txValidityInterval = (Nothing, ttl)
                , txWithdrawal = wdrl
                , txDelegationAction = delAction
                }
        txMeta <- liftHandler $ W.constructTxMeta @_ @s @k wrk wid txCtx ourInps ourOuts
        liftHandler
            $ W.submitTx @_ @s @k wrk wid (tx, txMeta, sealedTx)
    return $ ApiTxId (apiDecoded ^. #id)
  where
    (tx,_,_,_,_) = decodeTx tl sealedTx
    tl = ctx ^. W.transactionLayer @k
    ti :: TimeInterpreter (ExceptT PastHorizonException IO)
    nl = ctx ^. networkLayer
    ti = timeInterpreter nl

    isOutOurs (WalletOutput _) = True
    isOutOurs _ = False
    toTxOut (WalletOutput (ApiWalletOutput (ApiT addr, _) (Quantity amt) (ApiT tmap) _)) =
        TxOut addr (TokenBundle (Coin $ fromIntegral amt) tmap)
    toTxOut _ = error "we should have only our outputs at this point"
    getOurOuts apiDecodedTx =
        let generalOuts = apiDecodedTx ^. #outputs
        in map toTxOut $ filter isOutOurs generalOuts

    getOurWdrl rewardAcct path apiDecodedTx =
        let generalWdrls = apiDecodedTx ^. #withdrawals
            isWdrlOurs (ApiWithdrawalGeneral _ _ context) = context == Our
        in case filter isWdrlOurs generalWdrls of
            [ApiWithdrawalGeneral (ApiT acct, _) (Quantity amt) _] ->
                let acct' = invariant "reward account should be the same" acct (rewardAcct ==)
                in WithdrawalSelf acct' path (Coin amt)
            _ ->
                NoWithdrawal

    isInpOurs (WalletInput _) = True
    isInpOurs _ = False
    toTxInp (WalletInput (ApiWalletInput (ApiT txid) ix _ _ (Quantity amt) _)) =
        (TxIn txid ix, Coin $ fromIntegral amt)
    toTxInp _ = error "we should have only our inputs at this point"
    getOurInps apiDecodedTx =
        let generalInps = apiDecodedTx ^. #inputs
        in map toTxInp $ filter isInpOurs generalInps

    isForeign apiDecodedTx =
        let generalInps = apiDecodedTx ^. #inputs
            generalWdrls = apiDecodedTx ^. #withdrawals
            isInpForeign (WalletInput _) = False
            isInpForeign _ = True
            isWdrlForeign (ApiWithdrawalGeneral _ _ context) = context == External
        in
            all isInpForeign generalInps &&
            all isWdrlForeign generalWdrls

    samePaymentKey inp1 inp2 = case (inp1, inp2) of
        (WalletInput (ApiWalletInput _ _ _ derPath1 _ _), WalletInput (ApiWalletInput _ _ _ derPath2 _ _) ) ->
               derPath1 == derPath2
        _ -> False
    isJoiningOrQuitting = \case
        WalletDelegationCertificate (JoinPool _ (ApiT poolId)) ->
            Just $ Join poolId
        WalletDelegationCertificate (QuitPool _) ->
            Just Quit
        _ ->
            Nothing

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
        , AddressBookIso s
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

    (sel, tx, txMeta, txTime, pp) <- withWorkerCtx ctx wid liftE liftE $ \wrk -> do
        (action, _) <- liftHandler
            $ W.joinStakePool @_ @s @k wrk curEpoch pools pid poolStatus wid

        -- FIXME [ADP-1489] mkRewardAccountBuilder does itself read
        -- @currentNodeEra@ which is not guaranteed with the era read here. This
        -- could cause problems under exceptional circumstances.
        (wdrl, mkRwdAcct) <- mkRewardAccountBuilder @_ @s @_ @n ctx wid Nothing
        ttl <- liftIO $ W.getTxExpiry ti Nothing
        let txCtx = defaultTransactionCtx
                { txWithdrawal = wdrl
                , txValidityInterval = (Nothing, ttl)
                , txDelegationAction = Just action
                }
        (utxoAvailable, wallet, pendingTxs) <-
            liftHandler $ W.readWalletUTxOIndex @_ @s @k wrk wid
        -- FIXME [ADP-1489] pp and era are not guaranteed to be consistent,
        -- which could cause problems under exceptional circumstances.
        pp <- liftIO $ NW.currentProtocolParameters (wrk ^. networkLayer)
        era <- liftIO $ NW.currentNodeEra (wrk ^. networkLayer)
        let selectAssetsParams = W.SelectAssetsParams
                { outputs = []
                , pendingTxs
                , randomSeed = Nothing
                , txContext = txCtx
                , utxoAvailableForInputs = UTxOSelection.fromIndex utxoAvailable
                , utxoAvailableForCollateral = UTxOIndex.toMap utxoAvailable
                , wallet
                , selectionStrategy = SelectionStrategyOptimal
                }
        sel <- liftHandler
            $ W.selectAssets @_ @_ @s @k wrk era pp selectAssetsParams
            $ const Prelude.id
        sel' <- liftHandler
            $ W.assignChangeAddressesAndUpdateDb wrk wid genChange sel
        (tx, txMeta, txTime, sealedTx) <- liftHandler
            $ W.buildAndSignTransaction @_ @s @k
                wrk wid era mkRwdAcct pwd txCtx sel'
        liftHandler
            $ W.submitTx @_ @s @k wrk wid (tx, txMeta, sealedTx)

        pure (sel, tx, txMeta, txTime, pp)

    liftIO $ mkApiTransaction
        (timeInterpreter (ctx ^. networkLayer))
        (#pendingSince)
        MkApiTransactionParams
            { txId = tx ^. #txId
            , txFee = tx ^. #fee
            , txInputs = NE.toList $ second Just <$> sel ^. #inputs
              -- Joining a stake pool does not require collateral:
            , txCollateralInputs = []
            , txOutputs = tx ^. #outputs
            , txCollateralOutput = tx ^. #collateralOutput
            , txWithdrawals = tx ^. #withdrawals
            , txMeta
            , txMetadata = Nothing
            , txTime
            , txScriptValidity = tx ^. #scriptValidity
            , txDeposit = W.stakeKeyDeposit pp
            , txMetadataSchema = TxMetadataDetailedSchema
            }
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
        pp <- liftIO $ NW.currentProtocolParameters (wrk ^. networkLayer)
        era <- liftIO $ NW.currentNodeEra (wrk ^. networkLayer)
        deposit <- W.calcMinimumDeposit @_ @s @k wrk wid
        mkApiFee (Just deposit) [] <$>
            W.estimateFee (runSelection wrk era pp deposit w)
  where
    txCtx :: TransactionCtx
    txCtx = defaultTransactionCtx

    runSelection wrk era pp _deposit (utxoAvailable, wallet, pendingTxs) =
        W.selectAssets @_ @_ @s @k wrk era pp selectAssetsParams calcFee
      where
        calcFee _ = selectionDelta TokenBundle.getCoin
        selectAssetsParams = W.SelectAssetsParams
            { outputs = []
            , pendingTxs
            , randomSeed = Nothing
            , txContext = txCtx
            , utxoAvailableForInputs = UTxOSelection.fromIndex utxoAvailable
            , utxoAvailableForCollateral = UTxOIndex.toMap utxoAvailable
            , wallet
            , selectionStrategy = SelectionStrategyOptimal
            }

quitStakePool
    :: forall ctx s n k.
        ( ctx ~ ApiLayer s k
        , s ~ SeqState n k
        , AddressIndexDerivationType k ~ 'Soft
        , DelegationAddress n k
        , GenChange s
        , HasNetworkLayer IO ctx
        , IsOwned s k
        , SoftDerivation k
        , Typeable n
        , Typeable s
        , WalletKey k
        , AddressBookIso s
        )
    => ctx
    -> ApiT WalletId
    -> ApiWalletPassphrase
    -> Handler (ApiTransaction n)
quitStakePool ctx (ApiT wid) body = do
    let pwd = coerce $ getApiT $ body ^. #passphrase

    (sel, tx, txMeta, txTime, pp) <- withWorkerCtx ctx wid liftE liftE $ \wrk -> do
        -- FIXME [ADP-1489] mkRewardAccountBuilder does itself read
        -- @currentNodeEra@ which is not guaranteed with the era read here. This
        -- could cause problems under exceptional circumstances.
        (wdrl, mkRwdAcct) <-
            mkRewardAccountBuilder @_ @s @_ @n ctx wid (Just SelfWithdrawal)
        action <- liftHandler $ W.quitStakePool wrk wid wdrl
        ttl <- liftIO $ W.getTxExpiry ti Nothing
        let txCtx = defaultTransactionCtx
                { txWithdrawal = wdrl
                , txValidityInterval = (Nothing, ttl)
                , txDelegationAction = Just action
                }

        (utxoAvailable, wallet, pendingTxs) <-
            liftHandler $ W.readWalletUTxOIndex @_ @s @k wrk wid
        pp <- liftIO $ NW.currentProtocolParameters (wrk ^. networkLayer)
        era <- liftIO $ NW.currentNodeEra (wrk ^. networkLayer)
        let selectAssetsParams = W.SelectAssetsParams
                { outputs = []
                , pendingTxs
                , randomSeed = Nothing
                , txContext = txCtx
                , utxoAvailableForInputs = UTxOSelection.fromIndex utxoAvailable
                , utxoAvailableForCollateral = UTxOIndex.toMap utxoAvailable
                , wallet
                , selectionStrategy = SelectionStrategyOptimal
                }
        sel <- liftHandler
            $ W.selectAssets @_ @_ @s @k wrk era pp selectAssetsParams
            $ const Prelude.id
        sel' <- liftHandler
            $ W.assignChangeAddressesAndUpdateDb wrk wid genChange sel
        (tx, txMeta, txTime, sealedTx) <- liftHandler
            $ W.buildAndSignTransaction @_ @s @k
                wrk wid era mkRwdAcct pwd txCtx sel'
        liftHandler
            $ W.submitTx @_ @s @k wrk wid (tx, txMeta, sealedTx)

        pure (sel, tx, txMeta, txTime, pp)

    liftIO $ mkApiTransaction
        (timeInterpreter (ctx ^. networkLayer))
        (#pendingSince)
        MkApiTransactionParams
            { txId = tx ^. #txId
            , txFee = tx ^. #fee
            , txInputs = NE.toList $ second Just <$> sel ^. #inputs
              -- Quitting a stake pool does not require collateral:
            , txCollateralInputs = []
            , txOutputs = tx ^. #outputs
            , txCollateralOutput = tx ^. #collateralOutput
            , txWithdrawals = tx ^. #withdrawals
            , txMeta
            , txMetadata = Nothing
            , txTime
            , txScriptValidity = tx ^. #scriptValidity
            , txDeposit = W.stakeKeyDeposit pp
            , txMetadataSchema = TxMetadataDetailedSchema
            }
  where
    ti :: TimeInterpreter (ExceptT PastHorizonException IO)
    ti = timeInterpreter (ctx ^. networkLayer)

    genChange = delegationAddress @n

-- More testable helper for `listStakeKeys`.
--
-- TODO: Ideally test things like
-- no rewards => ada in distr == utxo balance
-- all keys in inputs appear (once) in output
listStakeKeys'
    :: forall (n :: NetworkDiscriminant) m. Monad m
    => UTxO.UTxO
        -- ^ The wallet's UTxO
    -> (Address -> Maybe RewardAccount)
        -- ^ Lookup reward account of addr
    -> (Set RewardAccount -> m (Map RewardAccount Coin))
        -- ^ Batch fetch of rewards
    -> [(RewardAccount, Natural, ApiWalletDelegation)]
        -- ^ The wallet's known stake keys, along with derivation index, and
        -- delegation status.
    -> m (ApiStakeKeys n)
listStakeKeys' utxo lookupStakeRef fetchRewards ourKeysWithInfo = do
        let distr = stakeKeyCoinDistr lookupStakeRef utxo
        let stakeKeysInUTxO = catMaybes $ Map.keys distr
        let stake acc = fromMaybe (Coin 0) $ Map.lookup acc distr

        let ourKeys = map (\(acc,_,_) -> acc) ourKeysWithInfo

        let allKeys = ourKeys <> stakeKeysInUTxO

        -- If we wanted to know whether a stake key is registered or not, we
        -- could expose the difference between `Nothing` and `Just 0` in the
        -- `NetworkLayer` interface.
        rewardsMap <- fetchRewards $ Set.fromList allKeys

        let rewards acc = fromMaybe (Coin 0) $
                Map.lookup acc rewardsMap

        let mkOurs (acc, ix, deleg) = ApiOurStakeKey
                { _index = ix
                , _key = (ApiT acc, Proxy)
                , _rewardBalance = coinToQuantity $
                    rewards acc
                , _delegation = deleg
                , _stake = coinToQuantity $
                    stake (Just acc) <> rewards acc
                }

        let mkForeign acc = ApiForeignStakeKey
                { _key = (ApiT acc, Proxy)
                , _rewardBalance = coinToQuantity $
                    rewards acc
                , _stake = coinToQuantity $
                    stake (Just acc) <> rewards acc
                }

        let foreignKeys = stakeKeysInUTxO \\ ourKeys

        let nullKey = ApiNullStakeKey
                { _stake = coinToQuantity $ stake Nothing
                }

        return $ ApiStakeKeys
            { _ours = map mkOurs ourKeysWithInfo
            , _foreign = map mkForeign foreignKeys
            , _none = nullKey
            }

listStakeKeys
    :: forall ctx s n k.
        ( ctx ~ ApiLayer s k
        , s ~ SeqState n k
        , HasNetworkLayer IO ctx
        , Typeable n
        , Typeable s
        )
    => (Address -> Maybe RewardAccount)
    -> ctx
    -> ApiT WalletId
    -> Handler (ApiStakeKeys n)
listStakeKeys lookupStakeRef ctx (ApiT wid) = do
    withWorkerCtx ctx wid liftE liftE $ \wrk -> liftHandler $ do
            (wal, meta, pending) <- W.readWallet @_ @s @k wrk wid
            let utxo = availableUTxO @s pending wal

            let takeFst (a,_,_) = a
            mourAccount <- fmap (fmap takeFst . eitherToMaybe)
                <$> liftIO . runExceptT $ W.readRewardAccount @_ @s @k @n wrk wid
            ourApiDelegation <- liftIO $ toApiWalletDelegation (meta ^. #delegation)
                (unsafeExtendSafeZone (timeInterpreter $ ctx ^. networkLayer))
            let ourKeys = case mourAccount of
                    Just acc -> [(acc, 0, ourApiDelegation)]
                    Nothing -> []

            liftIO $ listStakeKeys' @n
                utxo
                lookupStakeRef
                (fetchRewardAccountBalances nl)
                ourKeys
  where
    nl = ctx ^. networkLayer

{-------------------------------------------------------------------------------
                                Migrations
-------------------------------------------------------------------------------}

createMigrationPlan
    :: forall ctx n s k.
        ( ctx ~ ApiLayer s k
        , Bounded (Index (AddressIndexDerivationType k) 'AddressK)
        , HardDerivation k
        , IsOwned s k
        , Typeable n
        , Typeable s
        , WalletKey k
        )
    => ctx
    -> Maybe ApiWithdrawalPostData
        -- ^ What type of reward withdrawal to attempt
    -> ApiT WalletId
        -- ^ Source wallet
    -> ApiWalletMigrationPlanPostData n
        -- ^ Target addresses
    -> Handler (ApiWalletMigrationPlan n)
createMigrationPlan ctx withdrawalType (ApiT wid) postData = do
    -- FIXME [ADP-1489] mkRewardAccountBuilder does itself read
    -- @currentNodeEra@ which is not guaranteed with the era read here. This
    -- could cause problems under exceptional circumstances.
    (rewardWithdrawal, _) <-
        mkRewardAccountBuilder @_ @s @_ @n ctx wid withdrawalType
    withWorkerCtx ctx wid liftE liftE $ \wrk -> liftHandler $ do
        era <- liftIO $ NW.currentNodeEra (wrk ^. networkLayer)
        (wallet, _, _) <- withExceptT ErrCreateMigrationPlanNoSuchWallet $
            W.readWallet wrk wid
        plan <- W.createMigrationPlan wrk era wid rewardWithdrawal
        failWith ErrCreateMigrationPlanEmpty $ mkApiWalletMigrationPlan
            (getState wallet)
            (view #addresses postData)
            (rewardWithdrawal)
            (plan)

mkApiWalletMigrationPlan
    :: forall n s. IsOurs s Address
    => s
    -> NonEmpty (ApiT Address, Proxy n)
    -> Withdrawal
    -> MigrationPlan
    -> Maybe (ApiWalletMigrationPlan n)
mkApiWalletMigrationPlan s addresses rewardWithdrawal plan =
    mkApiPlan <$> maybeSelections
  where
    mkApiPlan :: NonEmpty (ApiCoinSelection n) -> ApiWalletMigrationPlan n
    mkApiPlan selections = ApiWalletMigrationPlan
        { selections
        , totalFee
        , balanceLeftover
        , balanceSelected
        }

    maybeSelections :: Maybe (NonEmpty (ApiCoinSelection n))
    maybeSelections = fmap mkApiCoinSelectionForMigration <$> maybeUnsignedTxs

    maybeSelectionWithdrawals
        :: Maybe (NonEmpty (W.SelectionWithoutChange, Withdrawal))
    maybeSelectionWithdrawals
        = W.migrationPlanToSelectionWithdrawals plan rewardWithdrawal
        $ getApiT . fst <$> addresses

    maybeUnsignedTxs = fmap mkUnsignedTx <$> maybeSelectionWithdrawals
      where
        mkUnsignedTx (selection, withdrawal) = W.selectionToUnsignedTx
            withdrawal (selection {change = []}) s

    totalFee :: Quantity "lovelace" Natural
    totalFee = coinToQuantity $ view #totalFee plan

    balanceLeftover :: ApiWalletMigrationBalance
    balanceLeftover = plan
        & view #unselected
        & UTxO.balance
        & mkApiWalletMigrationBalance

    balanceSelected :: ApiWalletMigrationBalance
    balanceSelected = mkApiWalletMigrationBalance $
        TokenBundle.fromCoin balanceRewardWithdrawal <> balanceUTxO
      where
        balanceUTxO = plan
            & view #selections
            & F.foldMap (view #inputBalance)
        balanceRewardWithdrawal = plan
            & view #selections
            & F.foldMap (view #rewardWithdrawal)

    mkApiCoinSelectionForMigration = mkApiCoinSelection [] [] Nothing Nothing

    mkApiWalletMigrationBalance :: TokenBundle -> ApiWalletMigrationBalance
    mkApiWalletMigrationBalance b = ApiWalletMigrationBalance
        { ada = coinToQuantity $ view #coin b
        , assets = ApiT $ view #tokens b
        }

migrateWallet
    :: forall ctx s k n p.
        ( ctx ~ ApiLayer s k
        , Bounded (Index (AddressIndexDerivationType k) 'AddressK)
        , HardDerivation k
        , HasNetworkLayer IO ctx
        , IsOwned s k
        , Typeable n
        , Typeable s
        , WalletKey k
        )
    => ctx
    -> Maybe ApiWithdrawalPostData
        -- ^ What type of reward withdrawal to attempt
    -> ApiT WalletId
    -> ApiWalletMigrationPostData n p
    -> Handler (NonEmpty (ApiTransaction n))
migrateWallet ctx withdrawalType (ApiT wid) postData = do
    -- FIXME [ADP-1489] mkRewardAccountBuilder does itself read
    -- @currentNodeEra@ which is not guaranteed with the era read here. This
    -- could cause problems under exceptional circumstances.
    (rewardWithdrawal, mkRewardAccount) <-
        mkRewardAccountBuilder @_ @s @_ @n ctx wid withdrawalType
    withWorkerCtx ctx wid liftE liftE $ \wrk -> do
        era <- liftIO $ NW.currentNodeEra (wrk ^. networkLayer)
        plan <- liftHandler $ W.createMigrationPlan wrk era wid rewardWithdrawal
        ttl <- liftIO $ W.getTxExpiry ti Nothing
        pp <- liftIO $ NW.currentProtocolParameters (wrk ^. networkLayer)
        selectionWithdrawals <- liftHandler
            $ failWith ErrCreateMigrationPlanEmpty
            $ W.migrationPlanToSelectionWithdrawals
                plan rewardWithdrawal addresses
        forM selectionWithdrawals $ \(selection, txWithdrawal) -> do
            let txContext = defaultTransactionCtx
                    { txWithdrawal
                    , txValidityInterval = (Nothing, ttl)
                    , txDelegationAction = Nothing
                    }
            (tx, txMeta, txTime, sealedTx) <- liftHandler $
                W.buildAndSignTransaction @_ @s @k
                    wrk
                    wid
                    era
                    mkRewardAccount
                    pwd
                    txContext (selection {change = []})
            liftHandler $
                W.submitTx @_ @s @k wrk wid (tx, txMeta, sealedTx)
            liftIO $ mkApiTransaction
                (timeInterpreter (ctx ^. networkLayer))
                (#pendingSince)
                MkApiTransactionParams
                    { txId = tx ^. #txId
                    , txFee = tx ^. #fee
                    , txInputs =
                        NE.toList $ second Just <$> selection ^. #inputs
                      -- Migrations never require collateral:
                    , txCollateralInputs = []
                    , txOutputs = tx ^. #outputs
                    , txCollateralOutput = tx ^. #collateralOutput
                    , txWithdrawals = tx ^. #withdrawals
                    , txMeta
                    , txMetadata = Nothing
                    , txTime
                    , txScriptValidity = tx ^. #scriptValidity
                    , txDeposit = W.stakeKeyDeposit pp
                    , txMetadataSchema = TxMetadataDetailedSchema
                    }
  where
    addresses = getApiT . fst <$> view #addresses postData
    pwd = coerce $ getApiT $ postData ^. #passphrase
    ti :: TimeInterpreter (ExceptT PastHorizonException IO)
    ti = timeInterpreter (ctx ^. networkLayer)

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
            (view #slotNo nodeTip)
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
    toApiEra (AnyCardanoEra AlonzoEra) = ApiAlonzo
    toApiEra (AnyCardanoEra BabbageEra) = ApiBabbage

    -- (network tip, next epoch)
    -- May be unavailable if the node is still syncing.
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
    -> TransactionLayer k W.SealedTx
    -> Handler ApiNetworkParameters
getNetworkParameters (_block0, genesisNp, _st) nl tl = do
    pp <- liftIO $ NW.currentProtocolParameters nl
    sp <- liftIO $ NW.currentSlottingParameters nl
    era <- liftIO $ NW.currentNodeEra nl
    let np = genesisNp { protocolParameters = pp, slottingParameters = sp }
    let txConstraints = constraints tl era pp
    liftIO $ toApiNetworkParameters np txConstraints (interpretQuery ti . toApiEpochInfo)
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
    -> ApiT W.SealedTx
    -> Handler ApiTxId
postExternalTransaction ctx (ApiT sealed) = do
    tx <- liftHandler $ W.submitExternalTx @ctx @k ctx sealed
    return $ ApiTxId (ApiT (tx ^. #txId))

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
    :: forall ctx s k ver.
        ( ctx ~ ApiLayer s k
        , SoftDerivation k
        , WalletKey k
        , GetAccount s k
        )
    => ctx
    -> ((ByteString, Role) -> VerificationKeyHashing -> ver)
    -> ApiT WalletId
    -> ApiT Role
    -> ApiT DerivationIndex
    -> Maybe Bool
    -> Handler ver
derivePublicKey ctx mkVer (ApiT wid) (ApiT role_) (ApiT ix) hashed = do
    withWorkerCtx @_ @s @k ctx wid liftE liftE $ \wrk -> do
        k <- liftHandler $ W.derivePublicKey @_ @s @k wrk wid role_ ix
        let (payload, hashing) = computeKeyPayload hashed (getRawKey k)
        pure $ mkVer (payload, role_) hashing

computeKeyPayload :: Maybe Bool -> XPub -> (ByteString, VerificationKeyHashing)
computeKeyPayload hashed k = case hashing of
    WithoutHashing -> (xpubPublicKey k, WithoutHashing)
    WithHashing -> (blake2b224 $ xpubPublicKey k, WithHashing)
  where
    hashing = case hashed of
        Nothing -> WithoutHashing
        Just v -> if v then WithHashing else WithoutHashing

postAccountPublicKey
    :: forall ctx s k account.
        ( ctx ~ ApiLayer s k
        , WalletKey k
        , GetPurpose k
        )
    => ctx
    -> (ByteString -> KeyFormat -> Index 'Hardened 'PurposeK -> account)
    -> ApiT WalletId
    -> ApiT DerivationIndex
    -> ApiPostAccountKeyDataWithPurpose
    -> Handler account
postAccountPublicKey ctx mkAccount (ApiT wid) (ApiT ix) (ApiPostAccountKeyDataWithPurpose (ApiT pwd) extd purposeM) = do
    withWorkerCtx @_ @s @k ctx wid liftE liftE $ \wrk -> do
        k <- liftHandler $ W.getAccountPublicKeyAtIndex @_ @s @k wrk wid pwd ix (getApiT <$> purposeM)
        pure $ mkAccount (publicKeyToBytes' extd $ getRawKey k) extd ixPurpose'
  where
    ixPurpose' =
        maybe (getPurpose @k) (Index . getDerivationIndex . getApiT) purposeM

publicKeyToBytes' :: KeyFormat -> XPub -> ByteString
publicKeyToBytes' = \case
    Extended -> xpubToBytes
    NonExtended -> xpubPublicKey

getAccountPublicKey
    :: forall ctx s k account.
        ( ctx ~ ApiLayer s k
        , GetAccount s k
        , WalletKey k
        , GetPurpose k
        )
    => ctx
    -> (ByteString -> KeyFormat -> Index 'Hardened 'PurposeK -> account)
    -> ApiT WalletId
    -> Maybe KeyFormat
    -> Handler account
getAccountPublicKey ctx mkAccount (ApiT wid) extended = do
    withWorkerCtx @_ @s @k ctx wid liftE liftE $ \wrk -> do
        k <- liftHandler $ W.readAccountPublicKey @_ @s @k wrk wid
        pure $ mkAccount (publicKeyToBytes' extd $ getRawKey k) extd (getPurpose @k)
  where
      extd = case extended of
          Just Extended -> Extended
          _ -> NonExtended

getPolicyKey
    :: forall ctx s k (n :: NetworkDiscriminant).
        ( ctx ~ ApiLayer s k
        , Typeable s
        , Typeable n
        )
    => ctx
    -> ApiT WalletId
    -> Maybe Bool
    -> Handler ApiPolicyKey
getPolicyKey ctx (ApiT wid) hashed = do
    withWorkerCtx @_ @s @k ctx wid liftE liftE $ \wrk -> do
        (k, _) <- liftHandler $ W.readPolicyPublicKey @_ @s @k @n wrk wid
        pure $ uncurry ApiPolicyKey (computeKeyPayload hashed k)

postPolicyKey
    :: forall ctx s (n :: NetworkDiscriminant).
        ( ctx ~ ApiLayer s ShelleyKey
        , s ~ SeqState n ShelleyKey
        )
    => ctx
    -> ApiT WalletId
    -> Maybe Bool
    -> ApiPostPolicyKeyData
    -> Handler ApiPolicyKey
postPolicyKey ctx (ApiT wid) hashed apiPassphrase =
    withWorkerCtx @_ @s @ShelleyKey ctx wid liftE liftE $ \wrk -> do
        k <- liftHandler $ W.writePolicyPublicKey @_ @s @n wrk wid pwd
        pure $ uncurry ApiPolicyKey (computeKeyPayload hashed (getRawKey k))
  where
    pwd = getApiT (apiPassphrase ^. #passphrase)

postPolicyId
    :: forall ctx s k (n :: NetworkDiscriminant).
        ( ctx ~ ApiLayer s k
        , WalletKey k
        , Typeable s
        , Typeable n
        )
    => ctx
    -> ApiT WalletId
    -> ApiPostPolicyIdData
    -> Handler ApiPolicyId
postPolicyId ctx (ApiT wid) payload = do
    let retrieveAllCosigners = foldScript (:) []
    let wrongMintingTemplate templ =
            isLeft (validateScriptOfTemplate RecommendedValidation templ)
            || length (retrieveAllCosigners templ) > 1
            || (L.any (/= Cosigner 0)) (retrieveAllCosigners templ)
    when ( wrongMintingTemplate scriptTempl ) $
        liftHandler $ throwE ErrGetPolicyIdWrongMintingBurningTemplate

    withWorkerCtx @_ @s @k ctx wid liftE liftE $ \wrk -> do
        (xpub, _) <- liftHandler $ W.readPolicyPublicKey @_ @s @k @n wrk wid
        pure $ ApiPolicyId $ ApiT $
            toTokenPolicyId @k scriptTempl (Map.singleton (Cosigner 0) xpub)
  where
    scriptTempl = getApiT (payload ^. #policyScriptTemplate)

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
    -> Passphrase "user"
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
        era <- liftIO $ NW.currentNodeEra (wrk ^. networkLayer)
        case (testEquality (typeRep @s) (typeRep @shelley), withdrawal) of
            (Nothing, Just{}) ->
                liftHandler $ throwE ErrReadRewardAccountNotAShelleyWallet

            (_, Nothing) ->
                pure (NoWithdrawal, selfRewardCredentials)

            (Just Refl, Just SelfWithdrawal) -> do
                (acct, _, path) <- liftHandler $ W.readRewardAccount @_ @s @k @n wrk wid
                wdrl <- liftHandler $ W.queryRewardBalance @_ wrk acct
                (, selfRewardCredentials) . WithdrawalSelf acct path
                    <$> liftIO (W.readNextWithdrawal @_ @k wrk era wdrl)

            (Just Refl, Just (ExternalWithdrawal (ApiMnemonicT mw))) -> do
                let (xprv, acct, path) = W.someRewardAccount @ShelleyKey mw
                wdrl <- liftHandler (W.queryRewardBalance @_ wrk acct)
                    >>= liftIO . W.readNextWithdrawal @_ @k wrk era
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
    -> [Coin]
    -> Maybe (DelegationAction, NonEmpty DerivationIndex)
    -> Maybe W.TxMetadata
    -> UnsignedTx input output change withdrawal
    -> ApiCoinSelection n
mkApiCoinSelection deps refunds mcerts metadata unsignedTx =
    ApiCoinSelection
        { inputs = mkApiCoinSelectionInput
            <$> unsignedTx ^. #unsignedInputs
        , outputs = mkApiCoinSelectionOutput
            <$> unsignedTx ^. #unsignedOutputs
        , change = mkApiCoinSelectionChange
            <$> unsignedTx ^. #unsignedChange
        , collateral = mkApiCoinSelectionCollateral
            <$> unsignedTx ^. #unsignedCollateral
        , withdrawals = mkApiCoinSelectionWithdrawal
            <$> unsignedTx ^. #unsignedWithdrawals
        , certificates = uncurry mkCertificates
            <$> mcerts
        , depositsTaken = mkApiCoin
            <$> deps
        , depositsReturned = mkApiCoin
            <$> refunds
        , metadata = ApiBytesT. serialiseToCBOR
            <$> metadata
        }
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

    mkApiCoinSelectionInput :: input -> ApiWalletInput n
    mkApiCoinSelectionInput
        (TxIn txid index, TxOut addr (TokenBundle amount assets), path) =
        ApiWalletInput
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

    mkApiCoinSelectionCollateral :: input -> ApiCoinSelectionCollateral n
    mkApiCoinSelectionCollateral
        (TxIn txid index, TxOut addr (TokenBundle amount _), path) =
        ApiCoinSelectionCollateral
            { id = ApiT txid
            , index = index
            , address = (ApiT addr, Proxy @n)
            , amount = coinToQuantity amount
            , derivationPath = ApiT <$> path
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

data MkApiTransactionParams = MkApiTransactionParams
    { txId :: Hash "Tx"
    , txFee :: Maybe Coin
    , txInputs :: [(TxIn, Maybe TxOut)]
    , txCollateralInputs :: [(TxIn, Maybe TxOut)]
    , txOutputs :: [TxOut]
    , txCollateralOutput :: Maybe TxOut
    , txWithdrawals :: Map RewardAccount Coin
    , txMeta :: W.TxMeta
    , txMetadata :: Maybe W.TxMetadata
    , txTime :: UTCTime
    , txScriptValidity :: Maybe W.TxScriptValidity
    , txDeposit :: Coin
    , txMetadataSchema :: TxMetadataSchema
    }
    deriving (Eq, Generic, Show)

mkApiTransaction
    :: forall n. ()
    => TimeInterpreter (ExceptT PastHorizonException IO)
    -> Lens' (ApiTransaction n) (Maybe ApiBlockReference)
    -> MkApiTransactionParams
    -> IO (ApiTransaction n)
mkApiTransaction timeInterpreter setTimeReference tx = do
    timeRef <- (#time .~ (tx ^. #txTime)) <$> makeApiBlockReference
        (neverFails
            "makeApiBlockReference shouldn't fail getting the time of \
            \transactions with slots in the past" timeInterpreter)
        (tx ^. (#txMeta . #slotNo))
        (natural (tx ^. (#txMeta . #blockHeight)))

    expRef <- traverse makeApiSlotReference' (tx ^. (#txMeta . #expiry))
    return $ apiTx & setTimeReference .~ Just timeRef & #expiresAt .~ expRef
  where
    -- Since tx expiry can be far in the future, we use unsafeExtendSafeZone for
    -- now.
    makeApiSlotReference' = makeApiSlotReference
        $ unsafeExtendSafeZone timeInterpreter

    apiTx :: ApiTransaction n
    apiTx = ApiTransaction
        { id = ApiT $ tx ^. #txId
        , amount = Quantity . fromIntegral $ tx ^. (#txMeta . #amount . #unCoin)
        , fee = Quantity $ maybe 0 (fromIntegral . unCoin) (tx ^. #txFee)
        , depositTaken = Quantity depositIfAny
        , depositReturned = Quantity reclaimIfAny
        , insertedAt = Nothing
        , pendingSince = Nothing
        , expiresAt = Nothing
        , depth = Nothing
        , direction = ApiT (tx ^. (#txMeta . #direction))
        , inputs =
            [ ApiTxInput (fmap (toAddressAmount @n) o) (ApiT i)
            | (i, o) <- tx ^. #txInputs
            ]
        , collateral =
            [ ApiTxCollateral (fmap toAddressAmountNoAssets o) (ApiT i)
            | (i, o) <- tx ^. #txCollateralInputs
            ]
        , outputs = toAddressAmount @n <$> tx ^. #txOutputs
        , collateralOutputs = ApiAsArray $
            toAddressAmount @n <$> tx ^. #txCollateralOutput
        , withdrawals = mkApiWithdrawal @n <$> Map.toList (tx ^. #txWithdrawals)
        , status = ApiT (tx ^. (#txMeta . #status))
        , metadata = TxMetadataWithSchema (tx ^. #txMetadataSchema)
            <$> tx ^. #txMetadata
        , scriptValidity = ApiT <$> tx ^. #txScriptValidity
        }

    depositIfAny :: Natural
    depositIfAny
        | tx ^. (#txMeta . #direction) == W.Outgoing =
            if totalIn < totalOut
            then 0
            else totalIn - totalOut
        | otherwise = 0

    -- (pending) when reclaim is coming we have (fee+out) - inp = deposit
    -- tx is incoming, and the wallet spent for fee and received deposit - fee as out
    -- (inLedger) when reclaim is accommodated we have out - inp < deposit as fee was incurred
    -- So in order to detect this we need to have
    -- 1. deposit
    -- 2. have inpsWithoutFee of the wallet non-empty
    -- 3. outs of the wallet non-empty
    -- 4. tx Incoming
    -- 5. outs - inpsWithoutFee <= deposit
    -- assumption: this should work when
    depositValue :: Natural
    depositValue = fromIntegral . unCoin $ tx ^. #txDeposit

    reclaimIfAny :: Natural
    reclaimIfAny
        | tx ^. (#txMeta . #direction) == W.Incoming =
              if ( totalIn > 0 && totalOut > 0 && totalOut > totalIn)
                 && (totalOut - totalIn <= depositValue) then
                  depositValue
              else
                  0
        | otherwise = 0

    totalIn :: Natural
    totalIn
        = sum (txOutValue <$> mapMaybe snd (tx ^. #txInputs))
        + sum (fromIntegral . unCoin <$> Map.elems (tx ^. #txWithdrawals))

    totalOut :: Natural
    totalOut
        = sum (txOutValue <$> tx ^. #txOutputs)
        + maybe 0 (fromIntegral . unCoin) (tx ^. #txFee)

    txOutValue :: TxOut -> Natural
    txOutValue = fromIntegral . unCoin . txOutCoin

    toAddressAmountNoAssets
        :: TxOut
        -> AddressAmountNoAssets (ApiT Address, Proxy n)
    toAddressAmountNoAssets (TxOut addr (TokenBundle.TokenBundle coin _)) =
        AddressAmountNoAssets (ApiT addr, Proxy @n) (mkApiCoin coin)

toAddressAmount
    :: forall (n :: NetworkDiscriminant). ()
    => TxOut
    -> AddressAmount (ApiT Address, Proxy n)
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
    , minimumCoins = Quantity . Coin.toNatural <$> minCoins
    , deposit = Quantity . Coin.toNatural $ fromMaybe (Coin 0) mDeposit
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

fromExternalInput :: ApiExternalInput n -> (TxIn, TxOut, Maybe (Hash "Datum"))
fromExternalInput ApiExternalInput
    { id = ApiT tid
    , index = ix
    , address = (ApiT addr, _)
    , amount = Quantity amt
    , assets = ApiT assets
    , datum
    }
  =
    ( TxIn tid ix
    , TxOut addr (TokenBundle (Coin $ fromIntegral amt) assets)
    , getApiT <$> datum
    )

fromApiRedeemer :: ApiRedeemer n -> Redeemer
fromApiRedeemer = \case
    ApiRedeemerSpending (ApiBytesT bytes) (ApiT i) ->
        RedeemerSpending bytes i
    ApiRedeemerMinting (ApiBytesT bytes) (ApiT p) ->
        RedeemerMinting bytes p
    ApiRedeemerRewarding (ApiBytesT bytes) r ->
        RedeemerRewarding bytes r

{-------------------------------------------------------------------------------
                                Api Layer
-------------------------------------------------------------------------------}

-- | Create a new instance of the wallet layer.
newApiLayer
    :: forall ctx s k.
        ( ctx ~ ApiLayer s k
        , IsOurs s RewardAccount
        , IsOurs s Address
        , AddressBookIso s
        , MaybeLight s
        )
    => Tracer IO WalletEngineLog
    -> (Block, NetworkParameters, SyncTolerance)
    -> NetworkLayer IO Block
    -> TransactionLayer k W.SealedTx
    -> DBFactory IO s k
    -> TokenMetadataClient IO
    -> (WorkerCtx ctx -> WalletId -> IO ())
        -- ^ Action to run concurrently with wallet restore
    -> IO ctx
newApiLayer tr g0 nw tl df tokenMeta coworker = do
    re <- Registry.empty
    let trTx = contramap MsgSubmitSealedTx tr
    let trW = contramap MsgWalletWorker tr
    locks <- Concierge.newConcierge
    let ctx = ApiLayer trTx trW g0 nw tl df re locks tokenMeta
    listDatabases df >>= mapM_ (startWalletWorker ctx coworker)
    return ctx

-- | Register a wallet restoration thread with the worker registry.
startWalletWorker
    :: forall ctx s k.
        ( ctx ~ ApiLayer s k
        , IsOurs s RewardAccount
        , IsOurs s Address
        , AddressBookIso s
        , MaybeLight s
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
        , AddressBookIso s
        , MaybeLight s
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
        , AddressBookIso s
        , MaybeLight s
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
            (race_
                (forever $ W.runLocalTxSubmissionPool txCfg ctx' wid)
                (coworker ctx' wid))
        }
    txCfg = W.defaultLocalTxSubmissionConfig

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
    Atomic handler operations
-------------------------------------------------------------------------------}
atomicallyWithHandler
    :: Ord lock
    => Concierge.Concierge IO lock -> lock -> Handler a -> Handler a
atomicallyWithHandler c l = Handler . Concierge.atomicallyWith c l . runHandler'

{-------------------------------------------------------------------------------
                                Error Handling
-------------------------------------------------------------------------------}

-- | Maps types to servant error responses.
class IsServerError e where
    -- | A structured human-readable error code to return to API clients.
    toServerError :: e -> ServerError

-- | Lift our wallet layer into servant 'Handler', by mapping each error to a
-- corresponding servant error.
liftHandler :: IsServerError e => ExceptT e IO a -> Handler a
liftHandler action = Handler (withExceptT toServerError action)

liftE :: IsServerError e => e -> Handler a
liftE = liftHandler . throwE

apiError :: ServerError -> ApiErrorCode -> Text -> ServerError
apiError err code message = err
    { errBody = Aeson.encode $ Aeson.object
        [ "code" .= code
        , "message" .= T.replace "\n" " " message
        ]
    , errHeaders =
        (hContentType, renderHeader $ contentType $ Proxy @JSON)
        : errHeaders err
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

instance IsServerError ErrCurrentEpoch where
    toServerError = \case
        ErrUnableToDetermineCurrentEpoch ->
            apiError err500 UnableToDetermineCurrentEpoch $ mconcat
                [ "I'm unable to determine the current epoch. "
                , "Please wait a while for the node to sync and try again."
                ]
        ErrCurrentEpochPastHorizonException e -> toServerError e

instance IsServerError ErrUnexpectedPoolIdPlaceholder where
    toServerError = \case
        ErrUnexpectedPoolIdPlaceholder ->
            apiError err400 BadRequest (pretty msg)
      where
        Left msg = fromText @PoolId "INVALID"

instance IsServerError ErrNoSuchWallet where
    toServerError = \case
        ErrNoSuchWallet wid ->
            apiError err404 NoSuchWallet $ mconcat
                [ "I couldn't find a wallet with the given id: "
                , toText wid
                ]

instance IsServerError ErrWalletNotResponding where
    toServerError = \case
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

instance IsServerError ErrWalletAlreadyExists where
    toServerError = \case
        ErrWalletAlreadyExists wid ->
            apiError err409 WalletAlreadyExists $ mconcat
                [ "This operation would yield a wallet with the following id: "
                , toText wid
                , " However, I already know of a wallet with this id."
                ]

instance IsServerError ErrCreateWallet where
    toServerError = \case
        ErrCreateWalletAlreadyExists e -> toServerError e
        ErrCreateWalletFailedToCreateWorker ->
            apiError err500 UnexpectedError $ mconcat
                [ "That's embarrassing. Your wallet looks good, but I couldn't "
                , "open a new database to store its data. This is unexpected "
                , "and likely not your fault. Perhaps, check your filesystem's "
                , "permissions or available space?"
                ]

instance IsServerError ErrWithRootKey where
    toServerError = \case
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
        ErrWithRootKeyWrongMnemonic wid ->
            apiError err403 WrongMnemonic $ mconcat
                [ "The given mnemonic doesn't match the one this wallet was created with "
                , ": "
                , toText wid
                ]
        ErrWithRootKeyWrongPassphrase wid (ErrPassphraseSchemeUnsupported s) ->
            apiError err501 WrongEncryptionPassphrase $ mconcat
                [ "This build is not compiled with support for the "
                , toText s <> " scheme used by the given wallet: "
                , toText wid
                ]

instance IsServerError ErrListAssets where
    toServerError = \case
        ErrListAssetsNoSuchWallet e -> toServerError e

instance IsServerError ErrGetAsset where
    toServerError = \case
        ErrGetAssetNoSuchWallet e -> toServerError e
        ErrGetAssetNotPresent ->
            apiError err404 AssetNotPresent $ mconcat
                [ "The requested asset is not associated with this wallet."
                ]

instance IsServerError ErrListUTxOStatistics where
    toServerError = \case
        ErrListUTxOStatisticsNoSuchWallet e -> toServerError e

instance IsServerError ErrSignPayment where
    toServerError = \case
        ErrSignPaymentMkTx e -> toServerError e
        ErrSignPaymentNoSuchWallet e -> (toServerError e)
            { errHTTPCode = 404
            , errReasonPhrase = errReasonPhrase err404
            }
        ErrSignPaymentWithRootKey e@ErrWithRootKeyNoRootKey{} -> (toServerError e)
            { errHTTPCode = 403
            , errReasonPhrase = errReasonPhrase err403
            }
        ErrSignPaymentWithRootKey e@ErrWithRootKeyWrongPassphrase{} -> toServerError e
        ErrSignPaymentWithRootKey e@ErrWithRootKeyWrongMnemonic{} -> toServerError e
        ErrSignPaymentIncorrectTTL e -> toServerError e

instance IsServerError ErrWitnessTx where
    toServerError = \case
        ErrWitnessTxSignTx e -> toServerError e
        ErrWitnessTxNoSuchWallet e -> (toServerError e)
            { errHTTPCode = 404
            , errReasonPhrase = errReasonPhrase err404
            }
        ErrWitnessTxWithRootKey e@ErrWithRootKeyNoRootKey{} -> (toServerError e)
            { errHTTPCode = 403
            , errReasonPhrase = errReasonPhrase err403
            }
        ErrWitnessTxWithRootKey e@ErrWithRootKeyWrongPassphrase{} -> toServerError e
        ErrWitnessTxWithRootKey e@ErrWithRootKeyWrongMnemonic{} -> toServerError e
        ErrWitnessTxIncorrectTTL e -> toServerError e

instance IsServerError ErrSignTx where
    toServerError = \case
        ErrSignTxAddressUnknown txin ->
            apiError err500 KeyNotFoundForAddress $ mconcat
                [ "I couldn't sign the given transaction because I "
                , "could not resolve the address of a transaction input "
                , "that I should be tracking: ", showT txin, "."
                ]
        ErrSignTxUnimplemented ->
            apiError err501 NotImplemented
                "This feature is not yet implemented."

instance IsServerError ErrMkTransaction where
    toServerError = \case
        ErrMkTransactionTxBodyError hint ->
            apiError err500 CreatedInvalidTransaction hint
        ErrMkTransactionInvalidEra _era ->
            apiError err500 CreatedInvalidTransaction $ mconcat
                [ "Whoops, it seems like I just experienced a hard-fork in the "
                , "middle of other tasks. This is a pretty rare situation but "
                , "as a result, I must throw away what I was doing. Please "
                , "retry your request."
                ]
        ErrMkTransactionJoinStakePool e -> toServerError e
        ErrMkTransactionQuitStakePool e -> toServerError e
        ErrMkTransactionNoSuchWallet wid -> toServerError (ErrNoSuchWallet wid)
        ErrMkTransactionIncorrectTTL e -> toServerError e

instance IsServerError ErrConstructTx where
    toServerError = \case
        ErrConstructTxWrongPayload ->
            apiError err403 CreatedInvalidTransaction $ mconcat
            [ "It looks like I've created an empty transaction "
            , "that does not have any payments, withdrawals, delegations, "
            , "metadata nor minting. Include at least one of them."
            ]
        ErrConstructTxBody e -> toServerError e
        ErrConstructTxNoSuchWallet e -> (toServerError e)
            { errHTTPCode = 404
            , errReasonPhrase = errReasonPhrase err404
            }
        ErrConstructTxReadRewardAccount e -> toServerError e
        ErrConstructTxIncorrectTTL e -> toServerError e
        ErrConstructTxMultidelegationNotSupported ->
            apiError err403 CreatedMultidelegationTransaction $ mconcat
            [ "It looks like I've created a transaction "
            , "with multiple delegations, which is not supported at this moment."
            , "Please use at most one delegation action: join, quit or none."
            ]
        ErrConstructTxMultiaccountNotSupported ->
            apiError err403 CreatedMultiaccountTransaction $ mconcat
            [ "It looks like I've created a transaction "
            , "with a delegation, which uses a stake key for the unsupported account."
            , "Please use delegation action engaging '0H' account."
            ]
        ErrConstructTxWrongMintingBurningTemplate ->
            apiError err403 CreatedWrongPolicyScriptTemplate $ mconcat
            [ "It looks like I've created a transaction with a minting/burning "
            , "policy script that either does not pass validation, contains "
            , "more than one cosigner, or has a cosigner that is different "
            , "from cosigner#0."
            ]
        ErrConstructTxAssetNameTooLong ->
            apiError err403 AssetNameTooLong $ mconcat
            [ "Attempted to create a transaction with an asset name that is "
            , "too long. The maximum length is 32 bytes."
            ]
        ErrConstructTxMintOrBurnAssetQuantityOutOfBounds ->
            apiError err403 MintOrBurnAssetQuantityOutOfBounds $ mconcat
            [ "Attempted to mint or burn an asset quantity that is out of "
            , "bounds. The asset quantity must be greater than zero and must "
            , "not exceed 9223372036854775807 (2^63 - 1)."
            ]
        ErrConstructTxWrongValidityBounds ->
            apiError err403 InvalidValidityBounds $ T.unwords
            [ "Attempted to create a transaction with invalid validity bounds."
            , "Please make sure that the 'invalid_before' bound precedes the"
            , "'invalid_hereafter' bound, and that you have not used negative"
            , "time values."
            ]
        ErrConstructTxValidityIntervalNotWithinScriptTimelock ->
            apiError err403 ValidityIntervalNotInsideScriptTimelock $ T.unwords
            [ "Attempted to create a transaction with a validity interval"
            , "that is not a subinterval of an associated script's timelock"
            , "interval."
            ]
        ErrConstructTxSharedWalletPending ->
            apiError err403 SharedWalletPending $ mconcat
            [ "Transaction for a shared wallet should not be tried for "
            , "a pending shared wallet. Make the wallet active before sending "
            , "transaction."
            ]
        ErrConstructTxNotImplemented _ ->
            apiError err501 NotImplemented
                "This feature is not yet implemented."

instance IsServerError ErrGetPolicyId where
    toServerError = \case
        ErrGetPolicyIdReadPolicyPubliKey e -> toServerError e
        ErrGetPolicyIdWrongMintingBurningTemplate ->
            apiError err403 CreatedWrongPolicyScriptTemplate $ mconcat
            [ "It looks like policy id is requested for a "
            , "policy script that either does not pass validation, contains "
            , "more than one cosigner, or has a cosigner that is different "
            , "from cosigner#0."
            ]

instance IsServerError ErrDecodeTx where
    toServerError = \case
        ErrDecodeTxNoSuchWallet e -> (toServerError e)
            { errHTTPCode = 404
            , errReasonPhrase = errReasonPhrase err404
            }

instance IsServerError ErrBalanceTx where
    toServerError = \case
        ErrByronTxNotSupported ->
            apiError err403 BalanceTxByronNotSupported
                "Balancing Byron transactions is not supported."
        ErrBalanceTxUpdateError (ErrExistingKeyWitnesses n) ->
            apiError err403 BalanceTxExistingKeyWitnesses $ mconcat
                [ "The transaction could not be balanced, because it contains "
                , T.pack (show n), " "
                , "existing key-witnesses which would be invalid after "
                , "the transaction body is modified. "
                , "Please sign the transaction after it is balanced instead."
                ]
        ErrBalanceTxSelectAssets err -> toServerError err
        ErrBalanceTxAssignRedeemers err -> toServerError err
        ErrBalanceTxConflictingNetworks ->
            apiError err403 BalanceTxConflictingNetworks $ T.unwords
                [ "There are withdrawals for multiple networks (e.g. both"
                , "mainnet and testnet) in the provided transaction. This"
                , "makes no sense, and I'm confused."
                ]
        ErrBalanceTxExistingCollateral ->
            apiError err403 BalanceTxExistingCollateral
                "I cannot balance transactions with pre-defined collateral."

        ErrBalanceTxExistingTotalCollateral ->
            apiError err403 BalanceTxExistingTotalCollateral $ T.unwords
                [ "I cannot balance transactions"
                , "with pre-defined total collateral."
                ]
        ErrBalanceTxExistingReturnCollateral ->
            apiError err403 BalanceTxExistingReturnCollateral $ T.unwords
                [ "Balancing transactions with pre-defined"
                , "collateral return outputs is not yet supported."
                ]
        ErrBalanceTxZeroAdaOutput ->
            apiError err501 BalanceTxZeroAdaOutput $ T.unwords
                [ "I don't currently support balancing transactions containing"
                , "one or more zero-ada outputs. In the future I might be able"
                , "to increase the values to the minimum allowed ada value."
                ]
        ErrBalanceTxInternalError (ErrFailedBalancing v) ->
            apiError err500 BalanceTxInternalError $ T.unwords
                [ "I have somehow failed to balance the transaction."
                , "The balance is"
                , T.pack (show v)
                ]
        ErrBalanceTxInternalError (ErrUnderestimatedFee c _) ->
            apiError err500 BalanceTxUnderestimatedFee $ T.unwords
                [ "I have somehow underestimated the fee of the transaction by"
                , pretty c
                , "and cannot finish balancing."
                ]
        ErrBalanceTxMaxSizeLimitExceeded ->
            apiError err403 BalanceTxMaxSizeLimitExceeded $ T.unwords
                [ "I was not able to balance the transaction without exceeding"
                , "the maximum transaction size."
                ]


instance IsServerError ErrRemoveTx where
    toServerError = \case
        ErrRemoveTxNoSuchWallet wid -> toServerError wid
        ErrRemoveTxNoSuchTransaction (ErrNoSuchTransaction _wid tid) ->
            apiError err404 NoSuchTransaction $ mconcat
                [ "I couldn't find a transaction with the given id: "
                , toText tid
                ]
        ErrRemoveTxAlreadyInLedger tid ->
            apiError err403 TransactionAlreadyInLedger $ mconcat
                [ "The transaction with id: ", toText tid,
                  " cannot be forgotten as it is already in the ledger."
                ]

instance IsServerError ErrPostTx where
    toServerError = \case
        ErrPostTxValidationError err ->
            apiError err500 CreatedInvalidTransaction $ mconcat
                [ "The submitted transaction was rejected by the local "
                , "node. Here's an error message that may help with "
                , "debugging:\n", err
                ]

instance IsServerError ErrSubmitTransaction where
    toServerError = \case
        ErrSubmitTransactionNoSuchWallet e -> (toServerError e)
            { errHTTPCode = 404
            , errReasonPhrase = errReasonPhrase err404
            }
        ErrSubmitTransactionForeignWallet ->
            apiError err403 ForeignTransaction $ mconcat
                [ "The transaction to be submitted is foreign to the current wallet "
                , "and cannot be sent. Submit a transaction that has either input "
                , "or withdrawal belonging to the wallet."
                ]
        ErrSubmitTransactionPartiallySignedOrNoSignedTx expectedWitsNo foundWitsNo ->
            apiError err403 MissingWitnessesInTransaction $ mconcat
                [ "The transaction has ", toText expectedWitsNo
                , " inputs and ", toText foundWitsNo, " witnesses included."
                , " Submit fully-signed transaction."
                ]
        ErrSubmitTransactionMultidelegationNotSupported ->
            apiError err403 CreatedMultidelegationTransaction $ mconcat
            [ "It looks like the transaction to be sent contains"
            , "multiple delegations, which is not supported at this moment."
            , "Please use at most one delegation action in a submitted transaction: join, quit or none."
            ]

instance IsServerError ErrSubmitTx where
    toServerError = \case
        ErrSubmitTxNetwork e -> toServerError e
        ErrSubmitTxNoSuchWallet e@ErrNoSuchWallet{} -> (toServerError e)
            { errHTTPCode = 404
            , errReasonPhrase = errReasonPhrase err404
            }
        ErrSubmitTxImpossible e -> toServerError e

instance IsServerError ErrUpdatePassphrase where
    toServerError = \case
        ErrUpdatePassphraseNoSuchWallet e -> toServerError e
        ErrUpdatePassphraseWithRootKey e  -> toServerError e

instance IsServerError ErrListTransactions where
    toServerError = \case
        ErrListTransactionsNoSuchWallet e -> toServerError e
        ErrListTransactionsStartTimeLaterThanEndTime e -> toServerError e
        ErrListTransactionsMinWithdrawalWrong ->
            apiError err400 MinWithdrawalWrong
            "The minimum withdrawal value must be at least 1 Lovelace."
        ErrListTransactionsPastHorizonException e -> toServerError e

instance IsServerError ErrStartTimeLaterThanEndTime where
    toServerError err = apiError err400 StartTimeLaterThanEndTime $ mconcat
        [ "The specified start time '"
        , toText $ Iso8601Time $ errStartTime err
        , "' is later than the specified end time '"
        , toText $ Iso8601Time $ errEndTime err
        , "'."
        ]

instance IsServerError PastHorizonException where
    toServerError _ = apiError err503 PastHorizon $ mconcat
        [ "Tried to convert something that is past the horizon"
        , " (due to uncertainty about the next hard fork)."
        , " Wait for the node to finish syncing to the hard fork."
        , " Depending on the blockchain, this process can take an"
        , " unknown amount of time."
        ]

instance IsServerError ErrGetTransaction where
    toServerError = \case
        ErrGetTransactionNoSuchWallet e -> toServerError e
        ErrGetTransactionNoSuchTransaction e -> toServerError e

instance IsServerError ErrNoSuchTransaction where
    toServerError = \case
        ErrNoSuchTransaction _wid tid ->
            apiError err404 NoSuchTransaction $ mconcat
                [ "I couldn't find a transaction with the given id: "
                , toText tid
                ]

instance IsServerError ErrStakePoolDelegation where
    toServerError = \case
        ErrStakePoolDelegationNoSuchWallet e -> toServerError e
        ErrStakePoolJoin e -> toServerError e
        ErrStakePoolQuit e -> toServerError e

instance IsServerError ErrCannotJoin where
    toServerError = \case
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

instance IsServerError ErrCannotQuit where
    toServerError = \case
        ErrNotDelegatingOrAboutTo ->
            apiError err403 NotDelegatingTo $ mconcat
                [ "It seems that you're trying to retire from delegation "
                , "although you're not even delegating, nor won't be in an "
                , "immediate future."
                ]
        ErrNonNullRewards rewards ->
            apiError err403 NonNullRewards $ mconcat
                [ "It seems that you're trying to retire from delegation "
                , "although you've unspoiled rewards in your rewards "
                , "account! Make sure to withdraw your ", pretty rewards
                , " lovelace first."
                ]

instance IsServerError ErrFetchRewards where
    toServerError = \case
        ErrFetchRewardsReadRewardAccount e -> toServerError e

instance IsServerError ErrReadRewardAccount where
    toServerError = \case
        ErrReadRewardAccountNoSuchWallet e -> toServerError e
        ErrReadRewardAccountNotAShelleyWallet ->
            apiError err403 InvalidWalletType $ mconcat
                [ "It is regrettable but you've just attempted an operation "
                , "that is invalid for this type of wallet. Only new 'Shelley' "
                , "wallets can do something with rewards and this one isn't."
                ]

instance IsServerError ErrReadPolicyPublicKey where
    toServerError = \case
        ErrReadPolicyPublicKeyNoSuchWallet e -> toServerError e
        ErrReadPolicyPublicKeyNotAShelleyWallet ->
            apiError err403 InvalidWalletType $ mconcat
                [ "You have attempted an operation that is invalid for this "
                , "type of wallet. Only wallets from the Shelley era onwards "
                , "can have rewards, but this wallet is from an era before "
                , "Shelley."
                ]
        ErrReadPolicyPublicKeyAbsent ->
            apiError err403 MissingPolicyPublicKey $ T.unwords
                [ "It seems the wallet lacks a policy public key. Therefore"
                , "it's not possible to create a minting/burning"
                , "transaction or get a policy id. Please first POST to endpoint"
                , "/wallets/{walletId}/policy-key to set a policy key."
                ]

instance IsServerError ErrWritePolicyPublicKey where
    toServerError = \case
        ErrWritePolicyPublicKeyNoSuchWallet e -> toServerError e
        ErrWritePolicyPublicKeyWithRootKey  e -> toServerError e

instance IsServerError ErrCreateRandomAddress where
    toServerError = \case
        ErrCreateAddrNoSuchWallet e -> toServerError e
        ErrCreateAddrWithRootKey  e -> toServerError e
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

instance IsServerError ErrImportRandomAddress where
    toServerError = \case
        ErrImportAddrNoSuchWallet e -> toServerError e
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

instance IsServerError ErrNotASequentialWallet where
    toServerError = \case
        ErrNotASequentialWallet ->
            apiError err403 InvalidWalletType $ mconcat
                [ "I cannot derive new address for this wallet type. "
                , "Make sure to use a sequential wallet style, like Icarus."
                ]

instance IsServerError ErrWithdrawalNotWorth where
    toServerError = \case
        ErrWithdrawalNotWorth ->
            apiError err403 WithdrawalNotWorth $ mconcat
                [ "I've noticed that you're requesting a withdrawal from an "
                , "account that is either empty or doesn't have a balance big "
                , "enough to deserve being withdrawn. I won't proceed with that "
                , "request."
                ]

instance IsServerError ErrSignMetadataWith where
    toServerError = \case
        ErrSignMetadataWithRootKey e -> toServerError e
        ErrSignMetadataWithNoSuchWallet e -> toServerError e
        ErrSignMetadataWithInvalidIndex e -> toServerError e

instance IsServerError ErrReadAccountPublicKey where
    toServerError = \case
        ErrReadAccountPublicKeyRootKey e -> toServerError e
        ErrReadAccountPublicKeyNoSuchWallet e -> toServerError e
        ErrReadAccountPublicKeyInvalidAccountIndex e -> toServerError e
        ErrReadAccountPublicKeyInvalidPurposeIndex e -> toServerError e

instance IsServerError ErrDerivePublicKey where
    toServerError = \case
        ErrDerivePublicKeyNoSuchWallet e -> toServerError e
        ErrDerivePublicKeyInvalidIndex e -> toServerError e

instance IsServerError ErrAddCosignerKey where
    toServerError = \case
        ErrAddCosignerKeyNoSuchWallet e -> toServerError e
        ErrAddCosignerKey WalletAlreadyActive ->
            apiError err403 SharedWalletNotPending $ mconcat
                [ "It looks like you've tried to add a cosigner key for a "
                , "shared wallet that is active. This can be done only for "
                , "pending shared wallet."
                ]
        ErrAddCosignerKey NoDelegationTemplate ->
            apiError err403 SharedWalletNoDelegationTemplate $ mconcat
                [ "It looks like you've tried to add a cosigner key to "
                , "a shared wallet's delegation template. This cannot be done "
                , "for the wallet that does not define any delegation template."
                ]
        ErrAddCosignerKey (KeyAlreadyPresent cred) ->
            apiError err403 SharedWalletKeyAlreadyExists $ mconcat
                [ "It looks like you've tried to add a cosigner key to a "
                , "shared wallet's ", toText cred, " template that is already "
                , "ascribed to another cosigner. "
                , "Please make sure to assign a different key to each cosigner."
                ]
        ErrAddCosignerKey (NoSuchCosigner cred (Cosigner c)) ->
            apiError err403 SharedWalletNoSuchCosigner $ mconcat
                [ "It looks like you've tried to add a cosigner key to a "
                , "shared wallet's ", toText cred, " template to a "
                , "non-existing cosigner index: ", pretty c,"."
                ]
        ErrAddCosignerKey CannotUpdateSharedWalletKey ->
            apiError err403 SharedWalletCannotUpdateKey $ mconcat
                [ "It looks like you've tried to update the key of a cosigner having "
                , "the shared wallet's account key. Only other cosigner key(s) can be updated."
                ]

instance IsServerError ErrConstructSharedWallet where
    toServerError = \case
        ErrConstructSharedWalletWrongScriptTemplate (ErrScriptTemplateInvalid cred reason) ->
            handleTemplateErr cred (toText reason)
        ErrConstructSharedWalletWrongScriptTemplate (ErrScriptTemplateMissingKey cred reason) ->
            handleTemplateErr cred reason
        ErrConstructSharedWalletInvalidIndex e -> toServerError e
      where
          handleTemplateErr cred reason =
            apiError err403 SharedWalletScriptTemplateInvalid $ mconcat
                [ "It looks like you've tried to create a shared wallet "
                , "with a template script for ", toText cred, " credential that does not "
                , "pass validation. The problem is: ", reason
                ]

instance IsServerError (ErrInvalidDerivationIndex 'Soft level) where
    toServerError = \case
        ErrIndexOutOfBound minIx maxIx _ix ->
            apiError err403 SoftDerivationRequired $ mconcat
                [ "It looks like you've provided a derivation index that is "
                , "out of bound. The index is well-formed, but I require "
                , "indexes valid for soft derivation only. That is, indexes "
                , "between ", pretty minIx, " and ", pretty maxIx, " without a suffix."
                ]

instance IsServerError (SelectionOutputError WalletSelectionContext) where
    toServerError = \case
        SelectionOutputSizeExceedsLimit e ->
            toServerError e
        SelectionOutputTokenQuantityExceedsLimit e ->
            toServerError e

instance IsServerError
    (SelectionOutputSizeExceedsLimitError WalletSelectionContext)
  where
    toServerError e = apiError err403 OutputTokenBundleSizeExceedsLimit $
        mconcat
        [ "One of the outputs you've specified contains too many assets. "
        , "Try splitting these assets across two or more outputs. "
        , "Destination address: "
        , pretty (fst output)
        , ". Asset count: "
        , pretty (TokenMap.size $ snd output ^. #tokens)
        , "."
        ]
      where
        output = view #outputThatExceedsLimit e

instance IsServerError
    (SelectionOutputTokenQuantityExceedsLimitError WalletSelectionContext)
  where
    toServerError e = apiError err403 OutputTokenQuantityExceedsLimit $ mconcat
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

instance IsServerError ErrCreateMigrationPlan where
    toServerError = \case
        ErrCreateMigrationPlanEmpty ->
            apiError err403 NothingToMigrate $ mconcat
                [ "I wasn't able to construct a migration plan. This could be "
                , "because your wallet is empty, or it could be because the "
                , "amount of ada in your wallet is insufficient to pay for "
                , "any of the funds to be migrated. Try adding some ada to "
                , "your wallet before trying again."
                ]
        ErrCreateMigrationPlanNoSuchWallet e -> toServerError e

instance IsServerError ErrSelectAssets where
    toServerError = \case
        ErrSelectAssetsPrepareOutputsError e -> toServerError e
        ErrSelectAssetsNoSuchWallet e -> toServerError e
        ErrSelectAssetsAlreadyWithdrawing tx ->
            apiError err403 AlreadyWithdrawing $ mconcat
                [ "I already know of a pending transaction with withdrawals: "
                , toText (tx ^. #txId)
                , ". Note that when I withdraw rewards, I "
                , "need to withdraw them fully for the Ledger to accept it. "
                , "There's therefore no point creating another conflicting "
                , "transaction; if, for some reason, you really want a new "
                , "transaction, then cancel the previous one first."
                ]
        ErrSelectAssetsSelectionError (SelectionBalanceErrorOf e) ->
            toServerError e
        ErrSelectAssetsSelectionError (SelectionCollateralErrorOf e) ->
            toServerError e
        ErrSelectAssetsSelectionError (SelectionOutputErrorOf e) ->
            toServerError e

instance IsServerError (SelectionBalanceError WalletSelectionContext) where
    toServerError = \case
        BalanceInsufficient e ->
            apiError err403 NotEnoughMoney $ mconcat
                [ "I can't process this payment as there are not "
                , "enough funds available in the wallet. I am "
                , "missing: ", pretty . Flat $ balanceMissing e
                ]
        SelectionLimitReached e ->
            apiError err403 TransactionIsTooBig $ mconcat
                [ "I am not able to finalize the transaction "
                , "because I need to select additional inputs and "
                , "doing so will make the transaction too big. Try "
                , "sending a smaller amount. I had already selected "
                , showT (length $ view #inputsSelected e), " inputs."
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
            apiError err403 CannotCoverFee $ T.unwords
                [ "I am unable to finalize the transaction, as there"
                , "is not enough ada available to pay for the fee and"
                , "also pay for the minimum ada quantities of all"
                , "change outputs. I need approximately"
                , pretty (shortfall e)
                , "ada to proceed. Try increasing your wallet balance"
                , "or sending a smaller amount."
                ]
        EmptyUTxO ->
            apiError err403 NotEnoughMoney $ T.unwords
                [ "Cannot create a transaction because the wallet"
                , "has no UTxO entries. At least one UTxO entry is"
                , "required in order to create a transaction."
                ]

instance IsServerError (SelectionCollateralError WalletSelectionContext) where
    toServerError e =
        apiError err403 InsufficientCollateral $ T.unwords
            [ "I'm unable to create this transaction because the balance"
            , "of pure ada UTxOs in your wallet is insufficient to cover"
            , "the minimum amount of collateral required."
            , "I need an ada amount of at least:"
            , pretty (view #minimumSelectionAmount e)
            , "The largest combination of pure ada UTxOs I could find is:"
            , pretty $ listF $ L.sort $ F.toList $
                view #largestCombinationAvailable e
            , "To fix this, you'll need to add one or more pure ada UTxOs"
            , "to your wallet that can cover the minimum amount required."
            ]

instance IsServerError (ErrInvalidDerivationIndex 'Hardened level) where
    toServerError = \case
        ErrIndexOutOfBound (Index minIx) (Index maxIx) _ix ->
            apiError err403 HardenedDerivationRequired $ mconcat
                [ "It looks like you've provided a derivation index that is "
                , "out of bound. The index is well-formed, but I require "
                , "indexes valid for hardened derivation only. That is, indexes "
                , "between 0H and ", pretty (Index $ maxIx - minIx), "H."
                ]

instance IsServerError ErrUpdateSealedTx where
    toServerError = \case
        ErrExistingKeyWitnesses{} ->
            apiError err400 ExistingKeyWitnesses $ T.unwords
                [ "I cannot proceed with the request because there are key"
                , "witnesses defined in the input transaction and, adjusting"
                , "the transaction body will render witnesses invalid!"
                , "Please make sure to remove all key witnesses from the request."
                ]

instance IsServerError ErrAssignRedeemers where
    toServerError = \case
        ErrAssignRedeemersScriptFailure r failure ->
            apiError err400 RedeemerScriptFailure $ T.unwords
                [ "I was unable to assign execution units to one of your"
                , "redeemers:", pretty r <> ";"
                , "Its execution is failing with the following error:"
                , T.pack failure <> "."
                ]
        ErrAssignRedeemersTargetNotFound r ->
            apiError err400 RedeemerTargetNotFound $ T.unwords
                [ "I was unable to resolve one of your redeemers to the location"
                , "indicated in the request payload:", pretty r <> ";"
                , "Please double-check both your serialised transaction and"
                , "the provided redeemers."
                ]
        ErrAssignRedeemersInvalidData r _ ->
            apiError err400 RedeemerInvalidData $ T.unwords
                [ "It looks like you have provided an invalid 'data' payload"
                , "for one of your redeemers since I am unable to decode it"
                , "into a valid Plutus data:", pretty r <> "."
                ]
        ErrAssignRedeemersTranslationError (TranslationLogicMissingInput inp) ->
             -- Note that although this error is thrown from
             -- '_assignScriptRedeemers', it's more related to balanceTransaction
             -- in general than to assigning redeemers. Hence we don't mention
             -- redeemers in the message.
             apiError err400 UnresolvedInputs $ T.unwords
                 [ "The transaction I was given contains inputs I don't know"
                 , "about. Please ensure all foreign inputs are specified as "
                 , "part of the API request. The unknown input is:\n\n"
                 , T.pack $ show inp
                 ]
        ErrAssignRedeemersTranslationError (TimeTranslationPastHorizon t) ->
            -- We differentiate this from @TranslationError@ for partial API
            -- backwards compatibility.
            apiError err400 PastHorizon $ T.unwords
                [ "The transaction's validity interval is past the horizon"
                , "of safe slot-to-time conversions."
                , "This may happen when I know about a future era"
                , "which has not yet been confirmed on-chain. Try setting the"
                , "bounds of the validity interval to be earlier.\n\n"
                , "Here are the full details: " <> t
                ]
        ErrAssignRedeemersTranslationError e ->
            apiError err400 TranslationError $ T.unwords
                [ "The transaction I was given contains bits that cannot be"
                , "translated in the current era. The following is wrong:\n\n"
                , showT e
                ]

instance IsServerError (Request, ServerError) where
    toServerError (req, err@(ServerError code _ body _))
      | not (isJSON body) = case code of
        400 | "Failed reading" `BS.isInfixOf` BL.toStrict body ->
            apiError err BadRequest $ mconcat
                [ "I couldn't understand the content of your message. If your "
                , "message is intended to be in JSON format, please check that "
                , "the JSON is valid."
                ]
        400 -> apiError err BadRequest (utf8 body)
        404 -> apiError err NotFound $ mconcat
            [ "I couldn't find the requested endpoint. If the endpoint "
            , "contains path parameters, please ensure they are well-formed, "
            , "otherwise I won't be able to route them correctly."
            ]
        405 -> apiError err MethodNotAllowed $ mconcat
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
            in apiError err NotAcceptable $ mconcat
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
            in apiError err UnsupportedMediaType $ mconcat
            [ "I'm really sorry but I only understand '", cType, "'. I need you "
            , "to tell me what language you're speaking in order for me to "
            , "understand your message. Please double-check your 'Content-Type' "
            , "request header and make sure it's set to '", cType, "'."
            ]
        501 -> apiError err NotImplemented
            "I'm really sorry but this endpoint is not implemented yet."
        _ -> apiError err UnexpectedError $ mconcat
            [ "It looks like something unexpected went wrong. Unfortunately I "
            , "don't yet know how to handle this type of situation. Here's "
            , "some information about what happened: ", utf8 body
            ]
      | otherwise = err
      where
        utf8 = T.replace "\"" "'" . T.decodeUtf8 . BL.toStrict
        isJSON = isJust . Aeson.decode @Aeson.Value

{-------------------------------------------------------------------------------
                               Logging
-------------------------------------------------------------------------------}

-- | The type of log messages coming from the server 'ApiLayer', which may or
-- may not be associated with a particular worker thread.
data WalletEngineLog
    = MsgWalletWorker (WorkerLog WalletId W.WalletWorkerLog)
    | MsgSubmitSealedTx TxSubmitLog
    deriving (Show, Eq)

instance ToText WalletEngineLog where
    toText = \case
        MsgWalletWorker msg -> toText msg
        MsgSubmitSealedTx msg -> toText msg

instance HasPrivacyAnnotation WalletEngineLog where
    getPrivacyAnnotation = \case
        MsgWalletWorker msg -> getPrivacyAnnotation msg
        MsgSubmitSealedTx msg -> getPrivacyAnnotation msg

instance HasSeverityAnnotation WalletEngineLog where
    getSeverityAnnotation = \case
        MsgWalletWorker msg -> getSeverityAnnotation msg
        MsgSubmitSealedTx msg -> getSeverityAnnotation msg
