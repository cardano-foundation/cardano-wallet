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

module Cardano.Wallet.Api.Http.Shelley.Server
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
    , decodeSharedTransaction
    , getBlocksLatestHeader
    , submitSharedTransaction

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
    )
    where

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
    ( NetworkId, SerialiseAsCBOR (..), toNetworkMagic, unNetworkMagic )
import Cardano.Api.Extra
    ( inAnyCardanoEra )
import Cardano.Api.Shelley
    ( ShelleyLedgerEra )
import Cardano.BM.Tracing
    ( HasPrivacyAnnotation (..), HasSeverityAnnotation (..) )
import Cardano.Mnemonic
    ( SomeMnemonic )
import Cardano.Pool.Types
    ( PoolId )
import Cardano.Wallet
    ( ErrBalanceTx (..)
    , ErrConstructSharedWallet (..)
    , ErrConstructTx (..)
    , ErrCreateMigrationPlan (..)
    , ErrGetPolicyId (..)
    , ErrNoSuchWallet (..)
    , ErrReadRewardAccount (..)
    , ErrSelectAssets (..)
    , ErrSignPayment (..)
    , ErrSubmitTransaction (..)
    , ErrUpdatePassphrase (..)
    , ErrWalletAlreadyExists (..)
    , ErrWalletNotResponding (..)
    , ErrWithRootKey (..)
    , ErrWithdrawalNotWorth (..)
    , ErrWitnessTx (..)
    , FeeEstimation (..)
    , HasNetworkLayer
    , TxSubmitLog
    , genesisData
    , manageRewardBalance
    , networkLayer
    )
import Cardano.Wallet.Address.Book
    ( AddressBookIso )
import Cardano.Wallet.Address.HasDelegation
    ( HasDelegation (..) )
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
import Cardano.Wallet.Api.Http.Server.Error
    ( IsServerError (..), apiError, liftE, liftHandler )
import Cardano.Wallet.Api.Http.Server.Handlers.Certificates
    ( getApiAnyCertificates )
import Cardano.Wallet.Api.Http.Server.Handlers.MintBurn
    ( convertApiAssetMintBurn, getTxApiAssetMintBurn )
import Cardano.Wallet.Api.Http.Server.Handlers.TxCBOR
    ( parseTxCBOR )
import Cardano.Wallet.Api.Http.Server.Tls
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
    , ApiPoolSpecifier (..)
    , ApiPostAccountKeyDataWithPurpose (..)
    , ApiPostPolicyIdData
    , ApiPostPolicyKeyData (..)
    , ApiPostRandomAddressData (..)
    , ApiPutAddressesData (..)
    , ApiRedeemer (..)
    , ApiScriptTemplateEntry (..)
    , ApiSealedTxEncoding (..)
    , ApiSelectCoinsPayments
    , ApiSelfWithdrawalPostData (..)
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
    , ApiWalletMode (..)
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
    , getApiMnemonicT
    , toApiAsset
    , toApiEra
    , toApiNetworkParameters
    , toApiUtxoStatistics
    )
import Cardano.Wallet.Api.Types.BlockHeader
    ( ApiBlockHeader, mkApiBlockHeader )
import Cardano.Wallet.Api.Types.Certificate
    ( mkApiAnyCertificate )
import Cardano.Wallet.Api.Types.Error
    ( ApiErrorInfo (..) )
import Cardano.Wallet.Api.Types.Key
    ( computeKeyPayload )
import Cardano.Wallet.Api.Types.MintBurn
    ( noApiAsset )
import Cardano.Wallet.Api.Types.SchemaMetadata
    ( TxMetadataSchema (..), TxMetadataWithSchema (TxMetadataWithSchema) )
import Cardano.Wallet.CoinSelection
    ( SelectionOf (..), SelectionStrategy (..), selectionDelta )
import Cardano.Wallet.Compat
    ( (^?) )
import Cardano.Wallet.DB
    ( DBFactory (..) )
import Cardano.Wallet.Network
    ( NetworkLayer (..), fetchRewardAccountBalances, timeInterpreter )
import Cardano.Wallet.Primitive.AddressDerivation
    ( BoundedAddressLength (..)
    , DelegationAddress (..)
    , Depth (..)
    , DerivationIndex (..)
    , DerivationType (..)
    , HardDerivation (..)
    , Index (..)
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
    ( SyncProgress (..) )
import Cardano.Wallet.Primitive.Types
    ( Block
    , BlockHeader (..)
    , NetworkParameters (..)
    , PoolLifeCycleStatus
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
    ( TokenBundle (..) )
import Cardano.Wallet.Primitive.Types.TokenMap
    ( AssetId (..), fromFlatList )
import Cardano.Wallet.Primitive.Types.TokenPolicy
    ( TokenName (..), TokenPolicyId (..), nullTokenName, tokenNameMaxLength )
import Cardano.Wallet.Primitive.Types.TokenQuantity
    ( TokenQuantity (..) )
import Cardano.Wallet.Primitive.Types.Tx
    ( TransactionInfo
    , Tx (..)
    , TxCBOR
    , TxChange (..)
    , TxIn (..)
    , TxOut (..)
    , TxStatus (..)
    , UnsignedTx (..)
    , cardanoTxIdeallyNoLaterThan
    , getSealedTxWitnesses
    , txOutCoin
    )
import Cardano.Wallet.Primitive.Types.Tx.Constraints
    ( txMintBurnMaxTokenQuantity )
import Cardano.Wallet.Registry
    ( HasWorkerCtx (..)
    , MkWorker (..)
    , WorkerLog (..)
    , defaultWorkerAfter
    , workerResource
    )
import Cardano.Wallet.Shelley.Compatibility.Ledger
    ( toLedger )
import Cardano.Wallet.Shelley.Pools
    ( EpochInfo (..), toEpochInfo )
import Cardano.Wallet.TokenMetadata
    ( TokenMetadataClient, fillMetadata )
import Cardano.Wallet.Transaction
    ( DelegationAction (..)
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
    ( forM, forever, join, void, when, (<=<), (>=>) )
import Control.Monad.IO.Class
    ( MonadIO, liftIO )
import Control.Monad.Trans.Except
    ( ExceptT (..), mapExceptT, runExceptT, throwE, withExceptT )
import Control.Monad.Trans.Maybe
    ( MaybeT (..), exceptToMaybeT )
import Control.Tracer
    ( Tracer, contramap )
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
    ( isInfixOf, sortOn, (\\) )
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
    ( pretty )
import GHC.Generics
    ( Generic )
import GHC.Stack
    ( HasCallStack )
import Network.Ntp
    ( NtpClient, getNtpStatus )
import Network.Socket
    ( Socket, close )
import Network.Wai.Handler.Warp
    ( Port )
import Network.Wai.Middleware.Logging
    ( ApiLog (..), newApiLoggerSettings, obfuscateKeys, withApiLogger )
import Network.Wai.Middleware.ServerError
    ( handleRawError )
import Numeric.Natural
    ( Natural )
import Servant
    ( Application, NoContent (..), err400, err404, err500, serve )
import Servant.Server
    ( Handler (..), runHandler )
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
import qualified Cardano.Wallet.Primitive.AddressDiscovery.Sequential as Seq
import qualified Cardano.Wallet.Primitive.AddressDiscovery.Shared as Shared
import qualified Cardano.Wallet.Primitive.Types as W
import qualified Cardano.Wallet.Primitive.Types.Coin as Coin
import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle
import qualified Cardano.Wallet.Primitive.Types.Tx as W
import qualified Cardano.Wallet.Primitive.Types.UTxO as UTxO
import qualified Cardano.Wallet.Primitive.Types.UTxOIndex.Internal as UTxOIndex
import qualified Cardano.Wallet.Primitive.Types.UTxOSelection as UTxOSelection
import qualified Cardano.Wallet.Registry as Registry
import qualified Cardano.Wallet.Write.Tx as WriteTx
import qualified Control.Concurrent.Concierge as Concierge
import qualified Data.ByteString as BS
import qualified Data.Foldable as F
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Network.Ntp as Ntp
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
        , ctx ~ ApiLayer s k 'CredFromKeyK
        , Seq.SupportsDiscovery n k
        , WalletKey k
        , HasDBFactory s k ctx
        , HasWorkerRegistry s k ctx
        , IsOurs s RewardAccount
        , Typeable s
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
        , ctx ~ ApiLayer s k 'CredFromKeyK
        , WalletKey k
        , Seq.SupportsDiscovery n k
        , HasDBFactory s k ctx
        , HasWorkerRegistry s k ctx
        , IsOurs s RewardAccount
        , MaybeLight s
        , Typeable s
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
        , ctx ~ ApiLayer s k 'CredFromKeyK
        , WalletKey k
        , Seq.SupportsDiscovery n k
        , HasWorkerRegistry s k ctx
        , IsOurs s RewardAccount
        , MaybeLight s
        , (k == SharedKey) ~ 'False
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
        ( ctx ~ ApiLayer s k 'CredFromKeyK
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
            { available = Coin.toQuantity (available ^. #coin)
            , total = Coin.toQuantity (total ^. #coin)
            , reward = Coin.toQuantity reward
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
    :: W.WalletDelegation -> TimeInterpreter IO -> IO ApiWalletDelegation
toApiWalletDelegation W.WalletDelegation{active,next} ti = do
    apiNext <- forM next $ \W.WalletDelegationNext{status,changesAt} -> do
        info <- interpretQuery ti $ toEpochInfo changesAt
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
        , k ~ SharedKey
        , ctx ~ ApiLayer s k 'CredFromScriptK
        , Shared.SupportsDiscovery n k
        , WalletKey k
        , HasDBFactory s k ctx
        , HasWorkerRegistry s k ctx
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
        , k ~ SharedKey
        , ctx ~ ApiLayer s k 'CredFromScriptK
        , Shared.SupportsDiscovery n k
        , WalletKey k
        , HasDBFactory s k ctx
        , HasWorkerRegistry s k ctx
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
        , k ~ SharedKey
        , ctx ~ ApiLayer s k 'CredFromScriptK
        , Shared.SupportsDiscovery n k
        , WalletKey k
        , HasDBFactory s k ctx
        , HasWorkerRegistry s k ctx
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
        ( ctx ~ ApiLayer s k 'CredFromScriptK
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
                { available = Coin.toQuantity (available ^. #coin)
                , total = Coin.toQuantity (total ^. #coin)
                , reward = Coin.toQuantity reward
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
        , k ~ SharedKey
        , ctx ~ ApiLayer s k 'CredFromScriptK
        , Shared.SupportsDiscovery n k
        , WalletKey k
        , HasDBFactory s k ctx
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
        ( ctx ~ ApiLayer s k 'CredFromKeyK
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
            { available = Coin.toQuantity $ TokenBundle.getCoin available
            , total = Coin.toQuantity $ TokenBundle.getCoin total
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
        ( ctx ~ ApiLayer s k 'CredFromKeyK
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
        ( ctx ~ ApiLayer s k 'CredFromKeyK
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
        ( ctx ~ ApiLayer s k 'CredFromKeyK
        , s ~ SeqState n k
        , k ~ IcarusKey
        , HasWorkerRegistry s k ctx
        , PaymentAddress n IcarusKey 'CredFromKeyK
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
        ( ctx ~ ApiLayer s k 'CredFromKeyK
        , s ~ SeqState n k
        , k ~ IcarusKey
        , HasWorkerRegistry s k ctx
        , PaymentAddress n IcarusKey 'CredFromKeyK
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
        ( ctx ~ ApiLayer s k 'CredFromKeyK
        , s ~ SeqState n k
        , k ~ IcarusKey
        , HasWorkerRegistry s k ctx
        , PaymentAddress n IcarusKey 'CredFromKeyK
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
        ( byron ~ ApiLayer (RndState n) ByronKey 'CredFromKeyK
        , icarus ~ ApiLayer (SeqState n IcarusKey) IcarusKey 'CredFromKeyK
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
        ( byron ~ ApiLayer (RndState n) ByronKey 'CredFromKeyK
        , icarus ~ ApiLayer (SeqState n IcarusKey) IcarusKey 'CredFromKeyK
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
    :: forall ctx s k ktype.
        ( ctx ~ ApiLayer s k ktype
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
    :: forall ctx s k ktype apiWallet.
        ( ctx ~ ApiLayer s k ktype
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
    :: forall ctx s k ktype apiWallet.
        ( ctx ~ ApiLayer s k ktype
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
    :: forall ctx s k ktype apiWallet.
        ( ctx ~ ApiLayer s k ktype
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
    :: forall ctx s k ktype.
        ( WalletKey k
        , ctx ~ ApiLayer s k ktype
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
        withWrk :: (WorkerCtx (ApiLayer s k ktype) -> Handler a) -> Handler a
        withWrk = withWorkerCtx ctx wid liftE liftE

putByronWalletPassphrase
    :: forall ctx s k.
        ( WalletKey k
        , ctx ~ ApiLayer s k 'CredFromKeyK
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
    :: forall ctx s k ktype.
        ( ctx ~ ApiLayer s k ktype
        )
    => ctx
    -> ApiT WalletId
    -> Handler ApiUtxoStatistics
getUTxOsStatistics ctx (ApiT wid) = do
    stats <- withWorkerCtx ctx wid liftE liftE $ \wrk -> liftHandler $
        W.listUtxoStatistics wrk wid
    return $ toApiUtxoStatistics stats

getWalletUtxoSnapshot
    :: forall ctx s k ktype. (ctx ~ ApiLayer s k ktype)
    => ctx
    -> ApiT WalletId
    -> Handler ApiWalletUtxoSnapshot
getWalletUtxoSnapshot ctx (ApiT wid) = do
    entries <- withWorkerCtx ctx wid liftE liftE $ \wrk -> liftHandler $
        W.getWalletUtxoSnapshot @_ @_ @_ @ktype wrk wid
    return $ mkApiWalletUtxoSnapshot entries
  where
    mkApiWalletUtxoSnapshot :: [(TokenBundle, Coin)] -> ApiWalletUtxoSnapshot
    mkApiWalletUtxoSnapshot bundleMinCoins = ApiWalletUtxoSnapshot
        { entries = mkApiWalletUtxoSnapshotEntry <$> bundleMinCoins }

    mkApiWalletUtxoSnapshotEntry
        :: (TokenBundle, Coin) -> ApiWalletUtxoSnapshotEntry
    mkApiWalletUtxoSnapshotEntry (bundle, minCoin) = ApiWalletUtxoSnapshotEntry
        { ada = Coin.toQuantity $ view #coin bundle
        , adaMinimum = Coin.toQuantity minCoin
        , assets = ApiT $ view #tokens bundle
        }

{-------------------------------------------------------------------------------
                                  Coin Selections
-------------------------------------------------------------------------------}

selectCoins
    :: forall ctx s k n.
        ( ctx ~ ApiLayer s k 'CredFromKeyK
        , SoftDerivation k
        , IsOurs s Address
        , GenChange s
        , Bounded (Index (AddressIndexDerivationType k) (AddressCredential k))
        , Typeable n
        , Typeable s
        , WalletKey k
        , BoundedAddressLength k
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
            W.selectAssets @_ @_ @s @k @'CredFromKeyK wrk era pp selectAssetsParams transform
        pure $ mkApiCoinSelection [] [] Nothing md utx

selectCoinsForJoin
    :: forall ctx s n k.
        ( s ~ SeqState n k
        , ctx ~ ApiLayer s k 'CredFromKeyK
        , DelegationAddress n k 'CredFromKeyK
        , Seq.SupportsDiscovery n k
        , Typeable s
        , BoundedAddressLength k
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
            $ W.selectAssets @_ @_ @s @k @'CredFromKeyK wrk era pp selectAssetsParams transform
        (_, _, path) <- liftHandler
            $ W.readRewardAccount @_ @s @k @n wrk wid

        let deposits = maybeToList deposit

        pure $ mkApiCoinSelection deposits [] (Just (action, path)) Nothing utx

selectCoinsForQuit
    :: forall ctx s n k.
        ( s ~ SeqState n k
        , ctx ~ ApiLayer s k 'CredFromKeyK
        , DelegationAddress n k 'CredFromKeyK
        , Seq.SupportsDiscovery n k
        , Bounded (Index (AddressIndexDerivationType k) (AddressCredential k))
        , Typeable s
        , WalletKey k
        , BoundedAddressLength k
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
            $ W.selectAssets @_ @_ @s @k @'CredFromKeyK wrk era pp selectAssetsParams transform
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
        ( ctx ~ ApiLayer s k 'CredFromKeyK
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
    :: forall s k ktype. IsOurs s Address =>
    ApiLayer s k ktype -> ApiT WalletId -> Handler (Set AssetId)
listAssetsBase ctx (ApiT wallet) =
    withWorkerCtx ctx wallet liftE liftE $ \wctx ->
        liftHandler $ W.listAssets wctx wallet

-- | Look up a single asset and its metadata.
--
-- NOTE: This is slightly inefficient because it greps through the transaction
-- history to check if the asset is associated with this wallet.
getAsset
    :: forall ctx s k ktype.
        ( ctx ~ ApiLayer s k ktype
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
    :: forall ctx s k ktype.
        ( ctx ~ ApiLayer s k ktype
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
        , ctx ~ ApiLayer s k 'CredFromKeyK
        , PaymentAddress n ByronKey 'CredFromKeyK
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
        , ctx ~ ApiLayer s k 'CredFromKeyK
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
        , ctx ~ ApiLayer s k 'CredFromKeyK
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
    :: forall ctx s k ktype n.
        ( ctx ~ ApiLayer s k ktype
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
    :: forall ctx s k ktype.
        ( ctx ~ ApiLayer s k ktype
        , Bounded (Index (AddressIndexDerivationType k) (AddressCredential k))
        , WalletKey k
        , IsOwned s k ktype
        , HardDerivation k
        )
    => ctx
    -> ApiT WalletId
    -> ApiSignTransactionPostData
    -> Handler ApiSerialisedTransaction
signTransaction ctx (ApiT wid) body = do
    let pwd = coerce $ body ^. #passphrase . #getApiT

    sealedTx' <- withWorkerCtx ctx wid liftE liftE $ \wrk -> liftHandler $ do
        let
            db = wrk ^. W.dbLayer @IO @s @k
            tl = wrk ^. W.transactionLayer @k @ktype
            nl = wrk ^. W.networkLayer
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
                        -> Maybe (k ktype XPrv, Passphrase "encryption")
                    keyLookup = isOwned (getState cp) (rootK, pwdP)

                era <- liftIO $ NW.currentNodeEra nl
                let sealedTx = body ^. #transaction . #getApiT
                pure $ W.signTransaction tl era keyLookup (rootK, pwdP) utxo sealedTx

    -- TODO: The body+witnesses seem redundant with the sealedTx already. What's
    -- the use-case for having them provided separately? In the end, the client
    -- should be able to decouple them if they need to.
    case body ^. #encoding of
        Just HexEncoded -> pure $ ApiSerialisedTransaction (ApiT sealedTx') HexEncoded
        _ -> pure $ ApiSerialisedTransaction (ApiT sealedTx') Base64Encoded

postTransactionOld
    :: forall ctx s k n.
        ( ctx ~ ApiLayer s k 'CredFromKeyK
        , GenChange s
        , HardDerivation k
        , HasNetworkLayer IO ctx
        , IsOwned s k 'CredFromKeyK
        , Bounded (Index (AddressIndexDerivationType k) (AddressCredential k))
        , Typeable n
        , Typeable s
        , WalletKey k
        , AddressBookIso s
        , BoundedAddressLength k
        , HasDelegation s)
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

    withWorkerCtx ctx wid liftE liftE $ \wrk -> do
        (sel, tx, txMeta, txTime, pp) <- atomicallyWithHandler
            (ctx ^. walletLocks) (PostTransactionOld wid) $ do
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
                $ W.selectAssets @_ @_ @s @k @'CredFromKeyK
                    wrk era pp selectAssetsParams
                $ const Prelude.id
            sel' <- liftHandler
                $ W.assignChangeAddressesAndUpdateDb wrk wid genChange sel
            (tx, txMeta, txTime, sealedTx) <- liftHandler
                $ W.buildAndSignTransaction @_ @s @k
                    wrk wid era mkRwdAcct pwd txCtx sel'
            liftHandler
                $ W.submitTx @_ @s @k wrk wid (tx, txMeta, sealedTx)
            pure (sel, tx, txMeta, txTime, pp)
        mkApiTransaction
            (timeInterpreter $ ctx ^. networkLayer)
            wrk wid
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
                , txCBOR  = tx ^. #txCBOR
                }
  where
    ti :: TimeInterpreter (ExceptT PastHorizonException IO)
    ti = timeInterpreter (ctx ^. networkLayer)

deleteTransaction
    :: forall ctx s k. ctx ~ ApiLayer s k 'CredFromKeyK
    => ctx
    -> ApiT WalletId
    -> ApiTxId
    -> Handler NoContent
deleteTransaction ctx (ApiT wid) (ApiTxId (ApiT (tid))) = do
    withWorkerCtx ctx wid liftE liftE $ \wrk -> liftHandler $
        W.forgetTx wrk wid tid
    return NoContent

listTransactions
    :: forall ctx s k n.
        ( Typeable s
        , Typeable n
        , ctx ~ ApiLayer s k 'CredFromKeyK
        , HasDelegation s
        )
    => ctx
    -> ApiT WalletId
    -> Maybe MinWithdrawal
    -> Maybe Iso8601Time
    -> Maybe Iso8601Time
    -> Maybe (ApiT SortOrder)
    -> TxMetadataSchema
    -> Handler [ApiTransaction n]
listTransactions
    ctx (ApiT wid) mMinWithdrawal mStart mEnd mOrder metadataSchema =
        withWorkerCtx ctx wid liftE liftE $ \wrk -> do
            txs <- liftHandler $
                W.listTransactions @_ @_ @_ wrk wid
                (Coin . fromIntegral . getMinWithdrawal <$> mMinWithdrawal)
                (getIso8601Time <$> mStart)
                (getIso8601Time <$> mEnd)
                (maybe defaultSortOrder getApiT mOrder)
            depo <- liftIO $ W.stakeKeyDeposit <$>
                NW.currentProtocolParameters (wrk ^. networkLayer)
            forM txs $ \tx ->
                mkApiTransactionFromInfo
                    (timeInterpreter (ctx ^. networkLayer))
                    wrk wid
                    depo
                    tx
                    metadataSchema
  where
    defaultSortOrder :: SortOrder
    defaultSortOrder = Descending

getTransaction
    :: forall ctx s k n.
        ( Typeable s
        , Typeable n
        , ctx ~ ApiLayer s k 'CredFromKeyK
        , HasDelegation s
        )
    => ctx
    -> ApiT WalletId
    -> ApiTxId
    -> TxMetadataSchema
    -> Handler (ApiTransaction n)
getTransaction ctx (ApiT wid) (ApiTxId (ApiT (tid))) metadataSchema =
    withWorkerCtx ctx wid liftE liftE $ \wrk -> do
        tx <- liftHandler $ W.getTransaction wrk wid tid
        depo <- liftIO $ W.stakeKeyDeposit <$>
            NW.currentProtocolParameters (wrk ^. networkLayer)
        mkApiTransactionFromInfo
                (timeInterpreter (ctx ^. networkLayer)) wrk wid depo tx
                metadataSchema

-- Populate an API transaction record with 'TransactionInfo' from the wallet
-- layer.
mkApiTransactionFromInfo
    :: (Typeable s, Typeable n, HasDelegation s)
    => TimeInterpreter (ExceptT PastHorizonException IO)
    -> W.WalletLayer IO s k 'CredFromKeyK
    -> WalletId
    -> Coin
    -> TransactionInfo
    -> TxMetadataSchema
    -> Handler (ApiTransaction n)
mkApiTransactionFromInfo ti wrk wid deposit info metadataSchema = do
    apiTx <- mkApiTransaction
        ti wrk wid status
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
            , txCBOR = info ^. #txInfoCBOR
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
        ( ctx ~ ApiLayer s k 'CredFromKeyK
        , Bounded (Index (AddressIndexDerivationType k) (AddressCredential k))
        , HardDerivation k
        , Typeable n
        , Typeable s
        , WalletKey k
        , BoundedAddressLength k
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
                W.selectAssets @_ @_ @s @k @'CredFromKeyK
                  wrk era pp selectAssetsParams getFee
        minCoins <- liftIO (W.calcMinimumCoinValues @_ @k @'CredFromKeyK wrk era (F.toList outs))
        liftHandler $ mkApiFee Nothing minCoins <$> W.estimateFee runSelection

constructTransaction
    :: forall ctx s k n.
        ( ctx ~ ApiLayer s k 'CredFromKeyK
        , Bounded (Index (AddressIndexDerivationType k) (AddressCredential k))
        , GenChange s
        , HardDerivation k
        , HasNetworkLayer IO ctx
        , IsOurs s Address
        , Typeable n
        , Typeable s
        , WalletKey k
        , BoundedAddressLength k
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
        decodeValidityInterval ti (body ^. #validityInterval)

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
    let apiwithdrawal = case body ^. #withdrawal of
            Just SelfWithdraw -> Just SelfWithdrawal
            _ -> Nothing
    (wdrl, _) <-
        mkRewardAccountBuilder @_ @s @_ @n ctx wid apiwithdrawal

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
                W.selectAssets @_ @_ @s @k @'CredFromKeyK
                  wrk era pp selectAssetsParams transform
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
            { transaction = case body ^. #encoding of
                    Just HexEncoded -> ApiSerialisedTransaction (ApiT tx) HexEncoded
                    _ -> ApiSerialisedTransaction (ApiT tx) Base64Encoded
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

decodeValidityInterval
    :: TimeInterpreter (ExceptT PastHorizonException IO)
    -> Maybe ApiValidityInterval
    -> Handler (SlotNo, SlotNo, Bool)
decodeValidityInterval ti validityInterval = do
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

    pure (before, hereafter, isThereNegativeTime)

-- TO-DO delegations/withdrawals
-- TO-DO minting/burning
constructSharedTransaction
    :: forall ctx s k n.
        ( k ~ SharedKey
        , s ~ SharedState n k
        , ctx ~ ApiLayer s k 'CredFromScriptK
        , GenChange s
        , HasNetworkLayer IO ctx
        , IsOurs s Address
        , BoundedAddressLength k
        , Typeable n
        )
    => ctx
    -> ArgGenChange s
    -> IO (Set PoolId)
    -> (PoolId -> IO PoolLifeCycleStatus)
    -> ApiT WalletId
    -> ApiConstructTransactionData n
    -> Handler (ApiConstructTransaction n)
constructSharedTransaction
    ctx genChange _knownPools _getPoolStatus (ApiT wid) body = do
    let isNoPayload =
            isNothing (body ^. #payments) &&
            isNothing (body ^. #withdrawal) &&
            isNothing (body ^. #metadata) &&
            isNothing (body ^. #mintBurn) &&
            isNothing (body ^. #delegations)
    when isNoPayload $
        liftHandler $ throwE ErrConstructTxWrongPayload

    let md = body ^? #metadata . traverse . #txMetadataWithSchema_metadata

    (before, hereafter, isThereNegativeTime) <-
        decodeValidityInterval ti (body ^. #validityInterval)

    when (hereafter < before || isThereNegativeTime) $
        liftHandler $ throwE ErrConstructTxWrongValidityBounds

    withWorkerCtx ctx wid liftE liftE $ \wrk -> do
        (cp, _, _) <- liftHandler $ withExceptT ErrConstructTxNoSuchWallet $
            W.readWallet @_ @s @k wrk wid

        let txCtx = defaultTransactionCtx
                { txWithdrawal = NoWithdrawal
                , txMetadata = md
                , txValidityInterval = (Just before, hereafter)
                , txDelegationAction = Nothing
                , txPaymentCredentialScriptTemplate =
                        Just (Shared.paymentTemplate $ getState cp)
                }

        let transform s sel =
                ( W.assignChangeAddresses genChange sel s
                    & uncurry (W.selectionToUnsignedTx (txWithdrawal txCtx))
                , sel
                , selectionDelta TokenBundle.getCoin sel
                )

        case Shared.ready (getState cp) of
            Shared.Pending ->
                liftHandler $ throwE ErrConstructTxSharedWalletPending
            Shared.Active _ -> do
                pp <- liftIO $ NW.currentProtocolParameters (wrk ^. networkLayer)
                era <- liftIO $ NW.currentNodeEra (wrk ^. networkLayer)

                (utxoAvailable, wallet, pendingTxs) <-
                    liftHandler $ W.readWalletUTxOIndex @_ @s @k wrk wid

                let runSelection outs = W.selectAssets @_ @_ @s @k @'CredFromScriptK
                        wrk era pp selectAssetsParams transform
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
                        Just (ApiPaymentAddresses content) ->
                            pure $ F.toList (addressAmountToTxOut <$> content)
                        _ ->
                            pure []

                    (sel', utx, fee') <- liftHandler $ runSelection outs
                    sel <- liftHandler $
                        W.assignChangeAddressesWithoutDbUpdate wrk wid genChange utx
                    (FeeEstimation estMin _) <- liftHandler $ W.estimateFee (pure fee')
                    pure (sel, sel', estMin)

                tx <- liftHandler
                    $ W.constructSharedTransaction @_ @s @k @n wrk wid era txCtx sel

                pure $ ApiConstructTransaction
                    { transaction = case body ^. #encoding of
                            Just HexEncoded -> ApiSerialisedTransaction (ApiT tx) HexEncoded
                            _ -> ApiSerialisedTransaction (ApiT tx) Base64Encoded
                    , coinSelection = mkApiCoinSelection [] [] Nothing md sel'
                    , fee = Quantity $ fromIntegral fee
                    }
  where
    ti :: TimeInterpreter (ExceptT PastHorizonException IO)
    ti = timeInterpreter (ctx ^. networkLayer)

decodeSharedTransaction
    :: forall ctx s k (n :: NetworkDiscriminant).
        ( ctx ~ ApiLayer s k 'CredFromScriptK
        , IsOurs s Address
        , HasNetworkLayer IO ctx
        )
    => ctx
    -> ApiT WalletId
    -> ApiSerialisedTransaction
    -> Handler (ApiDecodedTransaction n)
decodeSharedTransaction ctx (ApiT wid) (ApiSerialisedTransaction (ApiT sealed) _) = do
    era <- liftIO $ NW.currentNodeEra nl
    let (decodedTx, _toMint, _toBurn, _allCerts, interval, witsCount) =
            decodeTx tl era sealed
    let (Tx { txId
            , fee
            , resolvedInputs
            , resolvedCollateralInputs
            , outputs
            , metadata
            , scriptValidity
            }) = decodedTx
    (txinsOutsPaths, collateralInsOutsPaths, outsPath)
        <- withWorkerCtx ctx wid liftE liftE $ \wrk -> do
        inputPaths <-
            liftHandler $ W.lookupTxIns @_ @s @k wrk wid $
            fst <$> resolvedInputs
        collateralInputPaths <-
            liftHandler $ W.lookupTxIns @_ @s @k wrk wid $
            fst <$> resolvedCollateralInputs
        outputPaths <-
            liftHandler $ W.lookupTxOuts @_ @s @k wrk wid outputs
        pure
            ( inputPaths
            , collateralInputPaths
            , outputPaths
            )
    pure $ ApiDecodedTransaction
        { id = ApiT txId
        , fee = maybe (Quantity 0) (Quantity . fromIntegral . unCoin) fee
        , inputs = map toInp txinsOutsPaths
        , outputs = map toOut outsPath
        , collateral = map toInp collateralInsOutsPaths
        -- TODO: [ADP-1670]
        , collateralOutputs = ApiAsArray Nothing
        , withdrawals = []
        -- TODO minting/burning multisig
        , mint = emptyApiAssetMntBurn
        , burn = emptyApiAssetMntBurn
        -- TODO delegation/withdrawals multisig
        , certificates = []
        , depositsTaken = []
        , depositsReturned = []
        , metadata = ApiTxMetadata $ ApiT <$> metadata
        , scriptValidity = ApiT <$> scriptValidity
        , validityInterval = interval
        , witnessCount = witsCount
        }
  where
    tl = ctx ^. W.transactionLayer @k @'CredFromScriptK
    nl = ctx ^. W.networkLayer @IO

    emptyApiAssetMntBurn = ApiAssetMintBurn
        { tokens = []
        , walletPolicyKeyHash = Nothing
        , walletPolicyKeyIndex = Nothing
        }

balanceTransaction
    :: forall ctx s k (n :: NetworkDiscriminant).
        ( ctx ~ ApiLayer s k 'CredFromKeyK
        , HasNetworkLayer IO ctx
        , GenChange s
        , BoundedAddressLength k
        )
    => ctx
    -> ArgGenChange s
    -> ApiT WalletId
    -> ApiBalanceTransactionPostData n
    -> Handler ApiSerialisedTransaction
balanceTransaction ctx genChange (ApiT wid) body = do
    -- NOTE: Ideally we'd read @pp@ and @era@ atomically.
    pp <- liftIO $ NW.currentProtocolParameters nl
    -- TODO: This throws when still in the Byron era.
    era <- liftIO $ NW.currentNodeEra nl
    let nodePParams = fromJust $ W.currentNodeProtocolParameters pp
    withWorkerCtx ctx wid liftE liftE $ \wrk -> do
        wallet <- liftHandler $ W.readWalletUTxOIndex @_ @s @k wrk wid
        ti <- liftIO $ snapshot $ timeInterpreter $ ctx ^. networkLayer

        let mkPartialTx
                :: forall era. WriteTx.IsRecentEra era => Cardano.Tx era
                -> Handler (W.PartialTx era)
            mkPartialTx tx = do
                utxo <- fmap WriteTx.toCardanoUTxO $ mkLedgerUTxO $ body ^. #inputs
                pure $ W.PartialTx
                    tx
                    utxo
                    (fromApiRedeemer <$> body ^. #redeemers)
              where
                -- NOTE: There are a couple of spread-out pieces of logic
                -- dealing with the choice of era, most prominantly: tx, utxo,
                -- pparams / current node era. It /might/ be neater to have a
                -- single function dedicated to this choice instead; something
                -- like
                -- @@
                --      chooseEra
                --          :: InRecentEra Tx
                --          -> InRecentEra UTxO
                --          -> InRecentEra PParams
                --          -> (IsRecentEra era
                --              => Tx era
                --              -> UTxO era
                --              -> PParams era
                --              -> res)
                --          -> res
                -- @@

                mkRecentEra :: Handler (WriteTx.RecentEra era)
                mkRecentEra = case Cardano.cardanoEra @era of
                    Cardano.BabbageEra -> pure WriteTx.RecentEraBabbage
                    Cardano.AlonzoEra -> pure WriteTx.RecentEraAlonzo
                    _ -> liftHandler $ throwE $ ErrOldEraNotSupported era

                mkLedgerUTxO
                    :: [ApiExternalInput n]
                    -> Handler (WriteTx.UTxO (ShelleyLedgerEra era))
                mkLedgerUTxO ins = do
                    recentEra <- mkRecentEra
                    liftHandler
                        . ExceptT
                        . pure
                        . WriteTx.utxoFromTxOutsInRecentEra recentEra
                        . map fromExternalInput
                        $ ins

        let balanceTx
                :: forall era. Cardano.IsShelleyBasedEra era
                => W.PartialTx era
                -> Handler (Cardano.Tx era)
            balanceTx partialTx =
                liftHandler $ W.balanceTransaction @_ @IO @s @k @'CredFromKeyK
                    wrk
                    genChange
                    (pp, nodePParams)
                    ti
                    wallet
                    partialTx

        anyRecentTx <- maybeToHandler (ErrOldEraNotSupported era)
            . WriteTx.asAnyRecentEra
            . cardanoTxIdeallyNoLaterThan era
            . getApiT $ body ^. #transaction

        res <- WriteTx.withRecentEra anyRecentTx
            (fmap inAnyCardanoEra . balanceTx <=< mkPartialTx)

        case body ^. #encoding of
            Just HexEncoded ->
                pure $ ApiSerialisedTransaction
                (ApiT $ W.sealedTxFromCardano res) HexEncoded
            _ -> pure $ ApiSerialisedTransaction
                (ApiT $ W.sealedTxFromCardano res) Base64Encoded
  where
    nl = ctx ^. networkLayer

    maybeToHandler :: IsServerError e => e -> Maybe a -> Handler a
    maybeToHandler _ (Just a) = pure a
    maybeToHandler e Nothing  = liftHandler $ throwE e

decodeTransaction
    :: forall ctx s k n.
        ( ctx ~ ApiLayer s k 'CredFromKeyK
        , IsOurs s Address
        , Typeable s
        , Typeable n
        , HasNetworkLayer IO ctx
        )
    => ctx
    -> ApiT WalletId
    -> ApiSerialisedTransaction
    -> Handler (ApiDecodedTransaction n)
decodeTransaction ctx (ApiT wid) (ApiSerialisedTransaction (ApiT sealed) _) = do
    era <- liftIO $ NW.currentNodeEra nl
    let (decodedTx, toMint, toBurn, allCerts, interval, witsCount) =
            decodeTx tl era sealed
        Tx { txId
            , fee
            , resolvedInputs
            , resolvedCollateralInputs
            , outputs
            , withdrawals
            , metadata
            , scriptValidity
            } = decodedTx
    withWorkerCtx ctx wid liftE liftE $ \wrk -> do
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
        (minted, burned) <-
            convertApiAssetMintBurn @_ @s @k @n wrk wid (toMint, toBurn)
        let certs = mkApiAnyCertificate acct acctPath <$> allCerts
        pure $ ApiDecodedTransaction
            { id = ApiT txId
            , fee = maybe (Quantity 0) (Quantity . fromIntegral . unCoin) fee
            , inputs = map toInp inputPaths
            , outputs = map toOut outputPaths
            , collateral = map toInp collateralInputPaths
            -- TODO: [ADP-1670]
            , collateralOutputs = ApiAsArray Nothing
            , withdrawals = map (toWrdl acct) $ Map.assocs withdrawals
            , mint = minted
            , burn = burned
            , certificates = certs
            , depositsTaken =
                (Quantity . fromIntegral . unCoin . W.stakeKeyDeposit $ pp)
                    <$ filter ourRewardAccountRegistration certs
            , depositsReturned =
                (Quantity . fromIntegral . unCoin . W.stakeKeyDeposit $ pp)
                    <$ filter ourRewardAccountDeregistration certs
            , metadata = ApiTxMetadata $ ApiT <$> metadata
            , scriptValidity = ApiT <$> scriptValidity
            , validityInterval = interval
            , witnessCount = witsCount
            }
  where
    tl = ctx ^. W.transactionLayer @k @'CredFromKeyK
    nl = ctx ^. W.networkLayer @IO

    toWrdl acct (rewardKey, (Coin c)) =
        if rewardKey == acct then
           ApiWithdrawalGeneral (ApiT rewardKey, Proxy @n) (Quantity $ fromIntegral c) Our
        else
           ApiWithdrawalGeneral (ApiT rewardKey, Proxy @n) (Quantity $ fromIntegral c) External
    ourRewardAccountRegistration = \case
        WalletDelegationCertificate (RegisterRewardAccount _) -> True
        _ -> False
    ourRewardAccountDeregistration = \case
        WalletDelegationCertificate (QuitPool _) -> True
        _ -> False

toInp
    :: forall n. (TxIn, Maybe (TxOut, NonEmpty DerivationIndex))
    -> ApiTxInputGeneral n
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

toOut
    :: forall n. (TxOut, Maybe (NonEmpty DerivationIndex))
    -> ApiTxOutputGeneral n
toOut (txoutIncoming, Nothing) =
    ExternalOutput $ toAddressAmount @n txoutIncoming
toOut ((TxOut addr (TokenBundle (Coin c) tmap)), (Just path)) =
        WalletOutput $ ApiWalletOutput
            { address = (ApiT addr, Proxy @n)
            , amount = Quantity $ fromIntegral c
            , assets = ApiT tmap
            , derivationPath = NE.map ApiT path
            }

submitTransaction
    :: forall ctx s k (n :: NetworkDiscriminant).
        ( ctx ~ ApiLayer s k 'CredFromKeyK
        , HasNetworkLayer IO ctx
        , IsOwned s k 'CredFromKeyK
        , Typeable s
        , Typeable n
        )
    => ctx
    -> ApiT WalletId
    -> ApiSerialisedTransaction
    -> Handler ApiTxId
submitTransaction ctx apiw@(ApiT wid) apitx = do
    --TODO: revisit/possibly set proper ttls in ADP-1193
    ttl <- liftIO $ W.getTxExpiry ti Nothing
    era <- liftIO $ NW.currentNodeEra nl

    let sealedTx = getApiT . (view #serialisedTxSealed) $ apitx
    let (tx,_,_,_,_,_) = decodeTx tl era sealedTx

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
    tl = ctx ^. W.transactionLayer @k @'CredFromKeyK
    ti :: TimeInterpreter (ExceptT PastHorizonException IO)
    nl = ctx ^. networkLayer
    ti = timeInterpreter nl

    getOurWdrl rewardAcct path apiDecodedTx =
        let generalWdrls = apiDecodedTx ^. #withdrawals
            isWdrlOurs (ApiWithdrawalGeneral _ _ context) = context == Our
        in case filter isWdrlOurs generalWdrls of
            [ApiWithdrawalGeneral (ApiT acct, _) (Quantity amt) _] ->
                let acct' = invariant "reward account should be the same" acct (rewardAcct ==)
                in WithdrawalSelf acct' path (Coin amt)
            _ ->
                NoWithdrawal

    isJoiningOrQuitting = \case
        WalletDelegationCertificate (JoinPool _ (ApiT poolId)) ->
            Just $ Join poolId
        WalletDelegationCertificate (QuitPool _) ->
            Just Quit
        _ ->
            Nothing

samePaymentKey :: ApiTxInputGeneral n -> ApiTxInputGeneral n -> Bool
samePaymentKey inp1 inp2 = case (inp1, inp2) of
    (WalletInput (ApiWalletInput _ _ _ derPath1 _ _), WalletInput (ApiWalletInput _ _ _ derPath2 _ _) ) ->
           derPath1 == derPath2
    _ -> False

getOurOuts :: ApiDecodedTransaction n -> [TxOut]
getOurOuts apiDecodedTx =
    map toTxOut $ filter isOutOurs generalOuts
  where
    generalOuts = apiDecodedTx ^. #outputs
    isOutOurs (WalletOutput _) = True
    isOutOurs _ = False
    toTxOut (WalletOutput (ApiWalletOutput (ApiT addr, _) (Quantity amt) (ApiT tmap) _)) =
        TxOut addr (TokenBundle (Coin $ fromIntegral amt) tmap)
    toTxOut _ = error "we should have only our outputs at this point"

isInpOurs :: ApiTxInputGeneral n -> Bool
isInpOurs (WalletInput _) = True
isInpOurs _ = False

getOurInps :: ApiDecodedTransaction n -> [(TxIn,Coin)]
getOurInps apiDecodedTx =
    map toTxInp $ filter isInpOurs generalInps
  where
    generalInps = apiDecodedTx ^. #inputs
    toTxInp (WalletInput (ApiWalletInput (ApiT txid) ix _ _ (Quantity amt) _)) =
        (TxIn txid ix, Coin $ fromIntegral amt)
    toTxInp _ = error "we should have only our inputs at this point"

isForeign :: ApiDecodedTransaction n -> Bool
isForeign apiDecodedTx =
    let generalInps = apiDecodedTx ^. #inputs
        generalWdrls = apiDecodedTx ^. #withdrawals
        isInpForeign (WalletInput _) = False
        isInpForeign _ = True
        isWdrlForeign (ApiWithdrawalGeneral _ _ context) = context == External
    in
        all isInpForeign generalInps &&
        all isWdrlForeign generalWdrls

submitSharedTransaction
    :: forall ctx s k (n :: NetworkDiscriminant).
        ( ctx ~ ApiLayer s k 'CredFromScriptK
        , s ~ SharedState n k
        , HasNetworkLayer IO ctx
        , IsOwned s k 'CredFromScriptK
        )
    => ctx
    -> ApiT WalletId
    -> ApiSerialisedTransaction
    -> Handler ApiTxId
submitSharedTransaction ctx apiw@(ApiT wid) apitx = do
    ttl <- liftIO $ W.getTxExpiry ti Nothing
    era <- liftIO $ NW.currentNodeEra nl

    let sealedTx = getApiT . (view #serialisedTxSealed) $ apitx
    let (tx,_,_,_,_,_) = decodeTx tl era sealedTx

    apiDecoded <- decodeSharedTransaction @_ @s @k ctx apiw apitx
    when (isForeign apiDecoded) $
        liftHandler $ throwE ErrSubmitTransactionForeignWallet
    let ourOuts = getOurOuts apiDecoded
    let ourInps = getOurInps apiDecoded

    _ <- withWorkerCtx ctx wid liftE liftE $ \wrk -> do

        (cp, _, _) <- liftHandler $ withExceptT ErrSubmitTransactionNoSuchWallet $
            W.readWallet @_ @s @k wrk wid
        let (ScriptTemplate _ script) = (Shared.paymentTemplate $ getState cp)
        let witsPerInput = Shared.estimateMinWitnessRequiredPerInput script

        let witsRequiredForInputs =
                length $ L.nubBy samePaymentKey $
                filter isInpOurs $
                (apiDecoded ^. #inputs) ++ (apiDecoded ^. #collateral)
        let totalNumberOfWits = length $ getSealedTxWitnesses sealedTx
        let allWitsRequired = fromIntegral witsPerInput * witsRequiredForInputs
        when (allWitsRequired > totalNumberOfWits) $
            liftHandler $ throwE $
            ErrSubmitTransactionPartiallySignedOrNoSignedTx allWitsRequired totalNumberOfWits

        let txCtx = defaultTransactionCtx
                { txValidityInterval = (Nothing, ttl)
                }
        txMeta <- liftHandler $ W.constructTxMeta @_ @s @k wrk wid txCtx ourInps ourOuts
        liftHandler
            $ W.submitTx @_ @s @k wrk wid (tx, txMeta, sealedTx)
    return $ ApiTxId (apiDecoded ^. #id)
  where
    tl = ctx ^. W.transactionLayer @k @'CredFromScriptK
    ti :: TimeInterpreter (ExceptT PastHorizonException IO)
    nl = ctx ^. networkLayer
    ti = timeInterpreter nl

joinStakePool
    :: forall ctx s n k.
        ( ctx ~ ApiLayer s k 'CredFromKeyK
        , s ~ SeqState n k
        , AddressIndexDerivationType k ~ 'Soft
        , DelegationAddress n k 'CredFromKeyK
        , GenChange s
        , IsOwned s k 'CredFromKeyK
        , SoftDerivation k
        , Typeable n
        , Typeable s
        , WalletKey k
        , AddressBookIso s
        , BoundedAddressLength k
        , HasDelegation s
        )
    => ctx
    -> IO (Set PoolId)
       -- ^ Known pools
       -- We could maybe replace this with a @IO (PoolId -> Bool)@
    -> (PoolId -> IO PoolLifeCycleStatus)
    -> ApiPoolSpecifier
    -> ApiT WalletId
    -> ApiWalletPassphrase
    -> Handler (ApiTransaction n)
joinStakePool ctx knownPools getPoolStatus apiPool (ApiT wid) body = do
    let pwd = coerce $ getApiT $ body ^. #passphrase
    poolId <- case apiPool of
        AllPools -> liftE ErrUnexpectedPoolIdPlaceholder
        SpecificPool pool -> pure pool

    poolStatus <- liftIO (getPoolStatus poolId)
    pools <- liftIO knownPools
    curEpoch <- getCurrentEpoch ctx
    withWorkerCtx ctx wid liftE liftE $ \wrk -> do
        (action, _) <- liftHandler
            $ W.joinStakePool @_ @s @k wrk curEpoch pools poolId poolStatus wid

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
            $ W.selectAssets @_ @_ @s @k @'CredFromKeyK wrk era pp selectAssetsParams
            $ const Prelude.id
        sel' <- liftHandler
            $ W.assignChangeAddressesAndUpdateDb wrk wid genChange sel
        (tx, txMeta, txTime, sealedTx) <- liftHandler
            $ W.buildAndSignTransaction @_ @s @k
                wrk wid era mkRwdAcct pwd txCtx sel'
        liftHandler
            $ W.submitTx @_ @s @k wrk wid (tx, txMeta, sealedTx)
        mkApiTransaction
            (timeInterpreter (ctx ^. networkLayer))
            wrk wid
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
                , txCBOR  = tx ^. #txCBOR
                }
  where
    ti :: TimeInterpreter (ExceptT PastHorizonException IO)
    ti = timeInterpreter (ctx ^. networkLayer)

    genChange = delegationAddress @n

delegationFee
    :: forall ctx s n k.
        ( s ~ SeqState n k
        , ctx ~ ApiLayer s k 'CredFromKeyK
        , BoundedAddressLength k
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
        W.selectAssets @_ @_ @s @k @'CredFromKeyK wrk era pp selectAssetsParams calcFee
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
        ( ctx ~ ApiLayer s k 'CredFromKeyK
        , s ~ SeqState n k
        , AddressIndexDerivationType k ~ 'Soft
        , DelegationAddress n k 'CredFromKeyK
        , GenChange s
        , HasNetworkLayer IO ctx
        , IsOwned s k 'CredFromKeyK
        , SoftDerivation k
        , Typeable n
        , Typeable s
        , WalletKey k
        , AddressBookIso s
        , BoundedAddressLength k, HasDelegation s)
    => ctx
    -> ApiT WalletId
    -> ApiWalletPassphrase
    -> Handler (ApiTransaction n)
quitStakePool ctx (ApiT wid) body = do
    let pwd = coerce $ getApiT $ body ^. #passphrase

    withWorkerCtx ctx wid liftE liftE $ \wrk -> do
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
            $ W.selectAssets @_ @_ @s @k @'CredFromKeyK wrk era pp selectAssetsParams
            $ const Prelude.id
        sel' <- liftHandler
            $ W.assignChangeAddressesAndUpdateDb wrk wid genChange sel
        (tx, txMeta, txTime, sealedTx) <- liftHandler
            $ W.buildAndSignTransaction @_ @s @k
                wrk wid era mkRwdAcct pwd txCtx sel'
        liftHandler
            $ W.submitTx @_ @s @k wrk wid (tx, txMeta, sealedTx)
        mkApiTransaction
            (timeInterpreter (ctx ^. networkLayer))
            wrk wid
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
                , txCBOR  = tx ^. #txCBOR
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
                , _rewardBalance = Coin.toQuantity $
                    rewards acc
                , _delegation = deleg
                , _stake = Coin.toQuantity $
                    stake (Just acc) <> rewards acc
                }

        let mkForeign acc = ApiForeignStakeKey
                { _key = (ApiT acc, Proxy)
                , _rewardBalance = Coin.toQuantity $
                    rewards acc
                , _stake = Coin.toQuantity $
                    stake (Just acc) <> rewards acc
                }

        let foreignKeys = stakeKeysInUTxO \\ ourKeys

        let nullKey = ApiNullStakeKey
                { _stake = Coin.toQuantity $ stake Nothing
                }

        return $ ApiStakeKeys
            { _ours = map mkOurs ourKeysWithInfo
            , _foreign = map mkForeign foreignKeys
            , _none = nullKey
            }

listStakeKeys
    :: forall ctx s n k.
        ( ctx ~ ApiLayer s k 'CredFromKeyK
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
        ( ctx ~ ApiLayer s k 'CredFromKeyK
        , Bounded (Index (AddressIndexDerivationType k) (AddressCredential k))
        , HardDerivation k
        , IsOwned s k 'CredFromKeyK
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
    totalFee = Coin.toQuantity $ view #totalFee plan

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
        { ada = Coin.toQuantity $ view #coin b
        , assets = ApiT $ view #tokens b
        }

migrateWallet
    :: forall ctx s k n p.
        ( ctx ~ ApiLayer s k 'CredFromKeyK
        , Bounded (Index (AddressIndexDerivationType k) (AddressCredential k))
        , HardDerivation k
        , HasNetworkLayer IO ctx
        , IsOwned s k 'CredFromKeyK
        , Typeable n
        , Typeable s
        , WalletKey k
        , HasDelegation s
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
            mkApiTransaction
                (timeInterpreter (ctx ^. networkLayer))
                wrk wid
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
                    , txCBOR  = tx ^. #txCBOR
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
    :: forall ctx s k ktype. (ctx ~ ApiLayer s k ktype)
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
    => NetworkId
    -> NetworkLayer IO Block
    -> ApiWalletMode
    -> Handler ApiNetworkInformation
getNetworkInformation nid
    NetworkLayer
        { syncProgress
        , currentNodeTip
        , currentNodeEra
        , timeInterpreter
        }
    mode = liftIO $ do
        now <- currentRelativeTime ti
        nodeTip <- currentNodeTip
        nodeEra <- currentNodeEra
        apiNodeTip <- makeApiBlockReferenceFromHeader
            (neverFails "node tip is within safe-zone" timeInterpreter)
            nodeTip
        nowInfo <- runMaybeT $ networkTipInfo now
        progress <- syncProgress $ view #slotNo nodeTip
        pure Api.ApiNetworkInformation
            { Api.syncProgress = ApiT progress
            , Api.nextEpoch = snd <$> nowInfo
            , Api.nodeTip = apiNodeTip
            , Api.networkTip = fst <$> nowInfo
            , Api.nodeEra = toApiEra nodeEra
            , Api.networkInfo =
                Api.ApiNetworkInfo
                    ( case nid of
                        Cardano.Mainnet -> "mainnet"
                        Cardano.Testnet _ -> "testnet"
                    )
                    (fromIntegral $ unNetworkMagic $ toNetworkMagic nid)
            , Api.walletMode = mode
            }
  where
    ti :: TimeInterpreter (MaybeT IO)
    ti = hoistTimeInterpreter exceptToMaybeT timeInterpreter

    -- (network tip, next epoch)
    -- May be unavailable if the node is still syncing.
    networkTipInfo :: RelativeTime -> MaybeT IO (ApiSlotReference, EpochInfo)
    networkTipInfo now = do
        networkTipSlot <- interpretQuery ti $ ongoingSlotAt now
        tip <- makeApiSlotReference ti networkTipSlot
        let curEpoch = tip ^. #slotId . #epochNumber . #getApiT
        (_, nextEpochStart) <- interpretQuery ti $ timeOfEpoch curEpoch
        let nextEpoch = EpochInfo (succ curEpoch) nextEpochStart
        return (tip, nextEpoch)

getNetworkParameters
    :: (Block, NetworkParameters)
    -> NetworkLayer IO Block
    -> Handler ApiNetworkParameters
getNetworkParameters (_block0, genesisNp) nl = do
    pp <- liftIO $ NW.currentProtocolParameters nl
    sp <- liftIO $ NW.currentSlottingParameters nl
    let np = genesisNp { protocolParameters = pp, slottingParameters = sp }
    liftIO $ toApiNetworkParameters np (interpretQuery ti . toEpochInfo)
  where
    ti :: TimeInterpreter IO
    ti = neverFails
        "PastHorizonException should never happen in getNetworkParameters \
        \because the ledger is being queried for slotting info about its own \
        \tip."
        (timeInterpreter nl)

getNetworkClock :: NtpClient -> Bool -> Handler ApiNetworkClock
getNetworkClock client force =
    liftIO $ ApiNetworkClock <$> getNtpStatus client forceCheck
  where
    forceCheck =
        if force
            then Ntp.ForceBlockingRequest
            else Ntp.CanUseCachedResults

getBlocksLatestHeader :: NetworkLayer IO Block -> Handler ApiBlockHeader
getBlocksLatestHeader nl = liftIO $ mkApiBlockHeader <$> NW.currentNodeTip nl

{-------------------------------------------------------------------------------
                               Miscellaneous
-------------------------------------------------------------------------------}

postExternalTransaction
    :: forall ctx s k ktype.
        ( ctx ~ ApiLayer s k ktype
        )
    => ctx
    -> ApiT W.SealedTx
    -> Handler ApiTxId
postExternalTransaction ctx (ApiT sealed) = do
    tx <- liftHandler $ W.submitExternalTx @ctx @k @ktype ctx sealed
    return $ ApiTxId (ApiT (tx ^. #txId))

signMetadata
    :: forall ctx s k n.
        ( ctx ~ ApiLayer s k 'CredFromKeyK
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
    :: forall ctx s k ktype ver.
        ( ctx ~ ApiLayer s k ktype
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

postAccountPublicKey
    :: forall ctx s k ktype account.
        ( ctx ~ ApiLayer s k ktype
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
    :: forall ctx s k ktype account.
        ( ctx ~ ApiLayer s k ktype
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
        ( ctx ~ ApiLayer s k 'CredFromKeyK
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
        ( ctx ~ ApiLayer s ShelleyKey 'CredFromKeyK
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
        ( ctx ~ ApiLayer s k 'CredFromKeyK
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
        ( ctx ~ ApiLayer s k 'CredFromKeyK
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
        ( ctx ~ ApiLayer s k 'CredFromKeyK
        , shelley ~ SeqState n ShelleyKey
        , HardDerivation k
        , Bounded (Index (AddressIndexDerivationType k) (AddressCredential k))
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
                    <$> liftIO (W.readNextWithdrawal @_ @k @'CredFromKeyK wrk era wdrl)

            (Just Refl, Just (ExternalWithdrawal (ApiMnemonicT mw))) -> do
                let (xprv, acct, path) = W.someRewardAccount @ShelleyKey mw
                wdrl <- liftHandler (W.queryRewardBalance @_ wrk acct)
                    >>= liftIO . W.readNextWithdrawal @_ @k @'CredFromKeyK wrk era
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
            , amount = Coin.toQuantity amount
            , assets = ApiT assets
            , derivationPath = ApiT <$> path
            }

    mkApiCoinSelectionOutput :: output -> ApiCoinSelectionOutput n
    mkApiCoinSelectionOutput (TxOut addr (TokenBundle amount assets)) =
        ApiCoinSelectionOutput (ApiT addr, Proxy @n)
        (Coin.toQuantity amount)
        (ApiT assets)

    mkApiCoinSelectionChange :: change -> ApiCoinSelectionChange n
    mkApiCoinSelectionChange txChange =
        ApiCoinSelectionChange
            { address =
                (ApiT $ view #address txChange, Proxy @n)
            , amount =
                Coin.toQuantity $ view #amount txChange
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
            , amount = Coin.toQuantity amount
            , derivationPath = ApiT <$> path
            }

    mkApiCoinSelectionWithdrawal :: withdrawal -> ApiCoinSelectionWithdrawal n
    mkApiCoinSelectionWithdrawal (rewardAcct, wdrl, path) =
        ApiCoinSelectionWithdrawal
            { stakeAddress =
                (ApiT rewardAcct, Proxy @n)
            , amount =
                Coin.toQuantity wdrl
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
    , txCBOR :: Maybe TxCBOR
    }
    deriving (Eq, Generic, Show)



mkApiTransaction
    :: forall n s k . (Typeable s, Typeable n, HasDelegation s)
    => TimeInterpreter (ExceptT PastHorizonException IO)
    -> W.WalletLayer IO s k 'CredFromKeyK
    -> WalletId
    -> Lens' (ApiTransaction n) (Maybe ApiBlockReference)
    -> MkApiTransactionParams
    -> Handler (ApiTransaction n)
mkApiTransaction timeInterpreter wrk wid setTimeReference tx = do
    timeRef <- liftIO $ (#time .~ (tx ^. #txTime)) <$> makeApiBlockReference
            (neverFails
                "makeApiBlockReference shouldn't fail getting the time of \
                \transactions with slots in the past" timeInterpreter)
            (tx ^. (#txMeta . #slotNo))
            (natural (tx ^. (#txMeta . #blockHeight)))
    expRef <- liftIO $
        traverse makeApiSlotReference' (tx ^. (#txMeta . #expiry))

    parsedValues <- traverse parseTxCBOR $ tx ^. #txCBOR
    parsedCertificates <- if hasDelegation (Proxy @s)
        then traverse (getApiAnyCertificates wrk wid) parsedValues
        else pure Nothing
    parsedMintBurn <- forM parsedValues
        $ getTxApiAssetMintBurn @_ @s @k @n wrk wid
    let parsedValidity = view #validityInterval =<< parsedValues
        parsedIntegrity = view #scriptIntegrity =<< parsedValues
        parsedExtraSigs = view #extraSignatures <$> parsedValues

    return $
        apiTx
            & setTimeReference .~ Just timeRef
            & #expiresAt .~ expRef
            & #certificates .~ fromMaybe [] parsedCertificates
            & #mint  .~ maybe noApiAsset fst parsedMintBurn
            & #burn  .~ maybe noApiAsset snd parsedMintBurn
            & #validityInterval .~ parsedValidity
            & #scriptIntegrity .~ (ApiT <$> parsedIntegrity)
            & #extraSignatures .~ maybe [] (fmap ApiT) parsedExtraSigs
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
        , certificates = []
        , mint = ApiAssetMintBurn [] Nothing Nothing
        , burn = ApiAssetMintBurn [] Nothing Nothing
        , validityInterval = Nothing
        , scriptIntegrity = Nothing
        , extraSignatures = []
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
    TxOut addr (TokenBundle.TokenBundle (Coin.fromQuantity c) assets)

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

fromExternalInput
    :: ApiExternalInput n
    -> (WriteTx.TxIn, WriteTx.TxOutInRecentEra)
fromExternalInput ApiExternalInput
    { id = ApiT tid
    , index = ix
    , address = (ApiT addr, _)
    , amount = Quantity amt
    , assets = ApiT assets
    , datum
    }
  =
    let
        inp = toLedger $ TxIn tid ix
        script = Nothing
        addr' = toLedger addr
        val = toLedger $ TokenBundle (Coin.fromNatural amt) assets
        datum' = maybe WriteTx.NoDatum WriteTx.DatumHash (getApiT <$> datum)
        out = WriteTx.TxOutInRecentEra addr' val datum' script
    in
        (inp, out)

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
    :: forall ctx s k ktype.
        ( ctx ~ ApiLayer s k ktype
        , IsOurs s RewardAccount
        , IsOurs s Address
        , AddressBookIso s
        , MaybeLight s
        )
    => Tracer IO WalletEngineLog
    -> (Block, NetworkParameters)
    -> NetworkLayer IO Block
    -> TransactionLayer k ktype W.SealedTx
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
    :: forall ctx s k ktype.
        ( ctx ~ ApiLayer s k ktype
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
    (_, NetworkParameters gp _ _) = ctx ^. genesisData

-- | Register a wallet create and restore thread with the worker registry.
-- See 'Cardano.Wallet#createWallet'
createWalletWorker
    :: forall ctx s k ktype.
        ( ctx ~ ApiLayer s k ktype
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
    :: forall ctx s k ktype.
        ( ctx ~ ApiLayer s k ktype
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
            if wid `elem` wids
                then onNotResponding (ErrWalletNotResponding wid)
                else onMissing (ErrNoSuchWallet wid)
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
                                Locally defined server errors
-------------------------------------------------------------------------------}

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
