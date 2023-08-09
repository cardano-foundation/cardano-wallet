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
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

{-# HLINT ignore "Use record patterns" #-}

-- TODO: https://cardanofoundation.atlassian.net/browse/ADP-2841
{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 902
{-# OPTIONS_GHC -fno-warn-ambiguous-fields #-}
#endif

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
    , KeyHash (KeyHash)
    , KeyRole (..)
    , ScriptTemplate (..)
    , ValidationLevel (..)
    , foldScript
    , validateScriptOfTemplate
    )
import Cardano.Api
    ( NetworkId, SerialiseAsCBOR (..), toNetworkMagic, unNetworkMagic )
import Cardano.BM.Tracing
    ( HasPrivacyAnnotation (..), HasSeverityAnnotation (..) )
import Cardano.Mnemonic
    ( SomeMnemonic )
import Cardano.Pool.Types
    ( PoolId )
import Cardano.Tx.Balance.Internal.CoinSelection
    ( SelectionOf (..) )
import Cardano.Wallet
    ( BuiltTx (..)
    , DelegationFee (feePercentiles)
    , ErrConstructSharedWallet (..)
    , ErrConstructTx (..)
    , ErrCreateMigrationPlan (..)
    , ErrGetPolicyId (..)
    , ErrNoSuchWallet (..)
    , ErrReadRewardAccount (..)
    , ErrSignPayment (..)
    , ErrSubmitTransaction (..)
    , ErrUpdatePassphrase (..)
    , ErrWalletAlreadyExists (..)
    , ErrWalletNotResponding (..)
    , ErrWithRootKey (..)
    , ErrWitnessTx (..)
    , Fee (..)
    , HasNetworkLayer
    , Percentile (..)
    , TxSubmitLog
    , WalletException (..)
    , WalletWorkerLog (..)
    , dbLayer
    , dummyChangeAddressGen
    , genesisData
    , getCurrentEpochSlotting
    , logger
    , manageRewardBalance
    , networkLayer
    , readPrivateKey
    , readWalletMeta
    , transactionLayer
    , txWitnessTagForKey
    , utxoAssumptionsForWallet
    )
import Cardano.Wallet.Address.Book
    ( AddressBookIso )
import Cardano.Wallet.Address.Derivation
    ( AccountIxForStaking (..)
    , DelegationAddress (..)
    , Depth (..)
    , DerivationIndex (..)
    , DerivationType (..)
    , HardDerivation (..)
    , Index (..)
    , MkKeyFingerprint
    , RewardAccount (..)
    , Role
    , SoftDerivation (..)
    , delegationAddressS
    , deriveRewardAccount
    , stakeDerivationPath
    )
import Cardano.Wallet.Address.Derivation.Byron
    ( ByronKey, mkByronKeyFromMasterKey )
import Cardano.Wallet.Address.Derivation.Icarus
    ( IcarusKey )
import Cardano.Wallet.Address.Derivation.MintBurn
    ( scriptSlotIntervals, withinSlotInterval )
import Cardano.Wallet.Address.Derivation.SharedKey
    ( SharedKey (..), replaceCosignersWithVerKeys )
import Cardano.Wallet.Address.Derivation.Shelley
    ( ShelleyKey )
import Cardano.Wallet.Address.Discovery
    ( CompareDiscovery
    , GenChange (ArgGenChange)
    , GetAccount
    , GetPurpose (..)
    , IsOurs
    , KnownAddresses
    )
import Cardano.Wallet.Address.Discovery.Random
    ( RndState, mkRndState )
import Cardano.Wallet.Address.Discovery.Sequential
    ( DerivationPrefix (..)
    , SeqState (..)
    , defaultAddressPoolGap
    , getGap
    , mkSeqStateFromAccountXPub
    , purposeCIP1852
    )
import Cardano.Wallet.Address.Discovery.Shared
    ( CredentialType (..), SharedState (..) )
import Cardano.Wallet.Address.HasDelegation
    ( HasDelegation (..) )
import Cardano.Wallet.Address.Keys.MintBurn
    ( toTokenMapAndScript, toTokenPolicyId )
import Cardano.Wallet.Address.Keys.SequentialAny
    ( mkSeqStateFromRootXPrv )
import Cardano.Wallet.Address.Keys.Shared
    ( mkSharedStateFromAccountXPub
    , mkSharedStateFromRootXPrv
    , toSharedWalletId
    , validateScriptTemplates
    )
import Cardano.Wallet.Address.Keys.WalletKey
    ( AfterByron, digest, getRawKey, publicKey )
import Cardano.Wallet.Address.Keys.WitnessCount
    ( toWitnessCountCtx )
import Cardano.Wallet.Address.MaybeLight
    ( MaybeLight (..) )
import Cardano.Wallet.Address.States.IsOwned
    ( isOwned )
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
    ( IsServerError (..), apiError, handler, liftE, liftHandler )
import Cardano.Wallet.Api.Http.Server.Handlers.MintBurn
    ( convertApiAssetMintBurn, getTxApiAssetMintBurn )
import Cardano.Wallet.Api.Http.Server.Handlers.TxCBOR
    ( ParsedTxCBOR (..), parseTxCBOR )
import Cardano.Wallet.Api.Http.Server.Tls
    ( TlsConfiguration (..), requireClientAuth )
import Cardano.Wallet.Api.Types
    ( AccountPostData (..)
    , AddressAmount (..)
    , AddressAmountNoAssets (..)
    , ApiAccountPublicKey (..)
    , ApiAccountSharedPublicKey (..)
    , ApiActiveSharedWallet (..)
    , ApiAddressWithPath (..)
    , ApiAnyCertificate (..)
    , ApiAsArray (..)
    , ApiAsset (..)
    , ApiAssetMintBurn (..)
    , ApiBalanceTransactionPostData (..)
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
    , ApiIncompleteSharedWallet (..)
    , ApiMintBurnData (..)
    , ApiMintBurnDataFromScript (..)
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
    , ApiPolicyId (..)
    , ApiPolicyKey (..)
    , ApiPoolSpecifier (..)
    , ApiPostAccountKeyDataWithPurpose (..)
    , ApiPostPolicyIdData
    , ApiPostPolicyKeyData (..)
    , ApiPostRandomAddressData (..)
    , ApiPutAddressesData (..)
    , ApiRedeemer (..)
    , ApiScriptTemplate (..)
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
    ( ApiRewardAccount (..), mkApiAnyCertificate )
import Cardano.Wallet.Api.Types.Error
    ( ApiErrorInfo (..) )
import Cardano.Wallet.Api.Types.Key
    ( computeKeyPayload )
import Cardano.Wallet.Api.Types.MintBurn
    ( noApiAsset )
import Cardano.Wallet.Api.Types.SchemaMetadata
    ( TxMetadataSchema (..), TxMetadataWithSchema (TxMetadataWithSchema) )
import Cardano.Wallet.Api.Types.Transaction
    ( ApiAddress (..)
    , ApiLimit
    , ApiValidityIntervalExplicit (..)
    , fromApiLimit
    , mkApiWitnessCount
    )
import Cardano.Wallet.Compat
    ( (^?) )
import Cardano.Wallet.DB
    ( DBFactory (..), DBFresh, DBLayer, loadDBLayer )
import Cardano.Wallet.Flavor
    ( CredFromOf
    , Excluding
    , KeyFlavorS (..)
    , KeyOf
    , NetworkOf
    , WalletFlavor (..)
    , WalletFlavorS (..)
    , keyFlavorFromState
    , keyOfWallet
    , shelleyOrShared
    )
import Cardano.Wallet.Network
    ( NetworkLayer (..), fetchRewardAccountBalances, timeInterpreter )
import Cardano.Wallet.Pools
    ( EpochInfo (..), toEpochInfo )
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
import Cardano.Wallet.Primitive.NetworkId
    ( HasSNetworkId (..), NetworkDiscriminantCheck )
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
    , WalletDelegation
    , WalletId (..)
    , WalletMetadata (..)
    )
import Cardano.Wallet.Primitive.Types.Address
    ( Address (..), AddressState (..) )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..) )
import Cardano.Wallet.Primitive.Types.Credentials
    ( ClearCredentials, RootCredentials (..) )
import Cardano.Wallet.Primitive.Types.Hash
    ( Hash (..) )
import Cardano.Wallet.Primitive.Types.TokenBundle
    ( TokenBundle (..) )
import Cardano.Wallet.Primitive.Types.TokenMap
    ( AssetId (..), fromFlatList )
import Cardano.Wallet.Primitive.Types.TokenPolicy
    ( TokenName (..), TokenPolicyId (..), nullTokenName, tokenNameMaxLength )
import Cardano.Wallet.Primitive.Types.TokenQuantity
    ( TokenQuantity (..) )
import Cardano.Wallet.Primitive.Types.Tx
    ( Tx (..)
    , TxChange (..)
    , UnsignedTx (..)
    , cardanoTxInExactEra
    , getSealedTxWitnesses
    , sealedTxFromCardanoBody
    )
import Cardano.Wallet.Primitive.Types.Tx.Constraints
    ( txMintBurnMaxTokenQuantity )
import Cardano.Wallet.Primitive.Types.Tx.TransactionInfo
    ( TransactionInfo )
import Cardano.Wallet.Primitive.Types.Tx.TxIn
    ( TxIn (..) )
import Cardano.Wallet.Primitive.Types.Tx.TxMeta
    ( TxStatus (..) )
import Cardano.Wallet.Primitive.Types.Tx.TxOut
    ( TxOut (..) )
import Cardano.Wallet.Read.Tx.CBOR
    ( TxCBOR )
import Cardano.Wallet.Registry
    ( HasWorkerCtx (..)
    , MkWorker (..)
    , WorkerLog (..)
    , defaultWorkerAfter
    , workerResource
    )
import Cardano.Wallet.Shelley.Compatibility.Ledger
    ( toLedger )
import Cardano.Wallet.Shelley.Transaction
    ( TxWitnessTag )
import Cardano.Wallet.TokenMetadata
    ( TokenMetadataClient, fillMetadata )
import Cardano.Wallet.Transaction
    ( AnyExplicitScript (..)
    , DelegationAction (..)
    , PreSelection (..)
    , TransactionCtx (..)
    , TransactionLayer (..)
    , Withdrawal (..)
    , WitnessCount (..)
    , WitnessCountCtx (..)
    , defaultTransactionCtx
    )
import Cardano.Wallet.Unsafe
    ( unsafeRunExceptT )
import Cardano.Wallet.Write.Tx
    ( AnyRecentEra (..) )
import Cardano.Wallet.Write.Tx.Balance
    ( Redeemer (..), UTxOAssumptions (..) )
import Control.Arrow
    ( second, (&&&) )
import Control.DeepSeq
    ( NFData )
import Control.Error.Util
    ( failWith )
import Control.Monad
    ( forM, forever, join, void, when, (>=>) )
import Control.Monad.Error.Class
    ( throwError )
import Control.Monad.IO.Class
    ( MonadIO, liftIO )
import Control.Monad.Trans.Class
    ( lift )
import Control.Monad.Trans.Except
    ( ExceptT (..), except, runExceptT, throwE, withExceptT )
import Control.Monad.Trans.Maybe
    ( MaybeT (..), exceptToMaybeT )
import Control.Tracer
    ( Tracer, contramap )
import Data.Bifunctor
    ( first )
import Data.ByteString
    ( ByteString )
import Data.Coerce
    ( coerce )
import Data.Either
    ( isLeft, isRight )
import Data.Either.Extra
    ( eitherToMaybe, fromLeft' )
import Data.Function
    ( (&) )
import Data.Functor
    ( (<&>) )
import Data.Functor.Contravariant
    ( (>$<) )
import Data.Functor.Identity
    ( Identity (..) )
import Data.Generics.Internal.VL.Lens
    ( Lens', set, view, (.~), (^.) )
import Data.Generics.Labels
    ()
import Data.Generics.Product
    ( typed )
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
import Data.Traversable
    ( for )
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
import UnliftIO.Async
    ( race_ )
import UnliftIO.Concurrent
    ( threadDelay )
import UnliftIO.Exception
    ( IOException, bracket, throwIO, tryAnyDeep, tryJust )

import qualified Cardano.Address.Script as CA
import qualified Cardano.Address.Style.Shelley as CA
import qualified Cardano.Api as Cardano
import qualified Cardano.Wallet as W
import qualified Cardano.Wallet.Address.Derivation.Byron as Byron
import qualified Cardano.Wallet.Address.Derivation.Icarus as Icarus
import qualified Cardano.Wallet.Address.Discovery.Sequential as Seq
import qualified Cardano.Wallet.Address.Discovery.Shared as Shared
import qualified Cardano.Wallet.Api.Types as Api
import qualified Cardano.Wallet.DB as W
import qualified Cardano.Wallet.Delegation as WD
import qualified Cardano.Wallet.Network as NW
import qualified Cardano.Wallet.Primitive.Types as W
import qualified Cardano.Wallet.Primitive.Types.Coin as Coin
import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle
import qualified Cardano.Wallet.Primitive.Types.Tx.SealedTx as W
    ( SealedTx, sealedTxFromCardano )
import qualified Cardano.Wallet.Primitive.Types.Tx.Tx as W
    ( TxMetadata, TxScriptValidity )
import qualified Cardano.Wallet.Primitive.Types.Tx.TxMeta as W
    ( Direction (Incoming, Outgoing), TxMeta )
import qualified Cardano.Wallet.Primitive.Types.Tx.TxOut as TxOut
import qualified Cardano.Wallet.Primitive.Types.UTxO as UTxO
import qualified Cardano.Wallet.Read as Read
import qualified Cardano.Wallet.Registry as Registry
import qualified Cardano.Wallet.Write.ProtocolParameters as Write
import qualified Cardano.Wallet.Write.Tx as Write
import qualified Cardano.Wallet.Write.Tx.Balance as Write
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
    -> WalletDelegation
    -> Set Tx
    -> SyncProgress
    -> Handler w

--------------------- Shelley
postWallet
    :: forall ctx s k n.
        ( s ~ SeqState n k
        , k ~ ShelleyKey
        , ctx ~ ApiLayer s
        , Seq.SupportsDiscovery n k
        , HasDBFactory s ctx
        , HasWorkerRegistry s ctx
        , AddressBookIso s
        , MaybeLight s
        )
    => ctx
    -> ((SomeMnemonic, Maybe SomeMnemonic) -> Passphrase "encryption" -> k 'RootK XPrv)
    -> (XPub -> k 'AccountK XPub)
    -> WalletOrAccountPostData
    -> Handler ApiWallet
postWallet ctx generateKey liftKey (WalletOrAccountPostData body) = case body of
    Left body' -> postShelleyWallet ctx generateKey body'
    Right body' ->
        let action workerCtx _ =
                W.manageRewardBalance
                    (workerCtx ^. typed @(Tracer IO WalletWorkerLog))
                    (workerCtx ^. networkLayer)
                    (workerCtx ^. typed @(DBLayer IO (SeqState n ShelleyKey)))

        in postAccountWallet ctx mkShelleyWallet liftKey action body'

postShelleyWallet
    :: forall ctx s k n.
        ( s ~ SeqState n k
        , k ~ ShelleyKey
        , ctx ~ ApiLayer s
        , Seq.SupportsDiscovery n k
        , HasDBFactory s ctx
        , HasWorkerRegistry s ctx
        , IsOurs s RewardAccount
        , MaybeLight s
        , AddressBookIso s
        )
    => ctx
    -> ((SomeMnemonic, Maybe SomeMnemonic) -> Passphrase "encryption" -> k 'RootK XPrv)
    -> WalletPostData
    -> Handler ApiWallet
postShelleyWallet ctx generateKey body = do
    let state = mkSeqStateFromRootXPrv
            (keyFlavorFromState @s) (RootCredentials rootXPrv pwdP) purposeCIP1852 g
    void $ liftHandler $ createWalletWorker @_ @s ctx wid
        (\dbf -> W.createWallet @_ @s genesisParams dbf wid wName state)
        (\workerCtx _ -> W.manageRewardBalance
            (workerCtx ^. typed)
            (workerCtx ^. networkLayer)
            (workerCtx ^. typed @(DBLayer IO (SeqState n ShelleyKey)))
        )
    withWorkerCtx @_ @s ctx wid liftE liftE $ \wrk -> handler $
        W.attachPrivateKeyFromPwd wrk (rootXPrv, pwd)
    fst <$> getWallet ctx (mkShelleyWallet @_ @s) (ApiT wid)
  where
    seed = getApiMnemonicT (body ^. #mnemonicSentence)
    secondFactor = getApiMnemonicT <$> (body ^. #mnemonicSecondFactor)
    pwd = getApiT (body ^. #passphrase)
    pwdP = preparePassphrase currentPassphraseScheme pwd
    rootXPrv = generateKey (seed, secondFactor) pwdP
    g = maybe defaultAddressPoolGap getApiT (body ^. #addressPoolGap)
    wid = WalletId $ digest ShelleyKeyS $ publicKey ShelleyKeyS rootXPrv
    wName = getApiT (body ^. #name)
    genesisParams = ctx ^. #netParams

postAccountWallet
    :: forall ctx s k n w.
        ( s ~ SeqState n k
        , ctx ~ ApiLayer s
        , Seq.SupportsDiscovery n k
        , HasWorkerRegistry s ctx
        , IsOurs s RewardAccount
        , MaybeLight s
        , AddressBookIso s
        , WalletFlavor s
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
    void $ liftHandler $ createWalletWorker @_ @s ctx wid
        (\dbf -> W.createWallet @_ @s genesisParams dbf wid wName state)
        coworker
    fst <$> getWallet ctx mkWallet (ApiT wid)
  where
    g = maybe defaultAddressPoolGap getApiT (body ^. #addressPoolGap)
    wName = getApiT (body ^. #name)
    (ApiAccountPublicKey accXPubApiT) =  body ^. #accountPublicKey
    accXPub = getApiT accXPubApiT
    wid = WalletId $ digest (keyFlavorFromState @s) (liftKey accXPub)
    genesisParams = ctx ^. #netParams

mkShelleyWallet
    :: forall ctx s k n.
        ( ctx ~ ApiLayer s
        , s ~ SeqState n k
        , IsOurs s Address
        , IsOurs s RewardAccount
        , HasWorkerRegistry s ctx
        )
    => MkApiWallet ctx s ApiWallet
mkShelleyWallet ctx@ApiLayer{..} wid cp meta delegation pending progress = do
    reward <- withWorkerCtx @_ @s ctx wid liftE liftE $ \wrk ->
        -- never fails - returns zero if balance not found
        liftIO $ W.fetchRewardBalance @s $ wrk ^. dbLayer

    let ti = timeInterpreter netLayer

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
    apiDelegation <- liftIO $ toApiWalletDelegation delegation
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
        , ctx ~ ApiLayer s
        , Shared.SupportsDiscovery n k
        , HasDBFactory s ctx
        , HasWorkerRegistry s ctx
        )
    => ctx
    ->  ( (SomeMnemonic, Maybe SomeMnemonic)
          -> Passphrase "encryption"
          -> k 'RootK XPrv
        )
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
        , ctx ~ ApiLayer s
        , Shared.SupportsDiscovery n k
        , HasDBFactory s ctx
        , HasWorkerRegistry s ctx
        )
    => ctx
    ->  ( (SomeMnemonic, Maybe SomeMnemonic)
          -> Passphrase "encryption"
          -> k 'RootK XPrv
        )
    -> ApiSharedWalletPostDataFromMnemonics
    -> Handler ApiSharedWallet
postSharedWalletFromRootXPrv ctx generateKey body = do
    let wid = WalletId $ toSharedWalletId kF accXPub pTemplate dTemplateM
    validateScriptTemplates kF accXPub scriptValidation pTemplate dTemplateM
        & \case
            Left err -> liftHandler
                $ throwE
                $ ErrConstructSharedWalletWrongScriptTemplate err
            Right _ -> pure ()
    ix' <- liftHandler $ withExceptT ErrConstructSharedWalletInvalidIndex $
        W.guardHardIndex ix
    let state = mkSharedStateFromRootXPrv kF
            (RootCredentials rootXPrv pwdP) ix' g pTemplate dTemplateM
    let stateReadiness = state ^. #ready
    if stateReadiness == Shared.Pending
    then void $ liftHandler $ createNonRestoringWalletWorker @_ @s ctx wid
        (\dbf -> W.createWallet @_ @s genesisParams dbf wid wName state)
    else if isNothing dTemplateM then
        void $ liftHandler $ createWalletWorker @_ @s ctx wid
        (\dbf -> W.createWallet @_ @s genesisParams dbf wid wName state)
        idleWorker
    else void $ liftHandler $ createWalletWorker @_ @s ctx wid
        (\dbf -> W.createWallet @_ @s genesisParams dbf wid wName state)
        (\workerCtx _ -> W.manageSharedRewardBalance
            (workerCtx ^. typed)
            (workerCtx ^. networkLayer)
            (workerCtx ^. typed @(DBLayer IO (SharedState n SharedKey)))
        )
    withWorkerCtx @_ @s ctx wid liftE liftE $ \wrk -> handler $
        W.attachPrivateKeyFromPwd wrk (rootXPrv, pwd)
    fst <$> getWallet ctx (mkSharedWallet @_ @s) (ApiT wid)
  where
    kF = keyOfWallet wF
    wF = walletFlavor @s
    seed = body ^. #mnemonicSentence . #getApiMnemonicT
    secondFactor = getApiMnemonicT <$> body ^. #mnemonicSecondFactor
    pwdP = preparePassphrase currentPassphraseScheme pwd
    pwd = body ^. #passphrase . #getApiT
    rootXPrv = generateKey (seed, secondFactor) pwdP
    g = defaultAddressPoolGap
    ix = getApiT (body ^. #accountIndex)
    pTemplate = scriptTemplateFromSelf (getRawKey SharedKeyS accXPub)
        $ body ^. #paymentScriptTemplate
    dTemplateM = scriptTemplateFromSelf (getRawKey SharedKeyS accXPub)
        <$> body ^. #delegationScriptTemplate
    wName = getApiT (body ^. #name)
    accXPub = publicKey SharedKeyS
        $ deriveAccountPrivateKey pwdP rootXPrv (Index $ getDerivationIndex ix)
    scriptValidation =
        maybe RecommendedValidation getApiT (body ^. #scriptValidation)
    genesisParams = ctx ^. #netParams

postSharedWalletFromAccountXPub
    :: forall ctx s k n.
        ( s ~ SharedState n k
        , k ~ SharedKey
        , ctx ~ ApiLayer s
        , Shared.SupportsDiscovery n k
        , HasDBFactory s ctx
        , HasWorkerRegistry s ctx
        )
    => ctx
    -> (XPub -> k 'AccountK XPub)
    -> ApiSharedWalletPostDataFromAccountPubX
    -> Handler ApiSharedWallet
postSharedWalletFromAccountXPub ctx liftKey body = do
    let kF = keyFlavorFromState @s
        wid = WalletId $ toSharedWalletId kF
                (liftKey accXPub) pTemplate dTemplateM
    validateScriptTemplates kF
        (liftKey accXPub)
        scriptValidation
        pTemplate
        dTemplateM
        & \case
            Left err -> liftHandler
                $ throwE
                $ ErrConstructSharedWalletWrongScriptTemplate err
            Right _ -> pure ()
    acctIx <- liftHandler $ withExceptT ErrConstructSharedWalletInvalidIndex $
        W.guardHardIndex ix
    let state = mkSharedStateFromAccountXPub kF
            (liftKey accXPub) acctIx g pTemplate dTemplateM
    let stateReadiness = state ^. #ready
    if stateReadiness == Shared.Pending
    then void $ liftHandler $ createNonRestoringWalletWorker @_ @s ctx wid
        (\wrk -> W.createWallet @_ @s genesisParams wrk wid wName state)
    else if isNothing dTemplateM then
        void $ liftHandler $ createWalletWorker @_ @s ctx wid
        (\wrk -> W.createWallet @_ @s genesisParams wrk wid wName state)
        idleWorker
    else void $ liftHandler $ createWalletWorker @_ @s ctx wid
        (\wrk -> W.createWallet @_ @s genesisParams wrk wid wName state)
        (\workerCtx _ -> W.manageSharedRewardBalance
            (workerCtx ^. typed)
            (workerCtx ^. networkLayer)
            (workerCtx ^. typed @(DBLayer IO (SharedState n SharedKey)))
        )
    fst <$> getWallet ctx (mkSharedWallet @_ @s) (ApiT wid)
  where
    g = defaultAddressPoolGap
    ix = getApiT (body ^. #accountIndex)
    pTemplate = scriptTemplateFromSelf accXPub
        $ body ^. #paymentScriptTemplate
    dTemplateM = scriptTemplateFromSelf accXPub
        <$> body ^. #delegationScriptTemplate
    wName = getApiT (body ^. #name)
    (ApiAccountSharedPublicKey accXPubApiT) =  body ^. #accountPublicKey
    accXPub = getApiT accXPubApiT
    scriptValidation =
        maybe RecommendedValidation getApiT (body ^. #scriptValidation)
    genesisParams = ctx ^. #netParams

scriptTemplateFromSelf :: XPub -> ApiScriptTemplateEntry -> ScriptTemplate
scriptTemplateFromSelf xpub (ApiScriptTemplateEntry cosigners' template') =
    ScriptTemplate cosignersWithoutSelf template'
  where
    unSelf (SomeAccountKey xpub') = xpub'
    unSelf Self = xpub
    cosignersWithoutSelf = Map.map unSelf cosigners'

mkSharedWallet
    :: forall ctx s k n.
        ( ctx ~ ApiLayer s
        , s ~ SharedState n k
        , HasWorkerRegistry s ctx
        , Shared.SupportsDiscovery n k
        )
    => MkApiWallet ctx s ApiSharedWallet
mkSharedWallet ctx wid cp meta delegation pending progress =
    case Shared.ready st of
    Shared.Pending -> pure $ ApiSharedWallet $ Left $ ApiIncompleteSharedWallet
        { id = ApiT wid
        , name = ApiT $ meta ^. #name
        , accountIndex = ApiT $ DerivationIndex $ getIndex accIx
        , addressPoolGap = ApiT $ Shared.poolGap st
        , paymentScriptTemplate = ApiScriptTemplate $ Shared.paymentTemplate st
        , delegationScriptTemplate =
            ApiScriptTemplate <$> Shared.delegationTemplate st
        }
    Shared.Active _ -> do
        reward <- withWorkerCtx @_ @s ctx wid liftE liftE $ \wrk ->
            -- never fails - returns zero if balance not found
            liftIO $ W.fetchRewardBalance @s $ wrk ^. dbLayer

        let ti = timeInterpreter $ ctx ^. networkLayer
        apiDelegation <- liftIO $ toApiWalletDelegation delegation
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
            , paymentScriptTemplate = ApiScriptTemplate
                $ Shared.paymentTemplate st
            , delegationScriptTemplate = ApiScriptTemplate
                <$> Shared.delegationTemplate st
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
        , ctx ~ ApiLayer s
        , Shared.SupportsDiscovery n k
        )
    => ctx
    -> (XPub -> k 'AccountK XPub)
    -> CredentialType
    -> ApiT WalletId
    -> ApiSharedWalletPatchData
    -> Handler ApiSharedWallet
patchSharedWallet ctx liftKey cred (ApiT wid) body = do
    wal <- fst <$> getWallet ctx (mkSharedWallet @_ @s) (ApiT wid)
    withWorkerCtx ctx wid liftE liftE $ \wrk -> do
        liftHandler $ W.updateCosigner wrk (liftKey accXPub) cosigner cred
    wal' <- fst <$> getWallet ctx (mkSharedWallet @_ @s) (ApiT wid)
    -- we switch on restoring only after incomplete -> active transition
    -- active -> active when transition of updating cosigner keys takes place
    -- should not trigger this

    when (isRight (wal' ^. #wallet) && isLeft (wal ^. #wallet)) $ do
        (state, prvKeyM, meta) <- withWorkerCtx ctx wid liftE liftE
            $ \wrk -> handler $ do
                let db = wrk ^. W.dbLayer @IO @s
                db & \W.DBLayer
                    { atomically
                    , readCheckpoint
                    , walletState
                    } -> do
                        cp <- atomically readCheckpoint
                        let state = getState cp
                        --could be for account and root key wallets
                        prvKeyM <- atomically $ readPrivateKey walletState
                        meta <- atomically (readWalletMeta walletState)
                        pure (state, prvKeyM, meta)

        void $ deleteWallet ctx (ApiT wid)
        let wName = meta ^. #name
        void $ liftHandler $ createWalletWorker @_ @s ctx wid
            (\wrk ->
                W.createWallet @_ @s genesisParams wrk wid wName state)
            idleWorker
        withWorkerCtx @_ @s ctx wid liftE liftE $ \wrk ->
            handler $ W.updateWallet wrk (const meta)
        when (isJust prvKeyM)
            $ withWorkerCtx @_ @s ctx wid liftE liftE $ \wrk -> handler
            $ W.attachPrivateKeyFromPwdHashShelley wrk (fromJust prvKeyM)

    fst <$> getWallet ctx (mkSharedWallet @_ @s) (ApiT wid)
  where
    cosigner = getApiT (body ^. #cosigner)
    (ApiAccountSharedPublicKey accXPubApiT) = (body ^. #accountPublicKey)
    accXPub = getApiT accXPubApiT
    genesisParams = ctx ^. #netParams

--------------------- Legacy

postLegacyWallet
    :: forall ctx s k.
        ( ctx ~ ApiLayer s
        , KnownDiscovery s
        , IsOurs s RewardAccount
        , IsOurs s Address
        , MaybeLight s
        , HasNetworkLayer IO ctx
        , k ~ KeyOf s
        , AddressBookIso s
        , WalletFlavor s
        )
    => ctx
        -- ^ Surrounding Context
    -> (k 'RootK XPrv, Passphrase "user")
        -- ^ Root key
    -> ( DBFresh IO s
       -> WalletId
       -> ExceptT ErrWalletAlreadyExists IO (DBLayer IO s)
       )
        -- ^ How to create this legacy wallet
    -> Handler ApiByronWallet
postLegacyWallet ctx (rootXPrv, pwd) createWallet = do
    void $ liftHandler $ createWalletWorker @_ @s ctx wid
        (`createWallet` wid)
        idleWorker
    withWorkerCtx ctx wid liftE liftE $ \wrk -> handler $
        W.attachPrivateKeyFromPwd wrk (rootXPrv, pwd)
    fst <$> getWallet ctx mkLegacyWallet (ApiT wid)
  where
    kF = keyOfWallet wF
    wF = walletFlavor @s
    wid = WalletId
        $ digest kF
        $ publicKey kF rootXPrv

mkLegacyWallet
    :: forall ctx s .
        ( HasWorkerRegistry s ctx
        , HasDBFactory s ctx
        , KnownDiscovery s
        , HasNetworkLayer IO ctx
        , IsOurs s Address
        , IsOurs s RewardAccount
        )
    => ctx
    -> WalletId
    -> Wallet s
    -> WalletMetadata
    -> WalletDelegation
    -> Set Tx
    -> SyncProgress
    -> Handler ApiByronWallet
mkLegacyWallet ctx wid cp meta _ pending progress = do
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
        Just (WalletPassphraseInfo time EncryptWithScrypt) ->
            withWorkerCtx @_ @s ctx wid liftE liftE $ \wrk ->
                matchEmptyPassphrase (wrk ^. typed) <&> \case
                    Right{} -> Nothing
                    Left{} -> Just $ ApiWalletPassphraseInfo time

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

    matchEmptyPassphrase :: DBLayer IO s -> Handler (Either ErrWithRootKey ())
    matchEmptyPassphrase db = liftIO $ runExceptT $
        W.withRootKey @s db wid mempty Prelude.id (\_ _ -> pure ())

postRandomWallet
    :: forall ctx s k n.
        ( ctx ~ ApiLayer s
        , s ~ RndState n
        , k ~ ByronKey
        )
    => ctx
    -> ByronWalletPostData '[12,15,18,21,24]
    -> Handler ApiByronWallet
postRandomWallet ctx body = do
    s <- liftIO $ mkRndState rootXPrv <$> getStdRandom random
    postLegacyWallet ctx (rootXPrv, pwd) $ \wrk wid ->
        W.createWallet @_ @s genesisParams wrk wid wName s
  where
    wName    = body ^. #name . #getApiT
    seed     = body ^. #mnemonicSentence . #getApiMnemonicT
    pwd      = body ^. #passphrase . #getApiT
    pwdP     = preparePassphrase currentPassphraseScheme pwd
    rootXPrv = Byron.generateKeyFromSeed seed pwdP
    genesisParams = ctx ^. #netParams

postRandomWalletFromXPrv
    :: forall ctx s n.
        ( ctx ~ ApiLayer s
        , s ~ RndState n
        , HasNetworkLayer IO ctx
        )
    => ctx
    -> ByronWalletFromXPrvPostData
    -> Handler ApiByronWallet
postRandomWalletFromXPrv ctx body = do
    s <- liftIO $ mkRndState byronKey <$> getStdRandom random
    void $ liftHandler $ createWalletWorker @_ @s ctx wid
        (\wrk -> W.createWallet @_ @s
            genesisParams wrk wid wName s)
        idleWorker
    withWorkerCtx ctx wid liftE liftE $ \wrk -> handler $
        W.attachPrivateKeyFromPwdHashByron wrk (byronKey, pwd)
    fst <$> getWallet ctx mkLegacyWallet (ApiT wid)
  where
    wName = getApiT (body ^. #name)
    pwd   = getApiT (body ^. #passphraseHash)
    masterKey = getApiT (body ^. #encryptedRootPrivateKey)
    byronKey = mkByronKeyFromMasterKey masterKey
    wid = WalletId $ digest ByronKeyS $ publicKey ByronKeyS byronKey
    genesisParams = ctx ^. #netParams

postIcarusWallet
    :: forall ctx s k n.
        ( ctx ~ ApiLayer s
        , s ~ SeqState n k
        , k ~ IcarusKey
        , HasSNetworkId n
        )
    => ctx
    -> ByronWalletPostData '[12,15,18,21,24]
    -> Handler ApiByronWallet
postIcarusWallet ctx body = do
    postLegacyWallet ctx (rootXPrv, pwd) $ \wrk wid ->
        W.createIcarusWallet @s genesisParams wrk wid wName
            (RootCredentials rootXPrv pwdP)
  where
    wName    = body ^. #name . #getApiT
    seed     = body ^. #mnemonicSentence . #getApiMnemonicT
    pwd      = body ^. #passphrase . #getApiT
    pwdP     = preparePassphrase currentPassphraseScheme pwd
    rootXPrv = Icarus.generateKeyFromSeed seed pwdP
    genesisParams = ctx ^. #netParams

postTrezorWallet
    :: forall ctx s k n.
        ( ctx ~ ApiLayer s
        , s ~ SeqState n k
        , k ~ IcarusKey
        , HasSNetworkId n
        )
    => ctx
    -> ByronWalletPostData '[12,15,18,21,24]
    -> Handler ApiByronWallet
postTrezorWallet ctx body = do
    postLegacyWallet ctx (rootXPrv, pwd) $ \wrk wid ->
        W.createIcarusWallet @s genesisParams wrk wid wName
            (RootCredentials rootXPrv pwdP)
  where
    wName    = body ^. #name . #getApiT
    seed     = body ^. #mnemonicSentence . #getApiMnemonicT
    pwd      = body ^. #passphrase . #getApiT
    pwdP     = preparePassphrase currentPassphraseScheme pwd
    rootXPrv = Icarus.generateKeyFromSeed seed pwdP
    genesisParams = ctx ^. #netParams

postLedgerWallet
    :: forall ctx s k n.
        ( ctx ~ ApiLayer s
        , s ~ SeqState n k
        , k ~ IcarusKey
        , HasSNetworkId n
        )
    => ctx
    -> ByronWalletPostData '[12,15,18,21,24]
    -> Handler ApiByronWallet
postLedgerWallet ctx body = do
    postLegacyWallet ctx (rootXPrv, pwd) $ \wrk wid ->
        W.createIcarusWallet @s genesisParams wrk wid wName
            (RootCredentials rootXPrv pwdP)
  where
    wName    = body ^. #name . #getApiT
    mw       = body ^. #mnemonicSentence . #getApiMnemonicT
    pwd      = body ^. #passphrase . #getApiT
    pwdP     = preparePassphrase currentPassphraseScheme pwd
    rootXPrv = Icarus.generateKeyFromHardwareLedger mw pwdP
    genesisParams = ctx ^. #netParams

{-------------------------------------------------------------------------------
                             ApiLayer Discrimination
-------------------------------------------------------------------------------}

-- Legacy wallets like 'Byron Random' and 'Icarus Sequential' are handled
-- through the same API endpoints. However, they rely on different contexts.
-- Since they have identical ids, we actually lookup both contexts in sequence.
withLegacyLayer
    :: forall byron icarus n a.
        ( byron ~ ApiLayer (RndState n)
        , icarus ~ ApiLayer (SeqState n IcarusKey)
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
        ( byron ~ ApiLayer (RndState n)
        , icarus ~ ApiLayer (SeqState n IcarusKey)
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
        icarus
        wid
        onMissing
        onNotResponding
        (const withIcarus)

    tryByron onMissing onNotResponding = withWorkerCtx @_
        @(RndState n)
        byron
        wid
        onMissing
        onNotResponding
        (const withByron)

{-------------------------------------------------------------------------------
                                   Wallets
-------------------------------------------------------------------------------}

deleteWallet
    :: forall ctx s
     . ctx ~ ApiLayer s
    => ctx
    -> ApiT WalletId
    -> Handler NoContent
deleteWallet ctx (ApiT wid) = do
    -- Start a context so that an error is throw if the wallet doesn't exist.
    withWorkerCtx @_ @s ctx wid liftE
        (const $ pure()) (const $ pure ())

    liftIO $ Registry.unregister re wid
    liftIO $ removeDatabase df wid

    return NoContent
  where
    re = ctx ^. workerRegistry @s
    df = ctx ^. dbFactory @s

getWallet
    :: forall ctx s apiWallet.
        ( ctx ~ ApiLayer s
        , HasWorkerRegistry s ctx
        , HasDBFactory s ctx
        )
    => ctx
    -> MkApiWallet ctx s apiWallet
    -> ApiT WalletId
    -> Handler (apiWallet, UTCTime)
getWallet ctx mkApiWallet (ApiT wid) = do
    withWorkerCtx @_ @s ctx wid liftE whenNotResponding whenAlive
  where
    dbfa = ctx ^. dbFactory @s

    whenAlive wrk = do
        (cp, (meta, delegation), pending)
            <- handler $ W.readWallet wrk
        progress <- liftIO $ W.walletSyncProgress @_ @_ ctx cp
        (, meta ^. #creationTime)
            <$> mkApiWallet ctx wid cp meta delegation pending progress

    whenNotResponding _ = Handler $ ExceptT $ withDatabase dbfa wid
        $ \df -> runHandler $ do
            db <- liftHandler $ loadDBLayer df
            let wrk = hoistResource db (MsgFromWorker wid) ctx
            (cp, (meta, delegation), pending)
                <- handler $ W.readWallet wrk
            (, meta ^. #creationTime)
                <$> mkApiWallet ctx wid cp meta delegation pending NotResponding

listWallets
    :: forall ctx s apiWallet
     . ( ctx ~ ApiLayer s
       , NFData apiWallet
       )
    => ctx
    -> MkApiWallet ctx s apiWallet
    -> Handler [(apiWallet, UTCTime)]
listWallets ctx mkApiWallet = do
    wids <- liftIO $ listDatabases df
    liftIO $ sortOn snd . catMaybes <$> mapM maybeGetWallet (ApiT <$> wids)
  where
    df = ctx ^. dbFactory @s

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
    :: forall ctx s apiWallet
     . ctx ~ ApiLayer s
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
            handler $ W.updateWallet wrk (modify wName)
    fst <$> getWallet ctx mkApiWallet (ApiT wid)
  where
    modify :: W.WalletName -> WalletMetadata -> WalletMetadata
    modify wName meta = meta { name = wName }

putWalletPassphrase
    :: forall ctx s k .
        ( ctx ~ ApiLayer s
        , GetAccount s k
        , WalletFlavor s
        , KeyOf s ~ k
        , HardDerivation k
        )
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
                $ W.updateWalletPassphraseWithOldPassphrase
                    (walletFlavor @s) wrk wid (old, new)
        Right
            (Api.WalletPutPassphraseMnemonicData
                    (ApiMnemonicT mnemonic) sndFactor (ApiT new)
            ) -> do
            let encrPass = preparePassphrase currentPassphraseScheme new
                challengeKey = createKey
                    (mnemonic, getApiMnemonicT <$> sndFactor) encrPass
                challengPubKey = publicKey (keyFlavorFromState @s)
                    $ deriveAccountPrivateKey encrPass challengeKey minBound
            storedPubKey <- handler $ W.readAccountPublicKey wrk
            if getKey challengPubKey == getKey storedPubKey
                then handler $ W.updateWalletPassphraseWithMnemonic wrk
                        (challengeKey, new)
                else liftHandler
                    $ throwE
                    $ ErrUpdatePassphraseWithRootKey
                    $ ErrWithRootKeyWrongMnemonic wid
  where
    withWrk :: (WorkerCtx (ApiLayer s) -> Handler a) -> Handler a
    withWrk = withWorkerCtx ctx wid liftE liftE

putByronWalletPassphrase
    :: forall ctx s
     . ( WalletFlavor s
       , ctx ~ ApiLayer s
       )
    => ctx
    -> ApiT WalletId
    -> ByronWalletPutPassphraseData
    -> Handler NoContent
putByronWalletPassphrase ctx (ApiT wid) body = do
    let (ByronWalletPutPassphraseData oldM (ApiT new)) = body
    withWorkerCtx ctx wid liftE liftE $ \wrk -> liftHandler $ do
        let old = maybe mempty (coerce . getApiT) oldM
        W.updateWalletPassphraseWithOldPassphrase
            (walletFlavor @s) wrk wid (old, new)
    return NoContent

getUTxOsStatistics
    :: forall ctx s
     . ctx ~ ApiLayer s
    => ctx
    -> ApiT WalletId
    -> Handler ApiUtxoStatistics
getUTxOsStatistics ctx (ApiT wid) = do
    stats <- withWorkerCtx ctx wid liftE liftE $ \wrk -> handler $
        W.listUtxoStatistics wrk
    return $ toApiUtxoStatistics stats

getWalletUtxoSnapshot
    :: ApiLayer s
    -> ApiT WalletId
    -> Handler ApiWalletUtxoSnapshot
getWalletUtxoSnapshot ctx (ApiT wid) = do
    entries <- withWorkerCtx ctx wid liftE liftE $ \wrk -> handler $
        W.getWalletUtxoSnapshot wrk
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
    :: forall s n k.
        ( IsOurs s Address
        , WalletFlavor s
        , Excluding '[SharedKey] k
        , s ~ SeqState n k
        , AddressBookIso s
        , GenChange s
        )
    => ApiLayer s
    -> ArgGenChange s
    -> ApiT WalletId
    -> ApiSelectCoinsPayments n
    -> Handler (ApiCoinSelection n)
selectCoins ctx@ApiLayer {..} argGenChange (ApiT walletId) body = do
    withWorkerCtx ctx walletId liftE liftE $ \workerCtx -> do
        let db = workerCtx ^. dbLayer

        (Write.InAnyRecentEra _era pp, timeTranslation)
            <- liftIO $ W.readNodeTipStateForTxWrite netLayer

        withdrawal <-
            body ^. #withdrawal
                & maybe (pure NoWithdrawal)
                    (shelleyOnlyMkWithdrawal
                        netLayer
                        (txWitnessTagForKey $ keyOfWallet $ walletFlavor @s)
                        db)
        let genChange = W.defaultChangeAddressGen argGenChange
        let paymentOuts = NE.toList $ addressAmountToTxOut <$> body ^. #payments
        let txCtx = defaultTransactionCtx
                { txWithdrawal = withdrawal
                , txMetadata = getApiT <$> body ^. #metadata
                }

        (cardanoTx, walletState) <- liftIO $ W.buildTransaction @s
            db txLayer timeTranslation genChange pp txCtx paymentOuts

        let W.CoinSelection{..} =
                W.buildCoinSelectionForTransaction @s @n
                    walletState
                    paymentOuts
                    (W.getStakeKeyDeposit pp)
                    Nothing -- delegation action
                    cardanoTx

        pure ApiCoinSelection
            { inputs = mkApiCoinSelectionInput <$> inputs
            , outputs = mkApiCoinSelectionOutput <$> outputs
            , change = mkApiCoinSelectionChange <$> change
            , collateral = mkApiCoinSelectionCollateral <$> collateral
            , certificates = uncurry mkApiCoinSelectionCerts <$>
                delegationAction
            , withdrawals = mkApiCoinSelectionWithdrawal <$> withdrawals
            , depositsTaken = maybeToList $ Coin.toQuantity <$> deposit
            , depositsReturned = maybeToList $ Coin.toQuantity <$> refund
            , metadata = ApiBytesT. serialiseToCBOR
                <$> body ^? #metadata . traverse . #getApiT
            }

selectCoinsForJoin
    :: forall s n k.
        ( s ~ SeqState n k
        , WalletFlavor s
        , Excluding '[SharedKey] k
        , AddressBookIso s
        , Seq.SupportsDiscovery n k
        , DelegationAddress k 'CredFromKeyK
        )
    => ApiLayer s
    -> IO (Set PoolId)
       -- ^ Known pools
       -- We could maybe replace this with a @IO (PoolId -> Bool)@
    -> (PoolId -> IO PoolLifeCycleStatus)
    -> PoolId
    -> WalletId
    -> Handler (Api.ApiCoinSelection n)
selectCoinsForJoin ctx@ApiLayer{..}
    knownPools getPoolStatus poolId walletId = do
    --
    poolStatus <- liftIO $ getPoolStatus poolId
    pools <- liftIO knownPools
    (Write.InAnyRecentEra _era pp, timeTranslation)
        <- liftIO $ W.readNodeTipStateForTxWrite netLayer
    withWorkerCtx ctx walletId liftE liftE $ \workerCtx -> liftIO $ do
        let db = workerCtx ^. typed @(DBLayer IO s)
        currentEpochSlotting <- liftIO $ getCurrentEpochSlotting netLayer
        action <- liftIO $ WD.joinStakePoolDelegationAction @s
            (contramap MsgWallet $ workerCtx ^. logger)
            db
            currentEpochSlotting
            pools
            poolId
            poolStatus
        let changeAddrGen = W.defaultChangeAddressGen (delegationAddressS @n)

        let txCtx = defaultTransactionCtx { txDelegationAction = Just action }

        let paymentOuts = []

        (cardanoTx, walletState) <- W.buildTransaction @s
            db txLayer timeTranslation changeAddrGen pp txCtx
            paymentOuts

        let W.CoinSelection{..} =
                W.buildCoinSelectionForTransaction @s @n
                    walletState
                    paymentOuts
                    (W.getStakeKeyDeposit pp)
                    (Just action)
                    cardanoTx

        pure ApiCoinSelection
            { inputs = mkApiCoinSelectionInput <$> inputs
            , outputs = mkApiCoinSelectionOutput <$> outputs
            , change = mkApiCoinSelectionChange <$> change
            , collateral = mkApiCoinSelectionCollateral <$> collateral
            , certificates = uncurry mkApiCoinSelectionCerts <$>
                delegationAction
            , withdrawals = mkApiCoinSelectionWithdrawal <$> withdrawals
            , depositsTaken = maybeToList $ Coin.toQuantity <$> deposit
            , depositsReturned = maybeToList $ Coin.toQuantity <$> refund
            , metadata = Nothing
            }

selectCoinsForQuit
    :: forall s n k.
        ( s ~ SeqState n k
        , WalletFlavor s
        , Excluding '[SharedKey] k
        , AddressBookIso s
        , Seq.SupportsDiscovery n k
        , DelegationAddress k 'CredFromKeyK
        )
    => ApiLayer (SeqState n k)
    -> ApiT WalletId
    -> Handler (ApiCoinSelection n)
selectCoinsForQuit ctx@ApiLayer{..} (ApiT walletId) = do
    (Write.InAnyRecentEra _era pp, timeTranslation)
        <- liftIO $ W.readNodeTipStateForTxWrite netLayer
    withWorkerCtx ctx walletId liftE liftE $ \workerCtx -> liftIO $ do
        let db = workerCtx ^. typed @(DBLayer IO s)
        withdrawal <- W.shelleyOnlyMkSelfWithdrawal
            netLayer
            (txWitnessTagForKey $ keyOfWallet $ walletFlavor @s)
            db
        currentEpochSlotting <- liftIO $ getCurrentEpochSlotting netLayer
        action <- WD.quitStakePoolDelegationAction
            db currentEpochSlotting withdrawal
        let changeAddrGen = W.defaultChangeAddressGen (delegationAddressS @n)
        let txCtx = defaultTransactionCtx
                { txDelegationAction = Just action
                , txWithdrawal = withdrawal
                }

        let paymentOuts = []

        (cardanoTx, walletState) <- W.buildTransaction @s
            db txLayer timeTranslation changeAddrGen pp txCtx paymentOuts

        let W.CoinSelection{..} =
                W.buildCoinSelectionForTransaction @s @n
                    walletState
                    paymentOuts
                    (W.getStakeKeyDeposit pp)
                    (Just action)
                    cardanoTx

        pure ApiCoinSelection
            { inputs = mkApiCoinSelectionInput <$> inputs
            , outputs = mkApiCoinSelectionOutput <$> outputs
            , change = mkApiCoinSelectionChange <$> change
            , collateral = mkApiCoinSelectionCollateral <$> collateral
            , certificates = uncurry mkApiCoinSelectionCerts <$>
                delegationAction
            , withdrawals = mkApiCoinSelectionWithdrawal <$> withdrawals
            , depositsTaken = maybeToList $ Coin.toQuantity <$> deposit
            , depositsReturned = maybeToList $ Coin.toQuantity <$> refund
            , metadata = Nothing
            }

{-------------------------------------------------------------------------------
                                     Assets
-------------------------------------------------------------------------------}

data ErrGetAsset
     = ErrGetAssetNotPresent
    deriving (Eq, Show)

-- | All assets associated with this wallet, and their metadata (if metadata is
-- available). This list may include assets which have already been spent.
listAssets
    :: forall ctx s
     . ( ctx ~ ApiLayer s
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
    :: forall s k . (IsOurs s Address, k ~ KeyOf s) =>
    ApiLayer s -> ApiT WalletId -> Handler (Set AssetId)
listAssetsBase ctx (ApiT wallet) =
    withWorkerCtx ctx wallet liftE liftE $ \wctx ->
        handler $ W.listAssets wctx

-- | Look up a single asset and its metadata.
--
-- NOTE: This is slightly inefficient because it greps through the transaction
-- history to check if the asset is associated with this wallet.
getAsset
    :: forall ctx s .
        ( ctx ~ ApiLayer s
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
    :: forall ctx s .
        ( ctx ~ ApiLayer s
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
    :: forall ctx s n.
        ( s ~ RndState n
        , ctx ~ ApiLayer s
        , HasSNetworkId n
        )
    => ctx
    -> ApiT WalletId
    -> ApiPostRandomAddressData
    -> Handler (ApiAddressWithPath n)
postRandomAddress ctx (ApiT wid) body = do
    let pwd = coerce $ getApiT $ body ^. #passphrase
    let mix = getApiT <$> (body ^. #addressIndex)
    (addr, path) <- withWorkerCtx ctx wid liftE liftE
        $ \wrk -> liftHandler $ W.createRandomAddress @_ @n wrk wid pwd mix
    pure $ coerceAddress (addr, Unused, path)
  where
    coerceAddress (a, s, p) =
        ApiAddressWithPath (ApiAddress @n a) (ApiT s) (NE.map ApiT p)

putRandomAddress
    :: forall ctx s n.
        ( s ~ RndState n
        , ctx ~ ApiLayer s
        )
    => ctx
    -> ApiT WalletId
    -> ApiAddress n
    -> Handler NoContent
putRandomAddress ctx (ApiT wid) (ApiAddress addr)  = do
    withWorkerCtx ctx wid liftE liftE
        $ \wrk -> liftHandler $ W.importRandomAddresses wrk [addr]
    pure NoContent

putRandomAddresses
    :: forall ctx s n
     . ( s ~ RndState n
       , ctx ~ ApiLayer s
       )
    => ctx
    -> ApiT WalletId
    -> ApiPutAddressesData n
    -> Handler NoContent
putRandomAddresses ctx (ApiT wid) (ApiPutAddressesData addrs)  = do
    withWorkerCtx ctx wid liftE liftE
        $ \wrk -> liftHandler $ W.importRandomAddresses wrk addrs'
    pure NoContent
  where
    addrs' = apiAddress <$> addrs

listAddresses
    :: forall ctx s n.
        ( ctx ~ ApiLayer s
        , CompareDiscovery s
        , KnownAddresses s
        )
    => ctx
    -> (s -> Address -> Maybe Address)
    -> ApiT WalletId
    -> Maybe (ApiT AddressState)
    -> Handler [ApiAddressWithPath n]
listAddresses ctx normalize (ApiT wid) stateFilter = do
    addrs <- withWorkerCtx ctx wid liftE liftE $ \wrk -> handler $
        W.listAddresses wrk normalize
    return $ coerceAddress <$> filter filterCondition addrs
  where
    filterCondition :: (Address, AddressState, NonEmpty DerivationIndex) -> Bool
    filterCondition = case stateFilter of
        Nothing -> const True
        Just (ApiT s) -> \(_,state,_) -> (state == s)
    coerceAddress (a, s, p) =
        ApiAddressWithPath (ApiAddress @n a) (ApiT s) (NE.map ApiT p)

{-------------------------------------------------------------------------------
                                    Transactions
-------------------------------------------------------------------------------}

signTransaction
    :: forall s k
     .  ( Bounded (Index (AddressIndexDerivationType k) (AddressCredential k))
        , HardDerivation k
        , WalletFlavor s
        , KeyOf s ~ k
        , AccountIxForStaking s
        , IsOurs s Address
        , HasSNetworkId (NetworkOf s)
        )
    => ApiLayer s
    -> ApiT WalletId
    -> ApiSignTransactionPostData
    -> Handler ApiSerialisedTransaction
signTransaction ctx (ApiT wid) body = do
    let pwd = coerce $ body ^. #passphrase . #getApiT
        wF = walletFlavor @s
    sealedTx' <- withWorkerCtx ctx wid liftE liftE $ \wrk -> liftHandler $ do
        let
            db = wrk ^. W.dbLayer @IO @s
            tl = wrk ^. W.transactionLayer @k
            nl = wrk ^. W.networkLayer
        db & \W.DBLayer{atomically, readCheckpoint} ->
            W.withRootKey @s db wid pwd ErrWitnessTxWithRootKey
                $ \rootK scheme -> lift $ do
                    cp <- atomically readCheckpoint
                    let
                        pwdP :: Passphrase "encryption"
                        pwdP = preparePassphrase scheme pwd

                        utxo :: UTxO.UTxO
                        utxo = totalUTxO mempty cp

                        keyLookup = isOwned wF (getState cp) (rootK, pwdP)

                        accIxForStakingM :: Maybe (Index 'Hardened 'AccountK)
                        accIxForStakingM = getAccountIx @s (getState cp)

                        witCountCtx = shelleyOrShared wF
                            AnyWitnessCountCtx $ \flavor ->
                                toWitnessCountCtx flavor (getState cp)

                    era <- liftIO $ NW.currentNodeEra nl
                    let sealedTx = body ^. #transaction . #getApiT
                    pure $ W.signTransaction
                        (keyFlavorFromState @s)
                        tl era witCountCtx keyLookup
                        Nothing (RootCredentials rootK pwdP) utxo accIxForStakingM sealedTx

    -- TODO: The body+witnesses seem redundant with the sealedTx already. What's
    -- the use-case for having them provided separately? In the end, the client
    -- should be able to decouple them if they need to.
    pure $ ApiSerialisedTransaction (ApiT sealedTx') $
        case body ^. #encoding of
            Just HexEncoded -> HexEncoded
            _otherEncodings -> Base64Encoded

postTransactionOld
    :: forall ctx s n k.
        ( ctx ~ ApiLayer s
        , GenChange s
        , HardDerivation k
        , HasNetworkLayer IO ctx
        , Bounded (Index (AddressIndexDerivationType k) (AddressCredential k))
        , AddressBookIso s
        , HasDelegation s
        , IsOurs s Address
        , IsOurs s RewardAccount
        , WalletFlavor s
        , Excluding '[SharedKey] k
        , KeyOf s ~ k
        , CredFromOf s ~ 'CredFromKeyK
        , HasSNetworkId (NetworkOf s)
        , n ~ NetworkOf s
        )
    => ctx
    -> ArgGenChange s
    -> ApiT WalletId
    -> PostTransactionOldData n
    -> Handler (ApiTransaction n)
postTransactionOld ctx@ApiLayer{..} argGenChange (ApiT wid) body = do
    let outs = addressAmountToTxOut <$> body ^. #payments
    let md = body ^? #metadata . traverse . #txMetadataWithSchema_metadata
    let mTTL = body ^? #timeToLive . traverse . #getQuantity
    withWorkerCtx ctx wid liftE liftE $ \wrk -> do
        let db = wrk ^. dbLayer
        ttl <- liftIO $ W.transactionExpirySlot ti mTTL
        wdrl <- case body ^. #withdrawal of
            Nothing -> pure NoWithdrawal
            Just apiWdrl ->
                shelleyOnlyMkWithdrawal
                    netLayer
                    (txWitnessTagForKey $ keyOfWallet $ walletFlavor @s)
                    db
                    apiWdrl
        let txCtx = defaultTransactionCtx
                { txWithdrawal = wdrl
                , txMetadata = md
                , txValidityInterval = (Nothing, ttl)
                }
        (BuiltTx{..}, txTime) <- atomicallyWithHandler
            (ctx ^. walletLocks) (PostTransactionOld wid) $ liftIO $
                W.buildSignSubmitTransaction @s
                    db
                    netLayer
                    txLayer
                    (coerce $ getApiT $ body ^. #passphrase)
                    wid
                    (W.defaultChangeAddressGen argGenChange)
                    (PreSelection $ NE.toList outs)
                    txCtx

        pp <- liftIO $ NW.currentProtocolParameters netLayer
        mkApiTransaction
            (timeInterpreter netLayer)
            wrk
            #pendingSince
            MkApiTransactionParams
                { txId = builtTx ^. #txId
                , txFee = builtTx ^. #fee
                , txInputs = builtTx ^. #resolvedInputs
                , txCollateralInputs = []
                , txOutputs = builtTx ^. #outputs
                , txCollateralOutput = builtTx ^. #collateralOutput
                , txWithdrawals = builtTx ^. #withdrawals
                , txMeta = builtTxMeta
                , txMetadata = builtTx ^. #metadata
                , txTime
                , txScriptValidity = builtTx ^. #scriptValidity
                , txDeposit = W.stakeKeyDeposit pp
                , txMetadataSchema = TxMetadataDetailedSchema
                , txCBOR = builtTx ^. #txCBOR
                }
  where
    ti :: TimeInterpreter (ExceptT PastHorizonException IO)
    ti = timeInterpreter (ctx ^. networkLayer)

deleteTransaction
    :: forall ctx s
     . ctx ~ ApiLayer s
    => ctx
    -> ApiT WalletId
    -> ApiTxId
    -> Handler NoContent
deleteTransaction ctx (ApiT wid) (ApiTxId (ApiT (tid))) = do
    withWorkerCtx ctx wid liftE liftE $ \wrk -> liftHandler $
        W.forgetTx wrk tid
    return NoContent

listTransactions
    :: forall s n
     . ( HasDelegation s
       , WalletFlavor s
       )
    => ApiLayer s
    -> ApiT WalletId
    -> Maybe MinWithdrawal
    -> Maybe Iso8601Time
    -> Maybe Iso8601Time
    -> Maybe (ApiT SortOrder)
    -> Maybe ApiLimit
    -> Maybe (ApiAddress n)
    -> TxMetadataSchema
    -> Handler [ApiTransaction n]
listTransactions
    ctx (ApiT wid) mMinWithdrawal mStart mEnd mOrder mLimit mAddress metadataSchema =
        withWorkerCtx ctx wid liftE liftE $ \wrk -> do
            txs <- liftHandler $
                W.listTransactions wrk
                (Coin . fromIntegral . getMinWithdrawal <$> mMinWithdrawal)
                (getIso8601Time <$> mStart)
                (getIso8601Time <$> mEnd)
                (maybe defaultSortOrder getApiT mOrder)
                (fromApiLimit <$> mLimit)
                (apiAddress <$> mAddress)
            depo <- liftIO $ W.stakeKeyDeposit <$>
                NW.currentProtocolParameters (wrk ^. networkLayer)
            forM txs $ \tx ->
                mkApiTransactionFromInfo
                    (timeInterpreter (ctx ^. networkLayer))
                    wrk
                    depo
                    tx
                    metadataSchema
  where
    defaultSortOrder :: SortOrder
    defaultSortOrder = Descending

getTransaction
    :: forall s n
     . ( HasDelegation s
       , WalletFlavor s
       )
    => ApiLayer s
    -> ApiT WalletId
    -> ApiTxId
    -> TxMetadataSchema
    -> Handler (ApiTransaction n)
getTransaction ctx (ApiT wid) (ApiTxId (ApiT (tid))) metadataSchema =
    withWorkerCtx ctx wid liftE liftE $ \wrk -> do
        tx <- liftHandler $ W.getTransaction wrk tid
        depo <- liftIO $ W.stakeKeyDeposit <$>
            NW.currentProtocolParameters (wrk ^. networkLayer)
        mkApiTransactionFromInfo
                (timeInterpreter (ctx ^. networkLayer)) wrk depo tx
                metadataSchema

-- Populate an API transaction record with 'TransactionInfo' from the wallet
-- layer.
mkApiTransactionFromInfo
    :: ( HasDelegation s
       , WalletFlavor s
       )
    => TimeInterpreter (ExceptT PastHorizonException IO)
    -> W.WalletLayer IO s
    -> Coin
    -> TransactionInfo
    -> TxMetadataSchema
    -> Handler (ApiTransaction n)
mkApiTransactionFromInfo ti wrk deposit info metadataSchema = do
    apiTx <- mkApiTransaction
        ti wrk status
        MkApiTransactionParams
            { txId = info ^. #txInfoId
            , txFee = info ^. #txInfoFee
            , txInputs = info ^. #txInfoInputs
            , txCollateralInputs = info ^. #txInfoCollateralInputs
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
    status :: Lens' (ApiTransaction n) (Maybe ApiBlockReference)
    status = case info ^. #txInfoMeta . #status of
        Pending  -> #pendingSince
        InLedger -> #insertedAt
        Expired  -> #pendingSince

postTransactionFeeOld
    :: forall s n k
     . ( WalletFlavor s
       , Excluding '[SharedKey] k
       , AddressBookIso s
       , k ~ KeyOf s
       , CredFromOf s ~ 'CredFromKeyK
       )
    => ApiLayer s
    -> ApiT WalletId
    -> PostTransactionFeeOldData n
    -> Handler ApiFee
postTransactionFeeOld ctx@ApiLayer{..} (ApiT walletId) body = do
    (Write.InAnyRecentEra era pp, timeTranslation)
        <- liftIO $ W.readNodeTipStateForTxWrite netLayer

    -- Needed only for calcMinimumCoinValues. We'd ideally use the ledger @pp@
    -- above instead.
    walletPP <- liftIO $ currentProtocolParameters netLayer

    let mTTL = body ^? #timeToLive . traverse . #getQuantity
    withWorkerCtx ctx walletId liftE liftE $ \workerCtx -> do
        let db = workerCtx ^. dbLayer
        ttl <- liftIO $ W.transactionExpirySlot (timeInterpreter netLayer) mTTL
        wdrl <- case body ^. #withdrawal of
            Nothing -> pure NoWithdrawal
            Just apiWdrl ->
                shelleyOnlyMkWithdrawal
                    netLayer
                    (txWitnessTagForKey $ keyOfWallet $ walletFlavor @s)
                    db
                    apiWdrl
        let outputs = F.toList $ addressAmountToTxOut <$> body ^. #payments
            minCoins = W.calcMinimumCoinValues walletPP txLayer
                <$> outputs
        feePercentiles <- liftIO $ W.transactionFee @s
            db
            pp
            txLayer
            timeTranslation
            dummyChangeAddressGen
            defaultTransactionCtx
                { txWithdrawal = wdrl
                , txMetadata = body
                    ^? #metadata . traverse . #txMetadataWithSchema_metadata
                , txValidityInterval = (Nothing, ttl)
                }
            PreSelection{outputs}
        pure
            $ mkApiFee Nothing minCoins
            $ W.padFeePercentiles
                (Write.getFeePerByte era $ Write.pparamsLedger pp)
                padding
                feePercentiles
  where
    -- Padding to make the fee percentiles more imprecise, for the following
    -- reasons:
    --
    -- 1. dummyChangeAddressGen uses the longest possbile addresses. For byron
    -- wallets they are 83 bytes long, which is longer than what we'd
    -- expect in reality, and longer than the shortest Byron address we'd expect
    -- of 66 bytes. I.e. we could be off by up to 17 bytes.
    --
    -- 2. Our integration tests often assume that transaction fees are bounded
    -- absolutely by the estimate intervals provided by postTransactionFee.
    -- This assumption is flawed, since each estimate interval is merely a pair
    -- of 10- and 90-th percentile values: these are not absolute bounds.
    --
    -- But for the moment, it's still useful for integration tests to expect
    -- that eventual fees are bounded by prior fee estimates: during the risky
    -- transition to balanceTx they will compare the result of a rewritten
    -- version of postTransactionFee with the result of an unmodified version
    -- of postTransaction. They may also act as golden tests in lieu of
    -- better/more tests on the unit level. A small amount of padding is a
    -- cheap way to keep these checks.
    --
    -- In the context of a mainnet `minfeeA` value of 44 lovelace/byte the
    -- padding is negligible - less than 1/1000 ada.
    padding :: Quantity "byte" Word
    padding = Quantity 20

constructTransaction
    :: forall s n
     . (HasSNetworkId n, s ~ SeqState n ShelleyKey)
    => ApiLayer s
    -> ArgGenChange s
    -> IO (Set PoolId)
    -> (PoolId -> IO PoolLifeCycleStatus)
    -> ApiT WalletId
    -> ApiConstructTransactionData n
    -> Handler (ApiConstructTransaction n)
constructTransaction api argGenChange knownPools poolStatus apiWalletId body = do
    body & \(ApiConstructTransactionData _ _ _ _ _ _ _ _) ->
    -- Above is the way to get a compiler error when number of fields changes,
    -- in order not to forget to update the pattern below:
        case body of
            ApiConstructTransactionData
                { payments = Nothing
                , withdrawal = Nothing
                , metadata = Nothing
                , mintBurn = Nothing
                , delegations = Nothing
                } -> liftHandler $ throwE ErrConstructTxWrongPayload
            _ -> pure ()

    validityInterval <-
        liftHandler $ parseValidityInterval ti $ body ^. #validityInterval

    mintBurnData <-
        liftHandler $ except $ parseMintBurnData body validityInterval

    delegationRequest <-
        liftHandler $ traverse parseDelegationRequest $ body ^. #delegations

    when (isJust $ body ^. #referencePolicyScriptTemplate) $
        liftHandler $ throwE ErrConstructTxNotImplemented

    let metadata =
            body ^? #metadata . traverse . #txMetadataWithSchema_metadata

    withWorkerCtx api walletId liftE liftE $ \wrk -> do
        let db = wrk ^. dbLayer
            netLayer = wrk ^. networkLayer
            txLayer = wrk ^. transactionLayer @_ @'CredFromKeyK
            trWorker = MsgWallet >$< wrk ^. logger

        (Write.InAnyRecentEra (_era :: Write.RecentEra era) pp, _)
            <- liftIO $ W.readNodeTipStateForTxWrite netLayer

        withdrawal <- case body ^. #withdrawal of
            Just SelfWithdraw -> liftIO $
                W.shelleyOnlyMkSelfWithdrawal
                    netLayer
                    (txWitnessTagForKey $ keyOfWallet $ walletFlavor @s)
                    db
            _ -> pure NoWithdrawal

        let transactionCtx0 = defaultTransactionCtx
                { txWithdrawal = withdrawal
                , txMetadata = metadata
                , txValidityInterval = first Just validityInterval
                }
        currentEpochSlotting <- liftIO $ getCurrentEpochSlotting netLayer
        optionalDelegationAction <- liftHandler $
            forM delegationRequest $
                WD.handleDelegationRequest
                    trWorker
                    db currentEpochSlotting knownPools
                    poolStatus withdrawal

        let transactionCtx1 =
                case optionalDelegationAction of
                    Nothing -> transactionCtx0
                    Just action ->
                        transactionCtx0 { txDelegationAction = Just action }

        (transactionCtx2, policyXPubM) <-
            if isJust mintBurnData then do
                (policyXPub, _) <-
                    liftHandler $ W.readPolicyPublicKey wrk
                let isMinting (ApiMintBurnDataFromScript _ _ (ApiMint _)) = True
                    isMinting _ = False
                let getMinting = \case
                        ApiMintBurnDataFromScript
                            (ApiT scriptT)
                            (Just (ApiT tName))
                            (ApiMint (ApiMintData _ amt)) ->
                            toTokenMapAndScript ShelleyKeyS
                                scriptT
                                (Map.singleton (Cosigner 0) policyXPub)
                                tName
                                amt
                        _ -> error "getMinting should not be used in this way"
                let getBurning = \case
                        ApiMintBurnDataFromScript
                            (ApiT scriptT)
                            (Just (ApiT tName))
                            (ApiBurn (ApiBurnData amt)) ->
                            toTokenMapAndScript ShelleyKeyS
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
                        NE.toList $ fromJust mintBurnData
                let burningData =
                        toTokenMap &&& toScriptTemplateMap $
                        map getBurning $
                        filter (not . isMinting) $
                        NE.toList $ fromJust mintBurnData
                pure ( transactionCtx1
                    { txAssetsToMint = mintingData
                    , txAssetsToBurn = burningData
                    }
                    , Just policyXPub)
            else
                pure (transactionCtx1, Nothing)

        outs <- case body ^. #payments of
            Nothing -> pure []
            Just (ApiPaymentAddresses content) ->
                pure $ F.toList (addressAmountToTxOut <$> content)

        let mintWithAddress
                (ApiMintBurnDataFromScript _ _ (ApiMint (ApiMintData (Just _) _)))
                = True
            mintWithAddress _ = False
        let mintingOuts = case mintBurnData of
                Just mintBurns ->
                    coalesceTokensPerAddr $
                    map (toMintTxOut (fromJust policyXPubM)) $
                    filter mintWithAddress $
                    NE.toList mintBurns
                Nothing -> []

        unbalancedTx <- liftHandler $
            W.constructTransaction @n @'CredFromKeyK @era
                txLayer db transactionCtx2
                    PreSelection { outputs = outs <> mintingOuts }

        balancedTx <-
            balanceTransaction
                api
                argGenChange
                (utxoAssumptionsForWallet (walletFlavor @s))
                apiWalletId
                ApiBalanceTransactionPostData
                    { transaction = ApiT (sealedTxFromCardanoBody unbalancedTx)
                    , inputs = []
                    , redeemers = []
                    , encoding = body ^. #encoding
                    }

        apiDecoded <- decodeTransaction @_ @n api apiWalletId balancedTx

        (_, _, rewardPath) <- handler $ W.readRewardAccount @s db

        let deposits = case txDelegationAction transactionCtx2 of
                Just (JoinRegisteringKey _poolId) -> [W.getStakeKeyDeposit pp]
                _ -> []

        let refunds = case txDelegationAction transactionCtx2 of
                Just Quit -> [W.getStakeKeyDeposit pp]
                _ -> []

        pure ApiConstructTransaction
            { transaction = balancedTx
            , coinSelection = mkApiCoinSelection
                deposits
                refunds
                ((,rewardPath) <$> transactionCtx2 ^. #txDelegationAction)
                metadata
                (unsignedTx rewardPath (outs ++ mintingOuts) apiDecoded)
            , fee = apiDecoded ^. #fee
            }
  where
    ti :: TimeInterpreter (ExceptT PastHorizonException IO)
    ti = timeInterpreter (api ^. networkLayer)

    walletId = getApiT apiWalletId

    parseMintBurnData
        :: ApiConstructTransactionData n
        -> (SlotNo, SlotNo)
        -> Either ErrConstructTx (Maybe (NonEmpty (ApiMintBurnDataFromScript n)))
    parseMintBurnData tx validity = do
        when (notAllFromScript (tx ^. #mintBurn)) $
            Left ErrConstructTxNotImplemented
        let mbMintingBurning :: Maybe (NonEmpty (ApiMintBurnDataFromScript n))
            mbMintingBurning =
                fmap (handleMissingAssetName . takeMintingFromScript)
                <$> tx ^. #mintBurn
        for mbMintingBurning $ \mintBurnData -> do
            guardWrongMintingTemplate mintBurnData
            guardAssetNameTooLong mintBurnData
            guardAssetQuantityOutOfBounds mintBurnData
            guardOutsideValidityInterval validity mintBurnData
            Right mintBurnData
      where
        notAllFromScript = \case
            Nothing -> False
            Just mintData ->
                any isRight $ mintBurnData <$> NE.toList mintData

        -- we checked that only left are present in preceding line
        takeMintingFromScript (ApiMintBurnData mintData) =
            fromLeft' mintData

        handleMissingAssetName :: ApiMintBurnDataFromScript n -> ApiMintBurnDataFromScript n
        handleMissingAssetName mb = case mb ^. #assetName of
            Nothing -> mb {assetName = Just (ApiT nullTokenName)}
            Just _ -> mb

        guardWrongMintingTemplate
            :: NonEmpty (ApiMintBurnDataFromScript n) -> Either ErrConstructTx ()
        guardWrongMintingTemplate mintBurnData =
            when (any wrongMintingTemplate mintBurnData)
                $ Left ErrConstructTxWrongMintingBurningTemplate
          where
            wrongMintingTemplate (ApiMintBurnDataFromScript (ApiT script) _ _) =
                isLeft (validateScriptOfTemplate RecommendedValidation script)
                || countCosigners script /= (1 :: Int)
                || existsNonZeroCosigner script
            countCosigners = foldScript (const (+ 1)) 0
            existsNonZeroCosigner =
                foldScript (\cosigner a -> a || cosigner /= Cosigner 0) False

        guardAssetNameTooLong
            :: NonEmpty (ApiMintBurnDataFromScript n) -> Either ErrConstructTx ()
        guardAssetNameTooLong mintBurnData =
            when (any assetNameTooLong mintBurnData)
                $ Left ErrConstructTxAssetNameTooLong
          where
            assetNameTooLong = \case
                ApiMintBurnDataFromScript _ (Just (ApiT (UnsafeTokenName bs))) _ ->
                    BS.length bs > tokenNameMaxLength
                _ -> error "tokenName should be nonempty at this step"

        guardAssetQuantityOutOfBounds
            :: NonEmpty (ApiMintBurnDataFromScript n) -> Either ErrConstructTx ()
        guardAssetQuantityOutOfBounds mintBurnData =
            when (any assetQuantityOutOfBounds mintBurnData)
                $ Left ErrConstructTxMintOrBurnAssetQuantityOutOfBounds
          where
            assetQuantityOutOfBounds = \case
                ApiMintBurnDataFromScript _ _ (ApiMint (ApiMintData _ amt)) ->
                    amt <= 0 || amt > unTokenQuantity txMintBurnMaxTokenQuantity
                ApiMintBurnDataFromScript _ _ (ApiBurn (ApiBurnData amt)) ->
                    amt <= 0 || amt > unTokenQuantity txMintBurnMaxTokenQuantity

        guardOutsideValidityInterval
            :: (SlotNo, SlotNo)
            -> NonEmpty (ApiMintBurnDataFromScript n)
            -> Either ErrConstructTx ()
        guardOutsideValidityInterval (before, hereafter) mintBurnData =
            when (any notWithinValidityInterval mintBurnData) $
                Left ErrConstructTxValidityIntervalNotWithinScriptTimelock
          where
            notWithinValidityInterval (ApiMintBurnDataFromScript (ApiT script) _ _) =
                not $ withinSlotInterval before hereafter $
                    scriptSlotIntervals script

    unsignedTx path initialOuts decodedTx = UnsignedTx
        { unsignedCollateral =
            mapMaybe toUnsignedTxInp (decodedTx ^. #collateral)
        , unsignedInputs =
            mapMaybe toUnsignedTxInp (decodedTx ^. #inputs)
        , unsignedOutputs =
            -- HACK: we leverage that balanceTx will append change outputs after
            -- the initial outputs to tell them apart. 'List.\\' does not work
            -- when balanceTx may change ada-quantities of initial inputs.
            take (length initialOuts)
                $ map toUnsignedTxOut (decodedTx ^. #outputs)
        , unsignedChange =
            -- HACK: we leverage that balanceTx will append change outputs after
            -- the initial outputs to tell them apart. 'List.\\' does not work
            -- when balanceTx may change ada-quantities of initial inputs.
            drop (length initialOuts)
                $ map toUnsignedTxChange (decodedTx ^. #outputs)
        , unsignedWithdrawals =
            mapMaybe (toUsignedTxWdrl path) (decodedTx ^. #withdrawals)
        }

    toMintTxOut policyXPub
        (ApiMintBurnDataFromScript (ApiT scriptT) (Just (ApiT tName))
            (ApiMint (ApiMintData (Just addr) amt))) =
                let (assetId, tokenQuantity, _) =
                        toTokenMapAndScript ShelleyKeyS
                            scriptT (Map.singleton (Cosigner 0) policyXPub)
                            tName amt
                    assets = fromFlatList [(assetId, tokenQuantity)]
                in
                (addr, assets)
    toMintTxOut _ _ = error $ unwords
        [ "toMintTxOut can only be used in the minting context with addr"
        , "specified"
        ]

    coalesceTokensPerAddr =
        let toTxOut (addr, assets) =
                addressAmountToTxOut $
                AddressAmount addr (Quantity 0) (ApiT assets)
        in
        map toTxOut
            . Map.toList
            . foldr (uncurry (Map.insertWith (<>))) Map.empty

toUsignedTxWdrl
    :: c -> ApiWithdrawalGeneral n -> Maybe (RewardAccount, Coin, c)
toUsignedTxWdrl p = \case
    ApiWithdrawalGeneral (ApiRewardAccount rewardAcc) amount Our ->
        Just (rewardAcc, Coin.fromQuantity amount, p)
    ApiWithdrawalGeneral _ _ External ->
        Nothing

toUnsignedTxOut :: ApiTxOutputGeneral n -> TxOut
toUnsignedTxOut = \case
    WalletOutput o ->
        let address = apiAddress (o ^. #address)
            coin = Coin.fromQuantity (o ^. #amount)
            assets = getApiT (o ^. #assets)
        in
            TxOut address (TokenBundle coin assets)
    ExternalOutput o ->
        let address = apiAddress (o ^. #address)
            coin = Coin.fromQuantity (o ^. #amount)
            assets = getApiT (o ^. #assets)
        in
            TxOut address (TokenBundle coin assets)

toUnsignedTxInp
    :: ApiTxInputGeneral n
    -> Maybe (TxIn, TxOut, NonEmpty DerivationIndex)
toUnsignedTxInp = \case
    WalletInput i ->
        let txId = getApiT (i ^. #id)
            index = i ^. #index
            address = apiAddress (i ^. #address)
            derivationPath = fmap getApiT (i ^. #derivationPath)
            coin = Coin.fromQuantity (i ^. #amount)
            assets = getApiT (i ^. #assets)
            txIn = TxIn txId index
            txOut = TxOut address (TokenBundle coin assets)
        in
        Just (txIn, txOut, derivationPath)
    ExternalInput _ ->
        Nothing

toUnsignedTxChange
    :: ApiTxOutputGeneral n
    -> TxChange (NonEmpty DerivationIndex)
toUnsignedTxChange = \case
    WalletOutput o ->
        let address = apiAddress (o ^. #address)
            derivationPath = fmap getApiT (o ^. #derivationPath)
            coin = Coin.fromQuantity (o ^. #amount)
            assets = getApiT (o ^. #assets)
        in
            TxChange address coin assets derivationPath
    ExternalOutput _ ->
        error "constructTx.toUnsignedTxChange: change should always be ours"

parseDelegationRequest
    :: NonEmpty ApiMultiDelegationAction
    -> ExceptT ErrConstructTx IO WD.DelegationRequest
parseDelegationRequest (action :| otherActions) = except $
    case otherActions of
        [] -> case action of
           Joining (ApiT pool) stakeKeyIdx | isValidKeyIdx stakeKeyIdx ->
                Right $ WD.Join pool
           Leaving stakeKeyIdx | isValidKeyIdx stakeKeyIdx ->
                Right WD.Quit
           _ -> Left ErrConstructTxMultiaccountNotSupported
        -- Current limitation:
        -- at this moment we are handling just one delegation action:
        -- either joining pool, or rejoining or quitting
        -- When we support multi-account this should be lifted
        _ -> Left ErrConstructTxMultidelegationNotSupported
  where
    isValidKeyIdx (ApiStakeKeyIndex (ApiT derIndex)) =
        derIndex == DerivationIndex (getIndex @'Hardened minBound)

parseValidityInterval
    :: TimeInterpreter (ExceptT PastHorizonException IO)
    -> Maybe ApiValidityInterval
    -> ExceptT ErrConstructTx IO (SlotNo, SlotNo)
parseValidityInterval ti validityInterval = do
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
            Just (ApiValidityInterval Nothing Nothing) -> False
            Nothing -> False

    let fromValidityBound = liftIO . \case
            Left ApiValidityBoundUnspecified ->
                pure $ SlotNo 0
            Right ApiValidityBoundUnspecified ->
                W.transactionExpirySlot ti Nothing
            Right (ApiValidityBoundAsTimeFromNow (Quantity sec)) ->
                W.transactionExpirySlot ti (Just sec)
            Left (ApiValidityBoundAsTimeFromNow (Quantity sec)) ->
                W.transactionExpirySlot ti (Just sec)
            Right (ApiValidityBoundAsSlot (Quantity slot)) ->
                pure $ SlotNo slot
            Left (ApiValidityBoundAsSlot (Quantity slot)) ->
                pure $ SlotNo slot

    (before, hereafter) <- case validityInterval of
        Nothing -> do
            before' <- fromValidityBound (Left ApiValidityBoundUnspecified)
            hereafter' <- fromValidityBound (Right ApiValidityBoundUnspecified)
            pure (before', hereafter')
        Just (ApiValidityInterval before' hereafter') -> do
            before'' <- case before' of
                Nothing -> fromValidityBound (Left ApiValidityBoundUnspecified)
                Just val -> fromValidityBound (Left val)
            hereafter'' <- case hereafter' of
                Nothing -> fromValidityBound (Right ApiValidityBoundUnspecified)
                Just val -> fromValidityBound (Right val)
            pure (before'', hereafter'')

    when (hereafter < before || isThereNegativeTime) $
        throwE ErrConstructTxWrongValidityBounds

    pure (before, hereafter)

-- TO-DO minting/burning
-- TO-DO reference scripts
constructSharedTransaction
    :: forall n . HasSNetworkId n
    => ApiLayer (SharedState n SharedKey)
    -> ArgGenChange (SharedState n SharedKey)
    -> IO (Set PoolId)
    -> (PoolId -> IO PoolLifeCycleStatus)
    -> ApiT WalletId
    -> ApiConstructTransactionData n
    -> Handler (ApiConstructTransaction n)
constructSharedTransaction
    api argGenChange knownPools getPoolStatus (ApiT wid) body = do
    let isNoPayload =
            isNothing (body ^. #payments) &&
            isNothing (body ^. #withdrawal) &&
            isNothing (body ^. #metadata) &&
            isNothing (body ^. #mintBurn) &&
            isNothing (body ^. #delegations)
    when isNoPayload $
        liftHandler $ throwE ErrConstructTxWrongPayload

    let md = body ^? #metadata . traverse . #txMetadataWithSchema_metadata

    (before, hereafter) <- liftHandler $
        parseValidityInterval ti (body ^. #validityInterval)

    delegationRequest <-
        liftHandler $ traverse parseDelegationRequest $ body ^. #delegations

    withWorkerCtx api wid liftE liftE $ \wrk -> do
        let db = wrk ^. dbLayer
            netLayer = wrk ^. networkLayer
            txLayer = wrk ^. transactionLayer @SharedKey @'CredFromScriptK
            trWorker = MsgWallet >$< wrk ^. logger

        currentEpochSlotting <- liftIO $ getCurrentEpochSlotting netLayer
        (Write.InAnyRecentEra (_ :: Write.RecentEra era) pp, _)
            <- liftIO $ W.readNodeTipStateForTxWrite netLayer
        (cp, _, _) <- handler $ W.readWallet wrk

        let delegationTemplateM = Shared.delegationTemplate $ getState cp
        withdrawal <- case body ^. #withdrawal of
            Just SelfWithdraw -> liftIO $
                W.mkSelfWithdrawalShared @n
                    netLayer
                    (txWitnessTagForKey SharedKeyS)
                    delegationTemplateM
                    db
            _ -> pure NoWithdrawal

        when (isNothing delegationTemplateM && isJust delegationRequest) $
            liftHandler $ throwE ErrConstructTxDelegationInvalid

        optionalDelegationAction <- liftHandler $
            forM delegationRequest $
                WD.handleDelegationRequest
                    trWorker db currentEpochSlotting knownPools
                    getPoolStatus NoWithdrawal

        let txCtx = defaultTransactionCtx
                { txWithdrawal = withdrawal
                , txMetadata = md
                , txValidityInterval = (Just before, hereafter)
                , txDelegationAction = optionalDelegationAction
                , txPaymentCredentialScriptTemplate =
                        Just (Shared.paymentTemplate $ getState cp)
                , txStakingCredentialScriptTemplate = delegationTemplateM
                }
        case Shared.ready (getState cp) of
            Shared.Pending ->
                liftHandler $ throwE ErrConstructTxSharedWalletIncomplete
            Shared.Active _ -> do
                let outs = case (body ^. #payments) of
                        Nothing ->
                            []
                        Just (ApiPaymentAddresses content) ->
                            F.toList (addressAmountToTxOut <$> content)
                (unbalancedTx, scriptLookup) <- liftHandler $
                    W.constructUnbalancedSharedTransaction @n @'CredFromScriptK @era
                    txLayer db txCtx PreSelection {outputs = outs}

                balancedTx <-
                    balanceTransaction api argGenChange
                    (AllScriptPaymentCredentialsFrom
                        (Shared.paymentTemplate (getState cp)) scriptLookup)
                    (ApiT wid)
                        ApiBalanceTransactionPostData
                        { transaction =
                            ApiT $ sealedTxFromCardanoBody unbalancedTx
                        , inputs = []
                        , redeemers = []
                        , encoding = body ^. #encoding
                        }

                apiDecoded <- decodeSharedTransaction api (ApiT wid) balancedTx
                let deposits = case optionalDelegationAction of
                        Just (JoinRegisteringKey _poolId) ->
                            [W.getStakeKeyDeposit pp]
                        _ -> []
                let refunds = case optionalDelegationAction of
                        Just Quit -> [W.getStakeKeyDeposit pp]
                        _ -> []
                delCertsWithPath <- case optionalDelegationAction of
                    Nothing -> pure Nothing
                    Just action -> do
                        (_, _, path) <-
                            handler $ W.readRewardAccount @((SharedState n SharedKey)) db
                        pure $ Just (action, path)

                pathForWithdrawal <- case withdrawal of
                    WithdrawalSelf _ _ _ -> do
                        (_, _, path) <-
                            handler $ W.readRewardAccount @((SharedState n SharedKey)) db
                        pure $ Just path
                    _ ->
                        pure Nothing

                pure $ ApiConstructTransaction
                    { transaction = balancedTx
                    , coinSelection =
                        mkApiCoinSelection deposits refunds
                        delCertsWithPath md
                        (unsignedTx outs apiDecoded pathForWithdrawal)
                    , fee = apiDecoded ^. #fee
                    }
  where
    ti :: TimeInterpreter (ExceptT PastHorizonException IO)
    ti = timeInterpreter (api ^. networkLayer)

    unsignedTx initialOuts decodedTx pathM = UnsignedTx
        { unsignedCollateral =
            mapMaybe toUnsignedTxInp (decodedTx ^. #collateral)
        , unsignedInputs =
            mapMaybe toUnsignedTxInp (decodedTx ^. #inputs)
        , unsignedOutputs =
            take (length initialOuts)
                $ map toUnsignedTxOut (decodedTx ^. #outputs)
        , unsignedChange =
            drop (length initialOuts)
                $ map toUnsignedTxChange (decodedTx ^. #outputs)
        , unsignedWithdrawals = case pathM of
                Nothing -> []
                Just path ->
                    mapMaybe (toUsignedTxWdrl path) (decodedTx ^. #withdrawals)
        }


decodeSharedTransaction
    :: forall n . HasSNetworkId n
    => ApiLayer (SharedState n SharedKey)
    -> ApiT WalletId
    -> ApiSerialisedTransaction
    -> Handler (ApiDecodedTransaction n)
decodeSharedTransaction ctx (ApiT wid) (ApiSerialisedTransaction (ApiT sealed) _) = do
    era <- liftIO $ NW.currentNodeEra nl
    (txinsOutsPaths, collateralInsOutsPaths, outsPath, pp, certs, txId, fee
        , metadata, scriptValidity, interval, witsCount, withdrawals, rewardAcctM)
        <- withWorkerCtx ctx wid liftE liftE $ \wrk -> do
        (cp, _, _) <- handler $ W.readWallet wrk
        let witCountCtx = toWitnessCountCtx SharedWallet (getState cp)
        let (decodedTx, _toMint, _toBurn, allCerts, interval, witsCount) =
                decodeTx tl era witCountCtx sealed
        let (Tx { txId
                , fee
                , resolvedInputs
                , resolvedCollateralInputs
                , outputs
                , withdrawals
                , metadata
                , scriptValidity
                }) = decodedTx
        inputPaths <-
            handler $ W.lookupTxIns @_ wrk $ fst <$> resolvedInputs
        collateralInputPaths <-
            handler $ W.lookupTxIns @_ wrk $ fst <$> resolvedCollateralInputs
        outputPaths <-
            handler $ W.lookupTxOuts @_ wrk outputs
        pp <- liftIO $ NW.currentProtocolParameters (wrk ^. networkLayer)
        let delegationTemplateM = (getState cp) ^. #delegationTemplate
        let scriptM =
                flip (replaceCosignersWithVerKeys CA.Stake) minBound <$>
                delegationTemplateM
        let rewardAcctPath =
                stakeDerivationPath $ Shared.derivationPrefix $ getState cp
        let rewardAcctM = case scriptM of
                Just script ->
                    let scriptHash =
                            CA.unScriptHash $
                            CA.toScriptHash script
                    in Just $ FromScriptHash scriptHash
                Nothing -> Nothing
        let certs = mkApiAnyCertificate rewardAcctM rewardAcctPath <$> allCerts
        pure
            ( inputPaths
            , collateralInputPaths
            , outputPaths
            , pp
            , certs
            , txId
            , fee
            , metadata
            , scriptValidity
            , interval
            , witsCount
            , withdrawals
            , rewardAcctM
            )
    pure $ ApiDecodedTransaction
        { id = ApiT txId
        , fee = maybe (Quantity 0) (Quantity . fromIntegral . unCoin) fee
        , inputs = map toInp txinsOutsPaths
        , outputs = map toOut outsPath
        , collateral = map toInp collateralInsOutsPaths
        -- TODO: [ADP-1670]
        , collateralOutputs = ApiAsArray Nothing
        , withdrawals = case rewardAcctM of
                Nothing -> []
                Just acct -> map (toWrdl acct) $ Map.assocs withdrawals
        -- TODO minting/burning multisig
        , mint = emptyApiAssetMntBurn
        , burn = emptyApiAssetMntBurn
        , certificates = certs
        , depositsTaken =
            (Quantity . fromIntegral . unCoin . W.stakeKeyDeposit $ pp)
                <$ filter ourRewardAccountRegistration certs
        , depositsReturned =
            (Quantity . fromIntegral . unCoin . W.stakeKeyDeposit $ pp)
                <$ filter ourRewardAccountDeregistration certs
        , metadata = ApiTxMetadata $ ApiT <$> metadata
        , scriptValidity = ApiT <$> scriptValidity
        , validityInterval = ApiValidityIntervalExplicit <$> interval
        , witnessCount = mkApiWitnessCount witsCount
        }
  where
    tl = ctx ^. W.transactionLayer @SharedKey @'CredFromScriptK
    nl = ctx ^. W.networkLayer @IO

    emptyApiAssetMntBurn = ApiAssetMintBurn
        { tokens = []
        , walletPolicyKeyHash = Nothing
        , walletPolicyKeyIndex = Nothing
        }

balanceTransaction
    :: forall s
     . (GenChange s, WalletFlavor s)
    => ApiLayer s
    -> ArgGenChange s
    -> UTxOAssumptions
    -> ApiT WalletId
    -> ApiBalanceTransactionPostData (NetworkOf s)
    -> Handler ApiSerialisedTransaction
balanceTransaction
    ctx@ApiLayer{..} argGenChange utxoAssumptions (ApiT wid) body = do
    (Write.InAnyRecentEra recentEra pp, timeTranslation)
        <- liftIO $ W.readNodeTipStateForTxWrite netLayer

    withWorkerCtx ctx wid liftE liftE $ \wrk -> do
        (utxo, wallet, _txs) <- handler $ W.readWalletUTxO wrk

        partialTx <- parsePartialTx recentEra
        balancedTx <- liftHandler
            . fmap (Cardano.InAnyCardanoEra Write.cardanoEra . fst)
            $ Write.balanceTransaction
                utxoAssumptions
                pp
                timeTranslation
                (Write.constructUTxOIndex utxo)
                (W.defaultChangeAddressGen argGenChange)
                (getState wallet)
                partialTx

        case body ^. #encoding of
            Just HexEncoded ->
                pure $ ApiSerialisedTransaction
                (ApiT $ W.sealedTxFromCardano balancedTx) HexEncoded
            _ -> pure $ ApiSerialisedTransaction
                (ApiT $ W.sealedTxFromCardano balancedTx) Base64Encoded
  where
    parsePartialTx
        :: Write.IsRecentEra era
        => Write.RecentEra era
        -> Handler (Write.PartialTx era)
    parsePartialTx era = do
        let externalUTxO = Write.toCardanoUTxO
                $ Write.utxoFromTxOutsInRecentEra era
                $ map fromExternalInput
                $ body ^. #inputs

        tx <- maybe
                (liftHandler
                    . throwE
                    . W.ErrPartialTxNotInNodeEra
                    $ AnyRecentEra era)
                pure
            . cardanoTxInExactEra (Write.cardanoEraFromRecentEra era)
            . getApiT
            $ body ^. #transaction

        pure $ Write.PartialTx
            tx
            externalUTxO
            (fromApiRedeemer <$> body ^. #redeemers)

decodeTransaction
    :: forall s n
     . ( IsOurs s Address
       , WalletFlavor s
       , CredFromOf s ~ 'CredFromKeyK
       )
    => ApiLayer s
    -> ApiT WalletId
    -> ApiSerialisedTransaction
    -> Handler (ApiDecodedTransaction n)
decodeTransaction
    ctx@ApiLayer{..} (ApiT wid) (ApiSerialisedTransaction (ApiT sealed) _) = do
    era <- liftIO $ NW.currentNodeEra netLayer
    withWorkerCtx ctx wid liftE liftE $ \wrk -> do
        (k, _) <- liftHandler $ W.readPolicyPublicKey wrk
        let keyhash = KeyHash Policy (xpubToBytes k)
        let (decodedTx, toMint, toBurn, allCerts, interval, witsCount) =
                decodeTx tl era (ShelleyWalletCtx keyhash) sealed
        let Tx { txId
               , fee
               , resolvedInputs
               , resolvedCollateralInputs
               , outputs
               , withdrawals
               , metadata
               , scriptValidity
               } = decodedTx
        let db = wrk ^. dbLayer
        (acct, _, acctPath) <-
            liftHandler $ W.shelleyOnlyReadRewardAccount @s db
        inputPaths <-
            handler $ W.lookupTxIns wrk $
            fst <$> resolvedInputs
        collateralInputPaths <-
            handler $ W.lookupTxIns wrk $
            fst <$> resolvedCollateralInputs
        outputPaths <-
            handler $ W.lookupTxOuts wrk outputs
        pp <- liftIO $ NW.currentProtocolParameters (wrk ^. networkLayer)
        (minted, burned) <-
            convertApiAssetMintBurn wrk (toMint, toBurn)
        let certs = mkApiAnyCertificate (Just acct) acctPath <$> allCerts
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
            , validityInterval = ApiValidityIntervalExplicit <$> interval
            , witnessCount = mkApiWitnessCount witsCount
            }
  where
    tl = ctx ^. W.transactionLayer @(KeyOf s) @'CredFromKeyK

toWrdl
    :: RewardAccount
    -> (RewardAccount, Coin)
    -> ApiWithdrawalGeneral n
toWrdl acct (rewardKey, (Coin c)) =
    if rewardKey == acct then
        ApiWithdrawalGeneral (ApiRewardAccount rewardKey)
            (Quantity $ fromIntegral c) Our
    else
        ApiWithdrawalGeneral (ApiRewardAccount rewardKey)
            (Quantity $ fromIntegral c) External

ourRewardAccountRegistration :: ApiAnyCertificate n -> Bool
ourRewardAccountRegistration = \case
    WalletDelegationCertificate (RegisterRewardAccount _) -> True
    _ -> False

ourRewardAccountDeregistration :: ApiAnyCertificate n -> Bool
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
                , address = ApiAddress addr
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
            { address = ApiAddress addr
            , amount = Quantity $ fromIntegral c
            , assets = ApiT tmap
            , derivationPath = NE.map ApiT path
            }

submitTransaction
    :: forall ctx s k n
     . ( ctx ~ ApiLayer s
       , s ~ SeqState n k
       , WalletFlavor s
       , HasNetworkLayer IO ctx
       , MkKeyFingerprint k (Proxy n, k 'CredFromKeyK XPub)
       , MkKeyFingerprint k Address
       , SoftDerivation k
       , HasSNetworkId (NetworkOf s)
       , NetworkDiscriminantCheck k
       , AddressCredential k ~ 'CredFromKeyK
       )
    => ctx
    -> ApiT WalletId
    -> ApiSerialisedTransaction
    -> Handler ApiTxId
submitTransaction ctx apiw@(ApiT wid) apitx = do
    --TODO: revisit/possibly set proper ttls in ADP-1193
    ttl <- liftIO $ W.transactionExpirySlot ti Nothing
    era <- liftIO $ NW.currentNodeEra nl

    let sealedTx = getApiT . (view #serialisedTxSealed) $ apitx

    apiDecoded <- decodeTransaction @s @n ctx apiw apitx
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

    when (countJoinsQuits (apiDecoded ^. #certificates) > 1) $
        liftHandler $ throwE ErrSubmitTransactionMultidelegationNotSupported

    when (witsRequiredForInputs > totalNumberOfWits)
        $ liftHandler . throwE
        $ ErrSubmitTransactionPartiallySignedOrNoSignedTx
            witsRequiredForInputs totalNumberOfWits

    void $ withWorkerCtx ctx wid liftE liftE $ \wrk -> do
        (k, _) <- liftHandler $ W.readPolicyPublicKey wrk
        let keyhash = KeyHash Policy (xpubToBytes k)
        let (tx,_,_,_,_,_) = decodeTx tl era (ShelleyWalletCtx keyhash) sealedTx

        let db = wrk ^. dbLayer
        (acct, _, path) <- liftHandler
            $ W.shelleyOnlyReadRewardAccount @s db
        let wdrl = getOurWdrl acct path apiDecoded
        let txCtx = defaultTransactionCtx
                { -- TODO: [ADP-1193]
                  -- Get this from decodeTx:
                  txValidityInterval = (Nothing, ttl)
                , txWithdrawal = wdrl
                }
        txMeta <- handler $ W.constructTxMeta db txCtx ourInps ourOuts
        liftHandler $ W.submitTx (wrk ^. logger) db nl
            BuiltTx
                { builtTx = tx
                , builtTxMeta = txMeta
                , builtSealedTx = sealedTx
                }
    pure $ ApiTxId (apiDecoded ^. #id)
  where
    tl = ctx ^. W.transactionLayer @k @'CredFromKeyK
    ti :: TimeInterpreter (ExceptT PastHorizonException IO)
    nl = ctx ^. networkLayer
    ti = timeInterpreter nl

    getOurWdrl rewardAcct path apiDecodedTx =
        let generalWdrls = apiDecodedTx ^. #withdrawals
            isWdrlOurs (ApiWithdrawalGeneral _ _ context) = context == Our
        in case filter isWdrlOurs generalWdrls of
            [ApiWithdrawalGeneral (ApiRewardAccount acct) (Quantity amt) _] ->
                WithdrawalSelf
                    ( if rewardAcct == acct
                        then acct
                        else error "reward account should be the same"
                    )
                    path
                    (Coin amt)
            _ -> NoWithdrawal

    countJoinsQuits :: [ApiAnyCertificate n] -> Int
    countJoinsQuits = sum . fmap ( \case
            WalletDelegationCertificate (JoinPool _ _) -> 1
            WalletDelegationCertificate (QuitPool _) -> 1
            _ -> 0
        )

samePaymentKey :: ApiTxInputGeneral n -> ApiTxInputGeneral n -> Bool
samePaymentKey inp1 inp2 =
    case (inp1, inp2) of
        ( WalletInput (ApiWalletInput _ _ _ derPath1 _ _)
            , WalletInput (ApiWalletInput _ _ _ derPath2 _ _)
            ) -> derPath1 == derPath2
        _ -> False

getOurOuts :: ApiDecodedTransaction n -> [TxOut]
getOurOuts apiDecodedTx =
    map toTxOut $ filter isOutOurs generalOuts
  where
    generalOuts = apiDecodedTx ^. #outputs
    isOutOurs (WalletOutput _) = True
    isOutOurs _ = False
    toTxOut
        ( WalletOutput
                (ApiWalletOutput (ApiAddress addr) (Quantity amt) (ApiT tmap) _)
            ) =
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
    :: forall n . HasSNetworkId n
    => ApiLayer (SharedState n SharedKey)
    -> ApiT WalletId
    -> ApiSerialisedTransaction
    -> Handler ApiTxId
submitSharedTransaction ctx apiw@(ApiT wid) apitx = do
    ttl <- liftIO $ W.transactionExpirySlot ti Nothing
    era <- liftIO $ NW.currentNodeEra nl

    let sealedTx = getApiT . (view #serialisedTxSealed) $ apitx

    apiDecoded <- decodeSharedTransaction @n ctx apiw apitx
    when (isForeign apiDecoded) $
        liftHandler $ throwE ErrSubmitTransactionForeignWallet
    let ourOuts = getOurOuts apiDecoded
    let ourInps = getOurInps apiDecoded

    void $ withWorkerCtx ctx wid liftE liftE $ \wrk -> do

        (cp, _, _) <- handler $ W.readWallet @_ wrk
        let witCountCtx = toWitnessCountCtx SharedWallet (getState cp)
        let (tx,_,_,_,_, (WitnessCount _ nativeScripts _)) =
                decodeTx tl era witCountCtx sealedTx

        let numberStakingNativeScripts =
                length $ filter hasDelegationKeyHash nativeScripts
        let delegationScriptTemplateM = Shared.delegationTemplate $ getState cp
        let delegationWitsRequired = case delegationScriptTemplateM of
                Nothing -> 0
                Just (ScriptTemplate _ scriptD) ->
                    if numberStakingNativeScripts == 0 then
                        0
                    else if numberStakingNativeScripts == 1 then
                        Shared.estimateMinWitnessRequiredPerInput scriptD
                    else
                        error "wallet supports transactions with 0 or 1 staking script"

        let (ScriptTemplate _ scriptP) = Shared.paymentTemplate $ getState cp
        let pWitsPerInput = Shared.estimateMinWitnessRequiredPerInput scriptP
        let witsRequiredForInputs =
                length $ L.nubBy samePaymentKey $
                filter isInpOurs $
                (apiDecoded ^. #inputs) ++ (apiDecoded ^. #collateral)
        let totalNumberOfWits = length $ getSealedTxWitnesses sealedTx
        let paymentWitsRequired =
                fromIntegral pWitsPerInput * witsRequiredForInputs
        let allWitsRequired =
                paymentWitsRequired + fromIntegral delegationWitsRequired
        when (allWitsRequired > totalNumberOfWits) $
            liftHandler $ throwE $
            ErrSubmitTransactionPartiallySignedOrNoSignedTx
            allWitsRequired totalNumberOfWits

        let txCtx = defaultTransactionCtx
                { txValidityInterval = (Nothing, ttl)
                }
        let db = wrk ^. dbLayer
        txMeta <- handler $ W.constructTxMeta db txCtx ourInps ourOuts
        liftHandler $ W.submitTx (wrk ^. logger) db nl
            BuiltTx
                { builtTx = tx
                , builtTxMeta = txMeta
                , builtSealedTx = sealedTx
                }
    pure $ ApiTxId (apiDecoded ^. #id)
  where
    nl = ctx ^. networkLayer
    tl = ctx ^. W.transactionLayer @SharedKey @'CredFromScriptK
    ti :: TimeInterpreter (ExceptT PastHorizonException IO)
    ti = timeInterpreter nl

    retrieveAllKeyHashes (NativeExplicitScript s _) = foldScript (:) [] s
    retrieveAllKeyHashes _ = []

    isTimelock (NativeExplicitScript _ _) = True
    isTimelock _ = False

    isDelegationKeyHash (KeyHash CA.Delegation _) = True
    isDelegationKeyHash (KeyHash _ _) = False

    hasDelegationKeyHash s =
        isTimelock s && all isDelegationKeyHash (retrieveAllKeyHashes s)

joinStakePool
    :: forall s n k.
        ( s ~ SeqState n k
        , WalletFlavor s
        , Excluding '[SharedKey] k
        , AddressIndexDerivationType k ~ 'Soft
        , GenChange s
        , IsOurs (SeqState n k) RewardAccount
        , SoftDerivation k
        , AddressBookIso s
        , MkKeyFingerprint k (Proxy n, k 'CredFromKeyK XPub)
        , HasDelegation s
        , MkKeyFingerprint k Address, NetworkDiscriminantCheck k
        , AddressCredential k ~ 'CredFromKeyK
        , HasSNetworkId n
        )
    => ApiLayer s
    -> ArgGenChange s
    -> IO (Set PoolId)
       -- ^ Known pools
       -- We could maybe replace this with a @IO (PoolId -> Bool)@
    -> (PoolId -> IO PoolLifeCycleStatus)
    -> ApiPoolSpecifier
    -> ApiT WalletId
    -> ApiWalletPassphrase
    -> Handler (ApiTransaction n)
joinStakePool
    ctx@ApiLayer{..} argGenChange knownPools
    getPoolStatus apiPool (ApiT walletId) body = do
    poolId <- case apiPool of
        AllPools -> liftE ErrUnexpectedPoolIdPlaceholder
        SpecificPool pool -> pure pool
    poolStatus <- liftIO (getPoolStatus poolId)
    pools <- liftIO knownPools
    withWorkerCtx ctx walletId liftE liftE $ \wrk -> do
        let tr = wrk ^. logger
            db = wrk ^. typed @(DBLayer IO s)
            ti = timeInterpreter netLayer
        currentEpochSlotting <- liftIO $ getCurrentEpochSlotting netLayer
        (BuiltTx{..}, txTime) <- liftIO $
            W.buildSignSubmitTransaction @s
                db
                netLayer
                txLayer
                (coerce $ getApiT $ body ^. #passphrase)
                walletId
                (W.defaultChangeAddressGen argGenChange)
                (PreSelection [])
                =<< WD.joinStakePool
                    (MsgWallet >$< tr)
                    ti
                    db
                    currentEpochSlotting
                    pools
                    poolId
                    poolStatus

        pp <- liftIO $ NW.currentProtocolParameters netLayer
        mkApiTransaction ti wrk #pendingSince
            MkApiTransactionParams
                { txId = builtTx ^. #txId
                , txFee = builtTx ^. #fee
                , txInputs = builtTx ^. #resolvedInputs
                -- Joining a stake pool does not require collateral:
                , txCollateralInputs = []
                , txOutputs = builtTx ^. #outputs
                , txCollateralOutput = builtTx ^. #collateralOutput
                , txWithdrawals = builtTx ^. #withdrawals
                , txMeta = builtTxMeta
                , txMetadata = Nothing
                , txTime
                , txScriptValidity = builtTx ^. #scriptValidity
                , txDeposit = W.stakeKeyDeposit pp
                , txMetadataSchema = TxMetadataDetailedSchema
                , txCBOR = builtTx ^. #txCBOR
                }

delegationFee
    :: forall s n k
     . ( GenChange s
       , k ~ ShelleyKey
       , AddressBookIso s
       , s ~ SeqState n k
       , HasSNetworkId n
       )
    => ApiLayer s
    -> ApiT WalletId
    -> Handler ApiFee
delegationFee ctx@ApiLayer{..} (ApiT walletId) = do
    withWorkerCtx ctx walletId liftE liftE $ \workerCtx -> liftIO $ do
        W.DelegationFee {feePercentiles, deposit} <-
            W.delegationFee @s
                (workerCtx ^. dbLayer)
                netLayer
                txLayer
                (W.defaultChangeAddressGen (delegationAddressS @n))
        pure $ mkApiFee (Just deposit) [] feePercentiles

quitStakePool
    :: forall s n k.
        ( s ~ SeqState n k
        , k ~ ShelleyKey
        , GenChange s
        , AddressBookIso s
        , IsOurs (SeqState n k) RewardAccount
        , HasSNetworkId n
        )
    => ApiLayer s
    -> ArgGenChange s
    -> ApiT WalletId
    -> ApiWalletPassphrase
    -> Handler (ApiTransaction n)
quitStakePool ctx@ApiLayer{..} argGenChange (ApiT walletId) body = do
    withWorkerCtx ctx walletId liftE liftE $ \wrk -> do
        let db = wrk ^. typed @(DBLayer IO s)
            ti = timeInterpreter netLayer
        txCtx <- liftIO $ WD.quitStakePool netLayer db ti
        (BuiltTx{..}, txTime) <- liftIO $ do
            W.buildSignSubmitTransaction @s
                db
                netLayer
                txLayer
                (coerce $ getApiT $ body ^. #passphrase)
                walletId
                (W.defaultChangeAddressGen argGenChange)
                (PreSelection [])
                txCtx

        pp <- liftIO $ NW.currentProtocolParameters netLayer
        mkApiTransaction ti wrk #pendingSince
            MkApiTransactionParams
                { txId = builtTx ^. #txId
                , txFee = builtTx ^. #fee
                , txInputs = builtTx ^. #resolvedInputs
                -- Quitting a stake pool does not require collateral:
                , txCollateralInputs = []
                , txOutputs = builtTx ^. #outputs
                , txCollateralOutput = builtTx ^. #collateralOutput
                , txWithdrawals = builtTx ^. #withdrawals
                , txMeta = builtTxMeta
                , txMetadata = Nothing
                , txTime
                , txScriptValidity = builtTx ^. #scriptValidity
                , txDeposit = W.stakeKeyDeposit pp
                , txMetadataSchema = TxMetadataDetailedSchema
                , txCBOR = builtTx ^. #txCBOR
                }

-- More testable helper for `listStakeKeys`.
--
-- TODO: Ideally test things like
-- no rewards => ada in distr == utxo balance
-- all keys in inputs appear (once) in output
listStakeKeys'
    :: forall n m
     . Monad m
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

        let rewards acc = fromMaybe (Coin 0) $ Map.lookup acc rewardsMap

        let mkOurs (acc, ix, deleg) = ApiOurStakeKey
                { _index = ix
                , _key = ApiRewardAccount acc
                , _rewardBalance = Coin.toQuantity $
                    rewards acc
                , _delegation = deleg
                , _stake = Coin.toQuantity $
                    stake (Just acc) <> rewards acc
                }

        let mkForeign acc = ApiForeignStakeKey
                { _key = ApiRewardAccount acc
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
    :: forall s n
     . (s ~ SeqState n ShelleyKey)
    => (Address -> Maybe RewardAccount)
    -> ApiLayer s
    -> ApiT WalletId
    -> Handler (ApiStakeKeys n)
listStakeKeys lookupStakeRef ctx@ApiLayer{..} (ApiT wid) =
    withWorkerCtx ctx wid liftE liftE $ \wrk -> handler $ do
        let db = wrk ^. typed @(DBLayer IO s)
        (wal, (_, delegation) ,pending) <- W.readWallet wrk
        let utxo = availableUTxO @s pending wal
        let takeFst (a,_,_) = a
        ourAccount <- takeFst <$> liftIO (W.readRewardAccount @s db)
        ourApiDelegation <- liftIO $ toApiWalletDelegation delegation
            (unsafeExtendSafeZone (timeInterpreter $ ctx ^. networkLayer))
        let ourKeys = [(ourAccount, 0, ourApiDelegation)]
        liftIO $ listStakeKeys' @n
            utxo
            lookupStakeRef
            (fetchRewardAccountBalances netLayer)
            ourKeys

{-------------------------------------------------------------------------------
                                Migrations
-------------------------------------------------------------------------------}

createMigrationPlan
    :: forall s n k
     . ( WalletFlavor s
       , k ~ KeyOf s
       , CredFromOf s ~ 'CredFromKeyK
       , IsOurs s Address
       )
    => ApiLayer s
    -> Maybe ApiWithdrawalPostData
    -- ^ What type of reward withdrawal to attempt
    -> ApiT WalletId
    -- ^ Source wallet
    -> ApiWalletMigrationPlanPostData n
    -- ^ Target addresses
    -> Handler (ApiWalletMigrationPlan n)
createMigrationPlan ctx@ApiLayer{..} withdrawalType (ApiT wid) postData =
    withWorkerCtx ctx wid liftE liftE $ \wrk -> do
        let db = wrk ^. dbLayer
        rewardWithdrawal <- case withdrawalType of
            Nothing -> pure NoWithdrawal
            Just pd ->
                shelleyOnlyMkWithdrawal
                    netLayer
                    (txWitnessTagForKey $ keyOfWallet $ walletFlavor @s)
                    db
                    pd
        (wallet, _, _) <- handler $ W.readWallet wrk
        plan <- handler $ W.createMigrationPlan @_ wrk rewardWithdrawal
        liftHandler
            $ failWith ErrCreateMigrationPlanEmpty
            $ mkApiWalletMigrationPlan
                (getState wallet)
                (view #addresses postData)
                rewardWithdrawal
                plan

mkApiWalletMigrationPlan
    :: forall n s. IsOurs s Address
    => s
    -> NonEmpty (ApiAddress n)
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
        $ apiAddress <$> addresses

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
    :: forall s p k n.
        ( Bounded (Index (AddressIndexDerivationType k) (AddressCredential k))
        , HardDerivation k
        , WalletFlavor s
        , HasDelegation s
        , k ~ KeyOf s
        , CredFromOf s ~ 'CredFromKeyK
        , HasSNetworkId n
        , n ~ NetworkOf s
        , IsOurs s Address
        )
    => ApiLayer s
    -> Maybe ApiWithdrawalPostData
        -- ^ What type of reward withdrawal to attempt
    -> ApiT WalletId
    -> ApiWalletMigrationPostData n p
    -> Handler (NonEmpty (ApiTransaction n))
migrateWallet ctx@ApiLayer{..} withdrawalType (ApiT wid) postData = do
    mkRewardAccount <-
        case withdrawalType of
            Nothing -> pure $ selfRewardAccountBuilder (keyFlavorFromState @s)
            Just w -> either liftE pure
                $ shelleyOnlyRewardAccountBuilder (walletFlavor @s) w
    withWorkerCtx ctx wid liftE liftE $ \wrk -> do
        let db = wrk ^. dbLayer
            tr = wrk ^. logger
        era <- liftIO $ NW.currentNodeEra netLayer
        rewardWithdrawal <- case withdrawalType of
            Nothing -> pure NoWithdrawal
            Just pd ->
                shelleyOnlyMkWithdrawal
                    netLayer
                    (txWitnessTagForKey $ keyOfWallet $ walletFlavor @s)
                    db
                    pd
        plan <- handler $ W.createMigrationPlan @_ wrk rewardWithdrawal
        ttl <- liftIO $ W.transactionExpirySlot ti Nothing
        pp <- liftIO $ NW.currentProtocolParameters netLayer
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
                W.buildAndSignTransaction
                    wrk
                    wid
                    era
                    mkRewardAccount
                    pwd
                    txContext
                    (selection {change = []})

            liftHandler $ W.submitTx tr db netLayer
                BuiltTx
                    { builtTx = tx
                    , builtTxMeta = txMeta
                    , builtSealedTx = sealedTx
                    }
            mkApiTransaction ti wrk #pendingSince
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
                    , txCBOR = tx ^. #txCBOR
                    }
  where
    addresses = apiAddress <$> view #addresses postData
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
    :: forall ctx s . (ctx ~ ApiLayer s)
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
    -> NetworkLayer IO block
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
    -> NetworkLayer IO block
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

getBlocksLatestHeader :: NetworkLayer IO block -> Handler ApiBlockHeader
getBlocksLatestHeader nl = liftIO $ mkApiBlockHeader <$> NW.currentNodeTip nl

{-------------------------------------------------------------------------------
                               Miscellaneous
-------------------------------------------------------------------------------}

postExternalTransaction
    :: forall s
     . ApiLayer s
    -> ApiT W.SealedTx
    -> Handler ApiTxId
postExternalTransaction ctx (ApiT sealed) = do
    tx <- liftHandler $ W.submitExternalTx
            (tracerTxSubmit ctx) (ctx ^. #netLayer) (ctx ^. #txLayer) sealed
    return $ ApiTxId (ApiT (tx ^. #txId))

signMetadata
    :: forall ctx s k n.
        ( ctx ~ ApiLayer s
        , s ~ SeqState n k
        , HardDerivation k
        , AddressIndexDerivationType k ~ 'Soft
        , WalletFlavor s
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

    withWorkerCtx @_ @s ctx wid liftE liftE $ \wrk -> liftHandler $ do
        getSignature <$> W.signMetadataWith
            wrk wid (coerce pwd) (role_, ix) meta

derivePublicKey
    :: forall s k ver.
        ( SoftDerivation k
        , GetAccount s k
        , k ~ KeyOf s
        , WalletFlavor s
        )
    => ApiLayer s
    -> ((ByteString, Role) -> VerificationKeyHashing -> ver)
    -> ApiT WalletId
    -> ApiT Role
    -> ApiT DerivationIndex
    -> Maybe Bool
    -> Handler ver
derivePublicKey ctx mkVer (ApiT wid) (ApiT role_) (ApiT ix) hashed = do
    withWorkerCtx @_ @s ctx wid liftE liftE $ \wrk -> do
        k <- liftHandler $ W.derivePublicKey wrk role_ ix
        let (payload, hashing) = computeKeyPayload hashed
                $ getRawKey (keyFlavorFromState @s) k
        pure $ mkVer (payload, role_) hashing

postAccountPublicKey
    :: forall ctx s k account.
        ( ctx ~ ApiLayer s
        , GetPurpose k
        , k ~ KeyOf s
        , AfterByron k
        , WalletFlavor s
        )
    => ctx
    -> (ByteString -> KeyFormat -> Index 'Hardened 'PurposeK -> account)
    -> ApiT WalletId
    -> ApiT DerivationIndex
    -> ApiPostAccountKeyDataWithPurpose
    -> Handler account
postAccountPublicKey ctx mkAccount (ApiT wid) (ApiT ix)
    (ApiPostAccountKeyDataWithPurpose (ApiT pwd) extd purposeM) = do
    withWorkerCtx @_ @s ctx wid liftE liftE $ \wrk -> do
        k <- liftHandler $ W.getAccountPublicKeyAtIndex
            wrk wid pwd ix (getApiT <$> purposeM)
        pure $ mkAccount
            (publicKeyToBytes' extd $ getRawKey (keyFlavorFromState @s) k)
            extd
            ixPurpose'
  where
    ixPurpose' =
        maybe (getPurpose @k) (Index . getDerivationIndex . getApiT) purposeM

publicKeyToBytes' :: KeyFormat -> XPub -> ByteString
publicKeyToBytes' = \case
    Extended -> xpubToBytes
    NonExtended -> xpubPublicKey

getAccountPublicKey
    :: forall s k account.
        ( GetAccount s k
        , GetPurpose k
        , k ~ KeyOf s
        , WalletFlavor s
        )
    => ApiLayer s
    -> (ByteString -> KeyFormat -> Index 'Hardened 'PurposeK -> account)
    -> ApiT WalletId
    -> Maybe KeyFormat
    -> Handler account
getAccountPublicKey ctx mkAccount (ApiT wid) extended = do
    withWorkerCtx @_ @s ctx wid liftE liftE $ \wrk -> do
        k <- handler $ W.readAccountPublicKey wrk
        pure $ mkAccount
            (publicKeyToBytes' extd $ getRawKey (keyFlavorFromState @s) k)
            extd
            (getPurpose @k)
  where
      extd = case extended of
          Just Extended -> Extended
          _ -> NonExtended

getPolicyKey
    :: forall ctx s
     . ( ctx ~ ApiLayer s
       , WalletFlavor s
       )
    => ctx
    -> ApiT WalletId
    -> Maybe Bool
    -> Handler ApiPolicyKey
getPolicyKey ctx (ApiT wid) hashed = do
    withWorkerCtx @_ @s ctx wid liftE liftE $ \wrk -> do
        (k, _) <- liftHandler $ W.readPolicyPublicKey wrk
        pure $ uncurry ApiPolicyKey (computeKeyPayload hashed k)

postPolicyKey
    :: forall ctx s n
     . ( ctx ~ ApiLayer s
       , s ~ SeqState n ShelleyKey
       )
    => ctx
    -> ApiT WalletId
    -> Maybe Bool
    -> ApiPostPolicyKeyData
    -> Handler ApiPolicyKey
postPolicyKey ctx (ApiT wid) hashed apiPassphrase =
    withWorkerCtx @_ @s ctx wid liftE liftE $ \wrk -> do
        k <- liftHandler $ W.writePolicyPublicKey wrk wid pwd
        pure
            $ uncurry ApiPolicyKey
            $ computeKeyPayload hashed
            $ getRawKey (keyFlavorFromState @s) k
  where
    pwd = getApiT (apiPassphrase ^. #passphrase)

postPolicyId
    :: forall s k
     . ( WalletFlavor s
       , KeyOf s ~ k
       , AfterByron k
       )
    => ApiLayer s
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

    withWorkerCtx @_ @s ctx wid liftE liftE $ \wrk -> do
        (xpub, _) <- liftHandler $ W.readPolicyPublicKey wrk
        pure $ ApiPolicyId $ ApiT $
            toTokenPolicyId (keyFlavorFromState @s)
                scriptTempl (Map.singleton (Cosigner 0) xpub)
  where
    scriptTempl = getApiT (payload ^. #policyScriptTemplate)

{-------------------------------------------------------------------------------
                                  Helpers
-------------------------------------------------------------------------------}

-- | Handler for fetching the 'ArgGenChange' for the 'RndState' (i.e. the root
-- XPrv), necessary to derive new change addresses.
rndStateChange
    :: forall ctx s n.
        ( ctx ~ ApiLayer s
        , s ~ RndState n
        )
    => ctx
    -> ApiT WalletId
    -> Passphrase "user"
    -> Handler (ArgGenChange s)
rndStateChange ctx (ApiT wid) pwd =
    withWorkerCtx @_ @s ctx wid liftE liftE $ \wrk -> liftHandler $
        W.withRootKey (wrk ^. dbLayer) wid pwd ErrSignPaymentWithRootKey $
            \xprv scheme -> pure (xprv, preparePassphrase scheme pwd)

type RewardAccountBuilder k
    =  ClearCredentials k
    -> (XPrv, Passphrase "encryption")

mkWithdrawal
    :: forall n block
     . NetworkLayer IO block
    -> TxWitnessTag
    -> DBLayer IO (SeqState n ShelleyKey)
    -> ApiWithdrawalPostData
    -> Handler Withdrawal
mkWithdrawal netLayer txWitnessTag db = \case
    SelfWithdrawal ->
        liftIO $ W.mkSelfWithdrawal netLayer txWitnessTag db
    ExternalWithdrawal (ApiMnemonicT mnemonic) ->
        liftHandler . ExceptT
            $ W.mkExternalWithdrawal netLayer txWitnessTag mnemonic

-- | Unsafe version of `mkWithdrawal` that throws runtime error
-- when applied to a non-shelley or non-sequential wallet state.
shelleyOnlyMkWithdrawal
    :: forall s block
     . WalletFlavor s
    => NetworkLayer IO block
    -> TxWitnessTag
    -> DBLayer IO s
    -> ApiWithdrawalPostData
    -> Handler Withdrawal
shelleyOnlyMkWithdrawal netLayer txWitnessTag db postData =
    case walletFlavor @s of
        ShelleyWallet ->
            mkWithdrawal netLayer txWitnessTag db postData
        _ -> notShelleyWallet
  where
    notShelleyWallet =
        liftHandler $ throwE ErrReadRewardAccountNotAShelleyWallet

shelleyOnlyRewardAccountBuilder
    :: forall s k
     . ( Bounded (Index (AddressIndexDerivationType k) (AddressCredential k))
       , k ~ KeyOf s
       )
    => WalletFlavorS s
    -> ApiWithdrawalPostData
    -> Either ErrReadRewardAccount (RewardAccountBuilder k)
shelleyOnlyRewardAccountBuilder wF w =
    case wF of
        ShelleyWallet -> case w of
            SelfWithdrawal -> pure
                $ selfRewardAccountBuilder ShelleyKeyS
            ExternalWithdrawal (ApiMnemonicT m) -> do
                let (xprv, _acct, _path) = W.someRewardAccount @ShelleyKey m
                pure (const (xprv, mempty))
        _ -> throwError ErrReadRewardAccountNotAShelleyWallet

selfRewardAccountBuilder
    :: forall k
     . ( HardDerivation k
       , Bounded (Index (AddressIndexDerivationType k) (AddressCredential k))
       )
    => KeyFlavorS k
    -> RewardAccountBuilder k
selfRewardAccountBuilder keyF (RootCredentials rootK pwdP) =
    (getRawKey keyF (deriveRewardAccount pwdP rootK minBound), pwdP)

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
        , certificates = uncurry mkApiCoinSelectionCerts
            <$> mcerts
        , depositsTaken = Coin.toQuantity
            <$> deps
        , depositsReturned = Coin.toQuantity
            <$> refunds
        , metadata = ApiBytesT. serialiseToCBOR
            <$> metadata
        }

mkApiCoinSelectionCerts
    :: DelegationAction
    -> NonEmpty DerivationIndex
    -> NonEmpty Api.ApiCertificate
mkApiCoinSelectionCerts action ixs =
    case action of
        Join pid -> pure $ Api.JoinPool apiStakePath (ApiT pid)
        Quit -> pure $ Api.QuitPool apiStakePath
        JoinRegisteringKey pid -> NE.fromList
            [ Api.RegisterRewardAccount apiStakePath
            , Api.JoinPool apiStakePath (ApiT pid)
            ]
  where
    apiStakePath = ApiT <$> ixs

mkApiCoinSelectionInput
    :: forall n
     . (TxIn, TxOut, NonEmpty DerivationIndex)
     -> ApiWalletInput n
mkApiCoinSelectionInput
    (TxIn txid index, TxOut addr (TokenBundle amount assets), path) =
    ApiWalletInput
        { id = ApiT txid
        , index = index
        , address = ApiAddress addr
        , amount = Coin.toQuantity amount
        , assets = ApiT assets
        , derivationPath = ApiT <$> path
        }

mkApiCoinSelectionOutput :: forall n. TxOut -> ApiCoinSelectionOutput n
mkApiCoinSelectionOutput (TxOut addr (TokenBundle amount assets)) =
    ApiCoinSelectionOutput (ApiAddress addr)
    (Coin.toQuantity amount)
    (ApiT assets)

mkApiCoinSelectionChange
    :: forall n
     . TxChange (NonEmpty DerivationIndex)
    -> ApiCoinSelectionChange n
mkApiCoinSelectionChange txChange =
    ApiCoinSelectionChange
        { address = (ApiAddress $ view #address txChange)
        , amount = Coin.toQuantity $ view #amount txChange
        , assets = ApiT $ view #assets txChange
        , derivationPath = ApiT <$> view #derivationPath txChange
        }

mkApiCoinSelectionCollateral
    :: forall n
     . (TxIn, TxOut, NonEmpty DerivationIndex)
    -> ApiCoinSelectionCollateral n
mkApiCoinSelectionCollateral
    (TxIn txid index, TxOut addr (TokenBundle amount _), path) =
    ApiCoinSelectionCollateral
        { id = ApiT txid
        , index = index
        , address = ApiAddress addr
        , amount = Coin.toQuantity amount
        , derivationPath = ApiT <$> path
        }

mkApiCoinSelectionWithdrawal
    :: forall n
     . (RewardAccount, Coin, NonEmpty DerivationIndex)
    -> ApiCoinSelectionWithdrawal n
mkApiCoinSelectionWithdrawal (rewardAcct, wdrl, path) =
    ApiCoinSelectionWithdrawal
        { stakeAddress = ApiRewardAccount rewardAcct
        , amount = Coin.toQuantity wdrl
        , derivationPath = ApiT <$> path
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
    :: forall n s
     . ( HasDelegation s
       , WalletFlavor s
       )
    => TimeInterpreter (ExceptT PastHorizonException IO)
    -> W.WalletLayer IO s
    -> Lens' (ApiTransaction n) (Maybe ApiBlockReference)
    -> MkApiTransactionParams
    -> Handler (ApiTransaction n)
mkApiTransaction timeInterpreter wrk timeRefLens tx = do
    let db = wrk ^. typed @(DBLayer IO s)
    timeRef <- liftIO $ (#time .~ tx ^. #txTime) <$> makeApiBlockReference
        (neverFails
            "makeApiBlockReference shouldn't fail getting the time of \
            \transactions with slots in the past" timeInterpreter)
        (tx ^. #txMeta . #slotNo)
        (natural (tx ^. #txMeta . #blockHeight))
    expRef <- liftIO $ traverse makeApiSlotReference' (tx ^. #txMeta . #expiry)
    parsedValues <- traverse parseTxCBOR $ tx ^. #txCBOR
    parsedCertificates <-
        if hasDelegation (Proxy @s)
            then traverse (getApiAnyCertificates db (keyFlavorFromState @s)) parsedValues
            else pure Nothing
    parsedMintBurn <- forM parsedValues
        $ getTxApiAssetMintBurn wrk

    pure $ set timeRefLens (Just timeRef) $ ApiTransaction
        { id = ApiT $ tx ^. #txId
        , amount = Quantity . fromIntegral $ tx ^. #txMeta . #amount . #unCoin
        , fee = Quantity $ maybe 0 (fromIntegral . unCoin) (tx ^. #txFee)
        , depositTaken = Quantity depositIfAny
        , depositReturned = Quantity reclaimIfAny
        , insertedAt = Nothing
        , pendingSince = Nothing
        , expiresAt = expRef
        , depth = Nothing
        , direction = ApiT (tx ^. #txMeta . #direction)
        , inputs =
            [ ApiTxInput (toAddressAmount @n <$> o) (ApiT i)
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
        , status = ApiT (tx ^. #txMeta . #status)
        , metadata =
            TxMetadataWithSchema (tx ^. #txMetadataSchema) <$> tx ^. #txMetadata
        , scriptValidity = ApiT <$> tx ^. #txScriptValidity
        , certificates = fromMaybe [] parsedCertificates
        , mint = maybe noApiAsset fst parsedMintBurn
        , burn = maybe noApiAsset snd parsedMintBurn
        , validityInterval =
            ApiValidityIntervalExplicit
                <$> (view #validityInterval =<< parsedValues)
        , scriptIntegrity =
            ApiT <$> (view #scriptIntegrity =<< parsedValues)
        , extraSignatures =
            ApiT <$> (view #extraSignatures =<< maybe [] pure parsedValues)
        }
  where
    -- Since tx expiry can be far in the future, we use unsafeExtendSafeZone for
    -- now.
    makeApiSlotReference' = makeApiSlotReference
        $ unsafeExtendSafeZone timeInterpreter

    -- | Promote certificates of a transaction to API type,
    -- using additional context from the 'WorkerCtx'.
    getApiAnyCertificates db flavor ParsedTxCBOR{certificates} = case flavor of
        ShelleyKeyS -> do
            (rewardAcct, _, path) <- liftHandler
                $ W.shelleyOnlyReadRewardAccount @s db
            pure $ mkApiAnyCertificate (Just rewardAcct) path <$> certificates
        SharedKeyS -> do
            infoM <- liftHandler
                $ W.sharedOnlyReadRewardAccount @s db
            case infoM of
                Just (rewardAcct, path) ->
                    pure $ mkApiAnyCertificate (Just rewardAcct) path <$> certificates
                _ -> pure []
        _ ->
            pure []

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
    txOutValue = fromIntegral . unCoin . TxOut.coin

    toAddressAmountNoAssets
        :: TxOut
        -> AddressAmountNoAssets (ApiAddress n)
    toAddressAmountNoAssets (TxOut addr (TokenBundle.TokenBundle coin _)) =
        AddressAmountNoAssets (ApiAddress addr) (Coin.toQuantity coin)

toAddressAmount
    :: forall n
     . TxOut
    -> AddressAmount (ApiAddress n)
toAddressAmount (TxOut addr (TokenBundle.TokenBundle coin assets)) =
    AddressAmount (ApiAddress addr) (Coin.toQuantity coin) (ApiT assets)

mkApiFee
    :: Maybe Coin
    -> [Coin]
    -> (Percentile 10 Fee, Percentile 90 Fee)
    -> ApiFee
mkApiFee mDeposit minCoins (Percentile estMin, Percentile estMax) = ApiFee
    { estimatedMin = Quantity (Coin.toNatural (feeToCoin estMin))
    , estimatedMax = Quantity (Coin.toNatural (feeToCoin estMax))
    , minimumCoins = Quantity . Coin.toNatural <$> minCoins
    , deposit = Quantity . Coin.toNatural $ fromMaybe (Coin 0) mDeposit
    }

mkApiWithdrawal
    :: (RewardAccount, Coin)
    -> ApiWithdrawal n
mkApiWithdrawal (acct, c) =
    ApiWithdrawal (ApiRewardAccount acct) (Coin.toQuantity c)

addressAmountToTxOut
    :: AddressAmount (ApiAddress n)
    -> TxOut
addressAmountToTxOut (AddressAmount (ApiAddress addr) c (ApiT assets)) =
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
    pure ApiBlockReference
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
    -> (Write.TxIn, Write.TxOutInRecentEra)
fromExternalInput ApiExternalInput
    { id = ApiT tid
    , index = ix
    , address = ApiAddress addr
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
        datum' = maybe Write.NoDatum (Write.DatumHash . getApiT) datum
        out = Write.TxOutInRecentEra addr' val datum' script
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
    :: forall ctx s k.
        ( ctx ~ ApiLayer s
        , IsOurs s RewardAccount
        , IsOurs s Address
        , AddressBookIso s
        , MaybeLight s
        , k ~ KeyOf s
        )
    => Tracer IO WalletEngineLog
    -> (Block, NetworkParameters)
    -> NetworkLayer IO Read.Block
    -> TransactionLayer k (CredFromOf s) W.SealedTx
    -> DBFactory IO s
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
    :: forall ctx s .
        ( ctx ~ ApiLayer s
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
    before dbf wid = do
        edb <- runExceptT $ loadDBLayer dbf
        case edb of
            Left _ ->
                throwIO
                    $ ExceptionNoSuchWallet
                    $ ErrNoSuchWallet wid
            Right db -> do
                W.checkWalletIntegrity db gp
                pure db
    (_, NetworkParameters gp _ _) = ctx ^. genesisData

-- | Register a wallet create and restore thread with the worker registry.
-- See 'Cardano.Wallet#createWallet'
createWalletWorker
    :: forall ctx s .
        ( ctx ~ ApiLayer s
        , IsOurs s RewardAccount
        , IsOurs s Address
        , AddressBookIso s
        , MaybeLight s
        )
    => ctx
        -- ^ Surrounding API context
    -> WalletId
        -- ^ Wallet Id
    -> (DBFresh IO s -> ExceptT ErrWalletAlreadyExists IO (DBLayer IO s))
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
    before ctx' _ = unsafeRunExceptT $ createWallet ctx'
    re = ctx ^. workerRegistry @s

createNonRestoringWalletWorker
    :: forall ctx s .  ( ctx ~ ApiLayer s)
    => ctx
        -- ^ Surrounding API context
    -> WalletId
        -- ^ Wallet Id
    -> (DBFresh IO s -> ExceptT ErrWalletAlreadyExists IO (DBLayer IO s))
        -- ^ Create action
    -> ExceptT ErrCreateWallet IO WalletId
createNonRestoringWalletWorker ctx wid createWallet =
    liftIO (Registry.lookup re wid) >>= \case
        Just _ ->
            throwE $ ErrCreateWalletAlreadyExists $ ErrWalletAlreadyExists wid
        Nothing ->
            liftIO registerIdleWorker >>= \case
                Nothing -> throwE ErrCreateWalletFailedToCreateWorker
                Just _ -> pure wid
  where
    before ctx' = unsafeRunExceptT $ createWallet ctx'
    re = ctx ^. workerRegistry @s
    df = ctx ^. dbFactory
    config = MkWorker
        { workerAcquire = \k -> withDatabase df wid $ before >=> k
        , workerBefore = \_ _ -> pure ()
        , workerAfter = defaultWorkerAfter
        , workerMain = idleWorker
        }
    registerIdleWorker =
        fmap (const ctx) <$> Registry.register @_ @ctx re ctx wid config

-- | Create a worker for an existing wallet, register it, then start the worker
-- thread. This is used by 'startWalletWorker' and 'createWalletWorker'.
registerWorker
    :: forall ctx s .
        ( ctx ~ ApiLayer s
        , IsOurs s RewardAccount
        , IsOurs s Address
        , AddressBookIso s
        , MaybeLight s
        )
    => ctx
    -> (DBFresh IO s -> WalletId -> IO (DBLayer IO s))
        -- ^ First action to run after starting the worker thread.
    -> (WorkerCtx ctx -> WalletId -> IO ())
        -- ^ Action to run concurrently with restore.
    -> WalletId
    -> IO (Maybe ctx)
registerWorker ctx before coworker wid =
    fmap (const ctx) <$> Registry.register @_ @ctx re ctx wid config
  where
    re = ctx ^. workerRegistry @s
    dbfa = ctx ^. dbFactory
    config = MkWorker
        { workerAcquire = \k -> withDatabase dbfa wid $ \df -> before df wid >>= k
        , workerBefore = \_ _ -> pure ()
        , workerAfter = defaultWorkerAfter
        -- fixme: ADP-641 Review error handling here
        , workerMain = \ctx' _ -> race_
            (unsafeRunExceptT $ W.restoreWallet ctx')
            (race_
                (forever $ W.runLocalTxSubmissionPool txCfg ctx')
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
    :: forall ctx s m a.
        ( HasWorkerRegistry s ctx
        , HasDBFactory s ctx
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
    Registry.lookup (ctx ^. workerRegistry @s) wid >>= \case
        Nothing -> do
            wids <- liftIO $ listDatabases $ ctx ^. dbFactory @s
            if wid `elem` wids
                then onNotResponding (ErrWalletNotResponding wid)
                else onMissing (ErrNoSuchWallet wid)
        Just wrk ->
            action $ hoistResource (workerResource wrk) (MsgFromWorker wid) ctx

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
    = ErrCreateWalletAlreadyExists !ErrWalletAlreadyExists
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
            apiError err400 BadRequest $
                case fromText @PoolId "INVALID" of
                    Left msg -> pretty msg
                    Right _ -> "Invalid pool id placeholder"

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

instance IsServerError ErrGetAsset where
    toServerError = \case
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
    = MsgWalletWorker !(WorkerLog WalletId W.WalletWorkerLog)
    | MsgSubmitSealedTx !TxSubmitLog
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
