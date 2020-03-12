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
    ( Listen (..)
    , ListenError (..)
    , HostPreference
    , start
    , serve
    , server
    , byronServer
    , newApiLayer
    , withListeningSocket
    , LiftHandler(..)
    ) where

import Prelude

import Cardano.Pool.Metadata
    ( StakePoolMetadata )
import Cardano.Pool.Metrics
    ( ErrListStakePools (..)
    , ErrMetricsInconsistency (..)
    , StakePool (..)
    , StakePoolLayer (..)
    )
import Cardano.Wallet
    ( ErrAdjustForFee (..)
    , ErrCannotJoin (..)
    , ErrCannotQuit (..)
    , ErrCoinSelection (..)
    , ErrDecodeSignedTx (..)
    , ErrFetchRewards (..)
    , ErrJoinStakePool (..)
    , ErrListTransactions (..)
    , ErrListUTxOStatistics (..)
    , ErrMkTx (..)
    , ErrNoSuchWallet (..)
    , ErrPostTx (..)
    , ErrQuitStakePool (..)
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
    , ErrUpdatePassphrase (..)
    , ErrValidateSelection
    , ErrWalletAlreadyExists (..)
    , ErrWithRootKey (..)
    , ErrWrongPassphrase (..)
    , HasLogger
    , WalletLog
    , genesisData
    , networkLayer
    )
import Cardano.Wallet.Api
    ( Addresses
    , Api
    , ApiLayer (..)
    , ByronMigrations
    , ByronTransactions
    , ByronWallets
    , CoinSelections
    , HasDBFactory
    , HasWorkerRegistry
    , Network
    , Proxy_
    , StakePools
    , Transactions
    , Wallets
    , dbFactory
    , workerRegistry
    )
import Cardano.Wallet.Api.Types
    ( AccountPostData (..)
    , AddressAmount (..)
    , ApiAccountPublicKey (..)
    , ApiAddress (..)
    , ApiBlockReference (..)
    , ApiByronWallet (..)
    , ApiByronWalletBalance (..)
    , ApiByronWalletMigrationInfo (..)
    , ApiCoinSelection (..)
    , ApiCoinSelectionInput (..)
    , ApiEpochInfo (..)
    , ApiEpochNumber (..)
    , ApiErrorCode (..)
    , ApiFee (..)
    , ApiMnemonicT (..)
    , ApiNetworkClock (..)
    , ApiNetworkInformation (..)
    , ApiNetworkParameters (..)
    , ApiNetworkTip (..)
    , ApiPoolId (..)
    , ApiSelectCoinsData (..)
    , ApiStakePool (..)
    , ApiStakePoolMetrics (..)
    , ApiT (..)
    , ApiTimeReference (..)
    , ApiTransaction (..)
    , ApiTxId (..)
    , ApiTxInput (..)
    , ApiUtxoStatistics (..)
    , ApiWallet (..)
    , ApiWalletDelegation (..)
    , ApiWalletDelegationNext (..)
    , ApiWalletDelegationStatus (..)
    , ApiWalletPassphrase (..)
    , ByronWalletPostData (..)
    , Iso8601Time (..)
    , PostExternalTransactionData (..)
    , PostTransactionData
    , PostTransactionFeeData
    , SomeByronWalletPostData (..)
    , WalletBalance (..)
    , WalletOrAccountPostData (..)
    , WalletPostData (..)
    , WalletPutData (..)
    , WalletPutPassphraseData (..)
    , getApiMnemonicT
    , toApiNetworkParameters
    )
import Cardano.Wallet.DB
    ( DBFactory (..) )
import Cardano.Wallet.Network
    ( ErrCurrentNodeTip (..), ErrNetworkUnavailable (..), NetworkLayer )
import Cardano.Wallet.Primitive.AddressDerivation
    ( DelegationAddress (..)
    , Depth (..)
    , HardDerivation (..)
    , NetworkDiscriminant (..)
    , Passphrase
    , PaymentAddress (..)
    , WalletKey (..)
    , XPrv
    , digest
    , publicKey
    )
import Cardano.Wallet.Primitive.AddressDerivation.Byron
    ( ByronKey )
import Cardano.Wallet.Primitive.AddressDerivation.Icarus
    ( IcarusKey )
import Cardano.Wallet.Primitive.AddressDerivation.Shelley
    ( ShelleyKey )
import Cardano.Wallet.Primitive.AddressDiscovery
    ( GenChange (ArgGenChange), IsOwned )
import Cardano.Wallet.Primitive.AddressDiscovery.Random
    ( RndState, mkRndState )
import Cardano.Wallet.Primitive.AddressDiscovery.Sequential
    ( SeqState (..)
    , defaultAddressPoolGap
    , mkSeqStateFromAccountXPub
    , mkSeqStateFromRootXPrv
    )
import Cardano.Wallet.Primitive.CoinSelection
    ( CoinSelection (..), changeBalance, feeBalance, inputBalance )
import Cardano.Wallet.Primitive.Model
    ( Wallet, availableBalance, currentTip, getState, totalBalance )
import Cardano.Wallet.Primitive.Types
    ( Address
    , AddressState
    , Block
    , BlockchainParameters
    , Coin (..)
    , Hash (..)
    , HistogramBar (..)
    , PoolId
    , SortOrder (..)
    , SyncProgress
    , SyncTolerance
    , TransactionInfo (TransactionInfo)
    , Tx (..)
    , TxIn (..)
    , TxOut (..)
    , TxStatus (..)
    , UTxOStatistics (..)
    , UnsignedTx (..)
    , WalletId (..)
    , WalletMetadata (..)
    , slotAt
    , slotMinBound
    , syncProgressRelativeToTime
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
import Control.Applicative
    ( liftA2 )
import Control.Arrow
    ( second )
import Control.DeepSeq
    ( NFData )
import Control.Exception
    ( IOException, bracket, throwIO, tryJust )
import Control.Exception.Lifted
    ( finally )
import Control.Monad
    ( forM, forM_, unless, void, when )
import Control.Monad.IO.Class
    ( MonadIO, liftIO )
import Control.Monad.Trans.Except
    ( ExceptT (..), catchE, runExceptT, throwE, withExceptT )
import Control.Tracer
    ( Tracer )
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
    ( isInfixOf, isSubsequenceOf, sortOn )
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
    ( (:<|>) (..)
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
    ( Handler (..), ServerError (..) )
import System.IO.Error
    ( ioeGetErrorType
    , isAlreadyInUseError
    , isDoesNotExistError
    , isPermissionError
    , isUserError
    )
import System.Random
    ( getStdRandom, random )

import qualified Cardano.Wallet as W
import qualified Cardano.Wallet.Network as NW
import qualified Cardano.Wallet.Primitive.AddressDerivation.Byron as Rnd
import qualified Cardano.Wallet.Primitive.AddressDerivation.Icarus as Ica
import qualified Cardano.Wallet.Primitive.AddressDerivation.Shelley as Seq
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
    -> Socket
    -> Application
    -> IO ()
start settings tr socket application = do
    logSettings <- newApiLoggerSettings <&> obfuscateKeys (const sensitive)
    Warp.runSettingsSocket settings socket
        $ handleRawError (curry handler)
        $ withApiLogger tr logSettings
        application
  where
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
                                   Core API
-------------------------------------------------------------------------------}

-- | A Servant server for our wallet API
server
    :: forall t n byron icarus shelley.
        ( byron ~ ApiLayer (RndState 'Mainnet) t ByronKey
        , icarus ~ ApiLayer (SeqState 'Mainnet IcarusKey) t IcarusKey
        , shelley ~ ApiLayer (SeqState n ShelleyKey) t ShelleyKey
        , DelegationAddress n ShelleyKey
        , Buildable (ErrValidateSelection t)
        )
    => byron
    -> icarus
    -> shelley
    -> StakePoolLayer IO
    -> NtpClient
    -> Server (Api n)
server byron icarus shelley spl ntp =
         wallets
    :<|> addresses
    :<|> coinSelections
    :<|> transactions
    :<|> stakePools
    :<|> byronWallets
    :<|> byronTransactions
    :<|> byronMigrations
    :<|> network
    :<|> proxy
  where
    wallets :: Server Wallets
    wallets = deleteWallet shelley
        :<|> (fmap fst . getWallet shelley mkShelleyWallet)
        :<|> (fmap fst <$> listWallets shelley mkShelleyWallet)
        :<|> postWallet shelley
        :<|> putWallet shelley mkShelleyWallet
        :<|> putWalletPassphrase shelley
        :<|> getUTxOsStatistics shelley
        :<|> forceResyncWallet shelley

    addresses :: Server (Addresses n)
    addresses = listAddresses shelley

    coinSelections :: Server (CoinSelections n)
    coinSelections = selectCoins shelley

    transactions :: Server (Transactions n)
    transactions =
        postTransaction shelley (delegationAddress @n)
        :<|> listTransactions shelley
        :<|> postTransactionFee shelley
        :<|> deleteTransaction shelley

    stakePools :: Server (StakePools n)
    stakePools = listPools spl
        :<|> joinStakePool shelley spl
        :<|> quitStakePool shelley
        :<|> delegationFee shelley

    byronWallets :: Server ByronWallets
    byronWallets =
        (\case
            SomeRandomWallet x -> postRandomWallet byron x
            SomeIcarusWallet x -> postIcarusWallet icarus x
            SomeTrezorWallet x -> postTrezorWallet icarus x
            SomeLedgerWallet x -> postLedgerWallet icarus x
        )
        :<|> (\wid -> withLegacyLayer wid
                (byron , deleteWallet byron wid)
                (icarus, deleteWallet icarus wid)
             )
        :<|> (\wid -> withLegacyLayer wid
                (byron , fst <$> getWallet byron  mkLegacyWallet wid)
                (icarus, fst <$> getWallet icarus mkLegacyWallet wid)
             )
        :<|> liftA2 (\xs ys -> fmap fst $ sortOn snd $ xs ++ ys)
            (listWallets byron  mkLegacyWallet)
            (listWallets icarus mkLegacyWallet)
        :<|> (\wid tip -> withLegacyLayer wid
                (byron , forceResyncWallet byron  wid tip)
                (icarus, forceResyncWallet icarus wid tip)
             )

    byronTransactions :: Server (ByronTransactions n)
    byronTransactions =
             (\_ _ -> throwError err501)
        :<|>
             (\wid r0 r1 s -> withLegacyLayer wid
                (byron , listTransactions byron  wid r0 r1 s)
                (icarus, listTransactions icarus wid r0 r1 s)
             )
        :<|>
             (\_ _ -> throwError err501)
        :<|> (\wid txid -> withLegacyLayer wid
                (byron , deleteTransaction byron  wid txid)
                (icarus, deleteTransaction icarus wid txid)
             )

    byronMigrations :: Server (ByronMigrations n)
    byronMigrations =
             (\wid -> withLegacyLayer wid
                (byron , getMigrationInfo byron  wid)
                (icarus, getMigrationInfo icarus wid)
             )
        :<|> (\from to pwd -> withLegacyLayer from
                (byron , migrateWallet byron  shelley from to pwd)
                (icarus, migrateWallet icarus shelley from to pwd)
             )

    network :: Server Network
    network =
        getNetworkInformation genesis nl
        :<|> (getNetworkParameters genesis)
        :<|> (getNetworkClock ntp)
      where
        nl = shelley ^. networkLayer @t
        genesis = shelley ^. genesisData

    proxy :: Server Proxy_
    proxy = postExternalTransaction shelley


-- | A diminished servant server to serve Byron wallets only.
byronServer
    :: forall t n.
        ( Buildable (ErrValidateSelection t)
        , PaymentAddress n IcarusKey
        )
    => ApiLayer (RndState 'Mainnet) t ByronKey
    -> ApiLayer (SeqState 'Mainnet IcarusKey) t IcarusKey
    -> NtpClient
    -> Server (Api n)
byronServer byron icarus ntp =
         wallets
    :<|> addresses
    :<|> coinSelections
    :<|> transactions
    :<|> stakePools
    :<|> byronWallets
    :<|> byronTransactions
    :<|> byronMigrations
    :<|> network
    :<|> proxy
  where
    wallets :: Server Wallets
    wallets =
             (\_ -> throwError err501)
        :<|> (\_ -> throwError err501)
        :<|> throwError err501
        :<|> (\_ -> throwError err501)
        :<|> (\_ _ -> throwError err501)
        :<|> (\_ _ -> throwError err501)
        :<|> (\_ -> throwError err501)
        :<|> (\_ _ -> throwError err501)

    addresses :: Server (Addresses n)
    addresses _ _ = throwError err501

    coinSelections :: Server (CoinSelections n)
    coinSelections _ _ = throwError err501

    transactions :: Server (Transactions n)
    transactions =
             (\_ _ -> throwError err501)
        :<|> (\_ _ _ _ -> throwError err501)
        :<|> (\_ _ -> throwError err501)
        :<|> (\_ _ -> throwError err501)

    stakePools :: Server (StakePools n)
    stakePools =
             throwError err501
        :<|> (\_ _ _ -> throwError err501)
        :<|> (\_ _ -> throwError err501)
        :<|> (\_ -> throwError err501)

    byronWallets :: Server ByronWallets
    byronWallets =
        (\case
            SomeRandomWallet x -> postRandomWallet byron x
            SomeIcarusWallet x -> postIcarusWallet icarus x
            SomeTrezorWallet x -> postTrezorWallet icarus x
            SomeLedgerWallet x -> postLedgerWallet icarus x
        )
        :<|> (\wid -> withLegacyLayer wid
                (byron , deleteWallet byron wid)
                (icarus, deleteWallet icarus wid)
             )
        :<|> (\wid -> withLegacyLayer wid
                (byron , fst <$> getWallet byron  mkLegacyWallet wid)
                (icarus, fst <$> getWallet icarus mkLegacyWallet wid)
             )
        :<|> liftA2 (\xs ys -> fmap fst $ sortOn snd $ xs ++ ys)
            (listWallets byron  mkLegacyWallet)
            (listWallets icarus mkLegacyWallet)
        :<|> (\wid tip -> withLegacyLayer wid
                (byron , forceResyncWallet byron  wid tip)
                (icarus, forceResyncWallet icarus wid tip)
             )

    byronTransactions :: Server (ByronTransactions n)
    byronTransactions =
             (\wid tx -> withLegacyLayer wid
                 (byron , do
                    let pwd = getApiT $ tx ^. #passphrase
                    genChange <- rndStateChange byron wid pwd
                    postTransaction byron genChange wid tx
                 )
                 (icarus, do
                    let genChange k _ = paymentAddress @n k
                    postTransaction icarus genChange wid tx
                 )
             )
        :<|>
             (\wid r0 r1 s -> withLegacyLayer wid
                (byron , listTransactions byron  wid r0 r1 s)
                (icarus, listTransactions icarus wid r0 r1 s)
             )
        :<|>
            (\wid tx -> withLegacyLayer wid
                (byron , postTransactionFee byron wid tx)
                (icarus, postTransactionFee icarus wid tx)
            )
        :<|> (\wid txid -> withLegacyLayer wid
                (byron , deleteTransaction byron  wid txid)
                (icarus, deleteTransaction icarus wid txid)
             )

    byronMigrations :: Server (ByronMigrations n)
    byronMigrations =
             (\wid -> withLegacyLayer wid
                (byron , getMigrationInfo byron  wid)
                (icarus, getMigrationInfo icarus wid)
             )
        :<|> \_ _ _ -> throwError err501

    network :: Server Network
    network =
        getNetworkInformation genesis nl
        :<|> (getNetworkParameters genesis)
        :<|> (getNetworkClock ntp)
      where
        nl = icarus ^. networkLayer @t
        genesis = icarus ^. genesisData

    proxy :: Server Proxy_
    proxy = postExternalTransaction icarus

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
        , k ~ ShelleyKey
        , ctx ~ ApiLayer s t k
        , HasDBFactory s k ctx
        , HasWorkerRegistry s k ctx
        )
    => ctx
    -> WalletOrAccountPostData
    -> Handler ApiWallet
postWallet ctx (WalletOrAccountPostData body) = case body of
    Left body' -> postShelleyWallet ctx body'
    Right body' -> postAccountWallet ctx body'

postShelleyWallet
    :: forall ctx s t k n.
        ( s ~ SeqState n k
        , k ~ ShelleyKey
        , ctx ~ ApiLayer s t k
        , HasDBFactory s k ctx
        , HasWorkerRegistry s k ctx
        )
    => ctx
    -> WalletPostData
    -> Handler ApiWallet
postShelleyWallet ctx body = do
    let state = mkSeqStateFromRootXPrv (rootXPrv, pwd) g
    void $ liftHandler $ initWorker @_ @s @k ctx wid
        (\wrk -> W.createWallet  @(WorkerCtx ctx) @s @k wrk wid wName state)
        (\wrk -> W.restoreWallet @(WorkerCtx ctx) @s @t @k wrk wid)
    liftHandler $ withWorkerCtx @_ @s @k ctx wid throwE $ \wrk ->
        W.attachPrivateKey @_ @s @k wrk wid (rootXPrv, pwd)
    fst <$> getWallet ctx (mkShelleyWallet @_ @s @t @k) (ApiT wid)
  where
    seed = getApiMnemonicT (body ^. #mnemonicSentence)
    secondFactor = getApiMnemonicT <$> (body ^. #mnemonicSecondFactor)
    pwd = getApiT (body ^. #passphrase)
    rootXPrv = Seq.generateKeyFromSeed (seed, secondFactor) pwd
    g = maybe defaultAddressPoolGap getApiT (body ^. #addressPoolGap)
    wid = WalletId $ digest $ publicKey rootXPrv
    wName = getApiT (body ^. #name)

postAccountWallet
    :: forall ctx s t k n.
        ( s ~ SeqState n k
        , k ~ ShelleyKey
        , ctx ~ ApiLayer s t k
        , HasWorkerRegistry s k ctx
        )
    => ctx
    -> AccountPostData
    -> Handler ApiWallet
postAccountWallet ctx body = do
    let state = mkSeqStateFromAccountXPub accXPub g
    void $ liftHandler $ initWorker @_ @s @k ctx wid
        (\wrk -> W.createWallet  @(WorkerCtx ctx) @s @k wrk wid wName state)
        (\wrk -> W.restoreWallet @(WorkerCtx ctx) @s @t @k wrk wid)
    fst <$> getWallet ctx (mkShelleyWallet @_ @s @t @k) (ApiT wid)
  where
    g = maybe defaultAddressPoolGap getApiT (body ^. #addressPoolGap)
    wName = getApiT (body ^. #name)
    (ApiAccountPublicKey accXPubApiT) =  body ^. #accountPublicKey
    accXPub = getApiT accXPubApiT
    wid = WalletId $ digest accXPub

mkShelleyWallet
    :: forall ctx s t k n.
        ( ctx ~ ApiLayer s t k
        , s ~ SeqState n k
        , WalletKey k
        , HasWorkerRegistry s k ctx
        )
    => MkApiWallet ctx s ApiWallet
mkShelleyWallet ctx wid cp meta pending progress = do
    reward <- liftHandler $ withWorkerCtx @_ @s @k ctx wid liftE $
        \wrk -> W.fetchRewardBalance @_ @s @t @k wrk wid
    pure ApiWallet
        { addressPoolGap = ApiT $ getState cp ^. #externalPool . #gap
        , balance = ApiT $ WalletBalance
            { available = Quantity $ availableBalance pending cp
            , total = Quantity $ totalBalance pending cp
            , reward = Quantity $ fromIntegral $ getQuantity reward
            }
        , delegation = toApiWalletDelegation (meta ^. #delegation)
        , id = ApiT wid
        , name = ApiT $ meta ^. #name
        , passphrase = ApiT <$> meta ^. #passphraseInfo
        , state = ApiT progress
        , tip = getWalletTip cp
        }
  where
    liftE = throwE . ErrFetchRewardsNoSuchWallet
    (_, bp, _) = ctx ^. genesisData
    sp = W.slotParams bp

    toApiWalletDelegation W.WalletDelegation{active,next} =
        ApiWalletDelegation
            { active = toApiWalletDelegationNext Nothing active
            , next = flip map next $ \W.WalletDelegationNext{status,changesAt} ->
                toApiWalletDelegationNext (Just changesAt) status
            }

    toApiWalletDelegationNext mepoch = \case
        W.Delegating pid -> ApiWalletDelegationNext
            { status = Delegating
            , target = Just (ApiT pid)
            , changesAt = toApiEpochInfo <$> mepoch
            }

        W.NotDelegating -> ApiWalletDelegationNext
            { status = NotDelegating
            , target = Nothing
            , changesAt = toApiEpochInfo <$> mepoch
            }

    toApiEpochInfo ep =
        ApiEpochInfo (ApiT ep) (W.epochStartTime sp ep)

--------------------- Legacy

postLegacyWallet
    :: forall ctx s t k.
        ( ctx ~ ApiLayer s t k
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
    liftHandler $ withWorkerCtx ctx wid throwE $ \wrk ->
        W.attachPrivateKey wrk wid (rootXPrv, pwd)
    fst <$> getWallet ctx mkLegacyWallet (ApiT wid)
  where
    wid = WalletId $ digest $ publicKey rootXPrv

mkLegacyWallet
    :: MkApiWallet ctx s ApiByronWallet
mkLegacyWallet _ctx wid cp meta pending progress =
    pure ApiByronWallet
        { balance = ApiByronWalletBalance
            { available = Quantity $ availableBalance pending cp
            , total = Quantity $ totalBalance pending cp
            }
        , id = ApiT wid
        , name = ApiT $ meta ^. #name
        , passphrase = ApiT <$> meta ^. #passphraseInfo
        , state = ApiT progress
        , tip = getWalletTip cp
        }

postRandomWallet
    :: forall ctx s t k.
        ( ctx ~ ApiLayer s t k
        , s ~ RndState 'Mainnet
        , k ~ ByronKey
        )
    => ctx
    -> ByronWalletPostData '[12]
    -> Handler ApiByronWallet
postRandomWallet ctx body = do
    s <- liftIO $ mkRndState rootXPrv <$> getStdRandom random
    postLegacyWallet ctx (rootXPrv, pwd) $ \wrk wid ->
        W.createWallet @(WorkerCtx ctx) @s @k wrk wid wName s
  where
    wName = getApiT (body ^. #name)
    pwd   = getApiT (body ^. #passphrase)
    rootXPrv = Rnd.generateKeyFromSeed seed pwd
      where seed = getApiMnemonicT (body ^. #mnemonicSentence)

postIcarusWallet
    :: forall ctx s t k.
        ( ctx ~ ApiLayer s t k
        , s ~ SeqState 'Mainnet k
        , k ~ IcarusKey
        , HasWorkerRegistry s k ctx
        )
    => ctx
    -> ByronWalletPostData '[15]
    -> Handler ApiByronWallet
postIcarusWallet ctx body = do
    postLegacyWallet ctx (rootXPrv, pwd) $ \wrk wid ->
        W.createIcarusWallet @(WorkerCtx ctx) @s @k wrk wid wName (rootXPrv, pwd)
  where
    wName = getApiT (body ^. #name)
    pwd   = getApiT (body ^. #passphrase)
    rootXPrv = Ica.generateKeyFromSeed seed pwd
      where seed = getApiMnemonicT (body ^. #mnemonicSentence)

postTrezorWallet
    :: forall ctx s t k.
        ( ctx ~ ApiLayer s t k
        , s ~ SeqState 'Mainnet k
        , k ~ IcarusKey
        , HasWorkerRegistry s k ctx
        )
    => ctx
    -> ByronWalletPostData '[12,15,18,21,24]
    -> Handler ApiByronWallet
postTrezorWallet ctx body = do
    postLegacyWallet ctx (rootXPrv, pwd) $ \wrk wid ->
        W.createIcarusWallet @(WorkerCtx ctx) @s @k wrk wid wName (rootXPrv, pwd)
  where
    wName = getApiT (body ^. #name)
    pwd   = getApiT (body ^. #passphrase)
    rootXPrv = Ica.generateKeyFromSeed seed pwd
      where seed = getApiMnemonicT (body ^. #mnemonicSentence)

postLedgerWallet
    :: forall ctx s t k.
        ( ctx ~ ApiLayer s t k
        , s ~ SeqState 'Mainnet k
        , k ~ IcarusKey
        , HasWorkerRegistry s k ctx
        )
    => ctx
    -> ByronWalletPostData '[12,15,18,21,24]
    -> Handler ApiByronWallet
postLedgerWallet ctx body = do
    postLegacyWallet ctx (rootXPrv, pwd) $ \wrk wid ->
        W.createIcarusWallet @(WorkerCtx ctx) @s @k wrk wid wName (rootXPrv, pwd)
  where
    wName = getApiT (body ^. #name)
    pwd   = getApiT (body ^. #passphrase)
    rootXPrv = Ica.generateKeyFromHardwareLedger mw pwd
      where mw = getApiMnemonicT (body ^. #mnemonicSentence)

{-------------------------------------------------------------------------------
                             ApiLayer Discrimination
-------------------------------------------------------------------------------}

-- Legacy wallets like 'Byron Random' and 'Icarus Sequential' are handled
-- through the same API endpoints. However, they rely on different contexts.
-- Since they have identical ids, we actually lookup both contexts in sequence.
withLegacyLayer
    :: forall byron icarus t a.
        ( byron ~ ApiLayer (RndState 'Mainnet) t ByronKey
        , icarus ~ ApiLayer (SeqState 'Mainnet IcarusKey) t IcarusKey
        )
    => ApiT WalletId
    -> (byron, Handler a)
    -> (icarus, Handler a)
    -> Handler a
withLegacyLayer (ApiT wid) (byron, withByron) (icarus, withIcarus) =
    tryByron (const $ tryIcarus liftE)
  where
    liftE = liftHandler . throwE

    tryIcarus onMissing = withWorkerCtx @_
        @(SeqState 'Mainnet IcarusKey)
        @IcarusKey
        icarus
        wid
        onMissing
        (const withIcarus)

    tryByron onMissing = withWorkerCtx @_
        @(RndState 'Mainnet)
        @ByronKey
        byron
        wid
        onMissing
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
    liftHandler $ withWorkerCtx @_ @s @k ctx wid throwE $ \_ -> do
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
        )
    => ctx
    -> MkApiWallet ctx s apiWallet
    -> ApiT WalletId
    -> Handler (apiWallet, UTCTime)
getWallet ctx mkApiWallet (ApiT wid) = do
    (cp, meta, pending) <- liftHandler $ withWorkerCtx @_ @s @k ctx wid throwE $
        \wrk -> W.readWallet @_ @s @k wrk wid

    progress <- liftIO $
        W.walletSyncProgress ctx cp

    (, meta ^. #creationTime)
        <$> mkApiWallet ctx wid cp meta pending progress

listWallets
    :: forall ctx s t k apiWallet.
        ( ctx ~ ApiLayer s t k
        )
    => ctx
    -> MkApiWallet ctx s apiWallet
    -> Handler [(apiWallet, UTCTime)]
listWallets ctx mkApiWallet = do
    wids <- liftIO $ Registry.keys re
    sortOn snd <$> mapM (getWallet ctx mkApiWallet) (ApiT <$> wids)
  where
    re = ctx ^. workerRegistry @s @k

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
        Just (ApiT wName) -> liftHandler $ withWorkerCtx ctx wid throwE $ \wrk ->
            W.updateWallet wrk wid (modify wName)
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
    liftHandler $ withWorkerCtx ctx wid liftE $ \wrk ->
        W.updateWalletPassphrase wrk wid (old, new)
    return NoContent
  where
    liftE = throwE . ErrUpdatePassphraseNoSuchWallet

getUTxOsStatistics
    :: forall ctx s t k.
        ( ctx ~ ApiLayer s t k
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

forceResyncWallet
    :: forall ctx s t k.
        ( ctx ~ ApiLayer s t k
        )
    => ctx
    -> ApiT WalletId
    -> ApiNetworkTip
    -> Handler NoContent
forceResyncWallet ctx (ApiT wid) tip = guardTip (== W.slotMinBound) $ \pt -> do
    liftIO $ Registry.unregister re wid
    liftHandler (safeRollback pt) `finally` liftIO (registerWorker ctx wid)
  where
    re = ctx ^. workerRegistry @s @k
    df = ctx ^. dbFactory @s @k

    -- NOTE Safe because it happens without any worker running and, we've
    -- controlled that 'point' is genesis.
    safeRollback :: W.SlotId -> ExceptT ErrNoSuchWallet IO ()
    safeRollback point = do
        ExceptT $ withDatabase df wid $ \db -> do
            let wrk = hoistResource db (MsgFromWorker wid) ctx
            runExceptT $ W.rollbackBlocks wrk wid point

    guardTip
        :: (W.SlotId -> Bool)
        -> (W.SlotId -> Handler ())
        -> Handler NoContent
    guardTip predicate handler_ = do
        unless (predicate point) $ liftHandler $ throwE $ ErrRejectedTip tip
        handler_ point $> NoContent
      where
        point = W.SlotId
            { epochNumber = tip ^. #epochNumber . #getApiT
            , slotNumber  = tip ^. #slotNumber  . #getApiT
            }

{-------------------------------------------------------------------------------
                                  Coin Selections
-------------------------------------------------------------------------------}

selectCoins
    :: forall ctx s t k n.
        ( Buildable (ErrValidateSelection t)
        , DelegationAddress n k
        , k ~ ShelleyKey
        , s ~ SeqState n k
        , ctx ~ ApiLayer s t k
        )
    => ctx
    -> ApiT WalletId
    -> ApiSelectCoinsData n
    -> Handler (ApiCoinSelection n)
selectCoins ctx (ApiT wid) body =
    fmap mkApiCoinSelection
        $ liftHandler
        $ withWorkerCtx ctx wid liftE
        $ \wrk -> W.selectCoinsExternal @_ @s @t @k wrk wid (delegationAddress @n)
        $ coerceCoin <$> body ^. #payments
  where
    liftE = throwE . ErrSelectCoinsExternalNoSuchWallet


{-------------------------------------------------------------------------------
                                    Addresses
-------------------------------------------------------------------------------}

listAddresses
    :: forall ctx s t k n.
        ( DelegationAddress n k
        , k ~ ShelleyKey
        , s ~ SeqState n k
        , ctx ~ ApiLayer s t k
        )
    => ctx
    -> ApiT WalletId
    -> Maybe (ApiT AddressState)
    -> Handler [ApiAddress n]
listAddresses ctx (ApiT wid) stateFilter = do
    addrs <- liftHandler $ withWorkerCtx ctx wid throwE $ \wrk ->
        W.listAddresses @_ @s @k @n wrk wid
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
        , IsOwned s k
        , NFData s
        , Show s
        , ctx ~ ApiLayer s t k
        )
    => ctx
    -> ArgGenChange s
    -> ApiT WalletId
    -> PostTransactionData n
    -> Handler (ApiTransaction n)
postTransaction ctx genChange (ApiT wid) body = do
    let outs = coerceCoin <$> (body ^. #payments)
    let pwd = getApiT $ body ^. #passphrase

    selection <- liftHandler $ withWorkerCtx ctx wid liftE1 $ \wrk ->
        W.selectCoinsForPayment @_ @s @t wrk wid outs

    (tx, meta, time, wit) <- liftHandler $ withWorkerCtx ctx wid liftE2 $ \wrk ->
        W.signPayment @_ @s @t @k wrk wid genChange pwd selection

    liftHandler $ withWorkerCtx ctx wid liftE3 $ \wrk ->
        W.submitTx @_ @s @t @k wrk wid (tx, meta, wit)

    pure $ mkApiTransaction
        (txId tx)
        (fmap Just <$> selection ^. #inputs)
        (tx ^. #outputs)
        (meta, time)
        #pendingSince
  where
    liftE1 = throwE . ErrSelectForPaymentNoSuchWallet
    liftE2 = throwE . ErrSignPaymentNoSuchWallet
    liftE3 = throwE . ErrSubmitTxNoSuchWallet

deleteTransaction
    :: forall ctx s t k. ctx ~ ApiLayer s t k
    => ctx
    -> ApiT WalletId
    -> ApiTxId
    -> Handler NoContent
deleteTransaction ctx (ApiT wid) (ApiTxId (ApiT (tid))) = do
    liftHandler $ withWorkerCtx ctx wid liftE $ \wrk ->
        W.forgetPendingTx wrk wid tid
    return NoContent
  where
    liftE = throwE . ErrRemovePendingTxNoSuchWallet

listTransactions
    :: forall ctx s t k n. (ctx ~ ApiLayer s t k)
    => ctx
    -> ApiT WalletId
    -> Maybe Iso8601Time
    -> Maybe Iso8601Time
    -> Maybe (ApiT SortOrder)
    -> Handler [ApiTransaction n]
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
    mkApiTransactionFromInfo :: TransactionInfo -> ApiTransaction n
    mkApiTransactionFromInfo (TransactionInfo txid ins outs meta depth txtime) =
        apiTx { depth  }
      where
        apiTx = mkApiTransaction txid ins outs (meta, txtime) $
            case meta ^. #status of
                Pending  -> #pendingSince
                InLedger -> #insertedAt

postTransactionFee
    :: forall ctx s t k n.
        ( Buildable (ErrValidateSelection t)
        , ctx ~ ApiLayer s t k
        )
    => ctx
    -> ApiT WalletId
    -> PostTransactionFeeData n
    -> Handler ApiFee
postTransactionFee ctx (ApiT wid) body = do
    let outs = coerceCoin <$> (body ^. #payments)
    liftHandler $ withWorkerCtx ctx wid liftE $ \wrk ->
        (apiFee <$> W.selectCoinsForPayment @_ @s @t @k wrk wid outs)
            `catchE` handleCannotCover wrk
  where
    apiFee = ApiFee . Quantity . fromIntegral . feeBalance
    liftE = throwE . ErrSelectForPaymentNoSuchWallet
    handleCannotCover wrk = \case
        ErrSelectForPaymentFee (ErrCannotCoverFee missing) -> do
            (wallet, _, pending) <- withExceptT ErrSelectForPaymentNoSuchWallet $
                W.readWallet wrk wid
            let balance = availableBalance pending wallet
            pure $ ApiFee $ Quantity $ fromIntegral missing + balance

        e -> throwE e


{-------------------------------------------------------------------------------
                                    Stake Pools
-------------------------------------------------------------------------------}

listPools
    :: StakePoolLayer IO
    -> Handler [ApiStakePool]
listPools spl =
    liftHandler $ map (uncurry mkApiStakePool) <$> listStakePools spl
  where
    mkApiStakePool
        :: StakePool
        -> Maybe StakePoolMetadata
        -> ApiStakePool
    mkApiStakePool sp meta =
        ApiStakePool
            (ApiT $ poolId sp)
            (ApiStakePoolMetrics
                (Quantity $ fromIntegral $ getQuantity $ stake sp)
                (Quantity $ fromIntegral $ getQuantity $ production sp))
            (sp ^. #performance)
            meta
            (fromIntegral <$> sp ^. #cost)
            (Quantity $ sp ^. #margin)
            (sp ^. #desirability)
            (sp ^. #saturation)

joinStakePool
    :: forall ctx s t n k.
        ( DelegationAddress n k
        , s ~ SeqState n k
        , k ~ ShelleyKey
        , HardDerivation k
        , ctx ~ ApiLayer s t k
        )
    => ctx
    -> StakePoolLayer IO
    -> ApiPoolId
    -> ApiT WalletId
    -> ApiWalletPassphrase
    -> Handler (ApiTransaction n)
joinStakePool ctx spl apiPoolId (ApiT wid) (ApiWalletPassphrase (ApiT pwd)) = do
    pid <- case apiPoolId of
        ApiPoolIdPlaceholder -> liftHandler $ throwE ErrUnexpectedPoolIdPlaceholder
        ApiPoolId pid -> pure pid

    pools <- liftIO $ knownStakePools spl

    (tx, txMeta, txTime) <- liftHandler $ withWorkerCtx ctx wid liftE $ \wrk ->
        W.joinStakePool @_ @s @t @k wrk wid (pid, pools) (delegationAddress @n) pwd

    pure $ mkApiTransaction
        (txId tx)
        (second (const Nothing) <$> tx ^. #resolvedInputs)
        (tx ^. #outputs)
        (txMeta, txTime)
        #pendingSince
  where
    liftE = throwE . ErrJoinStakePoolNoSuchWallet

delegationFee
    :: forall ctx s t n k.
        ( s ~ SeqState n k
        , k ~ ShelleyKey
        , ctx ~ ApiLayer s t k
        )
    => ctx
    -> ApiT WalletId
    -> Handler ApiFee
delegationFee ctx (ApiT wid) = do
    liftHandler $ withWorkerCtx ctx wid liftE $ \wrk ->
         apiFee <$> W.selectCoinsForDelegation @_ @s @t @k wrk wid
  where
    apiFee = ApiFee . Quantity . fromIntegral . feeBalance
    liftE = throwE . ErrSelectForDelegationNoSuchWallet

quitStakePool
    :: forall ctx s t n k.
        ( DelegationAddress n k
        , s ~ SeqState n k
        , k ~ ShelleyKey
        , HardDerivation k
        , ctx ~ ApiLayer s t k
        )
    => ctx
    -> ApiT WalletId
    -> ApiWalletPassphrase
    -> Handler (ApiTransaction n)
quitStakePool ctx (ApiT wid) (ApiWalletPassphrase (ApiT pwd)) = do
    (tx, txMeta, txTime) <- liftHandler $ withWorkerCtx ctx wid liftE $ \wrk ->
        W.quitStakePool @_ @s @t @k wrk wid (delegationAddress @n) pwd

    pure $ mkApiTransaction
        (txId tx)
        (second (const Nothing) <$> tx ^. #resolvedInputs)
        (tx ^. #outputs)
        (txMeta, txTime)
        #pendingSince
  where
    liftE = throwE . ErrQuitStakePoolNoSuchWallet

{-------------------------------------------------------------------------------
                                Legacy Migrations
-------------------------------------------------------------------------------}

getMigrationInfo
    :: forall s t k. ()
    => ApiLayer s t k
        -- ^ Source wallet context (Legacy)
    -> ApiT WalletId
        -- ^ Source wallet (Legacy)
    -> Handler ApiByronWalletMigrationInfo
getMigrationInfo ctx (ApiT wid) = do
    infoFromSelections <$> getSelections
  where
    infoFromSelections :: [CoinSelection] -> ApiByronWalletMigrationInfo
    infoFromSelections =
        ApiByronWalletMigrationInfo
            . Quantity
            . fromIntegral
            . sum
            . fmap selectionFee

    selectionFee :: CoinSelection -> Word64
    selectionFee s = inputBalance s - changeBalance s

    getSelections :: Handler [CoinSelection]
    getSelections =
        liftHandler
            $ withWorkerCtx ctx wid (throwE . ErrSelectForMigrationNoSuchWallet)
            $ flip (W.selectCoinsForMigration @_ @s @t @k) wid

migrateWallet
    :: forall s t k n.
        ( Show s
        , NFData s
        , IsOwned s k
        , DelegationAddress n ShelleyKey
        )
    => ApiLayer s t k
        -- ^ Source wallet context
    -> ApiLayer (SeqState n ShelleyKey) t ShelleyKey
        -- ^ Target wallet context (Shelley)
    -> ApiT WalletId
        -- ^ Source wallet
    -> ApiT WalletId
        -- ^ Target wallet (Shelley)
    -> ApiWalletPassphrase
    -> Handler [ApiTransaction n]
migrateWallet srcCtx sheCtx (ApiT srcWid) (ApiT sheWid) migrateData = do
    -- FIXME
    -- Better error handling here to inform users if they messed up with the
    -- wallet ids.
    migration <- liftHandler $ do
        withWorkerCtx srcCtx srcWid liftE $ \srcWrk ->
            withWorkerCtx sheCtx sheWid liftE $ \sheWrk -> do
                cs <- W.selectCoinsForMigration @_ @_ @t srcWrk srcWid
                withExceptT ErrSelectForMigrationNoSuchWallet $
                    W.assignMigrationTargetAddresses sheWrk sheWid (delegationAddress @n) cs

    forM migration $ \cs -> do
        (tx, meta, time, wit) <- liftHandler
            $ withWorkerCtx srcCtx srcWid (throwE . ErrSignPaymentNoSuchWallet)
            $ \srcWrk -> W.signTx @_ @s @t @k srcWrk srcWid pwd cs
        liftHandler
            $ withWorkerCtx srcCtx srcWid (throwE . ErrSubmitTxNoSuchWallet)
            $ \wrk -> W.submitTx @_ @_ @t wrk srcWid (tx, meta, wit)
        pure $ mkApiTransaction
            (txId tx)
            (fmap Just <$> NE.toList (W.unsignedInputs cs))
            (NE.toList (W.unsignedOutputs cs))
            (meta, time)
            #pendingSince
  where
    pwd = getApiT $ migrateData ^. #passphrase
    liftE = throwE . ErrSelectForMigrationNoSuchWallet

{-------------------------------------------------------------------------------
                                    Network
-------------------------------------------------------------------------------}

getNetworkInformation
    :: forall t. ()
    => (Block, BlockchainParameters, SyncTolerance)
    -> NetworkLayer IO t Block
    -> Handler ApiNetworkInformation
getNetworkInformation (_block0, bp, st) nl = do
    now <- liftIO getCurrentTime
    nodeTip <- liftHandler (NW.currentNodeTip nl)
    let ntrkTip = fromMaybe slotMinBound (slotAt sp now)
    let nextEpochNo = unsafeEpochSucc (ntrkTip ^. #epochNumber)
    pure $ ApiNetworkInformation
        { syncProgress =
            ApiT $ syncProgressRelativeToTime st sp nodeTip now
        , nextEpoch =
            ApiEpochInfo
                { epochNumber = ApiT nextEpochNo
                , epochStartTime = W.epochStartTime sp nextEpochNo
                }
        , nodeTip =
            ApiBlockReference
                { epochNumber = ApiT $ nodeTip ^. (#slotId . #epochNumber)
                , slotNumber  = ApiT $ nodeTip ^. (#slotId . #slotNumber)
                , height = natural (nodeTip ^. #blockHeight)
                }
        , networkTip =
            ApiNetworkTip
                { epochNumber = ApiT $ ntrkTip ^. #epochNumber
                , slotNumber  = ApiT $ ntrkTip ^. #slotNumber
                }
        }
  where
    sp = W.slotParams bp

    -- Unsafe constructor for the next epoch. Chances to reach the last epoch
    -- are quite unlikely in this context :)
    unsafeEpochSucc :: HasCallStack => W.EpochNo -> W.EpochNo
    unsafeEpochSucc = fromMaybe bomb . W.epochSucc
      where bomb = error "reached final epoch of the Blockchain!?"

getNetworkParameters
    :: (Block, BlockchainParameters, SyncTolerance)
    -> ApiEpochNumber
    -> Handler ApiNetworkParameters
getNetworkParameters (_block0, bp, _st) apiEpochNum = do
    case apiEpochNum of
        ApiEpochNumber epochNum -> do
            now <- liftIO getCurrentTime
            let ntrkTip =
                    fromMaybe slotMinBound (slotAt (W.slotParams bp) now)
            let currentEpochNum = ntrkTip ^. #epochNumber
            when (currentEpochNum < epochNum) $
                liftHandler $ throwE $ ErrNoSuchEpoch
                    { errGivenEpoch = epochNum
                    , errCurrentEpoch = currentEpochNum
                    }
            pure (toApiNetworkParameters bp)

        ApiEpochNumberLatest ->
            pure (toApiNetworkParameters bp)

data ErrNoSuchEpoch = ErrNoSuchEpoch
    { errGivenEpoch :: W.EpochNo
    , errCurrentEpoch :: W.EpochNo
    } deriving (Eq, Show)

getNetworkClock :: NtpClient -> Handler ApiNetworkClock
getNetworkClock = liftIO . getNtpStatus

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
    -> ExceptT ErrCreateWallet IO WalletId
initWorker ctx wid createWallet restoreWallet =
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
            unsafeRunExceptT $ restoreWallet ctx'

        , workerAfter =
            defaultWorkerAfter

        , workerAcquire =
            withDatabase df wid
        }
    re = ctx ^. workerRegistry @s @k
    df = ctx ^. dbFactory @s @k

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
    -> Passphrase "encryption"
    -> Handler (ArgGenChange s)
rndStateChange ctx (ApiT wid) pwd =
    liftHandler $ withWorkerCtx @_ @s @k ctx wid liftE $ \wrk ->
        W.withRootKey @_ @s @k wrk wid pwd ErrSignPaymentWithRootKey $ \xprv ->
            pure (xprv, pwd)
  where
    liftE = throwE . ErrSignPaymentNoSuchWallet

-- | Makes an 'ApiCoinSelection' from the given 'UnsignedTx'.
mkApiCoinSelection :: forall n. UnsignedTx -> ApiCoinSelection n
mkApiCoinSelection (UnsignedTx inputs outputs) =
    ApiCoinSelection
        (mkApiCoinSelectionInput <$> inputs)
        (mkAddressAmount <$> outputs)
  where
    mkAddressAmount :: TxOut -> AddressAmount n
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
    :: forall n.
       Hash "Tx"
    -> [(TxIn, Maybe TxOut)]
    -> [TxOut]
    -> (W.TxMeta, UTCTime)
    -> Lens' (ApiTransaction n) (Maybe ApiTimeReference)
    -> ApiTransaction n
mkApiTransaction txid ins outs (meta, timestamp) setTimeReference =
    tx & setTimeReference .~ Just timeReference
  where
    tx :: ApiTransaction n
    tx = ApiTransaction
        { id = ApiT txid
        , amount = meta ^. #amount
        , insertedAt = Nothing
        , pendingSince = Nothing
        , depth = Quantity 0
        , direction = ApiT (meta ^. #direction)
        , inputs = [ApiTxInput (fmap toAddressAmount o) (ApiT i) | (i, o) <- ins]
        , outputs = toAddressAmount <$> outs
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

    toAddressAmount :: TxOut -> AddressAmount n
    toAddressAmount (TxOut addr (Coin c)) =
        AddressAmount (ApiT addr, Proxy @n) (Quantity $ fromIntegral c)

coerceCoin :: AddressAmount t -> TxOut
coerceCoin (AddressAmount (ApiT addr, _) (Quantity c)) =
    TxOut addr (Coin $ fromIntegral c)

natural :: Quantity q Word32 -> Quantity q Natural
natural = Quantity . fromIntegral . getQuantity

getWalletTip :: Wallet s -> ApiBlockReference
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
    :: forall ctx s t k. ctx ~ ApiLayer s t k
    => Tracer IO (WorkerLog WalletId WalletLog)
    -> (Block, BlockchainParameters, SyncTolerance)
    -> NetworkLayer IO t Block
    -> TransactionLayer t k
    -> DBFactory IO s k
    -> [WalletId]
    -> IO ctx
newApiLayer tr g0 nw tl df wids = do
    re <- Registry.empty
    let ctx = ApiLayer tr g0 nw tl df re
    forM_ wids (registerWorker ctx)
    return ctx

-- | Register a restoration worker to the registry.
registerWorker
    :: forall ctx s t k.
        ( ctx ~ ApiLayer s t k
        )
    => ApiLayer s t k
    -> WalletId
    -> IO ()
registerWorker ctx wid =
    void $ Registry.register @_ @ctx re ctx wid config
  where
    (_, bp, _) = ctx ^. genesisData
    re = ctx ^. workerRegistry
    df = ctx ^. dbFactory
    config = MkWorker
        { workerBefore = \ctx' _ ->
            runExceptT (W.checkWalletIntegrity ctx' wid bp)
                >>= either throwIO pure

        , workerMain = \ctx' _ -> do
            -- FIXME:
            -- Review error handling here
            unsafeRunExceptT $
                W.restoreWallet @(WorkerCtx ctx) @s @t ctx' wid

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
            action $ hoistResource (workerResource wrk) (MsgFromWorker wid) ctx
  where
    re = ctx ^. workerRegistry @s @k

{-------------------------------------------------------------------------------
                                Error Handling
-------------------------------------------------------------------------------}

-- | Lift our wallet layer into servant 'Handler', by mapping each error to a
-- corresponding servant error.
class LiftHandler e where
    liftHandler :: ExceptT e IO a -> Handler a
    liftHandler action = Handler (withExceptT handler action)
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

instance Buildable e => LiftHandler (ErrSelectForPayment e) where
    handler = \case
        ErrSelectForPaymentNoSuchWallet e -> handler e
        ErrSelectForPaymentCoinSelection e -> handler e
        ErrSelectForPaymentFee e -> handler e

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

instance LiftHandler ErrStartTimeLaterThanEndTime where
    handler err = apiError err400 StartTimeLaterThanEndTime $ mconcat
        [ "The specified start time '"
        , toText $ Iso8601Time $ errStartTime err
        , "' is later than the specified end time '"
        , toText $ Iso8601Time $ errEndTime err
        , "'."
        ]

instance LiftHandler ErrCurrentNodeTip where
    handler = \case
        ErrCurrentNodeTipNetworkUnreachable e -> handler e
        ErrCurrentNodeTipNotFound -> apiError err503 NetworkTipNotFound $ mconcat
            [ "I couldn't get the current network tip at the moment. It's "
            , "probably because the node is down or not started yet. Retrying "
            , "in a bit might give better results!"
            ]

instance LiftHandler ErrListStakePools where
     handler = \case
         ErrListStakePoolsMetricsInconsistency e -> handler e
         ErrListStakePoolsCurrentNodeTip e -> handler e
         ErrMetricsIsUnsynced p ->
             apiError err503 NotSynced $ mconcat
                 [ "I can't list stake pools yet because I need to scan the "
                 , "blockchain for metrics first. I'm at "
                 , toText p
                 ]

instance LiftHandler ErrMetricsInconsistency where
    handler = \case
        ErrProducerNotInDistribution producer ->
            apiError err500 UnexpectedError $ mconcat
                [ "Something is terribly wrong with the metrics I collected. "
                , "I recorded that some blocks were produced by "
                , toText producer
                , " but the node doesn't know about this stake pool!"
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
                    , ". I have already joined this pool; joining again would incur"
                    , " an unnecessary fee!"
                    ]
            ErrNoSuchPool pid ->
                apiError err404 NoSuchPool $ mconcat
                    [ "I couldn't find any stake pool with the given id: "
                    , toText pid
                    ]

instance LiftHandler ErrFetchRewards where
    handler = \case
        ErrFetchRewardsNoSuchWallet e -> handler e
        ErrFetchRewardsNetworkUnreachable e -> handler e

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

instance LiftHandler ErrNoSuchEpoch where
    handler = \case
        ErrNoSuchEpoch {errGivenEpoch,errCurrentEpoch}->
            apiError err404 NoSuchEpochNo $ mconcat
                [ "I couldn't show blockchain parameters for epoch number later"
                , " than current one. You requested "
                , pretty errGivenEpoch
                , " epoch. Current one is "
                , pretty errCurrentEpoch
                , ". Use smaller epoch than current or 'latest'."
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
