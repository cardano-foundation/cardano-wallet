{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-simplifiable-class-constraints #-}

module Test.Integration.Framework.DSL
    ( Context(..)
    , KnownCommand(..)
    , TxDescription(..)

    -- * Steps
    , request
    , unsafeRequest

    -- * Expectations
    , expectPathEventuallyExist
    , expectSuccess
    , expectError
    , expectErrorMessage
    , expectFieldEqual
    , expectFieldNotEqual
    , expectFieldSatisfy
    , expectFieldBetween
    , expectListItemFieldBetween
    , expectListItemFieldEqual
    , expectListItemFieldSatisfy
    , expectListSizeEqual
    , expectResponseCode
    , expectEventually
    , expectEventually'
    , expectValidJSON
    , expectCliFieldBetween
    , expectCliFieldEqual
    , expectCliFieldNotEqual
    , expectCliListItemFieldBetween
    , expectCliListItemFieldEqual
    , expectWalletUTxO
    , verify
    , Headers(..)
    , Payload(..)
    , RequestException(..)

    -- * Lens
    , addressPoolGap
    , amount
    , apparentPerformance
    , byronBalanceAvailable
    , balanceAvailable
    , byronBalanceTotal
    , balanceTotal
    , balanceReward
    , blocks
    , coinSelectionInputs
    , coinSelectionOutputs
    , delegation
    , direction
    , feeEstimator
    , inputs
    , metrics
    , nextEpoch
    , outputs
    , passphraseLastUpdate
    , stake
    , state
    , status
    , syncProgress
    , walletId
    , walletName

    -- * Helpers
    , (</>)
    , (!!)
    , byronWalletMnemonics
    , emptyByronWallet
    , emptyByronWalletWith
    , emptyRandomWallet
    , emptyWallet
    , emptyWalletWith
    , getFromResponse
    , getFromResponseList
    , json
    , joinStakePool
    , delegationFee
    , quitStakePool
    , selectCoins
    , listAddresses
    , listTransactions
    , listAllTransactions
    , tearDown
    , fixtureRawTx
    , fixtureRandomWallet
    , fixtureIcarusWallet
    , fixtureWallet
    , fixtureWalletWith
    , faucetAmt
    , faucetUtxoAmt
    , proc'
    , waitForServer
    , for
    , toQueryString
    , utcIso8601ToText
    , eventually
    , eventuallyUsingDelay
    , eventually_
    , eventuallyUsingDelay_
    , fixturePassphrase
    , waitForNextEpoch

    -- * Endpoints
    , postByronWalletEp
    , migrateByronWalletEp
    , calculateByronMigrationCostEp
    , getByronWalletEp
    , listByronWalletsEp
    , listByronTxEp
    , deleteByronWalletEp
    , deleteByronTxEp
    , getWalletEp
    , deleteWalletEp
    , getWalletUtxoEp
    , getAddressesEp
    , listStakePoolsEp
    , joinStakePoolEp
    , delegationFeeEp
    , quitStakePoolEp
    , selectCoinsEp
    , stakePoolEp
    , postTxEp
    , postExternalTxEp
    , postTxFeeEp
    , listTxEp
    , networkInfoEp
    , updateWalletPassEp
    , listWalletsEp
    , deleteTxEp

    -- * CLI
    , command
    , cardanoWalletCLI
    , generateMnemonicsViaCLI
    , createWalletViaCLI
    , deleteWalletViaCLI
    , getWalletUtxoStatisticsViaCLI
    , getWalletViaCLI
    , listAddressesViaCLI
    , listStakePoolsViaCLI
    , listWalletsViaCLI
    , updateWalletNameViaCLI
    , updateWalletPassphraseViaCLI
    , postTransactionViaCLI
    , postTransactionFeeViaCLI
    , listTransactionsViaCLI
    , postExternalTransactionViaCLI
    , deleteTransactionViaCLI
    ) where

import Cardano.CLI
    ( Port (..) )
import Cardano.Wallet.Api.Types
    ( AddressAmount
    , ApiAddress
    , ApiByronWallet
    , ApiByronWalletBalance
    , ApiCoinSelection
    , ApiCoinSelectionInput
    , ApiEpochInfo
    , ApiFee
    , ApiNetworkInformation
    , ApiStakePoolMetrics
    , ApiT (..)
    , ApiTransaction
    , ApiTxId
    , ApiTxInput (..)
    , ApiUtxoStatistics (..)
    , ApiWallet
    , Iso8601Time (..)
    )
import Cardano.Wallet.Primitive.AddressDerivation
    ( NetworkDiscriminant (..) )
import Cardano.Wallet.Primitive.AddressDiscovery.Sequential
    ( AddressPoolGap, getAddressPoolGap, mkAddressPoolGap )
import Cardano.Wallet.Primitive.Mnemonic
    ( entropyToMnemonic, genEntropy, mnemonicToText )
import Cardano.Wallet.Primitive.Types
    ( Address (..)
    , Coin (..)
    , Direction (..)
    , Hash (..)
    , HistogramBar (..)
    , PoolId (..)
    , SortOrder (..)
    , SyncProgress (..)
    , TxIn (..)
    , TxOut (..)
    , TxStatus (..)
    , UTxO (..)
    , UTxOStatistics (..)
    , WalletBalance (..)
    , WalletDelegation (..)
    , WalletId (..)
    , WalletName (..)
    , WalletPassphraseInfo (..)
    , computeUtxoStatistics
    , log10
    )
import Control.Concurrent
    ( threadDelay )
import Control.Concurrent.Async
    ( async, race, wait )
import Control.Exception
    ( SomeException (..), catch )
import Control.Monad
    ( forM_, join, unless, void )
import Control.Monad.Catch
    ( MonadCatch )
import Control.Monad.Fail
    ( MonadFail (..) )
import Control.Monad.IO.Class
    ( MonadIO, liftIO )
import Control.Retry
    ( capDelay, constantDelay, retrying )
import Crypto.Hash
    ( Blake2b_160, Digest, digestFromByteString )
import Data.Aeson
    ( FromJSON, Value )
import Data.Aeson.QQ
    ( aesonQQ )
import Data.Foldable
    ( toList )
import Data.Function
    ( (&) )
import Data.Generics.Internal.VL.Lens
    ( Lens', lens, set, view, (^.) )
import Data.Generics.Labels
    ()
import Data.Generics.Product.Fields
    ( HasField', getField, setField )
import Data.Generics.Product.Typed
    ( HasType, typed )
import Data.List
    ( (!!) )
import Data.List.NonEmpty
    ( NonEmpty )
import Data.Maybe
    ( catMaybes, fromMaybe )
import Data.Proxy
    ( Proxy (..) )
import Data.Quantity
    ( Quantity (..) )
import Data.Text
    ( Text )
import Data.Text.Class
    ( ToText (..) )
import Data.Time
    ( UTCTime )
import Data.Time.Text
    ( iso8601ExtendedUtc, utcTimeToText )
import Data.Word
    ( Word64 )
import GHC.TypeLits
    ( Symbol )
import Language.Haskell.TH.Quote
    ( QuasiQuoter )
import Network.HTTP.Client
    ( Manager )
import Network.HTTP.Types.Method
    ( Method )
import Numeric.Natural
    ( Natural )
import Prelude hiding
    ( fail )
import System.Command
    ( CmdResult, Stderr, Stdout (..), command )
import System.Directory
    ( doesPathExist )
import System.Exit
    ( ExitCode (..) )
import System.IO
    ( hClose, hFlush, hPutStr )
import System.Process
    ( CreateProcess (..)
    , StdStream (..)
    , proc
    , waitForProcess
    , withCreateProcess
    )
import Test.Hspec
    ( expectationFailure )
import Test.Hspec.Expectations.Lifted
    ( shouldBe, shouldContain, shouldNotBe )
import Test.Integration.Faucet
    ( nextTxBuilder, nextWallet )
import Test.Integration.Framework.Request
    ( Context (..)
    , Headers (..)
    , Payload (..)
    , RequestException (..)
    , TxDescription (..)
    , request
    , unsafeRequest
    )
import Web.HttpApiData
    ( ToHttpApiData (..) )

import qualified Cardano.Wallet.Primitive.Types as Types
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as TIO
import qualified Network.HTTP.Types.Status as HTTP
--
-- API response expectations
--

-- | Expect an error response, without any further assumptions.
expectError
    :: (MonadIO m, MonadFail m, Show a)
    => (s, Either RequestException a)
    -> m ()
expectError (_, res) = case res of
    Left _  -> return ()
    Right a -> wantedErrorButSuccess a

-- | Expect an error response, without any further assumptions.
expectErrorMessage
    :: (MonadIO m, MonadFail m, Show a)
    => String
    -> (s, Either RequestException a)
    -> m ()
expectErrorMessage want (_, res) = case res of
    Left (DecodeFailure msg)  ->
        BL8.unpack msg `shouldContain` want
    Left (ClientError _)  ->
        fail "expectErrorMessage: asserting ClientError not supported yet"
    Left (HttpException _) ->
        fail "expectErrorMessage: asserting HttpException not supported yet"
    Right a -> wantedErrorButSuccess a

-- | Expect a successful response, without any further assumptions.
expectSuccess
    :: (MonadIO m, MonadFail m)
    => (s, Either RequestException a)
    -> m ()
expectSuccess (_, res) = case res of
    Left e  -> wantedSuccessButError e
    Right _ -> return ()

-- | Expect a given response code on the response.
expectResponseCode
    :: (MonadIO m)
    => HTTP.Status
    -> (HTTP.Status, a)
    -> m ()
expectResponseCode want (got, _) =
    got `shouldBe` want

expectFieldEqual
    :: (MonadIO m, MonadFail m, Show a, Eq a)
    => Lens' s a
    -> a
    -> (HTTP.Status, Either RequestException s)
    -> m ()
expectFieldEqual getter a (_, res) = case res of
    Left e  -> wantedSuccessButError e
    Right s -> (view getter s) `shouldBe` a

expectFieldBetween
    :: (MonadIO m, MonadFail m, Show a, Ord a)
    => Lens' s a
    -> (a, a)
    -> (HTTP.Status, Either RequestException s)
    -> m ()
expectFieldBetween getter (aMin, aMax) (_, res) = case res of
    Left e  -> wantedSuccessButError e
    Right s ->
        case view getter s of
            a | a < aMin -> fail $
                "expected " <> show a <> " >= " <> show aMin
            a | a > aMax -> fail $
                "expected " <> show a <> " <= " <> show aMax
            _ ->
                return ()

expectFieldSatisfy
    :: (MonadIO m, MonadFail m, Show a)
    => Lens' s a
    -> (a -> Bool)
    -> (HTTP.Status, Either RequestException s)
    -> m ()
expectFieldSatisfy getter predicate (_, res) = case res of
    Left e  -> wantedSuccessButError e
    Right s ->
        let a = view getter s in
        if predicate a
            then return ()
            else fail $ "predicate failed for: " <> show a

expectFieldNotEqual
    :: (MonadIO m, MonadFail m, Show a, Eq a)
    => Lens' s a
    -> a
    -> (HTTP.Status, Either RequestException s)
    -> m ()
expectFieldNotEqual getter a (_, res) = case res of
    Left e  -> wantedSuccessButError e
    Right s -> (view getter s) `shouldNotBe` a

-- | Expects that returned data list's particular item field
--   matches the expected value.
--   e.g.
--   expectListItemFieldEqual 0 walletName "first" response
--   expectListItemFieldEqual 1 walletName "second" response
expectListItemFieldEqual
    :: (MonadIO m, MonadFail m, Show a, Eq a)
    => Int
    -> Lens' s a
    -> a
    -> (HTTP.Status, Either RequestException [s])
    -> m ()
expectListItemFieldEqual i getter a (c, res) = case res of
    Left e -> wantedSuccessButError e
    Right xs
        | length xs > i -> expectFieldEqual getter a (c, Right (xs !! i))
        | otherwise -> fail $
            "expectListItemFieldEqual: trying to access the #" <> show i <>
            " element from a list but there's none! "

expectListItemFieldSatisfy
    :: (MonadIO m, MonadFail m, Show a)
    => Int
    -> Lens' s a
    -> (a -> Bool)
    -> (HTTP.Status, Either RequestException [s])
    -> m ()
expectListItemFieldSatisfy i getter predicate (c, res) = case res of
    Left e -> wantedSuccessButError e
    Right xs
        | length xs > i ->
            expectFieldSatisfy getter predicate (c, Right (xs !! i))
        | otherwise -> fail $
            "expectListItemFieldSatisfy: trying to access the #" <> show i <>
            " element from a list but there's none! "

expectListItemFieldBetween
    :: (MonadIO m, MonadFail m, Show a, Eq a, Ord a)
    => Int
    -> Lens' s a
    -> (a, a)
    -> (HTTP.Status, Either RequestException [s])
    -> m ()
expectListItemFieldBetween i getter (aMin, aMax) (c, res) = case res of
    Left e -> wantedSuccessButError e
    Right xs
        | length xs > i ->
            expectFieldBetween getter (aMin, aMax) (c, Right (xs !! i))
        | otherwise -> fail $
            "expectListItemFieldBetween: trying to access the #" <> show i <>
            " element from a list but there's none! "

-- | Expects data list returned by the API to be of certain length
expectListSizeEqual
    :: (MonadIO m, MonadFail m, Foldable xs)
    => Int
    -> (HTTP.Status, Either RequestException (xs a))
    -> m ()
expectListSizeEqual l (_, res) = case res of
    Left e   -> wantedSuccessButError e
    Right xs -> length (toList xs) `shouldBe` l

-- | Expects wallet UTxO statistics from the request to be equal to
-- pre-calculated statistics.
expectWalletUTxO
    :: (MonadIO m, MonadFail m)
    => [Word64]
    -> Either RequestException ApiUtxoStatistics
    -> m ()
expectWalletUTxO coins = \case
    Left e  -> wantedSuccessButError e
    Right stats -> do
        let addr = Address "ARBITRARY"
        let constructUtxoEntry idx c =
                ( TxIn (Hash "ARBITRARY") idx
                , TxOut addr (Coin c)
                )
        let utxo = UTxO $ Map.fromList $ zipWith constructUtxoEntry [0..] coins
        let (UTxOStatistics hist stakes bType) =
                computeUtxoStatistics log10 utxo
        let distr = Map.fromList $ map (\(HistogramBar k v)-> (k,v)) hist
        (ApiUtxoStatistics (Quantity (fromIntegral stakes)) (ApiT bType) distr)
            `shouldBe` stats

-- | Expects wallet from the request to eventually reach the given state or
-- beyond.
expectEventually
    :: forall ctx s a m. (MonadIO m, MonadCatch m, MonadFail m)
    => (Ord a, Show a, FromJSON s)
    => (HasType (Text, Manager) ctx)
    => ctx
    -> (s -> (Method, Text))
    -> Lens' s a
    -> a
    -> (HTTP.Status, Either RequestException s)
    -> m ()
expectEventually ctx endpoint getter target (_, res) = case res of
    Left e -> wantedSuccessButError e
    Right s -> liftIO $ do
        winner <- race (threadDelay $ 60 * oneSecond) (loopUntilRestore s)
        case winner of
            Left _ -> expectationFailure $
                "waited more than 60s for value to reach or exceed "
                    <> show target
            Right _ ->
                return ()
  where
    loopUntilRestore :: s -> IO ()
    loopUntilRestore s = do
        r <- request @s ctx (endpoint s) Default Empty
        let current = getFromResponse getter r
        unless (current >= target) $
            let ms = 1000 in threadDelay (500 * ms) *> loopUntilRestore s

-- | Like 'expectEventually', but the target is part of the response.
expectEventuallyL
    :: (MonadIO m, MonadCatch m, MonadFail m)
    => (Ord a, Show a)
    => (HasType (Text, Manager) ctx)
    => ctx
    -> Lens' ApiWallet a
    -> Lens' ApiWallet a
    -> ApiWallet
    -> m ()
expectEventuallyL ctx getter target s = liftIO $ do
    let wid = s ^. walletId
    winner <- race (threadDelay $ 60 * oneSecond) (loopUntilRestore wid)
    case winner of
        Left _ -> expectationFailure
            "waited more than 60s for value to reach or exceed given target."
        Right _ ->
            return ()
  where
    loopUntilRestore :: Text -> IO ()
    loopUntilRestore wid = do
        r <- request @ApiWallet ctx ("GET", "v2/wallets/" <> wid) Default Empty
        unless (getFromResponse getter r >= getFromResponse target r) $
            let ms = 1000 in threadDelay (500 * ms) *> loopUntilRestore wid

-- | Same as 'expectEventually', but works directly on 'ApiWallet'
-- rather than on a response from the API.
expectEventually'
    :: forall ctx s a m. (MonadIO m, MonadCatch m, MonadFail m)
    => (Ord a, Show a, FromJSON s)
    => (HasType (Text, Manager) ctx)
    => ctx
    -> (s -> (Method, Text))
    -> Lens' s a
    -> a
    -> s
    -> m ()
expectEventually' ctx endpoint target value s = do
    rb <- request @s ctx (endpoint s) Default Empty
    expectEventually ctx endpoint target value rb

--
-- CLI output expectations
--

-- | Expects a given string to be a valid JSON output corresponding to some
-- given data-type 'a'. Returns this type if successful.
expectValidJSON
    :: forall m a. (MonadFail m, FromJSON a)
    => Proxy a
    -> String
    -> m a
expectValidJSON _ str =
    case Aeson.eitherDecode @a (BL.fromStrict $ T.encodeUtf8 $ T.pack str) of
        Left e -> fail $ "expected valid JSON but failed decoding: " <> show e
        Right a -> return a

expectCliFieldBetween
    :: (MonadIO m, MonadFail m, Show a, Ord a)
    => Lens' s a
    -> (a, a)
    -> s
    -> m ()
expectCliFieldBetween getter (aMin, aMax) s = case view getter s of
            a | a < aMin -> fail $
                "expected " <> show a <> " >= " <> show aMin
            a | a > aMax -> fail $
                "expected " <> show a <> " <= " <> show aMax
            _ ->
                return ()

expectCliListItemFieldBetween
    :: (MonadIO m, MonadFail m, Show a, Eq a, Ord a)
    => Int
    -> Lens' s a
    -> (a, a)
    -> [s]
    -> m ()
expectCliListItemFieldBetween i getter (aMin, aMax) xs
        | length xs > i = expectCliFieldBetween getter (aMin, aMax) (xs !! i)
        | otherwise = fail $
            "expectCliListItemFieldBetween: trying to access the #" <> show i <>
            " element from a list but there's none! "

expectCliFieldEqual
    :: (MonadIO m, Show a, Eq a)
    => Lens' s a
    -> a
    -> s
    -> m ()
expectCliFieldEqual getter a out = (view getter out) `shouldBe` a

expectCliFieldNotEqual
    :: (MonadIO m, Show a, Eq a)
    => Lens' s a
    -> a
    -> s
    -> m ()
expectCliFieldNotEqual getter a out = (view getter out) `shouldNotBe` a

-- | Same as 'expectListItemFieldEqual' but for CLI
expectCliListItemFieldEqual
    :: (MonadIO m, MonadFail m, Show a, Eq a)
    => Int
    -> Lens' s a
    -> a
    -> [s]
    -> m ()
expectCliListItemFieldEqual i getter a out
        | length out > i = expectCliFieldEqual getter a (out !! i)
        | otherwise = fail $
            "expectCliListItemFieldEqual: trying to access the #" <> show i <>
            " element from a list but there's none! "

-- | A file is eventually created on the given location
expectPathEventuallyExist :: FilePath -> IO ()
expectPathEventuallyExist filepath = do
    handle <- async doesPathExistNow
    winner <- race (threadDelay (60 * oneSecond)) (wait handle)
    case winner of
        Left _ -> expectationFailure $
            "waited more than 60s for " <> filepath <> " to be created!"
        Right _ ->
            return ()
  where
    doesPathExistNow = do
        doesPathExist filepath >>= \case
            True ->
                return ()
            False ->
                threadDelay (oneSecond `div` 2) >> doesPathExistNow

-- Lenses
--
addressPoolGap :: HasType (ApiT AddressPoolGap) s => Lens' s Int
addressPoolGap =
    lens _get _set
  where
    _get :: HasType (ApiT AddressPoolGap) s => s -> Int
    _get = fromIntegral . getAddressPoolGap . getApiT . view typed
    _set :: HasType (ApiT AddressPoolGap) s => (s, Int) -> s
    _set (s, v) = set typed (ApiT $ unsafeMkAddressPoolGap v) s

balanceAvailable :: HasType (ApiT WalletBalance) s => Lens' s Natural
balanceAvailable =
    lens _get _set
  where
    _get :: HasType (ApiT WalletBalance) s => s -> Natural
    _get = fromQuantity @"lovelace" . available . getApiT . view typed
    _set :: HasType (ApiT WalletBalance) s => (s, Natural) -> s
    _set (s, v) = set typed initBal s
      where
        -- TODO: Fix this setter: it sets all fields to the same value.
        initBal = ApiT $ WalletBalance
            { Types.available = Quantity v
            , Types.total = Quantity v
            , Types.reward = Quantity v
            }

byronBalanceAvailable :: HasType (ApiByronWalletBalance) s => Lens' s Natural
byronBalanceAvailable =
    lens _get _set
  where
    _get :: HasType (ApiByronWalletBalance) s => s -> Natural
    _get = fromQuantity @"lovelace" . available' . view typed
    _set :: HasType (ApiByronWalletBalance) s => (s, Natural) -> s
    _set (_s, _v) = error "byronBalanceAvailable setter unimplemented"
    available' :: ApiByronWalletBalance -> Quantity "lovelace" Natural
    available' = getField @"available"

balanceTotal :: HasType (ApiT WalletBalance) s => Lens' s Natural
balanceTotal =
    lens _get _set
  where
    _get :: HasType (ApiT WalletBalance) s => s -> Natural
    _get = fromQuantity @"lovelace" . Types.total . getApiT . view typed
    _set :: HasType (ApiT WalletBalance) s => (s, Natural) -> s
    _set (s, v) = set typed initBal s
      where
        -- TODO: Fix this setter: it sets all fields to the same value.
        initBal = ApiT $ WalletBalance
            { Types.available = Quantity v
            , Types.total = Quantity v
            , Types.reward = Quantity v
            }

byronBalanceTotal :: HasType (ApiByronWalletBalance) s => Lens' s Natural
byronBalanceTotal =
    lens _get _set
  where
    _get :: HasType (ApiByronWalletBalance) s => s -> Natural
    _get = fromQuantity @"lovelace" . total' . view typed
    _set :: HasType (ApiByronWalletBalance) s => (s, Natural) -> s
    _set (_s, _v) = error "byronBalanceTotal setter unimplemented"
    total' :: ApiByronWalletBalance -> Quantity "lovelace" Natural
    total' = getField @"total"


balanceReward :: HasType (ApiT WalletBalance) s => Lens' s Natural
balanceReward =
    lens _get _set
  where
    _get :: HasType (ApiT WalletBalance) s => s -> Natural
    _get = fromQuantity @"lovelace" . Types.reward . getApiT . view typed
    _set :: HasType (ApiT WalletBalance) s => (s, Natural) -> s
    _set (s, v) = set typed initBal s
      where
        -- TODO: Fix this setter: it sets all fields to the same value.
        initBal = ApiT $ WalletBalance
            { Types.available = Quantity v
            , Types.total = Quantity v
            , Types.reward = Quantity v
            }

delegation
    :: forall s d. (d ~ WalletDelegation (ApiT PoolId), HasType (ApiT d) s)
    => Lens' s (WalletDelegation (ApiT PoolId))
delegation =
    lens _get _set
  where
    _get :: s -> d
    _get = getApiT . view typed
    _set :: (s, d) -> s
    _set (s, v) = set typed (ApiT v ) s

feeEstimator
    :: Lens' (Context t) (TxDescription -> (Natural, Natural))
feeEstimator =
    lens _get _set
  where
    _get = _feeEstimator
    _set (ctx, v) = ctx { _feeEstimator = v }

passphraseLastUpdate
    :: forall s i. (i ~ WalletPassphraseInfo, HasType (Maybe (ApiT i)) s)
    => Lens' s (Maybe Text)
passphraseLastUpdate =
    lens _get _set
  where
    _get :: s -> Maybe Text
    _get = fmap (T.pack . show . lastUpdatedAt . getApiT) . view typed
    _set :: (s, Maybe Text) -> s
    _set (s, v) =
        set typed (ApiT . WalletPassphraseInfo . read . T.unpack <$> v) s

state :: HasField' "state" s (ApiT t) => Lens' s t
state =
    lens _get _set
  where
    _get :: HasField' "state" s (ApiT t) => s -> t
    _get = getApiT . getField @"state"
    _set :: HasField' "state" s (ApiT t) => (s, t) -> s
    _set (s, v) = setField @"state" (ApiT v) s

walletName :: HasType (ApiT WalletName) s => Lens' s Text
walletName =
    lens _get _set
  where
    _get :: HasType (ApiT WalletName) s => s -> Text
    _get = getWalletName . getApiT . view typed
    _set :: HasType (ApiT WalletName) s => (s, Text) -> s
    _set (s, v) = set typed (ApiT $ WalletName v) s

walletId :: HasType (ApiT WalletId) s => Lens' s Text
walletId =
    lens _get _set
  where
    _get :: HasType (ApiT WalletId) s => s -> Text
    _get = T.pack . show . getWalletId . getApiT . view typed
    _set :: HasType (ApiT WalletId) s => (s, Text) -> s
    _set (s, v) = set typed (ApiT $ WalletId (unsafeCreateDigest v)) s

amount :: HasType (Quantity "lovelace" Natural) s => Lens' s Natural
amount =
    lens _get _set
  where
    _get :: HasType (Quantity "lovelace" Natural) s => s -> Natural
    _get = fromQuantity @"lovelace" @Natural . view typed
    _set :: HasType (Quantity "lovelace" Natural) s => (s, Natural) -> s
    _set (s, v) = set typed (Quantity @"lovelace" @Natural v) s

direction :: HasType (ApiT Direction) s => Lens' s Direction
direction =
    lens _get _set
  where
    _get :: HasType (ApiT Direction) s => s -> Direction
    _get = getApiT . view typed
    _set :: HasType (ApiT Direction) s => (s, Direction) -> s
    _set (s, v) = set typed (ApiT v) s

inputs :: HasType [ApiTxInput t] s => Lens' s [ApiTxInput t]
inputs =
    lens _get _set
  where
    _get :: HasType [ApiTxInput t] s => s -> [ApiTxInput t]
    _get = view typed
    _set :: HasType [ApiTxInput t] s => (s, [ApiTxInput t]) -> s
    _set (s, v) = set typed v s

outputs
    :: forall s t a. (a ~ AddressAmount t, HasType (NonEmpty a) s)
    => Lens' s [a]
outputs =
    lens _get _set
  where
    _get :: s -> [a]
    _get = NE.toList . view typed
    _set :: (s, [a]) -> s
    _set (s, v) = set typed (NE.fromList v) s

coinSelectionInputs
    :: forall s n a.
        ( s ~ ApiCoinSelection n
        , a ~ ApiCoinSelectionInput n
        , HasType (NonEmpty a) s
        )
    => Lens' s [a]
coinSelectionInputs =
    lens _get _set
  where
    _get = NE.toList . view typed
    _set (s, v) = set typed (NE.fromList v) s

coinSelectionOutputs
    :: forall s n a.
        ( s ~ ApiCoinSelection n
        , a ~ AddressAmount n
        , HasType (NonEmpty a) s
        )
    => Lens' s [a]
coinSelectionOutputs =
    lens _get _set
  where
    _get = NE.toList . view typed
    _set (s, v) = set typed (NE.fromList v) s

status :: HasType (ApiT TxStatus) s => Lens' s TxStatus
status =
    lens _get _set
  where
    _get :: HasType (ApiT TxStatus) s => s -> TxStatus
    _get = getApiT . view typed
    _set :: HasType (ApiT TxStatus) s => (s, TxStatus) -> s
    _set (s, v) = set typed (ApiT v) s

syncProgress :: HasType (ApiT SyncProgress) s => Lens' s SyncProgress
syncProgress =
    lens _get _set
  where
    _get :: HasType (ApiT SyncProgress) s => s -> SyncProgress
    _get = getApiT . view typed
    _set :: HasType (ApiT SyncProgress) s => (s, SyncProgress) -> s
    _set (s, v) = set typed (ApiT v) s

metrics :: HasType ApiStakePoolMetrics s => Lens' s ApiStakePoolMetrics
metrics =
    lens _get _set
  where
    _get :: HasType ApiStakePoolMetrics s => s -> ApiStakePoolMetrics
    _get = view typed
    _set :: HasType ApiStakePoolMetrics s => (s, ApiStakePoolMetrics) -> s
    _set (s, v) = set typed v s

nextEpoch :: HasType ApiEpochInfo s => Lens' s ApiEpochInfo
nextEpoch =
    lens _get _set
  where
    _get :: HasType ApiEpochInfo s => s -> ApiEpochInfo
    _get = view typed
    _set :: HasType ApiEpochInfo s => (s, ApiEpochInfo) -> s
    _set (s, v) = set typed v s

stake
    :: HasField' "controlledStake" s (Quantity "lovelace" Natural)
    => Lens' s Natural
stake = lens
    (getQuantity . getField @"controlledStake")
    (\(s, v) -> setField @"controlledStake" (Quantity v) s)

blocks
    :: HasField' "producedBlocks" s (Quantity "block" Natural)
    => Lens' s Natural
blocks = lens
    (getQuantity . getField @"producedBlocks")
    (\(s, v) -> setField @"producedBlocks" (Quantity v) s)

apparentPerformance :: HasType Double s => Lens' s Double
apparentPerformance =
    lens _get _set
  where
    _get :: HasType Double s => s -> Double
    _get = view typed
    _set :: HasType Double s => (s, Double) -> s
    _set (s, v) = set typed v s

--
-- Helpers
--

waitForNextEpoch
    :: Context t
    -> IO ()
waitForNextEpoch ctx = do
    epoch <- getFromResponse (#nodeTip . #epochNumber) <$>
        request @ApiNetworkInformation ctx networkInfoEp Default Empty
    eventually $ do
        epoch' <- getFromResponse (#nodeTip . #epochNumber) <$>
            request @ApiNetworkInformation ctx networkInfoEp Default Empty
        unless (getApiT epoch' > getApiT epoch) $ fail "not yet"

-- Retry the given action a couple of time until it doesn't throw, or until it
-- has been retried enough.
--
-- It is like 'eventuallyUsingDelay', but with the default delay of 500 ms
-- between retries.
eventually :: IO a -> IO a
eventually = eventuallyUsingDelay (500 * ms)
  where
    ms = 1000

-- Retry the given action a couple of time until it doesn't throw, or until it
-- has been retried enough.
--
-- It sleeps for a specified delay between retries.
eventuallyUsingDelay
    :: Int -- ^ Delay in microseconds
    -> IO a
    -> IO a
eventuallyUsingDelay delay io = do
    winner <- race (threadDelay $ 180 * oneSecond) trial
    case winner of
        Left _ -> fail
            "waited more than 3min for action to eventually resolve."
        Right a ->
            return a
  where
    trial = io `catch` \(_ :: SomeException) -> do
        threadDelay delay
        trial

eventually_ :: IO () -> IO ()
eventually_ = eventuallyUsingDelay_ (500 * ms)
  where
    ms = 1000

eventuallyUsingDelay_
    :: Int -- ^ Delay in microseconds
    -> IO ()
    -> IO ()
eventuallyUsingDelay_ = eventuallyUsingDelay

utcIso8601ToText :: UTCTime -> Text
utcIso8601ToText = utcTimeToText iso8601ExtendedUtc

byronWalletStyles :: [Text]
byronWalletStyles = ["random", "icarus", "trezor", "ledger"]

byronWalletMnemonics :: Map.Map Text (IO [Text])
byronWalletMnemonics = Map.fromList $ zip byronWalletStyles
        [ mnemonicToText @12 . entropyToMnemonic <$> genEntropy
        , mnemonicToText @15 . entropyToMnemonic <$> genEntropy
        , mnemonicToText @24 . entropyToMnemonic <$> genEntropy
        , mnemonicToText @24 . entropyToMnemonic <$> genEntropy
        ]

-- | Create an empty byron wallet
emptyByronWallet :: Text -> Context t -> IO ApiByronWallet
emptyByronWallet style ctx  = do
    mnemonic <- (byronWalletMnemonics Map.! style)
    emptyByronWalletWith ctx style (style <> " wallet", mnemonic, "Secure Passphrase")

emptyRandomWallet :: Context t -> IO ApiByronWallet
emptyRandomWallet = emptyByronWallet "random"

emptyByronWalletWith :: Context t -> Text -> (Text, [Text], Text) -> IO ApiByronWallet
emptyByronWalletWith ctx style (name, mnemonic, pass) = do
    let payload = Json [aesonQQ| {
            "name": #{name},
            "mnemonic_sentence": #{mnemonic},
            "passphrase": #{pass}
        }|]
    r <- request @ApiByronWallet ctx (postByronWalletEp style) Default payload
    expectResponseCode @IO HTTP.status201 r
    return (getFromResponse id r)

-- | Create an empty wallet
emptyWallet :: Context t -> IO ApiWallet
emptyWallet ctx = do
    mnemonic <- (mnemonicToText . entropyToMnemonic) <$> genEntropy @160
    let payload = Json [aesonQQ| {
            "name": "Empty Wallet",
            "mnemonic_sentence": #{mnemonic},
            "passphrase": "Secure Passphrase"
        }|]
    r <- request @ApiWallet ctx postWalletEp Default payload
    expectResponseCode @IO HTTP.status201 r
    return (getFromResponse id r)

-- | Create an empty shelley wallet
emptyWalletWith :: Context t -> (Text, Text, Int) -> IO ApiWallet
emptyWalletWith ctx (name, passphrase, addrPoolGap) = do
    mnemonic <- (mnemonicToText . entropyToMnemonic) <$> genEntropy @160
    let payload = Json [aesonQQ| {
            "name": #{name},
            "mnemonic_sentence": #{mnemonic},
            "passphrase": #{passphrase},
            "address_pool_gap" : #{addrPoolGap}
        }|]
    r <- request @ApiWallet ctx postWalletEp Default payload
    expectResponseCode @IO HTTP.status201 r
    return (getFromResponse id r)

fixtureRawTx
    :: Context t
    -> (Address, Natural)
    -> IO BL.ByteString
fixtureRawTx ctx (addr, amt) =
    nextTxBuilder (_faucet ctx) >>= \build ->
        BL.fromStrict <$> build (addr, Coin $ fromIntegral amt)

-- | Default passphrase used for fixture wallets
fixturePassphrase
    :: Text
fixturePassphrase =
    "cardano-wallet"

-- | Restore a faucet and wait until funds are available.
fixtureWallet
    :: Context t
    -> IO ApiWallet
fixtureWallet ctx = do
    mnemonics <- mnemonicToText <$> nextWallet @"shelley" (_faucet ctx)
    let payload = Json [aesonQQ| {
            "name": "Faucet Wallet",
            "mnemonic_sentence": #{mnemonics},
            "passphrase": #{fixturePassphrase}
            } |]
    (_, w) <- unsafeRequest @ApiWallet ctx postWalletEp payload
    race (threadDelay sixtySeconds) (checkBalance w) >>= \case
        Left _ -> fail "fixtureWallet: waited too long for initial transaction"
        Right a -> return a
  where
    sixtySeconds = 60*oneSecond
    checkBalance w = do
        r <- request @ApiWallet ctx (getWalletEp w) Default Empty
        if getFromResponse balanceAvailable r > 0
            then return (getFromResponse id r)
            else threadDelay oneSecond *> checkBalance w

-- | Restore a faucet Random wallet and wait until funds are available.
fixtureRandomWallet
    :: Context t
    -> IO ApiByronWallet
fixtureRandomWallet ctx = do
    mnemonics <- mnemonicToText <$> nextWallet @"random" (_faucet ctx)
    fixtureLegacyWallet ctx "random" mnemonics

-- | Restore a faucet Icarus wallet and wait until funds are available.
fixtureIcarusWallet
    :: Context t
    -> IO ApiByronWallet
fixtureIcarusWallet ctx = do
    mnemonics <- mnemonicToText <$> nextWallet @"icarus" (_faucet ctx)
    fixtureLegacyWallet ctx "icarus" mnemonics

-- | Restore a legacy wallet (Byron or Icarus)
fixtureLegacyWallet
    :: Context t
    -> Text
    -> [Text]
    -> IO ApiByronWallet
fixtureLegacyWallet ctx style mnemonics = do
    let payload = Json [aesonQQ| {
            "name": "Faucet Byron Wallet",
            "mnemonic_sentence": #{mnemonics},
            "passphrase": #{fixturePassphrase}
            } |]
    (_, w) <- unsafeRequest @ApiByronWallet ctx (postByronWalletEp style) payload
    race (threadDelay sixtySeconds) (checkBalance w) >>= \case
        Left _ ->
            fail "fixtureByronWallet: waited too long for initial transaction"
        Right a ->
            return a
  where
    sixtySeconds = 60*oneSecond
    checkBalance w = do
        r <- request @ApiByronWallet ctx (getByronWalletEp w) Default Empty
        if getFromResponse byronBalanceAvailable r > 0
            then return (getFromResponse id r)
            else threadDelay oneSecond *> checkBalance w

-- | Restore a wallet with the given UTxO distribution. Note that there's a
-- limitation to what can be done here. We only have 10 UTxO available in each
-- faucet and they "only" have 'faucetUtxoAmt = 100_000 Ada' in each.
--
-- This function makes no attempt at ensuring the request is valid, so be
-- careful.
fixtureWalletWith
    :: Context t
    -> [Natural]
    -> IO ApiWallet
fixtureWalletWith ctx coins0 = do
    src <- fixtureWallet ctx
    dest <- emptyWallet ctx
    mapM_ (moveCoins src dest) (groupsOf 10 coins0)
    void $ request @() ctx (deleteWalletEp src) Default Empty
    snd <$> unsafeRequest @ApiWallet ctx (getWalletEp dest) Empty
  where
    -- | Move coins from a wallet to another
    moveCoins
        :: ApiWallet
            -- ^ Source Wallet
        -> ApiWallet
            -- ^ Destination wallet
        -> [Natural]
            -- ^ Coins to move
        -> IO ()
    moveCoins src dest coins = do
        balance <- getFromResponse balanceAvailable
            <$> request @ApiWallet ctx (getWalletEp dest) Default Empty
        addrs <- fmap (view #id) . getFromResponse id
            <$> request @[ApiAddress 'Testnet]
                ctx (getAddressesEp dest "") Default Empty
        let payments = for (zip coins addrs) $ \(amt, addr) -> [aesonQQ|{
                "address": #{addr},
                "amount": {
                    "quantity": #{amt},
                    "unit": "lovelace"
                }
            }|]
        let payload = Json [aesonQQ|{
                "payments": #{payments :: [Value]},
                "passphrase": #{fixturePassphrase}
            }|]
        request @(ApiTransaction 'Testnet) ctx (postTxEp src) Default payload
            >>= expectResponseCode HTTP.status202
        expectEventually'
            ctx getWalletEp balanceAvailable (sum (balance:coins)) dest
        expectEventuallyL ctx balanceAvailable balanceTotal src

-- | Total amount on each faucet wallet
faucetAmt :: Natural
faucetAmt = 10 * faucetUtxoAmt

-- | Each faucet wallet is composed of 10 times a single faucet UTxO of 100_000
-- Ada.
faucetUtxoAmt :: Natural
faucetUtxoAmt = ada 100_000
  where
    ada = (*) (1_000_000)

fromQuantity :: Quantity (u :: Symbol) a -> a
fromQuantity (Quantity a) = a

getFromResponse
    :: Lens' s a
    -> (HTTP.Status, Either RequestException s)
    -> a
getFromResponse getter (_, res) = case res of
    Left _  -> error "getFromResponse failed to get item"
    Right s -> view getter s

getFromResponseList
    :: Int
    -> Lens' s a
    -> (HTTP.Status, Either RequestException [s])
    -> a
getFromResponseList i getter (_, res) = case res of
    Left _ -> error "getFromResponseList failed to get item"
    Right xs
        | length xs > i -> view getter (xs !! i)
        | otherwise -> error $
            "getFromResponseList: trying to access the #" <> show i <>
            " element from a list but there's none! "

json :: QuasiQuoter
json = aesonQQ

joinStakePool
    :: forall t w. (HasType (ApiT WalletId) w)
    => Context t
    -> ApiT PoolId
    -> (w, Text)
    -> IO (HTTP.Status, Either RequestException (ApiTransaction 'Testnet))
joinStakePool ctx p (w, pass) = do
    let payload = Json [aesonQQ| {
            "passphrase": #{pass}
            } |]
    request @(ApiTransaction 'Testnet) ctx (joinStakePoolEp p w) Default payload

quitStakePool
    :: forall t w. (HasType (ApiT WalletId) w)
    => Context t
    -> ApiT PoolId
    -> (w, Text)
    -> IO (HTTP.Status, Either RequestException (ApiTransaction 'Testnet))
quitStakePool ctx p (w, pass) = do
    let payload = Json [aesonQQ| {
            "passphrase": #{pass}
            } |]
    request @(ApiTransaction 'Testnet) ctx (quitStakePoolEp p w) Default payload

selectCoins
    :: forall t w. (HasType (ApiT WalletId) w)
    => Context t
    -> w
    -> NonEmpty (AddressAmount 'Testnet)
    -> IO (HTTP.Status, Either RequestException (ApiCoinSelection 'Testnet))
selectCoins ctx w payments = do
    let payload = Json [aesonQQ| {
            "payments": #{payments}
            } |]
    request @(ApiCoinSelection 'Testnet) ctx (selectCoinsEp w) Default payload

delegationFee
    :: forall t w. (HasType (ApiT WalletId) w)
    => Context t
    -> w
    -> IO (HTTP.Status, Either RequestException ApiFee)
delegationFee ctx w = do
    request @ApiFee ctx (delegationFeeEp w) Default Empty

listAddresses
    :: Context t
    -> ApiWallet
    -> IO [ApiAddress 'Testnet]
listAddresses ctx w = do
    (_, addrs) <- unsafeRequest
        @[ApiAddress 'Testnet] ctx (getAddressesEp w "") Empty
    return addrs

listAllTransactions
    :: Context t
    -> ApiWallet
    -> IO [ApiTransaction 'Testnet]
listAllTransactions ctx w = listTransactions ctx w Nothing Nothing Nothing

listTransactions
    :: Context t
    -> ApiWallet
    -> Maybe UTCTime
    -> Maybe UTCTime
    -> Maybe SortOrder
    -> IO [ApiTransaction 'Testnet]
listTransactions ctx wallet mStart mEnd mOrder = do
    (_, txs) <- unsafeRequest @[ApiTransaction 'Testnet] ctx path Empty
    return txs
  where
    path = listTxEp wallet $ toQueryString $ catMaybes
        [ ("start", ) . toText <$> (Iso8601Time <$> mStart)
        , ("end"  , ) . toText <$> (Iso8601Time <$> mEnd  )
        , ("order", ) . toText <$> mOrder
        ]

toQueryString :: [(Text, Text)] -> Text
toQueryString kvs = if T.null suffix then mempty else "?" <> suffix
  where
    suffix = T.intercalate "&" $ buildQueryParam <$> kvs
    buildQueryParam (k, v) = k <> "=" <> toQueryParam v

infixr 5 </>
(</>) :: ToHttpApiData a => Text -> a -> Text
base </> next = mconcat [base, "/", toQueryParam next]

-- | teardown after each test (currently only deleting all wallets)
tearDown :: Context t -> IO ()
tearDown ctx = do
    resp <- request @[ApiWallet] ctx ("GET", "v2/wallets") Default Empty
    forM_ (wallets (snd resp)) $ \wal -> do
        let endpoint = "v2/wallets" </> wal ^. walletId
        d <- request @Value ctx ("DELETE", endpoint) None Empty
        expectResponseCode HTTP.status204 d
    respByron <-
        request @[ApiByronWallet] ctx ("GET", "v2/byron-wallets") Default Empty
    forM_ (wallets (snd respByron)) $ \wal -> do
        let endpoint = "v2/byron-wallets" </> wal ^. walletId
        d <- request @Value ctx ("DELETE", endpoint) None Empty
        expectResponseCode HTTP.status204 d
 where
     wallets :: forall w . Either RequestException [w] -> [w]
     wallets c = case c of
         Left e -> error $ "deleteAllWallets: Cannot return wallets: " <> show e
         Right s -> s

-- | Wait for a booting wallet server to start. Wait up to 30s or fail.
waitForServer
    :: forall t ctx. (HasType (Port "wallet") ctx, KnownCommand t)
    => ctx
    -> IO ()
waitForServer ctx = void $ retrying
    (capDelay (30*oneSecond) $ constantDelay oneSecond)
    -- NOTE
    -- We still bind the output and error streams to some custom handles because
    -- if we don't, the library defaults to `stdout` and `stderr` which can get
    -- quite noisy.
    (\_ (e, _ :: Stderr, _ :: Stdout) -> pure $ e == ExitFailure 1)
    (const $ listWalletsViaCLI @t ctx)

unsafeCreateDigest :: Text -> Digest Blake2b_160
unsafeCreateDigest s = fromMaybe
    (error $ "unsafeCreateDigest failed to create digest from: " <> show s)
    (digestFromByteString $ B8.pack $ T.unpack s)

unsafeMkAddressPoolGap :: Int -> AddressPoolGap
unsafeMkAddressPoolGap g = case (mkAddressPoolGap $ fromIntegral g) of
    Right a -> a
    Left _ -> error $ "unsafeMkAddressPoolGap: bad argument: " <> show g

wantedSuccessButError
    :: (MonadFail m, Show e)
    => e
    -> m void
wantedSuccessButError =
    fail . ("expected a successful response but got an error: " <>) . show

wantedErrorButSuccess
    :: (MonadFail m, Show a)
    => a
    -> m void
wantedErrorButSuccess =
    fail . ("expected an error but got a successful response: " <>) . show

-- | Apply 'a' to all actions in sequence
verify :: (Monad m) => a -> [a -> m ()] -> m ()
verify a = mapM_ (a &)

---
--- Endoints
---

networkInfoEp :: (Method, Text)
networkInfoEp =
    ( "GET"
    , "v2/network/information"
    )

postByronWalletEp :: Text -> (Method, Text)
postByronWalletEp style =
    ( "POST"
    , "v2/byron-wallets/" <> style
    )

listByronWalletsEp :: (Method, Text)
listByronWalletsEp =
    ( "GET"
    , "v2/byron-wallets"
    )

listByronTxEp :: ApiByronWallet -> Text -> (Method, Text)
listByronTxEp w query =
    ( "GET"
    , "v2/byron-wallets/" <> w ^. walletId <> "/transactions" <> query
    )

migrateByronWalletEp
    :: forall w n. (HasType (ApiT WalletId) w, HasType (ApiT WalletId) n)
    => w
    -> n
    -> (Method, Text)
migrateByronWalletEp wSrc wDest =
    ( "POST"
    , "v2/byron-wallets/" <>
        wSrc ^. walletId <> "/migrations/" <> wDest ^. walletId
    )

calculateByronMigrationCostEp
    :: forall w. (HasType (ApiT WalletId) w)
    => w
    -> (Method, Text)
calculateByronMigrationCostEp w =
    ( "GET"
    , "v2/byron-wallets/" <> w ^. walletId <> "/migrations"
    )

getByronWalletEp :: ApiByronWallet -> (Method, Text)
getByronWalletEp w =
    ( "GET"
    , "v2/byron-wallets/" <> w ^. walletId
    )

deleteByronWalletEp :: ApiByronWallet -> (Method, Text)
deleteByronWalletEp w =
    ( "DELETE"
    , "v2/byron-wallets/" <> w ^. walletId
    )

deleteByronTxEp :: ApiByronWallet -> ApiTxId -> (Method, Text)
deleteByronTxEp w tid =
    ( "DELETE"
    , "v2/byron-wallets/" <> w ^. walletId <> "/transactions/" <>
        (toUrlPiece tid)
    )

listStakePoolsEp :: (Method, Text)
listStakePoolsEp =
    ( "GET"
    , "v2/stake-pools"
    )

stakePoolEp
    :: forall w. (HasType (ApiT WalletId) w)
    => Method
    -> ApiT PoolId
    -> w
    -> (Method, Text)
stakePoolEp verb poolId w =
    ( verb
    , "v2/stake-pools/"
        <> toText (getApiT poolId)
        <> "/wallets/"
        <> w ^. walletId
    )

joinStakePoolEp
    :: forall w. (HasType (ApiT WalletId) w)
    => ApiT PoolId
    -> w
    -> (Method, Text)
joinStakePoolEp = stakePoolEp "PUT"

delegationFeeEp
    :: forall w. (HasType (ApiT WalletId) w)
    => w
    -> (Method, Text)
delegationFeeEp w =
    ( "GET"
    , "v2/wallets/" <> w ^. walletId <> "/delegations/fees"
    )

quitStakePoolEp
    :: forall w. (HasType (ApiT WalletId) w)
    => ApiT PoolId
    -> w
    -> (Method, Text)
quitStakePoolEp = stakePoolEp "DELETE"

selectCoinsEp :: HasType (ApiT WalletId) w => w -> (Method, Text)
selectCoinsEp w =
    ( "POST"
    , "v2/wallets/" <> w ^. walletId <> "/coin-selections/random"
    )

postWalletEp :: (Method, Text)
postWalletEp =
    ( "POST"
    , "v2/wallets"
    )

postExternalTxEp :: (Method, Text)
postExternalTxEp =
    ( "POST"
    , "v2/proxy/transactions"
    )

getWalletEp :: ApiWallet -> (Method, Text)
getWalletEp w =
    ( "GET"
    , "v2/wallets/" <> w ^. walletId
    )

listWalletsEp :: (Method, Text)
listWalletsEp =
    ( "GET"
    , "v2/wallets"
    )

deleteWalletEp :: ApiWallet -> (Method, Text)
deleteWalletEp w =
    ( "DELETE"
    , "v2/wallets/" <> w ^. walletId
    )

getWalletUtxoEp :: ApiWallet -> (Method, Text)
getWalletUtxoEp w =
    ( "GET"
    , "v2/wallets/" <> w ^. walletId <> "/statistics/utxos"
    )

getAddressesEp :: ApiWallet -> Text -> (Method, Text)
getAddressesEp w stateFilter =
    ( "GET"
    , "v2/wallets/" <> w ^. walletId <> "/addresses" <> stateFilter
    )

postTxEp :: ApiWallet -> (Method, Text)
postTxEp w =
    ( "POST"
    , "v2/wallets/" <> w ^. walletId <> "/transactions"
    )

deleteTxEp :: ApiWallet -> ApiTxId -> (Method, Text)
deleteTxEp w tid =
    ( "DELETE"
    , "v2/wallets/" <> w ^. walletId <> "/transactions/" <> (toUrlPiece tid)
    )

listTxEp :: ApiWallet -> Text -> (Method, Text)
listTxEp w query =
    ( "GET"
    , "v2/wallets/" <> w ^. walletId <> "/transactions" <> query
    )

postTxFeeEp :: ApiWallet -> (Method, Text)
postTxFeeEp w =
    ( "POST"
    , "v2/wallets/" <> w ^. walletId <> "/transactions/fees"
    )

updateWalletPassEp :: ApiWallet -> (Method, Text)
updateWalletPassEp w =
    ( "PUT"
    , "v2/wallets/" <> w ^. walletId <> "/passphrase"
    )

---
--- CLI
---

-- | A class to select the right command for a given 'Context t'
class KnownCommand t where
    commandName :: String

-- | Run a command using the 'cardano-wallet' executable for the target @t@.
cardanoWalletCLI
    :: forall t r. (CmdResult r, KnownCommand t)
    => [String]
    -> IO r
cardanoWalletCLI = command [] (commandName @t)

generateMnemonicsViaCLI
    :: forall t r. (CmdResult r, KnownCommand t)
    => [String]
    -> IO r
generateMnemonicsViaCLI args = cardanoWalletCLI @t
    (["mnemonic", "generate"] ++ args)

createWalletViaCLI
    :: forall t s. (HasType (Port "wallet") s, KnownCommand t)
    => s
    -> [String]
    -> String
    -> String
    -> String
    -> IO (ExitCode, String, Text)
createWalletViaCLI ctx args mnemonics secondFactor passphrase = do
    let portArgs =
            [ "--port", show (ctx ^. typed @(Port "wallet")) ]
    let fullArgs =
            [ "wallet", "create" ] ++ portArgs ++ args
    let process = proc' (commandName @t) fullArgs
    withCreateProcess process $
        \(Just stdin) (Just stdout) (Just stderr) h -> do
            hPutStr stdin mnemonics
            hPutStr stdin secondFactor
            hPutStr stdin (passphrase ++ "\n")
            hPutStr stdin (passphrase ++ "\n")
            hFlush stdin
            hClose stdin
            c <- waitForProcess h
            out <- TIO.hGetContents stdout
            err <- TIO.hGetContents stderr
            return (c, T.unpack out, err)

deleteWalletViaCLI
    :: forall t r s. (CmdResult r, KnownCommand t, HasType (Port "wallet") s)
    => s
    -> String
    -> IO r
deleteWalletViaCLI ctx walId = cardanoWalletCLI @t
    ["wallet", "delete", "--port", show (ctx ^. typed @(Port "wallet")), walId ]

getWalletViaCLI
    :: forall t r s. (CmdResult r, KnownCommand t, HasType (Port "wallet") s)
    => s
    -> String
    -> IO r
getWalletViaCLI ctx walId = cardanoWalletCLI @t
    ["wallet", "get", "--port", show (ctx ^. typed @(Port "wallet")) , walId ]

getWalletUtxoStatisticsViaCLI
    :: forall t r s. (CmdResult r, KnownCommand t, HasType (Port "wallet") s)
    => s
    -> String
    -> IO r
getWalletUtxoStatisticsViaCLI ctx walId = cardanoWalletCLI @t
    ["wallet", "utxo", "--port", show (ctx ^. typed @(Port "wallet")) , walId ]

listAddressesViaCLI
    :: forall t r s. (CmdResult r, KnownCommand t, HasType (Port "wallet") s)
    => s
    -> [String]
    -> IO r
listAddressesViaCLI ctx args = cardanoWalletCLI @t $
    ["address", "list", "--port", show (ctx ^. typed @(Port "wallet"))] ++ args

listStakePoolsViaCLI
    :: forall t r s. (CmdResult r, KnownCommand t, HasType (Port "wallet") s)
    => s
    -> IO r
listStakePoolsViaCLI ctx = cardanoWalletCLI @t
    ["stake-pool", "list", "--port", show (ctx ^. typed @(Port "wallet")) ]

listWalletsViaCLI
    :: forall t r s. (CmdResult r, KnownCommand t, HasType (Port "wallet") s)
    => s
    -> IO r
listWalletsViaCLI ctx = cardanoWalletCLI @t
    ["wallet", "list", "--port", show (ctx ^. typed @(Port "wallet")) ]

updateWalletNameViaCLI
    :: forall t r s. (CmdResult r, KnownCommand t, HasType (Port "wallet") s)
    => s
    -> [String]
    -> IO r
updateWalletNameViaCLI ctx args = cardanoWalletCLI @t
    (["wallet", "update", "name", "--port", walletPort] ++ args)
  where
    walletPort = show (ctx ^. typed @(Port "wallet"))

updateWalletPassphraseViaCLI
    :: forall t s. (KnownCommand t, HasType (Port "wallet") s)
    => s
    -> String
        -- ^ Wallet id
    -> String
        -- ^ Old passphrase
    -> String
        -- ^ New passphrase
    -> String
        -- ^ New passphrase (repeated for confirmation)
    -> IO (ExitCode, Text, Text)
updateWalletPassphraseViaCLI ctx wid ppOld ppNew ppNewConfirm = do
    let process = proc' (commandName @t)
            [ "wallet", "update", "passphrase"
            , "--port", show (ctx ^. typed @(Port "wallet"))
            , wid
            ]
    withCreateProcess process $
        \(Just stdin) (Just stdout) (Just stderr) h -> do
            hPutStr stdin (ppOld <> "\n")
            hPutStr stdin (ppNew <> "\n")
            hPutStr stdin (ppNewConfirm <> "\n")
            hFlush stdin
            hClose stdin
            c <- waitForProcess h
            out <- TIO.hGetContents stdout
            err <- TIO.hGetContents stderr
            pure (c, out, err)

postTransactionViaCLI
    :: forall t s. (HasType (Port "wallet") s, KnownCommand t)
    => s
    -> String
    -> [String]
    -> IO (ExitCode, String, Text)
postTransactionViaCLI ctx passphrase args = do
    let portArgs =
            ["--port", show (ctx ^. typed @(Port "wallet"))]
    let fullArgs =
            ["transaction", "create"] ++ portArgs ++ args
    let process = proc' (commandName @t) fullArgs
    withCreateProcess process $
        \(Just stdin) (Just stdout) (Just stderr) h -> do
            hPutStr stdin (passphrase ++ "\n")
            hFlush stdin
            hClose stdin
            c <- waitForProcess h
            out <- TIO.hGetContents stdout
            err <- TIO.hGetContents stderr
            return (c, T.unpack out, err)

postTransactionFeeViaCLI
    :: forall t s. (HasType (Port "wallet") s, KnownCommand t)
    => s
    -> [String]
    -> IO (ExitCode, String, Text)
postTransactionFeeViaCLI ctx args = do
    let portArgs =
            ["--port", show (ctx ^. typed @(Port "wallet"))]
    let fullArgs =
            ["transaction", "fees"] ++ portArgs ++ args
    let process = proc' (commandName @t) fullArgs
    withCreateProcess process $ \_ (Just stdout) (Just stderr) h -> do
        c <- waitForProcess h
        out <- TIO.hGetContents stdout
        err <- TIO.hGetContents stderr
        return (c, T.unpack out, err)

listTransactionsViaCLI
    :: forall t r s . (CmdResult r, HasType (Port "wallet") s, KnownCommand t)
    => s
    -> [String]
    -> IO r
listTransactionsViaCLI ctx args = cardanoWalletCLI @t $ join
    [ ["transaction", "list"]
    , ["--port", show (ctx ^. typed @(Port "wallet"))]
    , args
    ]

postExternalTransactionViaCLI
    :: forall t r s . (CmdResult r, HasType (Port "wallet") s, KnownCommand t)
    => s
    -> [String]
    -> IO r
postExternalTransactionViaCLI ctx args = cardanoWalletCLI @t $ join
    [ ["transaction", "submit"]
    , ["--port", show (ctx ^. typed @(Port "wallet"))]
    , args
    ]

deleteTransactionViaCLI
    :: forall t r s. (CmdResult r, KnownCommand t, HasType (Port "wallet") s)
    => s
    -> String
    -> String
    -> IO r
deleteTransactionViaCLI ctx wid tid = cardanoWalletCLI @t $ join
    [ ["transaction", "forget"]
    , ["--port", show (ctx ^. typed @(Port "wallet")), wid, tid]
    ]

proc' :: FilePath -> [String] -> CreateProcess
proc' cmd args = (proc cmd args)
    { std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe }

oneSecond :: Int
oneSecond = 1_000 * oneMillisecond

oneMillisecond :: Int
oneMillisecond = 1_000

-- | Creates group of at most `n` elements. Last group may be smaller if
-- it's not properly divisible.
groupsOf :: Int -> [a] -> [[a]]
groupsOf _ [] = []
groupsOf n xs = take n xs : groupsOf n (drop n xs)

-- | 'map' flipped.
for :: [a] -> (a -> b) -> [b]
for = flip map
