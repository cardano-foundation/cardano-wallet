{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

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
    , expectFieldBetween
    , expectListItemFieldEqual
    , expectListSizeEqual
    , expectResponseCode
    , expectEventually
    , expectEventually'
    , expectValidJSON
    , expectCliFieldBetween
    , expectCliFieldEqual
    , expectCliFieldNotEqual
    , expectCliListItemFieldEqual
    , expectWalletUTxO
    , verify
    , Headers(..)
    , Payload(..)
    , RequestException(..)

    -- * Lens
    , addressPoolGap
    , amount
    , balanceAvailable
    , balanceTotal
    , delegation
    , direction
    , feeEstimator
    , passphraseLastUpdate
    , state
    , status
    , walletId
    , walletName

    -- * Helpers
    , (</>)
    , (!!)
    , emptyWallet
    , emptyWalletWith
    , getFromResponse
    , json
    , listAddresses
    , tearDown
    , fixtureWallet
    , fixtureWalletWith
    , faucetAmt
    , faucetUtxoAmt
    , proc'
    , waitForServer
    , collectStreams
    , shouldContainT
    , shouldNotContainT
    , for

    -- * Endpoints
    , getWalletEp
    , deleteWalletEp
    , getWalletUtxoEp
    , getAddressesEp
    , postTxEp
    , postTxFeeEp
    , updateWalletPassEp

    -- * CLI
    , cardanoWalletCLI
    , generateMnemonicsViaCLI
    , createWalletViaCLI
    , deleteWalletViaCLI
    , getWalletUtxoStatisticsViaCLI
    , getWalletViaCLI
    , listAddressesViaCLI
    , listWalletsViaCLI
    , updateWalletNameViaCLI
    , updateWalletPassphraseViaCLI
    , postTransactionViaCLI
    , postTransactionFeeViaCLI
    , listTransactionsViaCLI
    ) where

import Prelude hiding
    ( fail )

import Cardano.Wallet.Api.Types
    ( ApiAddress
    , ApiT (..)
    , ApiTransaction
    , ApiUtxoStatistics (..)
    , ApiWallet
    )
import Cardano.Wallet.Primitive.AddressDiscovery.Sequential
    ( AddressPoolGap, getAddressPoolGap, mkAddressPoolGap )
import Cardano.Wallet.Primitive.Mnemonic
    ( entropyToMnemonic, genEntropy, mnemonicToText )
import Cardano.Wallet.Primitive.Types
    ( Address (..)
    , Coin (..)
    , DecodeAddress (..)
    , Direction (..)
    , EncodeAddress (..)
    , Hash (..)
    , HistogramBar (..)
    , PoolId (..)
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
    ( async, concurrently, race, wait )
import Control.Concurrent.MVar
    ( MVar, modifyMVar_, newMVar, takeMVar )
import Control.Exception
    ( SomeException (..), finally, try )
import Control.Monad
    ( forM_, join, unless, void, (>=>) )
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
import Data.Functor
    ( (<&>) )
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
import Data.Maybe
    ( fromMaybe )
import Data.Proxy
    ( Proxy (..) )
import Data.Quantity
    ( Quantity (..) )
import Data.Text
    ( Text )
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
import Network.Wai.Handler.Warp
    ( Port )
import Numeric.Natural
    ( Natural )
import System.Command
    ( CmdResult, Stderr, Stdout, command )
import System.Directory
    ( doesPathExist )
import System.Exit
    ( ExitCode (..) )
import System.IO
    ( BufferMode (..), Handle, hClose, hFlush, hPutStr, hSetBuffering )
import System.Process
    ( CreateProcess (..)
    , ProcessHandle
    , StdStream (..)
    , proc
    , terminateProcess
    , waitForProcess
    , withCreateProcess
    )
import Test.Hspec
    ( expectationFailure )
import Test.Hspec.Expectations.Lifted
    ( shouldBe, shouldContain, shouldNotBe, shouldNotContain )
import Test.Integration.Faucet
    ( nextWallet )
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
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as TIO
import qualified Network.HTTP.Types.Status as HTTP
--
-- API response expectations
--

-- | Expect an errored response, without any further assumptions
expectError
    :: (MonadIO m, MonadFail m, Show a)
    => (s, Either RequestException a)
    -> m ()
expectError (_, res) = case res of
    Left _  -> return ()
    Right a -> wantedErrorButSuccess a

-- | Expect an errored response, without any further assumptions
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

-- | Expect a successful response, without any further assumptions
expectSuccess
    :: (MonadIO m, MonadFail m)
    => (s, Either RequestException a)
    -> m ()
expectSuccess (_, res) = case res of
    Left e  -> wantedSuccessButError e
    Right _ -> return ()

-- | Expect a given response code on the response
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
--   matches the expected value
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

-- | Expects data list returned by the API to be of certain length
expectListSizeEqual
    :: (MonadIO m, MonadFail m, Foldable xs)
    => Int
    -> (HTTP.Status, Either RequestException (xs a))
    -> m ()
expectListSizeEqual l (_, res) = case res of
    Left e   -> wantedSuccessButError e
    Right xs -> length (toList xs) `shouldBe` l

-- | Expects wallet UTxO statistics from the request to be equal to pre-calculated
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
        let (UTxOStatistics hist stakes bType) = computeUtxoStatistics log10 utxo
        let distr = Map.fromList $ map (\(HistogramBar k v)-> (k,v)) hist
        (ApiUtxoStatistics (Quantity (fromIntegral stakes)) (ApiT bType) distr)
            `shouldBe` stats

-- | Expects wallet from the request to eventually reach the given state or
-- beyond
expectEventually
    :: (MonadIO m, MonadCatch m, MonadFail m)
    => (Ord a, Show a)
    => (HasType (Text, Manager) ctx)
    => ctx
    -> Lens' ApiWallet a
    -> a
    -> (HTTP.Status, Either RequestException ApiWallet)
    -> m ()
expectEventually ctx getter target (_, res) = case res of
    Left e -> wantedSuccessButError e
    Right s -> liftIO $ do
        let wid = s ^. walletId
        winner <- race (threadDelay $ 60 * oneSecond) (loopUntilRestore wid)
        case winner of
            Left _ -> expectationFailure $
                "waited more than 60s for value to exceed " <> show target
            Right _ ->
                return ()
  where
    loopUntilRestore :: Text -> IO ()
    loopUntilRestore wid = do
        r <- request @ApiWallet ctx ("GET", "v2/wallets/" <> wid) Default Empty
        let target' = getFromResponse getter r
        unless (target' >= target) $
            let ms = 1000 in threadDelay (500 * ms) *> loopUntilRestore wid

-- | Like `expectEventually'` but the target is part of the response
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
            "waited more than 60s for value to exceed given target."
        Right _ ->
            return ()
  where
    loopUntilRestore :: Text -> IO ()
    loopUntilRestore wid = do
        r <- request @ApiWallet ctx ("GET", "v2/wallets/" <> wid) Default Empty
        unless (getFromResponse getter r >= getFromResponse target r) $
            let ms = 1000 in threadDelay (500 * ms) *> loopUntilRestore wid

-- | Same as `expectEventually` but work directly on ApiWallet
-- , not response from the API
expectEventually'
    :: (MonadIO m, MonadCatch m, MonadFail m)
    => (Ord a, Show a)
    => (HasType (Text, Manager) ctx)
    => ctx
    -> Lens' ApiWallet a
    -> a
    -> ApiWallet
    -> m ()
expectEventually' ctx target value wallet = do
    rb <- request @ApiWallet ctx (getWalletEp wallet) Default Empty
    expectEventually ctx target value rb
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
            initBal =
                (ApiT $ WalletBalance {available = Quantity v, Types.total = Quantity v })

balanceTotal :: HasType (ApiT WalletBalance) s => Lens' s Natural
balanceTotal =
    lens _get _set
  where
    _get :: HasType (ApiT WalletBalance) s => s -> Natural
    _get = fromQuantity @"lovelace" . Types.total . getApiT . view typed
    _set :: HasType (ApiT WalletBalance) s => (s, Natural) -> s
    _set (s, v) = set typed initBal s
        where
            initBal =
                (ApiT $ WalletBalance {available = Quantity v, Types.total = Quantity v })

delegation
    :: HasType (ApiT (WalletDelegation (ApiT PoolId))) s
    => Lens' s (WalletDelegation (ApiT PoolId))
delegation =
    lens _get _set
  where
    _get
        :: HasType (ApiT (WalletDelegation (ApiT PoolId))) s
        => s
        -> (WalletDelegation (ApiT PoolId))
    _get = getApiT . view typed
    _set
        :: HasType (ApiT (WalletDelegation (ApiT PoolId))) s
        => (s, (WalletDelegation (ApiT PoolId)))
        -> s
    _set (s, v) = set typed (ApiT v ) s

feeEstimator
    :: Lens' (Context t) (TxDescription -> (Natural, Natural))
feeEstimator =
    lens _get _set
  where
    _get = _feeEstimator
    _set (ctx, v) = ctx { _feeEstimator = v }

passphraseLastUpdate
    :: HasType (Maybe (ApiT WalletPassphraseInfo)) s
    => Lens' s (Maybe Text)
passphraseLastUpdate =
    lens _get _set
  where
    _get :: HasType (Maybe (ApiT WalletPassphraseInfo)) s => s -> Maybe Text
    _get = fmap (T.pack . show . lastUpdatedAt . getApiT) . view typed
    _set :: HasType (Maybe (ApiT WalletPassphraseInfo)) s => (s, Maybe Text) -> s
    _set (s, v) = set typed (ApiT . WalletPassphraseInfo . read . T.unpack <$> v) s

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

status :: HasType (ApiT TxStatus) s => Lens' s TxStatus
status =
    lens _get _set
  where
    _get :: HasType (ApiT TxStatus) s => s -> TxStatus
    _get = getApiT . view typed
    _set :: HasType (ApiT TxStatus) s => (s, TxStatus) -> s
    _set (s, v) = set typed (ApiT v) s

--
-- Helpers
--

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
    expectResponseCode @IO HTTP.status202 r
    return (getFromResponse id r)

-- | Create an empty wallet
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
    expectResponseCode @IO HTTP.status202 r
    return (getFromResponse id r)

-- | Restore a faucet and wait until funds are available.
fixtureWallet
    :: Context t
    -> IO ApiWallet
fixtureWallet ctx = do
    mnemonics <- mnemonicToText <$> nextWallet (_faucet ctx)
    let payload = Json [aesonQQ| {
            "name": "Faucet Wallet",
            "mnemonic_sentence": #{mnemonics},
            "passphrase": "cardano-wallet"
            } |]
    r <- request @ApiWallet ctx ("POST", "v2/wallets") Default payload
    let wid = getFromResponse walletId r
    race (threadDelay sixtySeconds) (checkBalance wid) >>= \case
        Left _ -> fail "fixtureWallet: waited too long for initial transaction"
        Right a -> return a
  where
    sixtySeconds = 60*oneSecond
    checkBalance :: Text -> IO ApiWallet
    checkBalance wid = do
        r <- request @ApiWallet ctx ("GET", "v2/wallets/" <> wid) Default Empty
        if getFromResponse balanceAvailable r > 0
            then return (getFromResponse id r)
            else threadDelay oneSecond *> checkBalance wid

-- | Restore a wallet with the given UTxO distribution. Note that there's a
-- limitation to what can be done here. We only have 10 UTxO available in each
-- faucet and they "only" have 'faucetUtxoAmt = 100_000 Ada' in each.
--
-- This function makes no attempt at ensuring the request is valid, so be
-- careful.
fixtureWalletWith
    :: forall t. (EncodeAddress t, DecodeAddress t)
    => Context t
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
            <$> request @[ApiAddress t] ctx (getAddressesEp dest "") Default Empty
        let payments = for (zip coins addrs) $ \(amt, addr) -> [aesonQQ|{
                "address": #{addr},
                "amount": {
                    "quantity": #{amt},
                    "unit": "lovelace"
                }
            }|]
        let payload = Json [aesonQQ|{
                "payments": #{payments :: [Value]},
                "passphrase": "cardano-wallet"
            }|]
        request @(ApiTransaction t) ctx (postTxEp src) Default payload
            >>= expectResponseCode HTTP.status202
        expectEventually' ctx balanceAvailable (sum (balance:coins)) dest
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

json :: QuasiQuoter
json = aesonQQ

listAddresses
    :: forall t. DecodeAddress t
    => Context t
    -> ApiWallet
    -> IO [ApiAddress t]
listAddresses ctx w = do
    (_, addrs) <- unsafeRequest @[ApiAddress t] ctx (getAddressesEp w "") Empty
    return addrs

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
 where
     wallets :: Either RequestException [ApiWallet] -> [ApiWallet]
     wallets c = case c of
         Left e -> error $ "deleteAllWallets: Cannot return wallets: " <> show e
         Right s -> s

-- | Wait for a booting wallet server to start. Wait up to 30s or fail.
waitForServer
    :: forall t ctx. (HasType Port ctx, KnownCommand t)
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

postWalletEp :: (Method, Text)
postWalletEp =
    ( "POST"
    , "v2/wallets"
    )

getWalletEp :: ApiWallet -> (Method, Text)
getWalletEp w =
    ( "GET"
    , "v2/wallets/" <> w ^. walletId
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

-- | Run a command using the 'cardano-wallet' executable. We run it through
-- stack as we intend to also get code-coverage from running these commands!
cardanoWalletCLI
    :: forall t r. (CmdResult r, KnownCommand t)
    => [String]
    -> IO r
cardanoWalletCLI args = command [] "stack"
    (["exec", "--", commandName @t] ++ args)

generateMnemonicsViaCLI
    :: forall t r. (CmdResult r, KnownCommand t)
    => [String]
    -> IO r
generateMnemonicsViaCLI args = cardanoWalletCLI @t
    (["mnemonic", "generate"] ++ args)

createWalletViaCLI
    :: forall t s. (HasType Port s, KnownCommand t)
    => s
    -> [String]
    -> String
    -> String
    -> String
    -> IO (ExitCode, String, Text)
createWalletViaCLI ctx args mnemonics secondFactor passphrase = do
    let portArgs =
            [ "--port", show (ctx ^. typed @Port) ]
    let fullArgs =
            [ "wallet", "create" ] ++ portArgs ++ args
    let process = proc' (commandName @t) fullArgs
    withCreateProcess process $ \(Just stdin) (Just stdout) (Just stderr) h -> do
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
    :: forall t r s. (CmdResult r, KnownCommand t, HasType Port s)
    => s
    -> String
    -> IO r
deleteWalletViaCLI ctx walId = cardanoWalletCLI @t
    ["wallet", "delete", "--port", show (ctx ^. typed @Port), walId ]

getWalletViaCLI
    :: forall t r s. (CmdResult r, KnownCommand t, HasType Port s)
    => s
    -> String
    -> IO r
getWalletViaCLI ctx walId = cardanoWalletCLI @t
    ["wallet", "get", "--port", show (ctx ^. typed @Port) , walId ]

getWalletUtxoStatisticsViaCLI
    :: forall t r s. (CmdResult r, KnownCommand t, HasType Port s)
    => s
    -> String
    -> IO r
getWalletUtxoStatisticsViaCLI ctx walId = cardanoWalletCLI @t
    ["wallet", "utxo", "--port", show (ctx ^. typed @Port) , walId ]

listAddressesViaCLI
    :: forall t r s. (CmdResult r, KnownCommand t, HasType Port s)
    => s
    -> [String]
    -> IO r
listAddressesViaCLI ctx args = cardanoWalletCLI @t
    (["address", "list", "--port", show (ctx ^. typed @Port)] ++ args)

listWalletsViaCLI
    :: forall t r s. (CmdResult r, KnownCommand t, HasType Port s)
    => s
    -> IO r
listWalletsViaCLI ctx = cardanoWalletCLI @t
    ["wallet", "list", "--port", show (ctx ^. typed @Port) ]

updateWalletNameViaCLI
    :: forall t r s. (CmdResult r, KnownCommand t, HasType Port s)
    => s
    -> [String]
    -> IO r
updateWalletNameViaCLI ctx args = cardanoWalletCLI @t
    (["wallet", "update", "name", "--port", show (ctx ^. typed @Port)] ++ args)

updateWalletPassphraseViaCLI
    :: forall t s. (KnownCommand t, HasType Port s)
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
            , "--port", show (ctx ^. typed @Port)
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
    :: forall t s. (HasType Port s, KnownCommand t)
    => s
    -> String
    -> [String]
    -> IO (ExitCode, String, Text)
postTransactionViaCLI ctx passphrase args = do
    let portArgs =
            ["--port", show (ctx ^. typed @Port)]
    let fullArgs =
            ["transaction", "create"] ++ portArgs ++ args
    let process = proc' (commandName @t) fullArgs
    withCreateProcess process $ \(Just stdin) (Just stdout) (Just stderr) h -> do
        hPutStr stdin (passphrase ++ "\n")
        hFlush stdin
        hClose stdin
        c <- waitForProcess h
        out <- TIO.hGetContents stdout
        err <- TIO.hGetContents stderr
        return (c, T.unpack out, err)

postTransactionFeeViaCLI
    :: forall t s. (HasType Port s, KnownCommand t)
    => s
    -> [String]
    -> IO (ExitCode, String, Text)
postTransactionFeeViaCLI ctx args = do
    let portArgs =
            ["--port", show (ctx ^. typed @Port)]
    let fullArgs =
            ["transaction", "fees"] ++ portArgs ++ args
    let process = proc' (commandName @t) fullArgs
    withCreateProcess process $ \_ (Just stdout) (Just stderr) h -> do
        c <- waitForProcess h
        out <- TIO.hGetContents stdout
        err <- TIO.hGetContents stderr
        return (c, T.unpack out, err)

listTransactionsViaCLI
    :: forall t r s . (CmdResult r, HasType Port s, KnownCommand t)
    => s
    -> [String]
    -> IO r
listTransactionsViaCLI ctx args = cardanoWalletCLI @t $ join
    [ ["transaction", "list"]
    , ["--port", show (ctx ^. typed @Port)]
    , args
    ]

-- There is a dependency cycle in the packages.
--
-- cardano-wallet-launcher depends on cardano-wallet-http-bridge so that it can
-- import the HttpBridge module (resp. with jormungandr).
--
-- These packages (cardano-wallet-http-bridge, cardano-wallet-jormungandr) should
-- have build-tool-depends: cardano-wallet:cardano-wallet-launcher so that it can
-- run launcher in the tests. But that dependency can't be expressed in the
-- cabal file, because otherwise there would be a cycle.
--
-- So one hacky way to work around it is by running programs under "stack exec".
proc' :: FilePath -> [String] -> CreateProcess
proc' cmd args = (proc "stack" (["exec", "--", cmd] ++ args))
    { std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe }

-- | Collect lines from standard output and error streams for 30 seconds, or,
-- until a given limit is for both streams.
collectStreams :: (Int, Int) -> CreateProcess -> IO (Text, Text)
collectStreams (nOut0, nErr0) p = do
    let safeP = p { std_out = CreatePipe, std_err = CreatePipe }
    mvar <- newMVar (mempty, mempty)
    withCreateProcess safeP $ \_ (Just o) (Just e) ph -> do
        hSetBuffering o LineBuffering
        hSetBuffering e LineBuffering
        let io = race
                (threadDelay (65 * oneSecond))
                (collect mvar ((o, nOut0), (e, nErr0)) ph)
        void $ io `finally` do
            -- NOTE
            -- Somehow, calling 'terminateProcess' isn't sufficient. We also
            -- need to close the handles otherwise, the function resolves but
            -- the processes remains hanging there for a while...
            terminateProcess ph
            flush o
            flush e
    takeMVar mvar
  where
    flush :: Handle -> IO ()
    flush = try @SomeException . TIO.hGetContents >=> print

    collect
        :: MVar (Text, Text)
        -> ((Handle, Int), (Handle, Int))
        -> ProcessHandle
        -> IO ()
    collect mvar ((stdout, nOut), (stderr, nErr)) ph
        | nOut <= 0 && nErr <= 0 = return ()
        | otherwise = do
            ((out, nOut'), (err, nErr')) <- concurrently
                (getNextLine nOut stdout)
                (getNextLine nErr stderr)
            modifyMVar_ mvar (\(out0, err0) -> return (out0 <> out, err0 <> err))
            collect mvar ((stdout, nOut'), (stderr, nErr')) ph

    getNextLine :: Int -> Handle -> IO (Text, Int)
    getNextLine n h
        | n <= 0 = return (mempty, n)
        | otherwise = do
            threadDelay (10 * oneMillisecond)
            try @SomeException (TIO.hGetLine h) <&> \case
                Left _  -> (mempty, n)
                Right l -> (l, n-1)

-- | Like `shouldContain`, but with 'Text'
shouldContainT :: Text -> Text -> IO ()
shouldContainT a b = T.unpack a `shouldContain` T.unpack b

-- | Like `shouldNotContain`, but with 'Text'
shouldNotContainT :: Text -> Text -> IO ()
shouldNotContainT a b = T.unpack a `shouldNotContain` T.unpack b

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
