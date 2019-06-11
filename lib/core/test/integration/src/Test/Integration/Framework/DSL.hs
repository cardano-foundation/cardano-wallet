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

    -- * Steps
    , request
    , unsafeRequest

    -- * Expectations
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
    , expectCliListItemFieldEqual
    , verify
    , Headers(..)
    , Payload(..)
    , RequestException(..)

    -- * Lens
    , addressPoolGap
    , balanceAvailable
    , balanceTotal
    , delegation
    , passphraseLastUpdate
    , walletId
    , walletName
    , state
    , amount
    , direction
    , status

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

    -- * Endpoints
    , getWalletEp
    , deleteWalletEp
    , getAddressesEp
    , postTxEp
    , updateWalletPassEp

    -- * CLI
    , cardanoWalletCLI
    , cardanoWalletLauncherCLI
    , generateMnemonicsViaCLI
    , createWalletViaCLI
    , deleteWalletViaCLI
    , getWalletViaCLI
    , listAddressesViaCLI
    , listWalletsViaCLI
    , updateWalletViaCLI
    , postTransactionViaCLI
    ) where

import Prelude hiding
    ( fail )

import Cardano.Wallet.Api.Types
    ( ApiAddress, ApiT (..), ApiTransaction, ApiWallet )
import Cardano.Wallet.Primitive.AddressDiscovery
    ( AddressPoolGap, getAddressPoolGap, mkAddressPoolGap )
import Cardano.Wallet.Primitive.Mnemonic
    ( entropyToMnemonic, genEntropy, mnemonicToText )
import Cardano.Wallet.Primitive.Types
    ( DecodeAddress (..)
    , Direction (..)
    , EncodeAddress (..)
    , PoolId (..)
    , TxStatus (..)
    , WalletBalance (..)
    , WalletDelegation (..)
    , WalletId (..)
    , WalletName (..)
    , WalletPassphraseInfo (..)
    )
import Control.Concurrent
    ( threadDelay )
import Control.Concurrent.Async
    ( race )
import Control.Monad
    ( forM_, unless, void )
import Control.Monad.Catch
    ( MonadCatch )
import Control.Monad.Fail
    ( MonadFail (..) )
import Control.Monad.IO.Class
    ( MonadIO )
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
import Data.Maybe
    ( fromMaybe )
import Data.Proxy
    ( Proxy (..) )
import Data.Quantity
    ( Quantity (..) )
import Data.Text
    ( Text )
import GHC.TypeLits
    ( Symbol )
import Language.Haskell.TH.Quote
    ( QuasiQuoter )
import Network.HTTP.Types.Method
    ( Method )
import Numeric.Natural
    ( Natural )
import System.Command
    ( CmdResult, command )
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
import Test.Hspec.Expectations.Lifted
    ( shouldBe, shouldContain, shouldNotBe )
import Test.Integration.Faucet
    ( nextWallet )
import Test.Integration.Framework.Request
    ( Context (..)
    , Headers (..)
    , Payload (..)
    , RequestException (..)
    , request
    , unsafeRequest
    )
import Web.HttpApiData
    ( ToHttpApiData (..) )

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.Text as T
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

-- | Expects wallet from the request to eventually reach the given state or
-- beyond
expectEventually
    :: (MonadIO m, MonadCatch m, MonadFail m, Ord a)
    => Context t
    -> Lens' ApiWallet a
    -> a
    -> (HTTP.Status, Either RequestException ApiWallet)
    -> m ()
expectEventually ctx getter target (_, res) = case res of
    Left e -> wantedSuccessButError e
    Right s -> loopUntilRestore (s ^. walletId)
  where
    loopUntilRestore :: (MonadIO m, MonadCatch m) => Text -> m ()
    loopUntilRestore wid = do
        r <- request @ApiWallet ctx ("GET", "v2/wallets/" <> wid) Default Empty
        let target' = getFromResponse getter r
        unless (target' >= target) $ loopUntilRestore wid

-- | Same as `expectEventually` but work directly on ApiWallet
-- , not response from the API
expectEventually'
    :: (MonadIO m, MonadCatch m, MonadFail m, Ord a)
    => Context t
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
    case Aeson.eitherDecode @a (BL8.pack str) of
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

--
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
                (ApiT $ WalletBalance {available = Quantity v, total = Quantity v })

balanceTotal :: HasType (ApiT WalletBalance) s => Lens' s Natural
balanceTotal =
    lens _get _set
  where
    _get :: HasType (ApiT WalletBalance) s => s -> Natural
    _get = fromQuantity @"lovelace" . total . getApiT . view typed
    _set :: HasType (ApiT WalletBalance) s => (s, Natural) -> s
    _set (s, v) = set typed initBal s
        where
            initBal =
                (ApiT $ WalletBalance {available = Quantity v, total = Quantity v })

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
fixtureWallet ctx@(Context _ _ _ faucet _) = do
    mnemonics <- mnemonicToText <$> nextWallet faucet
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
    oneSecond = 1_000_000
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
fixtureWalletWith ctx coins = do
    wSrc <- fixtureWallet ctx
    wUtxo <- emptyWallet ctx
    (_, addrs) <-
        unsafeRequest @[ApiAddress t] ctx (getAddressesEp wUtxo "") Empty
    let addrIds = view #id <$> addrs
    let payments = flip map (zip coins addrIds) $ \(coin, addr) -> [aesonQQ|{
            "address": #{addr},
            "amount": {
                "quantity": #{coin},
                "unit": "lovelace"
            }
        }|]
    let payload = Json [aesonQQ|{
            "payments": #{payments :: [Value]},
            "passphrase": "cardano-wallet"
        }|]
    request @(ApiTransaction t) ctx (postTxEp wSrc) Default payload
        >>= expectResponseCode HTTP.status202
    r <- request @ApiWallet ctx (getWalletEp wUtxo) Default Empty
    verify r [ expectEventually ctx balanceAvailable (sum coins) ]
    void $ request @() ctx (deleteWalletEp wSrc) Default Empty
    return (getFromResponse id r)

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

updateWalletPassEp :: ApiWallet -> (Method, Text)
updateWalletPassEp w =
    ( "PUT"
    , "v2/wallets/" <> w ^. walletId <> "/passphrase"
    )

---
--- CLI
---

-- | Run a command using the 'cardano-wallet-launcher' executable. We run it
-- through stack as we intend to also get code-coverage from running these
-- commands!
cardanoWalletLauncherCLI :: CmdResult r => [String] -> IO r
cardanoWalletLauncherCLI args = command [] "stack"
    (["exec", "--", "cardano-wallet-launcher"] ++ args)

-- | Run a command using the 'cardano-wallet' executable. We run it through
-- stack as we intend to also get code-coverage from running these commands!
cardanoWalletCLI :: CmdResult r => [String] -> IO r
cardanoWalletCLI args = command [] "stack"
    (["exec", "--", "cardano-wallet"] ++ args)

generateMnemonicsViaCLI :: CmdResult r => [String] -> IO r
generateMnemonicsViaCLI args = cardanoWalletCLI
    (["mnemonic", "generate"] ++ args)

createWalletViaCLI
    :: [String]
    -> String
    -> String
    -> String
    -> IO (ExitCode, String, Text)
createWalletViaCLI args mnemonics secondFactor passphrase = do
    let fullArgs =
            [ "exec", "--", "cardano-wallet"
            , "wallet", "create", "--port", "1337"
            ] ++ args
    let process = (proc "stack" fullArgs)
            { std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe }
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

deleteWalletViaCLI :: CmdResult r => String -> IO r
deleteWalletViaCLI walId = cardanoWalletCLI
    ["wallet", "delete", "--port", "1337", walId ]

getWalletViaCLI :: CmdResult r => String -> IO r
getWalletViaCLI walId = cardanoWalletCLI
    ["wallet", "get", "--port", "1337" , walId ]

listAddressesViaCLI :: CmdResult r => [String] -> IO r
listAddressesViaCLI args = cardanoWalletCLI
    (["address", "list", "--port", "1337"] ++ args)

listWalletsViaCLI :: CmdResult r => IO r
listWalletsViaCLI = cardanoWalletCLI
    ["wallet", "list", "--port", "1337" ]

updateWalletViaCLI :: CmdResult r => [String] -> IO r
updateWalletViaCLI args = cardanoWalletCLI
    (["wallet", "update", "--port", "1337"] ++ args)

postTransactionViaCLI :: String -> [String] ->  IO (ExitCode, String, Text)
postTransactionViaCLI passphrase args = do
    let fullArgs =
            [ "exec", "--", "cardano-wallet"
            , "transaction", "create", "--port", "1337"
            ] ++ args
    let process = (proc "stack" fullArgs)
            { std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe }
    withCreateProcess process $ \(Just stdin) (Just stdout) (Just stderr) h -> do
        hPutStr stdin (passphrase ++ "\n")
        hFlush stdin
        hClose stdin
        c <- waitForProcess h
        out <- TIO.hGetContents stdout
        err <- TIO.hGetContents stderr
        return (c, T.unpack out, err)
