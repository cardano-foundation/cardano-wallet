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
    , expectField
    , expectListField
    , expectListSize
    , expectResponseCode
    , expectValidJSON
    , expectCliField
    , expectCliListField
    , expectWalletUTxO
    , between
    , (.>=)
    , (.<=)
    , (.>)
    , verify
    , Headers(..)
    , Payload(..)
    , RequestException(..)

    -- * Lens
    , walletId

    -- * Helpers
    , (</>)
    , (!!)
    , restoreWalletFromPubKey
    , emptyRandomWallet
    , emptyRandomWalletMws
    , emptyRandomWalletWithPasswd
    , emptyIcarusWallet
    , emptyIcarusWalletMws
    , emptyByronWalletWith
    , emptyWallet
    , emptyWalletWith
    , emptyByronWalletFromXPrvWith
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
    , fixtureRandomWalletMws
    , fixtureRandomWalletAddrs
    , fixtureRandomWalletWith
    , fixtureIcarusWallet
    , fixtureIcarusWalletMws
    , fixtureIcarusWalletAddrs
    , fixtureIcarusWalletWith
    , fixtureWallet
    , fixtureWalletWith
    , fixtureWalletWithMnemonics
    , faucetAmt
    , faucetUtxoAmt
    , proc'
    , waitForServer
    , for
    , utcIso8601ToText
    , eventually
    , eventuallyUsingDelay
    , fixturePassphrase
    , fixturePassphraseEncrypted
    , waitForNextEpoch
    , waitAllTxsInLedger
    , toQueryString
    , withMethod
    , withPathParam
    , icarusAddresses
    , randomAddresses
    , shelleyAddresses
    , pubKeyFromMnemonics
    , rootPrvKeyFromMnemonics
    , unsafeGetTransactionTime
    , getTxId

    -- * Delegation helpers
    , mkEpochInfo
    , notDelegating
    , delegating
    , getSlotParams

    -- * CLI
    , command
    , cardanoWalletCLI
    , generateMnemonicsViaCLI
    , createWalletViaCLI
    , createWalletFromPublicKeyViaCLI
    , deleteWalletViaCLI
    , getWalletUtxoStatisticsViaCLI
    , getWalletViaCLI
    , createAddressViaCLI
    , importAddressViaCLI
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
    , getTransactionViaCLI
    ) where

import Cardano.CLI
    ( Port (..) )
import Cardano.Mnemonic
    ( MkSomeMnemonic (..)
    , Mnemonic
    , SomeMnemonic (..)
    , entropyToMnemonic
    , genEntropy
    , mnemonicToText
    )
import Cardano.Wallet.Api.Types
    ( AddressAmount
    , ApiAddress
    , ApiByronWallet
    , ApiCoinSelection
    , ApiEpochInfo (ApiEpochInfo)
    , ApiFee
    , ApiNetworkInformation
    , ApiNetworkParameters (..)
    , ApiT (..)
    , ApiTransaction
    , ApiTxId (ApiTxId)
    , ApiUtxoStatistics (..)
    , ApiWallet
    , ApiWalletDelegation (..)
    , ApiWalletDelegationNext (..)
    , ApiWalletDelegationStatus (..)
    , DecodeAddress (..)
    , DecodeStakeAddress (..)
    , EncodeAddress (..)
    , Iso8601Time (..)
    , WalletStyle (..)
    , insertedAt
    , time
    )
import Cardano.Wallet.Primitive.AddressDerivation
    ( AccountingStyle (..)
    , HardDerivation (..)
    , NetworkDiscriminant (..)
    , Passphrase (..)
    , PaymentAddress (..)
    , PersistPublicKey (..)
    , WalletKey (..)
    , hex
    , preparePassphrase
    )
import Cardano.Wallet.Primitive.AddressDerivation.Byron
    ( ByronKey (..) )
import Cardano.Wallet.Primitive.AddressDerivation.Icarus
    ( IcarusKey )
import Cardano.Wallet.Primitive.AddressDerivation.Jormungandr
    ( generateKeyFromSeed )
import Cardano.Wallet.Primitive.AddressDerivation.Shelley
    ( ShelleyKey )
import Cardano.Wallet.Primitive.Slotting
    ( SlotParameters (..), epochStartTime )
import Cardano.Wallet.Primitive.SyncProgress
    ( SyncProgress (..) )
import Cardano.Wallet.Primitive.Types
    ( ActiveSlotCoefficient (..)
    , Address (..)
    , Coin (..)
    , EpochLength (..)
    , EpochNo
    , Hash (..)
    , HistogramBar (..)
    , PoolId (..)
    , SlotLength (..)
    , SortOrder (..)
    , TxIn (..)
    , TxOut (..)
    , TxStatus (..)
    , UTxO (..)
    , UTxOStatistics (..)
    , WalletId (..)
    , computeUtxoStatistics
    , log10
    )
import Control.Arrow
    ( second )
import Control.Concurrent
    ( threadDelay )
import Control.Concurrent.Async
    ( async, race, wait )
import Control.Exception
    ( SomeException (..), catch )
import Control.Monad
    ( forM_, join, unless, void )
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
import Data.Functor.Identity
    ( Identity (..) )
import Data.Generics.Internal.VL.Lens
    ( Lens', lens, set, view, (^.) )
import Data.Generics.Labels
    ()
import Data.Generics.Product.Typed
    ( HasType, typed )
import Data.IORef
    ( newIORef, readIORef, writeIORef )
import Data.List
    ( (!!) )
import Data.List.NonEmpty
    ( NonEmpty )
import Data.Maybe
    ( fromMaybe )
import Data.Proxy
    ( Proxy (..) )
import Data.Quantity
    ( Quantity (..) )
import Data.Text
    ( Text )
import Data.Time
    ( UTCTime )
import Data.Time.Text
    ( iso8601ExtendedUtc, utcTimeToText )
import Data.Word
    ( Word64 )
import Language.Haskell.TH.Quote
    ( QuasiQuoter )
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
    ( Expectation, HasCallStack, expectationFailure )
import Test.Hspec.Expectations.Lifted
    ( shouldBe, shouldContain, shouldSatisfy )
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

import qualified Cardano.Wallet.Api.Link as Link
import qualified Cardano.Wallet.Primitive.AddressDerivation.Byron as Byron
import qualified Cardano.Wallet.Primitive.AddressDerivation.Icarus as Icarus
import qualified Cardano.Wallet.Primitive.AddressDerivation.Shelley as Shelley
import qualified Cardano.Wallet.Primitive.Types as W
import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Write as CBOR
import qualified Crypto.Scrypt as Scrypt
import qualified Data.Aeson as Aeson
import qualified Data.ByteArray as BA
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

-- | Expect an error response, without any further assumptions.
expectError
    :: (HasCallStack, MonadIO m, MonadFail m, Show a)
    => (s, Either RequestException a)
    -> m ()
expectError (_, res) = case res of
    Left _  -> return ()
    Right a -> wantedErrorButSuccess a

-- | Expect an error response, without any further assumptions.
expectErrorMessage
    :: (HasCallStack, MonadIO m, MonadFail m, Show a)
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
    :: (HasCallStack, MonadIO m, MonadFail m)
    => (s, Either RequestException a)
    -> m ()
expectSuccess (_, res) = case res of
    Left e  -> wantedSuccessButError e
    Right _ -> return ()

-- | Expect a given response code on the response.
expectResponseCode
    :: (HasCallStack, MonadIO m, Show a)
    => HTTP.Status
    -> (HTTP.Status, a)
    -> m ()
expectResponseCode want (got, a) =
    if got == want
        then pure ()
        else liftIO $ expectationFailure $ unlines
            [ "expected: " <> show want
            , " but got: " <> show got
            , ""
            , "from the following response: " <> show a
            ]

expectField
    :: (HasCallStack, MonadIO m, MonadFail m, Show a)
    => Lens' s a
    -> (a -> Expectation)
    -> (HTTP.Status, Either RequestException s)
    -> m ()
expectField getter predicate (_, res) = case res of
    Left e  -> wantedSuccessButError e
    Right s ->
        let a = view getter s in
        liftIO $ predicate a

expectListField
    :: (HasCallStack, MonadIO m, MonadFail m, Show a)
    => Int
    -> Lens' s a
    -> (a -> Expectation)
    -> (HTTP.Status, Either RequestException [s])
    -> m ()
expectListField i getter predicate (c, res) = case res of
    Left e -> wantedSuccessButError e
    Right xs
        | length xs > i ->
            expectField getter predicate (c, Right (xs !! i))
        | otherwise -> fail $
            "expectListField: trying to access the #" <> show i <>
            " element from a list but there's none! "

-- | Expects data list returned by the API to be of certain length
expectListSize
    :: (HasCallStack, MonadIO m, MonadFail m, Foldable xs)
    => Int
    -> (HTTP.Status, Either RequestException (xs a))
    -> m ()
expectListSize l (_, res) = case res of
    Left e   -> wantedSuccessButError e
    Right xs -> length (toList xs) `shouldBe` l

-- | Expects wallet UTxO statistics from the request to be equal to
-- pre-calculated statistics.
expectWalletUTxO
    :: (HasCallStack, MonadIO m, MonadFail m)
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

--
-- CLI output expectations
--

-- | Expects a given string to be a valid JSON output corresponding to some
-- given data-type 'a'. Returns this type if successful.
expectValidJSON
    :: forall m a. (HasCallStack, MonadFail m, FromJSON a)
    => Proxy a
    -> String
    -> m a
expectValidJSON _ str =
    case Aeson.eitherDecode @a (BL.fromStrict $ T.encodeUtf8 $ T.pack str) of
        Left e -> fail $ "expected valid JSON but failed decoding: " <> show e
        Right a -> return a

expectCliListField
    :: (HasCallStack, MonadIO m, MonadFail m, Show a)
    => Int
    -> Lens' s a
    -> (a -> Expectation)
    -> [s]
    -> m ()
expectCliListField i getter predicate xs
        | length xs > i = expectCliField getter predicate (xs !! i)
        | otherwise = fail $
            "expectCliListField: trying to access the #" <> show i <>
            " element from a list but there's none! "

expectCliField
    :: (HasCallStack, MonadIO m, MonadFail m, Show a)
    => Lens' s a
    -> (a -> Expectation)
    -> s
    -> m ()
expectCliField getter predicate out = do
    let a = (view getter out)
    liftIO $ predicate a

-- | A file is eventually created on the given location
expectPathEventuallyExist :: HasCallStack => FilePath -> IO ()
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

walletId :: HasType (ApiT WalletId) s => Lens' s Text
walletId =
    lens _get _set
  where
    _get :: HasType (ApiT WalletId) s => s -> Text
    _get = T.pack . show . getWalletId . getApiT . view typed
    _set :: HasType (ApiT WalletId) s => (s, Text) -> s
    _set (s, v) = set typed (ApiT $ WalletId (unsafeCreateDigest v)) s

--
-- Helpers
--
getTxId :: (ApiTransaction n) -> String
getTxId tx = T.unpack $ toUrlPiece $ ApiTxId (tx ^. #id)

unsafeGetTransactionTime
    :: [ApiTransaction n]
    -> UTCTime
unsafeGetTransactionTime txs =
    case fmap time . insertedAt <$> txs of
        (Just t):_ -> t
        _ -> error "Expected at least one transaction with a time."

waitAllTxsInLedger
    :: forall n t. (DecodeAddress n, DecodeStakeAddress n)
    => Context t
    -> ApiWallet
    -> IO ()
waitAllTxsInLedger ctx w = eventually "waitAllTxsInLedger: all txs in ledger" $ do
    let ep = Link.listTransactions @'Shelley w
    (_, txs) <- unsafeRequest @[ApiTransaction n] ctx ep Empty
    view (#status . #getApiT) <$> txs `shouldSatisfy` all (== InLedger)

waitForNextEpoch
    :: Context t
    -> IO ()
waitForNextEpoch ctx = do
    epoch <- getFromResponse (#nodeTip . #epochNumber) <$>
        request @ApiNetworkInformation ctx Link.getNetworkInfo Default Empty
    eventually "waitForNextEpoch: goes to next epoch" $ do
        epoch' <- getFromResponse (#nodeTip . #epochNumber) <$>
            request @ApiNetworkInformation ctx Link.getNetworkInfo Default Empty
        unless (getApiT epoch' > getApiT epoch) $ fail "not yet"

between :: (Ord a, Show a) => (a, a) -> a -> Expectation
between (min', max') x
    | min' <= x && x <= max'
        = return ()
    | otherwise
        = fail $ mconcat
            [ show x
            , " ∉ ["
            , show min'
            , ", "
            , show max'
            ]

(.>) :: (Ord a, Show a) => a -> a -> Expectation
x .> bound
    | x > bound
        = return ()
    | otherwise
        = fail $ mconcat
            [ show x
            , " does not satisfy (> "
            , show bound
            , ")"
            ]

(.>=) :: (Ord a, Show a) => a -> a -> Expectation
a .>= b
    | a >= b
        = return ()
    | otherwise
        = fail $ mconcat
            [ show a
            , " does not satisfy (>= "
            , show b
            , ")"
            ]

(.<=) :: (Ord a, Show a) => a -> a -> Expectation
a .<= b
    | a <= b
        = return ()
    | otherwise
        = fail $ mconcat
            [ show a
            , " does not satisfy (<= "
            , show b
            , ")"
            ]

-- Retry the given action a couple of time until it doesn't throw, or until it
-- has been retried enough.
--
-- It is like 'eventuallyUsingDelay', but with the default delay of 500 ms
-- between retries.
eventually :: String -> IO a -> IO a
eventually = eventuallyUsingDelay (500 * ms)
  where
    ms = 1000

-- Retry the given action a couple of time until it doesn't throw, or until it
-- has been retried enough.
--
-- It sleeps for a specified delay between retries.
eventuallyUsingDelay
    :: Int -- ^ Delay in microseconds
    -> String -- ^ Brief description of the IO action
    -> IO a
    -> IO a
eventuallyUsingDelay delay desc io = do
    lastErrorRef <- newIORef Nothing
    -- NOTE
    -- This __90s__ is mostly justified by the parameters in the shelley
    -- genesis. The longest action we have two wait for are about 2 epochs,
    -- which corresponds to 80s with the current parameters. Using something
    -- much longer than that isn't really useful (in particular, this doesn't
    -- depend on the host machine running the test, because the protocol moves
    -- forward at the same speed regardless...)
    winner <- race (threadDelay $ 90 * oneSecond) (trial lastErrorRef)
    case winner of
        Left () -> do
            lastError <- readIORef lastErrorRef
            fail $ mconcat
                [ "Waited longer than 2 minutes for an action to resolve. "
                , "Action: "
                , show desc
                , ". Error condition: "
                , show lastError
                ]
        Right a ->
            return a
  where
    trial lastErrorRef = loop
      where
        loop = io `catch` \(e :: SomeException) -> do
            writeIORef lastErrorRef (Just e)
            threadDelay delay
            loop

utcIso8601ToText :: UTCTime -> Text
utcIso8601ToText = utcTimeToText iso8601ExtendedUtc

-- | Restore HW Wallet from pub key
restoreWalletFromPubKey
    :: forall w (style :: WalletStyle) t.
        ( Link.Discriminate style
        , Link.PostWallet style
        , HasType (ApiT WalletId) w
        , HasType (ApiT SyncProgress) w
        , Show w
        , FromJSON w
        )
    => Context t
    -> Text
    -> Text
    -> IO w
restoreWalletFromPubKey ctx pubKey name = do
    let payloadRestore = Json [aesonQQ| {
            "name": #{name},
            "account_public_key": #{pubKey}
        }|]
    r <- request @w ctx (Link.postWallet @style) Default payloadRestore
    expectResponseCode @IO HTTP.status201 r
    let wid = getFromResponse id r
    eventually "restoreWalletFromPubKey: wallet is 100% synced " $ do
        rg <- request @w ctx (Link.getWallet @style wid) Default Empty
        expectField (typed @(ApiT SyncProgress) . #getApiT) (`shouldBe` Ready) rg
    return wid

-- | Create an empty wallet
emptyRandomWallet :: Context t -> IO ApiByronWallet
emptyRandomWallet ctx = do
    mnemonic <- mnemonicToText @12 . entropyToMnemonic <$> genEntropy
    emptyByronWalletWith ctx "random"
        ("Random Wallet", mnemonic, fixturePassphrase)

emptyRandomWalletMws :: Context t -> IO (ApiByronWallet, Mnemonic 12)
emptyRandomWalletMws ctx = do
    mnemonic <- entropyToMnemonic <$> genEntropy
    (,mnemonic) <$> emptyByronWalletWith ctx "random"
        ("Random Wallet", mnemonicToText @12 mnemonic, fixturePassphrase)

emptyIcarusWallet :: Context t -> IO ApiByronWallet
emptyIcarusWallet ctx = do
    mnemonic <- mnemonicToText @15 . entropyToMnemonic <$> genEntropy
    emptyByronWalletWith ctx "icarus"
        ("Icarus Wallet", mnemonic, fixturePassphrase)

emptyIcarusWalletMws :: Context t -> IO (ApiByronWallet, Mnemonic 15)
emptyIcarusWalletMws ctx = do
    mnemonic <- entropyToMnemonic <$> genEntropy
    (,mnemonic) <$> emptyByronWalletWith ctx "icarus"
        ("Icarus Wallet",mnemonicToText @15 mnemonic, fixturePassphrase)

emptyRandomWalletWithPasswd :: Context t -> Text -> IO ApiByronWallet
emptyRandomWalletWithPasswd ctx rawPwd = do
    let pwd = preparePassphrase W.EncryptWithScrypt
            $ Passphrase
            $ BA.convert
            $ T.encodeUtf8 rawPwd
    seed <- SomeMnemonic @12 . entropyToMnemonic <$> genEntropy
    let key = T.decodeUtf8
            $ hex
            $ Byron.getKey
            $ Byron.generateKeyFromSeed seed pwd
    pwdH <- T.decodeUtf8 . hex <$> encryptPasswordWithScrypt pwd
    emptyByronWalletFromXPrvWith ctx "random" ("Random Wallet", key, pwdH)
  where
    encryptPasswordWithScrypt =
        fmap Scrypt.getEncryptedPass
        . Scrypt.encryptPassIO Scrypt.defaultParams
        . Scrypt.Pass
        . CBOR.toStrictByteString
        . CBOR.encodeBytes
        . BA.convert

emptyByronWalletWith
    :: forall t. ()
    => Context t
    -> String
    -> (Text, [Text], Text)
    -> IO ApiByronWallet
emptyByronWalletWith ctx style (name, mnemonic, pass) = do
    let payload = Json [aesonQQ| {
            "name": #{name},
            "mnemonic_sentence": #{mnemonic},
            "passphrase": #{pass},
            "style": #{style}
        }|]
    r <- request @ApiByronWallet ctx
        (Link.postWallet @'Byron) Default payload
    expectResponseCode @IO HTTP.status201 r
    return (getFromResponse id r)

emptyByronWalletFromXPrvWith
    :: forall t. ()
    => Context t
    -> String
    -> (Text, Text, Text)
    -> IO ApiByronWallet
emptyByronWalletFromXPrvWith ctx style (name, key, passHash) = do
    let payload = Json [aesonQQ| {
            "name": #{name},
            "encrypted_root_private_key": #{key},
            "passphrase_hash": #{passHash},
            "style": #{style}
        }|]
    r <- request @ApiByronWallet ctx
        (Link.postWallet @'Byron) Default payload
    expectResponseCode @IO HTTP.status201 r
    return (getFromResponse id r)

-- | Create an empty wallet
emptyWallet :: Context t -> IO ApiWallet
emptyWallet ctx = do
    mnemonic <- (mnemonicToText . entropyToMnemonic) <$> genEntropy @160
    let payload = Json [aesonQQ| {
            "name": "Empty Wallet",
            "mnemonic_sentence": #{mnemonic},
            "passphrase": #{fixturePassphrase}
        }|]
    r <- request @ApiWallet ctx
        (Link.postWallet @'Shelley) Default payload
    expectResponseCode @IO HTTP.status201 r
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
    r <- request @ApiWallet ctx
        (Link.postWallet @'Shelley) Default payload
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
fixturePassphrase :: Text
fixturePassphrase =
    "cardano-wallet"

-- | fixturePassphrase encrypted by Scrypt function
fixturePassphraseEncrypted :: Text
fixturePassphraseEncrypted =
    "31347c387c317c2b6a6f747446495a6a566d586f43374c6c54425a576c\
    \597a425834515177666475467578436b4d485569733d7c78324d646738\
    \49554a3232507235676531393575445a76583646552b7757395a6a6a2f\
    \51303054356c654751794279732f7662753367526d726c316c657a7150\
    \43676d364e6758476d4d2f4b6438343265304b4945773d3d"

-- | Restore a faucet and wait until funds are available.
fixtureWallet
    :: Context t
    -> IO ApiWallet
fixtureWallet ctx = do
    (w, _) <- fixtureWalletWithMnemonics ctx
    return w

fixtureWalletWithMnemonics
    :: Context t
    -> IO (ApiWallet, [Text])
fixtureWalletWithMnemonics ctx = do
    mnemonics <- mnemonicToText <$> nextWallet @"shelley" (_faucet ctx)
    let payload = Json [aesonQQ| {
            "name": "Faucet Wallet",
            "mnemonic_sentence": #{mnemonics},
            "passphrase": #{fixturePassphrase}
            } |]
    (_, w) <- unsafeRequest @ApiWallet ctx
        (Link.postWallet @'Shelley) payload
    race (threadDelay sixtySeconds) (checkBalance w) >>= \case
        Left _ -> fail "fixtureWallet: waited too long for initial transaction"
        Right a -> return (a, mnemonics)
  where
    sixtySeconds = 60*oneSecond
    checkBalance w = do
        r <- request @ApiWallet ctx
            (Link.getWallet @'Shelley w) Default Empty
        if getFromResponse (#balance . #getApiT . #available) r > Quantity 0
            then return (getFromResponse id r)
            else threadDelay oneSecond *> checkBalance w

-- | Restore a faucet Random wallet and wait until funds are available.
fixtureRandomWalletMws
    :: Context t
    -> IO (ApiByronWallet, Mnemonic 12)
fixtureRandomWalletMws ctx = do
    mnemonics <- nextWallet @"random" (_faucet ctx)
    (,mnemonics) <$> fixtureLegacyWallet ctx "random" (mnemonicToText mnemonics)

fixtureRandomWallet
    :: Context t
    -> IO ApiByronWallet
fixtureRandomWallet = fmap fst . fixtureRandomWalletMws

fixtureRandomWalletAddrs
    :: forall (n :: NetworkDiscriminant) t.
        ( PaymentAddress n ByronKey
        )
    => Context t
    -> IO (ApiByronWallet, [Address])
fixtureRandomWalletAddrs =
    fmap (second (randomAddresses @n)) . fixtureRandomWalletMws

-- | Restore a wallet with the given UTxO distribution. Note that there's a
-- limitation to what can be done here. We only have 10 UTxO available in each
-- faucet and they "only" have 'faucetUtxoAmt = 100_000 Ada' in each.
--
-- This function makes no attempt at ensuring the request is valid, so be
-- careful.
--
-- TODO: Remove duplication between Shelley / Byron fixtures.
fixtureRandomWalletWith
    :: forall (n :: NetworkDiscriminant) t.
        ( EncodeAddress n
        , DecodeAddress n
        , DecodeStakeAddress n
        , PaymentAddress n ByronKey
        )
    => Context t
    -> [Natural]
    -> IO ApiByronWallet
fixtureRandomWalletWith ctx coins0 = do
    src  <- fixtureRandomWallet ctx
    mws  <- entropyToMnemonic <$> genEntropy
    dest <- emptyByronWalletWith ctx "random"
        ("Random Wallet", mnemonicToText @12 mws, fixturePassphrase)
    let addrs = randomAddresses @n mws
    mapM_ (moveByronCoins @n ctx src (dest, addrs)) (groupsOf 10 coins0)
    void $ request @() ctx
        (Link.deleteWallet @'Byron src) Default Empty
    snd <$> unsafeRequest @ApiByronWallet ctx
        (Link.getWallet @'Byron dest) Empty

-- | Restore a faucet Icarus wallet and wait until funds are available.
fixtureIcarusWalletMws
    :: Context t
    -> IO (ApiByronWallet, Mnemonic 15)
fixtureIcarusWalletMws ctx = do
    mnemonics <- nextWallet @"icarus" (_faucet ctx)
    (,mnemonics) <$> fixtureLegacyWallet ctx "icarus" (mnemonicToText mnemonics)

fixtureIcarusWallet
    :: Context t
    -> IO ApiByronWallet
fixtureIcarusWallet = fmap fst . fixtureIcarusWalletMws

fixtureIcarusWalletAddrs
    :: forall (n :: NetworkDiscriminant) t.
        ( PaymentAddress n IcarusKey
        )
    => Context t
    -> IO (ApiByronWallet, [Address])
fixtureIcarusWalletAddrs =
    fmap (second (icarusAddresses @n)) . fixtureIcarusWalletMws

-- | Restore a wallet with the given UTxO distribution. Note that there's a
-- limitation to what can be done here. We only have 10 UTxO available in each
-- faucet and they "only" have 'faucetUtxoAmt = 100_000 Ada' in each.
--
-- This function makes no attempt at ensuring the request is valid, so be
-- careful.
--
-- TODO: Remove duplication between Shelley / Byron fixtures.
fixtureIcarusWalletWith
    :: forall (n :: NetworkDiscriminant) t.
        ( EncodeAddress n
        , DecodeAddress n
        , DecodeStakeAddress n
        , PaymentAddress n IcarusKey
        )
    => Context t
    -> [Natural]
    -> IO ApiByronWallet
fixtureIcarusWalletWith ctx coins0 = do
    src  <- fixtureIcarusWallet ctx
    mws  <- entropyToMnemonic <$> genEntropy
    dest <- emptyByronWalletWith ctx "icarus"
        ("Icarus Wallet", mnemonicToText @15 mws, fixturePassphrase)
    let addrs = icarusAddresses @n mws
    mapM_ (moveByronCoins @n ctx src (dest, addrs)) (groupsOf 10 coins0)
    void $ request @() ctx
        (Link.deleteWallet @'Byron src) Default Empty
    snd <$> unsafeRequest @ApiByronWallet ctx
        (Link.getWallet @'Byron dest) Empty


-- | Restore a legacy wallet (Byron or Icarus)
fixtureLegacyWallet
    :: forall t. ()
    => Context t
    -> String
    -> [Text]
    -> IO ApiByronWallet
fixtureLegacyWallet ctx style mnemonics = do
    let payload = Json [aesonQQ| {
            "name": "Faucet Byron Wallet",
            "mnemonic_sentence": #{mnemonics},
            "passphrase": #{fixturePassphrase},
            "style": #{style}
            } |]
    (_, w) <- unsafeRequest @ApiByronWallet ctx
        (Link.postWallet @'Byron) payload
    race (threadDelay sixtySeconds) (checkBalance w) >>= \case
        Left _ ->
            fail "fixtureByronWallet: waited too long for initial transaction"
        Right a ->
            return a
  where
    sixtySeconds = 60*oneSecond
    checkBalance w = do
        r <- request @ApiByronWallet ctx
            (Link.getWallet @'Byron w) Default Empty
        if getFromResponse (#balance . #available) r > Quantity 0
            then return (getFromResponse id r)
            else threadDelay oneSecond *> checkBalance w

-- | Restore a wallet with the given UTxO distribution. Note that there's a
-- limitation to what can be done here. We only have 10 UTxO available in each
-- faucet and they "only" have 'faucetUtxoAmt = 100_000 Ada' in each.
--
-- This function makes no attempt at ensuring the request is valid, so be
-- careful.
fixtureWalletWith
    :: forall n t.
        ( EncodeAddress n
        , DecodeAddress n
        , DecodeStakeAddress n
        )
    => Context t
    -> [Natural]
    -> IO ApiWallet
fixtureWalletWith ctx coins0 = do
    src <- fixtureWallet ctx
    dest <- emptyWallet ctx
    mapM_ (moveCoins src dest) (groupsOf 10 coins0)
    void $ request @() ctx
        (Link.deleteWallet @'Shelley src) Default Empty
    snd <$> unsafeRequest @ApiWallet ctx
        (Link.getWallet @'Shelley dest) Empty
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
        balance <- getFromResponse (#balance . #getApiT . #available . #getQuantity)
            <$> request @ApiWallet ctx
                    (Link.getWallet @'Shelley dest) Default Empty
        addrs <- fmap (view #id) . getFromResponse id
            <$> request @[ApiAddress n] ctx
                    (Link.listAddresses @'Shelley dest) Default Empty
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
        request @(ApiTransaction n) ctx
            (Link.createTransaction @'Shelley src) Default payload
            >>= expectResponseCode HTTP.status202
        eventually "balance available = balance total" $ do
            rb <- request @ApiWallet ctx
                (Link.getWallet @'Shelley dest) Default Empty
            expectField
                (#balance . #getApiT . #available)
                (`shouldBe` Quantity (sum (balance:coins))) rb
            ra <- request @ApiWallet ctx
                (Link.getWallet @'Shelley src) Default Empty

            getFromResponse (#balance . #getApiT . #available) ra
                `shouldBe`
                    getFromResponse (#balance . #getApiT . #total) ra

-- | Move coins from a wallet to another
moveByronCoins
    :: forall (n :: NetworkDiscriminant) t.
        ( EncodeAddress n
        , DecodeAddress n
        , DecodeStakeAddress n
        )
    => Context t
        -- ^ Api context
    -> ApiByronWallet
        -- ^ Source Wallet
    -> (ApiByronWallet, [Address])
        -- ^ Destination wallet
    -> [Natural]
        -- ^ Coins to move
    -> IO ()
moveByronCoins ctx src (dest, addrs) coins = do
    balance <- getFromResponse (#balance . #available . #getQuantity)
        <$> request @ApiByronWallet ctx (Link.getWallet @'Byron dest) Default Empty
    let payments = for (zip coins addrs) $ \(amt, addr) ->
            let addrStr = encodeAddress @n addr
            in [aesonQQ|{
                "address": #{addrStr},
                "amount": {
                    "quantity": #{amt},
                    "unit": "lovelace"
                }
            }|]
    let payload = Json [aesonQQ|{
            "payments": #{payments :: [Value]},
            "passphrase": #{fixturePassphrase}
        }|]
    request @(ApiTransaction n) ctx
        (Link.createTransaction @'Byron src) Default payload
        >>= expectResponseCode HTTP.status202
    eventually "balance available = balance total" $ do
        rb <- request @ApiByronWallet ctx
            (Link.getWallet @'Byron dest) Default Empty
        expectField (#balance . #available)
            (`shouldBe` Quantity (sum (balance:coins))) rb
        ra <- request @ApiByronWallet ctx
            (Link.getWallet @'Byron src) Default Empty
        getFromResponse (#balance . #available) ra
            `shouldBe` getFromResponse (#balance . #total) ra


-- | Total amount on each faucet wallet
faucetAmt :: Natural
faucetAmt = 10 * faucetUtxoAmt

-- | Each faucet wallet is composed of 10 times a single faucet UTxO of 100_000
-- Ada.
faucetUtxoAmt :: Natural
faucetUtxoAmt = ada 100_000
  where
    ada = (*) (1_000_000)

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
    :: forall n t w.
        ( HasType (ApiT WalletId) w
        , DecodeAddress n
        , DecodeStakeAddress n
        )
    => Context t
    -> ApiT PoolId
    -> (w, Text)
    -> IO (HTTP.Status, Either RequestException (ApiTransaction n))
joinStakePool ctx p (w, pass) = do
    let payload = Json [aesonQQ| {
            "passphrase": #{pass}
            } |]
    request @(ApiTransaction n) ctx
        (Link.joinStakePool (Identity p) w) Default payload

quitStakePool
    :: forall n t w.
        ( HasType (ApiT WalletId) w
        , DecodeAddress n
        , DecodeStakeAddress n
        )
    => Context t
    -> (w, Text)
    -> IO (HTTP.Status, Either RequestException (ApiTransaction n))
quitStakePool ctx (w, pass) = do
    let payload = Json [aesonQQ| {
            "passphrase": #{pass}
            } |]
    request @(ApiTransaction n) ctx
        (Link.quitStakePool w) Default payload

selectCoins
    :: forall n style t w.
        ( HasType (ApiT WalletId) w
        , DecodeAddress n
        , EncodeAddress n
        , Link.Discriminate style
        )
    => Context t
    -> w
    -> NonEmpty (AddressAmount (ApiT Address, Proxy n))
    -> IO (HTTP.Status, Either RequestException (ApiCoinSelection n))
selectCoins ctx w payments = do
    let payload = Json [aesonQQ| {
            "payments": #{payments}
        } |]
    request @(ApiCoinSelection n) ctx
        (Link.selectCoins @style w) Default payload

delegationFee
    :: forall t w. (HasType (ApiT WalletId) w)
    => Context t
    -> w
    -> IO (HTTP.Status, Either RequestException ApiFee)
delegationFee ctx w = do
    request @ApiFee ctx (Link.getDelegationFee w) Default Empty

-- | Generate an infinite list of addresses for random wallets.
--
-- To be typically used as:
--
-- >>> take 1 (randomAddresses @n)
-- [addr]
randomAddresses
    :: forall (n :: NetworkDiscriminant). (PaymentAddress n ByronKey)
    => Mnemonic 12
    -> [Address]
randomAddresses mw =
    let
        (seed, pwd) =
            (SomeMnemonic mw, mempty)
        rootXPrv =
            Byron.generateKeyFromSeed seed pwd
        accXPrv =
            Byron.deriveAccountPrivateKey pwd rootXPrv minBound
        addrXPrv =
            Byron.deriveAddressPrivateKey pwd accXPrv
    in
        [ paymentAddress @n (publicKey $ addrXPrv ix)
        | ix <- [minBound..maxBound]
        ]

-- | Generate an infinite list of addresses for icarus wallets
--
-- To be typically used as:
--
-- >>> take 1 (icarusAddresses @n)
-- [addr]
icarusAddresses
    :: forall (n :: NetworkDiscriminant). (PaymentAddress n IcarusKey)
    => Mnemonic 15
    -> [Address]
icarusAddresses mw =
    let
        (seed, pwd) =
            (SomeMnemonic mw, mempty)
        rootXPrv =
            Icarus.generateKeyFromSeed seed pwd
        accXPrv =
            deriveAccountPrivateKey pwd rootXPrv minBound
        addrXPrv =
            deriveAddressPrivateKey pwd accXPrv UTxOExternal
    in
        [ paymentAddress @n (publicKey $ addrXPrv ix)
        | ix <- [minBound..maxBound]
        ]

-- | Generate an infinite list of addresses for shelley wallets.
--
-- To be typically used as:
--
-- >>> take 1 (shelleyAddresses @n)
-- [addr]
shelleyAddresses
    :: forall (n :: NetworkDiscriminant). (PaymentAddress n ShelleyKey)
    => Mnemonic 15
    -> [Address]
shelleyAddresses mw =
    let
        (seed, pwd) =
            (SomeMnemonic mw, mempty)
        rootXPrv =
            Shelley.generateKeyFromSeed (seed, Nothing) pwd
        accXPrv =
            deriveAccountPrivateKey pwd rootXPrv minBound
        addrXPrv =
            deriveAddressPrivateKey pwd accXPrv UTxOExternal
    in
        [ paymentAddress @n (publicKey $ addrXPrv ix)
        | ix <- [minBound..maxBound]
        ]

listAddresses
    :: forall n t. (DecodeAddress n)
    => Context t
    -> ApiWallet
    -> IO [ApiAddress n]
listAddresses ctx w = do
    let link = Link.listAddresses @'Shelley w
    (_, addrs) <- unsafeRequest @[ApiAddress n] ctx link Empty
    return addrs

listAllTransactions
    :: forall n t w.
        ( DecodeAddress n
        , DecodeStakeAddress n
        , HasType (ApiT WalletId) w
        )
    => Context t
    -> w
    -> IO [ApiTransaction n]
listAllTransactions ctx w =
    listTransactions ctx w Nothing Nothing (Just Descending)

listTransactions
    :: forall n t w.
        ( DecodeAddress n
        , DecodeStakeAddress n
        , HasType (ApiT WalletId) w
        )
    => Context t
    -> w
    -> Maybe UTCTime
    -> Maybe UTCTime
    -> Maybe SortOrder
    -> IO [ApiTransaction n]
listTransactions ctx wallet mStart mEnd mOrder = do
    (_, txs) <- unsafeRequest @[ApiTransaction n] ctx path Empty
    return txs
  where
    path = Link.listTransactions' @'Shelley wallet
        Nothing
        (Iso8601Time <$> mStart)
        (Iso8601Time <$> mEnd)
        mOrder

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

--
-- Manipulating endpoints
--

-- | Override the method of a particular endpoint, mostly to exercise invalid
-- endpoints from existing ones.
withMethod :: Method -> (Method, Text) -> (Method, Text)
withMethod method (_, path) = (method, path)

-- | Modifies the value of a path parameter at position 'n' (indexed from 0)
-- with the given update function. Throws if the given endpoint has no path
-- parameter in the given position.
--
-- >>> Link.getWallet @Shelley myWallet
-- ( "GET", "v2/wallets/2512a00e9653fe49a44a5886202e24d77eeb998f" )
--
-- >>> withPathParam 0 (<> "suffix") $ Link.getWallet @Shelley myWallet
-- ( "GET", "v2/wallets/2512a00e9653fe49a44a5886202e24d77eeb998fsuffix" )
--
-- >>> withPathParam 1 (const "suffix") $ Link.joinStakePool @Shelley myPool myWallet
-- ( "GET", "v2/stake-pools/2512a00e9653fe49a44a5886202e24d77eeb998f/wallets/patate" )
withPathParam :: Int -> (Text -> Text) -> (Method, Text) -> (Method, Text)
withPathParam n0 update (method, path) =
    case T.splitOn "/" path of
        [] -> errInvalidEndpoint

        version:q ->
            (method, T.intercalate "/" (version:findAndModify n0 q))
  where
    findAndModify 0 (k:v:q) = k : update v : q
    findAndModify n (k:v:q) = k : v : findAndModify (n - 1) q
    findAndModify _ _       = errInvalidEndpoint

    errInvalidEndpoint :: a
    errInvalidEndpoint =
        error $ "modifyPathParam: invalid endpoint: " <> show (method, path)

toQueryString :: [(Text, Text)] -> Text
toQueryString kvs = if T.null suffix then mempty else "?" <> suffix
  where
    suffix = T.intercalate "&" $ buildQueryParam <$> kvs
    buildQueryParam (k, v) = k <> "=" <> toQueryParam v

infixr 5 </>
(</>) :: ToHttpApiData a => Text -> a -> Text
base </> thenext = mconcat [base, "/", toQueryParam thenext]

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
    (["recovery-phrase", "generate"] ++ args)

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
            [ "wallet", "create", "from-recovery-phrase" ] ++ portArgs ++ args
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

createWalletFromPublicKeyViaCLI
    :: forall t r s. (CmdResult r, HasType (Port "wallet") s, KnownCommand t)
    => s
    -> [String]
        -- ^ NAME, [--address-pool-gap INT], ACCOUNT_PUBLIC_KEY
    -> IO r
createWalletFromPublicKeyViaCLI ctx args = cardanoWalletCLI @t $
    [ "wallet", "create", "from-public-key", "--port"
    , show (ctx ^. typed @(Port "wallet"))] ++ args

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

createAddressViaCLI
    :: forall t s. (KnownCommand t, HasType (Port "wallet") s)
    => s
    -> [String]
        -- ^ Args
    -> String
        -- ^ Pass
    -> IO (ExitCode, Text, Text)
        -- ^ (ExitCode, StdOut, StdErr)
createAddressViaCLI ctx args pass = do
    let execArgs =
            [ "address", "create"
            , "--port", show (ctx ^. typed @(Port "wallet"))
            ] ++ args
    let process = proc' (commandName @t) execArgs
    withCreateProcess process $
        \(Just stdin) (Just stdout) (Just stderr) h -> do
            hPutStr stdin (pass <> "\n")
            hFlush stdin
            hClose stdin
            c <- waitForProcess h
            out <- TIO.hGetContents stdout
            err <- TIO.hGetContents stderr
            pure (c, out, err)

importAddressViaCLI
    :: forall t r s. (CmdResult r, KnownCommand t, HasType (Port "wallet") s)
    => s
    -> [String]
        -- ^ Args
    -> IO r
        -- ^ (ExitCode, StdOut, StdErr)
importAddressViaCLI ctx args = cardanoWalletCLI @t $
    ["address", "import", "--port", show (ctx ^. typed @(Port "wallet"))] ++ args

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
    :: forall t r s. (CmdResult r, HasType (Port "wallet") s, KnownCommand t)
    => s
    -> [String]
    -> IO r
postTransactionFeeViaCLI ctx args = cardanoWalletCLI @t $ join
        [ ["transaction", "fees"]
        , ["--port", show (ctx ^. typed @(Port "wallet"))]
        , args
        ]

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

getTransactionViaCLI
    :: forall t r s. (CmdResult r, KnownCommand t, HasType (Port "wallet") s)
    => s
    -> String
    -> String
    -> IO r
getTransactionViaCLI ctx wid tid = cardanoWalletCLI @t $ join
    [ ["transaction", "get"]
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

--
-- Helper for random wallets from Xprv
--
rootPrvKeyFromMnemonics :: [Text] -> Text -> Text
rootPrvKeyFromMnemonics mnemonics pass =
    T.decodeUtf8 $ hex $ getKey $ Byron.generateKeyFromSeed seed
        (preparePassphrase W.EncryptWithScrypt rawPassd)
 where
     (Right seed) = mkSomeMnemonic @'[12] mnemonics
     rawPassd = Passphrase $ BA.convert $ T.encodeUtf8 pass

--
-- Helper for HWWallets, getting pubKey from mnemonic sentence
--
pubKeyFromMnemonics :: [Text] -> Text
pubKeyFromMnemonics mnemonics =
    T.decodeUtf8 $ serializeXPub $ publicKey
       $ deriveAccountPrivateKey mempty rootXPrv minBound
 where
     (Right seed) = mkSomeMnemonic @'[15] mnemonics
     rootXPrv = generateKeyFromSeed (seed, Nothing) mempty

--
-- Helper for delegation statuses
--
getSlotParams
    :: (Context t)
    -> IO (EpochNo, SlotParameters)
getSlotParams ctx = do
    r1 <- request @ApiNetworkInformation ctx
          Link.getNetworkInfo Default Empty
    let (ApiT currentEpoch) = getFromResponse (#networkTip . #epochNumber) r1

    let endpoint = ( "GET", "v2/network/parameters" )
    r2 <- request @ApiNetworkParameters ctx endpoint Default Empty
    let (Quantity slotL) = getFromResponse #slotLength r2
    let (Quantity epochL) = getFromResponse #epochLength r2
    let (Quantity coeff) = getFromResponse #activeSlotCoefficient r2
    let (ApiT genesisBlockDate) = getFromResponse #blockchainStartTime r2
    let sp = SlotParameters
            (EpochLength epochL)
            (SlotLength slotL)
            (genesisBlockDate)
            (ActiveSlotCoefficient coeff)

    return (currentEpoch, sp)

-- | Handy constructor for ApiEpochInfo
mkEpochInfo
    :: EpochNo
    -- ^ Epoch to construct
    -> SlotParameters
    -- ^ Blockchain slot parameters
    -> ApiEpochInfo
mkEpochInfo epochNo sp =
    ApiEpochInfo
        (ApiT epochNo)
        (epochStartTime sp epochNo)

-- | Wallet not delegating and not about to join any stake pool.
notDelegating
    :: [(Maybe (ApiT PoolId), ApiEpochInfo)]
    -- ^ Pools to be joined & epoch at which the new delegation will become active
    -> ApiWalletDelegation
notDelegating nexts = ApiWalletDelegation
    { active = ApiWalletDelegationNext NotDelegating Nothing Nothing
    , next = flip map nexts $ \(mpid, epochInfo) -> case mpid of
        Nothing ->
            ApiWalletDelegationNext NotDelegating Nothing (Just epochInfo)
        Just pid ->
            ApiWalletDelegationNext Delegating (Just pid) (Just epochInfo)
    }

delegating
    :: ApiT PoolId
    -- ^ Pool joined
    -> [(Maybe (ApiT PoolId), ApiEpochInfo)]
    -- ^ Pools to be joined & epoch at which the new delegation will become active
    -> ApiWalletDelegation
delegating pidActive nexts = (notDelegating nexts)
    { active = ApiWalletDelegationNext Delegating (Just pidActive) Nothing
    }
