{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

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
    , expectListItemFieldEqual
    , expectListSizeEqual
    , expectResponseCode
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

    -- * Helpers
    , (</>)
    , (!!)
    , getFromResponse
    , json
    , tearDown
    ) where

import Prelude hiding
    ( fail )

import Cardano.Wallet.Api.Types
    ( ApiT (..), ApiWallet )
import Cardano.Wallet.Primitive.AddressDiscovery
    ( AddressPoolGap, getAddressPoolGap, mkAddressPoolGap )
import Cardano.Wallet.Primitive.Types
    ( PoolId (..)
    , WalletBalance (..)
    , WalletDelegation (..)
    , WalletId (..)
    , WalletName (..)
    , WalletPassphraseInfo (..)
    , WalletState (..)
    )
import Control.Monad
    ( forM_ )
import Control.Monad.Fail
    ( MonadFail (..) )
import Control.Monad.IO.Class
    ( MonadIO )
import Crypto.Hash
    ( Blake2b_160, Digest, digestFromByteString )
import Data.Aeson
    ( Value )
import Data.Aeson.QQ
    ( aesonQQ )
import Data.Foldable
    ( toList )
import Data.Function
    ( (&) )
import Data.Generics.Internal.VL.Lens
    ( Lens', lens, set, view, (^.) )
import Data.Generics.Product.Typed
    ( HasType, typed )
import Data.List
    ( (!!) )
import Data.Maybe
    ( fromMaybe )
import Data.Quantity
    ( Quantity (..) )
import Data.Text
    ( Text )
import Data.Time
    ( UTCTime )
import Data.Word
    ( Word8 )
import GHC.TypeLits
    ( Symbol )
import Language.Haskell.TH.Quote
    ( QuasiQuoter )
import Numeric.Natural
    ( Natural )
import Test.Hspec.Expectations.Lifted
    ( shouldBe, shouldContain, shouldNotBe )
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

import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.Text as T
import qualified Network.HTTP.Types.Status as HTTP


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
    Left (DecodeFailure msg)  -> BL8.unpack msg `shouldContain` want
    Left (ClientError _)  -> fail "expectErrorMessage: asserting ClientError not\
                             \ supported yet"
    Right a -> wantedErrorButSuccess a

-- | Expect a successful response, without any further assumptions
expectSuccess
    :: (MonadIO m, MonadFail m, Show a)
    => (s, Either RequestException a)
    -> m ()
expectSuccess (_, res) = case res of
    Left e  -> wantedSuccessButError e
    Right _ -> return ()

-- | Expect a given response code on the response
expectResponseCode
    :: (MonadIO m, MonadFail m)
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

-- | Apply 'a' to all actions in sequence
verify :: (Monad m) => a -> [a -> m ()] -> m ()
verify a = mapM_ (a &)

--
-- Lenses
--
addressPoolGap :: HasType (ApiT AddressPoolGap) s => Lens' s Word8
addressPoolGap =
    lens _get _set
  where
    _get :: HasType (ApiT AddressPoolGap) s => s -> Word8
    _get = getAddressPoolGap . getApiT . view typed
    _set :: HasType (ApiT AddressPoolGap) s => (s, Word8) -> s
    _set (s, v) = set typed (ApiT $ unsafeMkAddressPoolGap $ fromIntegral v) s

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

passphraseLastUpdate :: HasType (ApiT WalletPassphraseInfo) s => Lens' s Text
passphraseLastUpdate =
    lens _get _set
  where
    _get :: HasType (ApiT WalletPassphraseInfo) s => s -> Text
    _get = T.pack . show . lastUpdatedAt . getApiT . view typed
    _set :: HasType (ApiT WalletPassphraseInfo) s => (s, Text) -> s
    _set (s, v) =
        set typed (ApiT $ WalletPassphraseInfo ((read $ T.unpack v) :: UTCTime)) s

state :: HasType (ApiT WalletState) s => Lens' s WalletState
state =
    lens _get _set
  where
    _get :: HasType (ApiT WalletState) s => s -> WalletState
    _get = getApiT . view typed
    _set :: HasType (ApiT WalletState) s => (s, WalletState) -> s
    _set (s, v) = set typed (ApiT v ) s

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
--
-- Helpers
--

fromQuantity :: Quantity (u :: Symbol) a -> a
fromQuantity (Quantity a) = a

getFromResponse
    :: (Show a, Eq a)
    => Lens' s a
    -> (HTTP.Status, Either RequestException s)
    -> a
getFromResponse getter (_, res) = case res of
    Left _  -> error "getFromResponse failed to get item"
    Right s -> view getter s

json :: QuasiQuoter
json = aesonQQ

infixr 5 </>
(</>) :: ToHttpApiData a => Text -> a -> Text
base </> next = mconcat [base, "/", toQueryParam next]

-- | teardown after each test (currently only deleting all wallets)
tearDown :: Context -> IO ()
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

unsafeMkAddressPoolGap :: Integer -> AddressPoolGap
unsafeMkAddressPoolGap g = case (mkAddressPoolGap g) of
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
