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
    , expectFieldEqual
    , expectFieldNotEqual
    , expectResponseCode
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
    ) where

import Prelude hiding
    ( fail )

import Cardano.Wallet.Api.Types
    ( ApiT (..) )
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
import Control.Monad.Fail
    ( MonadFail (..) )
import Control.Monad.IO.Class
    ( MonadIO )
import Crypto.Hash
    ( Blake2b_160, Digest, digestFromByteString )
import Data.Aeson.QQ
    ( aesonQQ )
import Data.Generics.Internal.VL.Lens
    ( Lens', lens, set, view )
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
    ( shouldBe, shouldNotBe )
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
    -> Maybe a
getFromResponse getter (_, res) = case res of
    Left _  -> Nothing
    Right s -> Just (view getter s)

json :: QuasiQuoter
json = aesonQQ

infixr 5 </>
(</>) :: ToHttpApiData a => Text -> a -> Text
base </> next = mconcat [base, "/", toQueryParam next]

unsafeCreateDigest :: Text -> Digest Blake2b_160
unsafeCreateDigest s = fromMaybe
    (error $ "unsafeCreateDigest failed to create digest from: " <> show s)
    (digestFromByteString $ B8.pack $ T.unpack s)

unsafeMkAddressPoolGap :: Word8 -> AddressPoolGap
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
