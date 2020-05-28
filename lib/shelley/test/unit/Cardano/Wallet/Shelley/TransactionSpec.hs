{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Cardano.Wallet.Shelley.TransactionSpec
    ( spec
    ) where

import Prelude

import Cardano.Address.Derivation
    ( XPrv )
import Cardano.Mnemonic
    ( ConsistentEntropy
    , EntropySize
    , Mnemonic
    , SomeMnemonic (..)
    , entropyToMnemonic
    )
import Cardano.Wallet.Primitive.AddressDerivation
    ( DelegationAddress (..)
    , Depth (..)
    , NetworkDiscriminant (..)
    , Passphrase (..)
    , PaymentAddress (..)
    , getRawKey
    , publicKey
    )
import Cardano.Wallet.Primitive.AddressDerivation.Shelley
    ( ShelleyKey (..), generateKeyFromSeed )
import Cardano.Wallet.Primitive.CoinSelection
    ( CoinSelection (..) )
import Cardano.Wallet.Primitive.Types
    ( Address (..)
    , Coin (..)
    , EpochLength (..)
    , Hash (..)
    , ProtocolMagic (..)
    , SealedTx (..)
    , SlotId (..)
    , Tx (..)
    , TxIn (..)
    , TxOut (..)
    , fromFlatSlot
    )
import Cardano.Wallet.Shelley.Compatibility
    ( Shelley )
import Cardano.Wallet.Shelley.Transaction
    ( newTransactionLayer )
import Cardano.Wallet.Transaction
    ( TransactionLayer, estimateSize, mkStdTx )
import Cardano.Wallet.Unsafe
    ( unsafeMkEntropy, unsafeMkSomeMnemonicFromEntropy )
import Data.Function
    ( (&) )
import Data.Proxy
    ( Proxy (..) )
import Data.Quantity
    ( Quantity (..) )
import GHC.TypeLits
    ( natVal )
import Test.Hspec
    ( Spec, describe, it )
import Test.QuickCheck
    ( Arbitrary (..)
    , Gen
    , InfiniteList (..)
    , Property
    , choose
    , counterexample
    , oneof
    , property
    , vector
    , withMaxSuccess
    )

import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS

spec :: Spec
spec = do
    describe "Shelley TransactionLayer" $ do
        it "estimateSize never underestimates"
            $ withMaxSuccess 2000
            $ property prop_estimateSizeNeverUnderestimates

prop_estimateSizeNeverUnderestimates
    :: [(TxIn, TxOut)]
    -> [TxOut]
    -> Property
prop_estimateSizeNeverUnderestimates ins outs = do
    let (_tx, SealedTx bytes) = mkTestingTx
            (SlotId 0 0)
            ins
            outs
    let actualSize = BS.length bytes
    let Quantity estimatedSize = estimateSize tl $ CoinSelection ins outs []
    if estimatedSize < actualSize
    then property False & counterexample (mconcat
        [ "Estimated size too low! Estimated: "
        , show estimatedSize
        , ", actual: "
        , show actualSize
        ])
    else property True

mkTestingTx
    :: SlotId
    -> [(TxIn, TxOut)]
    -> [TxOut]
    -> (Tx, SealedTx)
mkTestingTx s ins outs =
    either (error . show) id $
        mkStdTx tl keystore s ins outs

tl :: TransactionLayer (IO Shelley) ShelleyKey
tl = newTransactionLayer (Proxy @'Mainnet) pm epochLength
  where
    pm = ProtocolMagic 42

keystore
    :: Address
    -> Maybe (ShelleyKey 'AddressK XPrv, Passphrase "encryption")
keystore = const $ Just (dummyKey, mempty)

instance Arbitrary SlotId where
    arbitrary = fromFlatSlot epochLength <$> choose (0, 100)

epochLength :: EpochLength
epochLength = EpochLength 10

instance Arbitrary (ShelleyKey 'AddressK XPrv) where
    shrink _ = []
    arbitrary = ShelleyKey . getRawKey <$> genRootKeys

dummyKey :: ShelleyKey level XPrv
dummyKey =
    ShelleyKey $ getRawKey $ generateKeyFromSeed (mw, Nothing) mempty
  where
    ent = BS.pack $ replicate 20 0
    mw = unsafeMkSomeMnemonicFromEntropy (Proxy @15) ent

genRootKeys :: Gen (ShelleyKey 'RootK XPrv)
genRootKeys = do
    mnemonic <- arbitrary
    e <- genPassphrase @"encryption" (0, 16)
    return $ generateKeyFromSeed mnemonic e
  where
    genPassphrase :: (Int, Int) -> Gen (Passphrase purpose)
    genPassphrase range = do
        n <- choose range
        InfiniteList bytes _ <- arbitrary
        return $ Passphrase $ BA.convert $ BS.pack $ take n bytes

instance Arbitrary SomeMnemonic where
    arbitrary = SomeMnemonic <$> genMnemonic @12

genMnemonic
    :: forall mw ent csz.
     ( ConsistentEntropy ent mw csz
     , EntropySize mw ~ ent
     )
    => Gen (Mnemonic mw)
genMnemonic = do
        let n = fromIntegral (natVal $ Proxy @(EntropySize mw)) `div` 8
        bytes <- BS.pack <$> vector n
        let ent = unsafeMkEntropy @(EntropySize mw) bytes
        return $ entropyToMnemonic ent

instance Show XPrv where
    show _ = "<xprv>"

instance Arbitrary TxIn where
    arbitrary = do
        tid <- Hash . BS.pack <$> vector 32
        return $ TxIn tid 0

instance Arbitrary Address where
    arbitrary = oneof $ map return
        [ paymentAddress @'Mainnet (publicKey dummyKey)
        , delegationAddress @'Mainnet (publicKey dummyKey) (publicKey dummyKey)
        ]

instance Arbitrary TxOut where
    arbitrary = TxOut
        <$> arbitrary
        <*> (Coin <$> arbitrary)
