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
    ( XPrv, XPub, xpubFromBytes )
import Cardano.Wallet.Primitive.AddressDerivation
    ( DelegationAddress (..)
    , Depth (..)
    , NetworkDiscriminant (..)
    , Passphrase (..)
    , PaymentAddress (..)
    , hex
    )
import Cardano.Wallet.Primitive.AddressDerivation.Shelley
    ( ShelleyKey (..) )
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
    ( unsafeXPrv )
import Data.Function
    ( (&) )
import Data.Maybe
    ( fromJust )
import Data.Proxy
    ( Proxy (..) )
import Data.Quantity
    ( Quantity (..) )
import Test.Hspec
    ( Spec, describe, it )
import Test.QuickCheck
    ( Arbitrary (..)
    , Property
    , choose
    , counterexample
    , oneof
    , property
    , vector
    , withMaxSuccess
    )

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

spec :: Spec
spec = do
    describe "Shelley TransactionLayer" $ do
        it "estimateSize never underestimates"
            $ withMaxSuccess 2000
            $ property prop_estimateSizeNeverUnderestimates

prop_estimateSizeNeverUnderestimates
    :: [(TxIn, TxOut)]
    -> [TxOut]
    -> [TxOut]
    -> Property
prop_estimateSizeNeverUnderestimates ins outs chgs = do
    let (_tx, SealedTx bytes) = mkTestingTx (SlotId 0 0) ins (outs ++ chgs)
    let actualSize = BS.length bytes
    let Quantity estimatedSize = estimateSize tl
            $ CoinSelection ins outs (coin <$> chgs)
    if (estimatedSize < actualSize) || (estimatedSize > actualSize + margin)
    then property False
        & counterexample (mconcat
            [ "Estimated size out of bounds! Estimated "
            , show estimatedSize
            , ", but should be between: "
            , show actualSize
            , " and "
            , show (actualSize + margin)
            ])
        & counterexample ("Serialized tx: " <> T.unpack (T.decodeUtf8 $ hex bytes))
    else property True
  where
    -- NOTE
    -- 8 = TTL which is encoded as integer, going from 1 to 9 bytes.
    --
    -- 32 = size of a public key. We assume the "worse" for change address,
    --      which is delegation address with 2 public keys.
    margin = 8 + 32 * length chgs

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
keystore (Address bytes) = Just
    -- We don't need the actual private key, but we do want the private key to
    -- be (relatively) unique and with a 1:1 mapping with given addresses.
    ( ShelleyKey $ unsafeXPrv $ BS.take 128 $ mconcat $ replicate 4 bytes
    , mempty
    )

epochLength :: EpochLength
epochLength = EpochLength 10

instance Arbitrary SlotId where
    arbitrary = fromFlatSlot epochLength <$> choose (0, 100)

instance Arbitrary TxIn where
    arbitrary = do
        tid <- Hash . BS.pack <$> vector 32
        ix  <- choose (0, 256)
        return $ TxIn tid ix

instance Arbitrary Address where
    arbitrary = oneof
        [ paymentAddress @'Mainnet <$> genPubKey
        , delegationAddress @'Mainnet <$> genPubKey <*> genPubKey
        ]
      where
        genPubKey = ShelleyKey <$> arbitrary

instance Arbitrary TxOut where
    arbitrary = TxOut
        <$> arbitrary
        <*> (Coin <$> arbitrary)

instance Arbitrary XPub where
    arbitrary =
        fromJust . xpubFromBytes . BS.pack <$> vector 64
