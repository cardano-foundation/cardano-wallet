{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Internal.Cardano.Write.Tx.SignSpec where

import Prelude

import Cardano.Binary
    ( serialize'
    )
import Cardano.Crypto.Wallet
    ( XPrv
    , toXPub
    )
import Cardano.Ledger.Api
    ( BootstrapWitness
    , StandardCrypto
    , addrTxWitsL
    , bootAddrTxWitsL
    , emptyPParams
    , mkBasicTx
    , mkBasicTxBody
    , ppMinFeeAL
    , witsTxL
    )
import Cardano.Ledger.Keys.Bootstrap
    ( makeBootstrapWitness
    )
import Cardano.Mnemonic
    ( SomeMnemonic (..)
    , entropyToMnemonic
    , mkEntropy
    )
import Cardano.Wallet.Address.Derivation
    ( Depth (..)
    , DerivationType (WholeDomain)
    , Index
    )
import Cardano.Wallet.Address.Derivation.Byron
    ( byronKey
    )
import Cardano.Wallet.Address.Encoding
    ( toHDPayloadAddress
    )
import Cardano.Wallet.Primitive.NetworkId
    ( NetworkId (..)
    , SNetworkId (..)
    , withSNetworkId
    )
import Cardano.Wallet.Primitive.Passphrase
    ( Passphrase
    )
import Control.Lens
    ( over
    , set
    , view
    , (.~)
    , (^.)
    )
import Data.Function
    ( (&)
    )
import Data.IntCast
    ( intCast
    )
import Data.Word
    ( Word8
    )
import Internal.Cardano.Write.Tx
    ( AnyRecentEra (..)
    , Coin (..)
    , IsRecentEra
    , KeyWitnessCount (..)
    , RecentEra (..)
    , Tx
    , TxBody
    , evaluateMinimumFee
    , serializeTx
    )
import Internal.Cardano.Write.Tx.SizeEstimation
    ( sizeOf_BootstrapWitnesses
    )
import Numeric.Natural
    ( Natural
    )
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Extra
    ( (.>=.)
    )

import qualified Cardano.Api as CardanoApi
import qualified Cardano.Api.Byron as CardanoApi
import qualified Cardano.Chain.Common as Byron
import qualified Cardano.Crypto as CC
import qualified Cardano.Crypto.Hash.Class as Crypto
import qualified Cardano.Crypto.Wallet as Crypto.HD
import qualified Cardano.Wallet.Address.Derivation as W
import qualified Cardano.Wallet.Address.Derivation.Byron as Byron
import qualified Cardano.Wallet.Primitive.Types.Address as W
import qualified Cardano.Wallet.Primitive.Types.Tx.Constraints as W
import qualified Data.ByteString as BS
import qualified Data.Set as Set

spec :: Spec
spec = do
    describe "bootstrap witnesses" $ do
        -- Used in 'estimateTxSize', and in turn used by coin-selection
        let coinSelectionEstimatedSize :: Natural -> Natural
            coinSelectionEstimatedSize = W.unTxSize . sizeOf_BootstrapWitnesses

        let withNoKeyWits tx = tx
                & (witsTxL . addrTxWitsL) .~ mempty
                & (witsTxL . bootAddrTxWitsL) .~ mempty

        let measuredWitSize
                :: IsRecentEra era
                => Tx era
                -> Natural
            measuredWitSize tx = fromIntegral
                $ serializedSize tx
                - serializedSize (withNoKeyWits tx)

        let evaluateMinimumFeeSize
                :: forall era. IsRecentEra era
                => Tx era
                -> Natural
            evaluateMinimumFeeSize tx = fromIntegral
                $ unCoin
                $ evaluateMinimumFee
                    pp
                    (withNoKeyWits tx)
                    (KeyWitnessCount 0 (fromIntegral $ length wits))
              where
                wits = tx ^. witsTxL . bootAddrTxWitsL

                -- Dummy PParams to ensure a Coin-delta corresponds to a
                -- size-delta.
                pp = emptyPParams & set ppMinFeeAL (Coin 1)

        let evaluateMinimumFeeDerivedWitSize tx
                = evaluateMinimumFeeSize tx
                - evaluateMinimumFeeSize (withNoKeyWits tx)

        it "coin-selection's size estimation == balanceTx's size estimation"
            $ property
            $ prop_bootstrapWitnesses
            $ \n tx -> do
                let balanceSize = evaluateMinimumFeeDerivedWitSize tx
                let csSize = coinSelectionEstimatedSize $ intCast n
                balanceSize === csSize
                -- >= would suffice, but we can be stronger

        it "balanceTx's size estimation >= measured serialized size"
            $ property
            $ prop_bootstrapWitnesses
            $ \n tx -> do
                let estimated = evaluateMinimumFeeDerivedWitSize tx
                let measured = measuredWitSize tx
                let overestimation
                        | estimated > measured = estimated - measured
                        | otherwise            = 0

                let tabulateOverestimation = tabulate "overestimation/wit" $
                        if n == 0
                        then [show overestimation <> " (but with no wits)"]
                        else [show $ overestimation `div` fromIntegral n]

                estimated .>=. measured
                    & tabulateOverestimation

{-# ANN prop_bootstrapWitnesses ("HLint: ignore Eta reduce" :: String) #-}
prop_bootstrapWitnesses
    :: (forall era. IsRecentEra era => Word8 -> Tx era -> Property)
    -> Word8
    -- ^ Number of bootstrap witnesses.
    --
    -- Testing with [0, 255] should be sufficient.
    -> AnyRecentEra
    -> CardanoApi.NetworkId
    -- ^ Network - will be encoded inside the witness.
    -> Index 'WholeDomain 'AccountK
    -- ^ Account index - will be encoded inside the witness.
    -> Index 'WholeDomain 'CredFromKeyK
    -- ^ Index for the first of the 'n' addresses.
    -> Property
prop_bootstrapWitnesses
    p n (AnyRecentEra (_ :: RecentEra era)) net accIx addr0Ix =
    let
        -- Start incrementing the ixs upward, and if we reach 'maxBound', loop
        -- around, to ensure we always have 'n' unique indices.
        addrIxs = take (fromIntegral n)
            $ [addr0Ix .. maxBound] ++ filter (< addr0Ix) [minBound .. addr0Ix]

        body = mkBasicTxBody

        wits :: [BootstrapWitness StandardCrypto]
        wits = map (dummyWitForIx body) addrIxs

        tx = mkBasicTx body
            & (witsTxL . bootAddrTxWitsL) .~ Set.fromList wits
    in
        p n tx
  where
    rootK = Byron.generateKeyFromSeed dummyMnemonic mempty
    pwd = mempty

    dummyWitForIx
        :: TxBody era
        -> Index 'WholeDomain 'CredFromKeyK
        -> BootstrapWitness StandardCrypto
    dummyWitForIx body ix =
        let
            accK = Byron.deriveAccountPrivateKey pwd rootK accIx
            addrKeyAtIx i = Byron.deriveAddressPrivateKey pwd accK i

            addrK = addrKeyAtIx $ toEnum $ fromEnum ix
            addr = case net of
                CardanoApi.Mainnet ->
                    W.paymentAddress SMainnet $ over byronKey toXPub addrK
                CardanoApi.Testnet _magic ->
                    -- The choice of network magic here is not important. The
                    -- size of the witness will not be affected by it. What may
                    -- affect the size, is the 'CardanoApi.NetworkId' we pass to
                    -- 'mkByronWitness' above.
                    withSNetworkId (NTestnet 0) $ \testnet ->
                        W.paymentAddress testnet $ over byronKey toXPub addrK
        in
            mkByronWitness body net addr
                (view byronKey addrK, pwd)

    -- TODO [ADP-2675] Avoid duplication with "Shelley.Transaction"
    -- https://cardanofoundation.atlassian.net/browse/ADP-2675
    mkByronWitness
        :: TxBody era
        -> CardanoApi.NetworkId
        -> W.Address
        -> (XPrv, Passphrase "encryption")
        -> BootstrapWitness StandardCrypto
    mkByronWitness body network addr encryptedKey =
        makeBootstrapWitness txHash (decrypt encryptedKey) addrAttr
      where
        txHash = Crypto.castHash $ Crypto.hashWith serialize' body

        decrypt (xprv, pwd') = CC.SigningKey
            $ Crypto.HD.xPrvChangePass pwd' BS.empty xprv

        addrAttr = Byron.mkAttributes $ Byron.AddrAttributes
            (toHDPayloadAddress addr)
            (CardanoApi.toByronNetworkMagic network)

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

serializedSize
    :: forall era. IsRecentEra era
    => Tx era
    -> Int
serializedSize = BS.length . serializeTx

dummyMnemonic :: SomeMnemonic
dummyMnemonic = SomeMnemonic $ either (error . show) id
    (entropyToMnemonic @12 <$> mkEntropy "0000000000000000")

--------------------------------------------------------------------------------
-- Arbitrary instances, generators, and shrinkers
--------------------------------------------------------------------------------

instance Arbitrary (Index 'WholeDomain depth) where
    arbitrary = arbitraryBoundedEnum
    shrink = shrinkBoundedEnum

instance Arbitrary AnyRecentEra where
    arbitrary = elements
        [ AnyRecentEra RecentEraBabbage
        , AnyRecentEra RecentEraConway
        ]

instance Arbitrary CardanoApi.NetworkId where
    arbitrary = oneof
        [ pure CardanoApi.Mainnet
        , CardanoApi.Testnet . CardanoApi.NetworkMagic <$> arbitrary
        ]
