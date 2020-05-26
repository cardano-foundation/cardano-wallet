{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.Byron.TransactionSpec
    ( spec
    , goldenMainnet__1_1
    , goldenMainnet__2_2
    , goldenMainnet__1_25
    , goldenMainnet__25_1
    , goldenTestnet__1_1
    , goldenTestnet__2_2
    , goldenTestnet__1_25
    , goldenTestnet__25_1
    ) where

import Prelude

import Cardano.Address.Derivation
    ( XPrv )
import Cardano.Crypto.Wallet
    ( generateNew, xpub )
import Cardano.Wallet.Byron.Transaction
    ( newTransactionLayer )
import Cardano.Wallet.Byron.Transaction.Size
    ( MaxSizeOf )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (..)
    , NetworkDiscriminant (..)
    , Passphrase (..)
    , PaymentAddress (..)
    , WalletKey
    , publicKey
    )
import Cardano.Wallet.Primitive.AddressDerivation.Byron
    ( ByronKey (..) )
import Cardano.Wallet.Primitive.AddressDerivation.Icarus
    ( IcarusKey (..) )
import Cardano.Wallet.Primitive.CoinSelection
    ( CoinSelection (..)
    , CoinSelectionOptions (..)
    , changeBalance
    , inputBalance
    , outputBalance
    )
import Cardano.Wallet.Primitive.CoinSelection.LargestFirst
    ( largestFirst )
import Cardano.Wallet.Primitive.Fee
    ( Fee (..)
    , FeeOptions (..)
    , FeePolicy (..)
    , OnDanglingChange (..)
    , adjustForFee
    , computeFee
    , rebalanceChangeOutputs
    )
import Cardano.Wallet.Primitive.Types
    ( Address (..)
    , Coin (..)
    , Hash (..)
    , ProtocolMagic (..)
    , SealedTx (..)
    , TxIn (..)
    , TxOut (..)
    , UTxO (..)
    , mainnetMagic
    , testnetMagic
    )
import Cardano.Wallet.Transaction
    ( TransactionLayer (..) )
import Cardano.Wallet.Unsafe
    ( unsafeFromHex )
import Control.Arrow
    ( first )
import Control.Monad.Trans.Except
    ( runExceptT )
import Data.ByteArray.Encoding
    ( Base (..), convertToBase )
import Data.ByteString
    ( ByteString )
import Data.Function
    ( (&) )
import Data.Functor.Identity
    ( Identity (..) )
import Data.List.NonEmpty
    ( NonEmpty (..) )
import Data.Proxy
    ( Proxy (..) )
import Data.Quantity
    ( Quantity (..) )
import Data.Word
    ( Word32 )
import Fmt
    ( pretty )
import Test.Hspec
    ( Spec, SpecWith, describe, expectationFailure, it, shouldBe )
import Test.QuickCheck
    ( Arbitrary (..)
    , Gen
    , Property
    , choose
    , counterexample
    , elements
    , forAllBlind
    , forAllShrinkBlind
    , property
    , scale
    , vector
    , vectorOf
    , withMaxSuccess
    )

import qualified Cardano.Byron.Codec.Cbor as CBOR
import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Pretty as CBOR
import qualified Codec.CBOR.Write as CBOR
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map

spec :: Spec
spec = do
    describe "Coin Selection w/ Byron" $ do
        it "REG #1561 - Correct balancing of amounts close to the limit" $ do
            let opts = FeeOptions
                    { estimateFee = computeFee feePolicy . estimateSize tlayer
                    , dustThreshold = minBound
                    , onDanglingChange = SaveMoney
                    }
                  where
                    tlayer =
                        newTransactionLayer @'Mainnet @ByronKey Proxy mainnetMagic
                    feePolicy =
                        LinearFee (Quantity 155381) (Quantity 43) (Quantity 0)

            let addr = Address "fake-address"
            let utxo = UTxO mempty
            let csel = CoinSelection
                    { inputs =
                        [ ( TxIn (Hash "0") 0
                          , TxOut addr (Coin 1_000_000)
                          )
                        ]
                    , outputs =
                        [ TxOut addr (Coin 800_000)
                        ]
                    , change =
                        [Coin 30_556]
                    }

            runExceptT (adjustForFee opts utxo csel) >>= \case
                Left e -> expectationFailure $ "failed with: " <> show e
                Right{}-> pure ()

    it "1561 - The fee balancing algorithm converges for any coin selection."
        $ property
        $ withMaxSuccess 10000
        $ forAllBlind (genSelection @'Mainnet @ByronKey) prop_rebalanceChangeOutputs

    describe "Fee estimation calculation" $ do
        it "Byron / Mainnet" $ property $
            propSizeEstimation @'Mainnet @ByronKey mainnetMagic
                (genSelection @'Mainnet @ByronKey)
                (\n -> vectorOf n $ genLegacyAddress @'Mainnet @ByronKey)

        it "Byron / Testnet" $ property $
            propSizeEstimation @('Testnet 459045235) @ByronKey (testnetMagic @459045235)
                (genSelection @('Testnet 459045235) @ByronKey)
                (\n -> vectorOf n $ genLegacyAddress @('Testnet 459045235) @ByronKey)

        it "Icarus / Mainnet" $ property $
            propSizeEstimation @'Mainnet @IcarusKey mainnetMagic
                (genSelection @'Mainnet @IcarusKey)
                (\n -> vectorOf n $ genLegacyAddress @'Mainnet @IcarusKey)

        it "Icarus / Testnet" $ property $
            propSizeEstimation @('Testnet 459045235) @IcarusKey (testnetMagic @459045235)
                (genSelection @('Testnet 459045235) @IcarusKey)
                (\n -> vectorOf n $ genLegacyAddress @('Testnet 459045235) @IcarusKey)

    describe "Golden Tests - Cardano-SL - signed tx (Mainnet)" $ do
        let proxy = Proxy @'Mainnet
        goldenTestSignedTx proxy mainnetMagic 1
            [ (xprv "address-number-0", Coin 42)
            ] goldenMainnet__1_1

        goldenTestSignedTx proxy mainnetMagic 2
            [ (xprv "address-number-0", Coin 42)
            , (xprv "address-number-1", Coin 14)
            ] goldenMainnet__2_2


        goldenTestSignedTx proxy mainnetMagic 25
            [ (xprv "address-number-0", Coin 14)
            ] goldenMainnet__25_1

        goldenTestSignedTx proxy mainnetMagic 1
            [ (xprv "address-number-0", Coin 14)
            , (xprv "address-number-1", Coin 42)
            , (xprv "address-number-2", Coin 287)
            , (xprv "address-number-3", Coin 647)
            , (xprv "address-number-4", Coin 1145)
            , (xprv "address-number-5", Coin 2178)
            , (xprv "address-number-6", Coin 6874)
            , (xprv "address-number-7", Coin 9177)
            , (xprv "address-number-8", Coin 21412)
            , (xprv "address-number-9", Coin 35787)
            , (xprv "address-number-10", Coin 66745)
            , (xprv "address-number-11", Coin 142141)
            , (xprv "address-number-12", Coin 314142)
            , (xprv "address-number-13", Coin 666666)
            , (xprv "address-number-14", Coin 1389571)
            , (xprv "address-number-15", Coin 8589934592)
            , (xprv "address-number-16", Coin 1)
            , (xprv "address-number-17", Coin 1)
            , (xprv "address-number-18", Coin 1)
            , (xprv "address-number-19", Coin 1)
            , (xprv "address-number-20", Coin 1)
            , (xprv "address-number-21", Coin 1)
            , (xprv "address-number-22", Coin 1)
            , (xprv "address-number-23", Coin 1)
            , (xprv "address-number-24", Coin 1)
            , (xprv "address-number-25", Coin 1)
            ] goldenMainnet__1_25

    describe "Golden Tests - Cardano-SL - signed tx (Testnet)" $ do
        let proxy = Proxy @('Testnet 1097911063)
        let magic = ProtocolMagic 1097911063
        goldenTestSignedTx proxy magic 1
            [ (xprv "address-number-0", Coin 42)
            ] goldenTestnet__1_1

        goldenTestSignedTx proxy magic 2
            [ (xprv "address-number-0", Coin 42)
            , (xprv "address-number-1", Coin 14)
            ] goldenTestnet__2_2

        goldenTestSignedTx proxy magic 25
            [ (xprv "address-number-0", Coin 14)
            ] goldenTestnet__25_1

        goldenTestSignedTx proxy magic 1
            [ (xprv "address-number-0", Coin 14)
            , (xprv "address-number-1", Coin 42)
            , (xprv "address-number-2", Coin 287)
            , (xprv "address-number-3", Coin 647)
            , (xprv "address-number-4", Coin 1145)
            , (xprv "address-number-5", Coin 2178)
            , (xprv "address-number-6", Coin 6874)
            , (xprv "address-number-7", Coin 9177)
            , (xprv "address-number-8", Coin 21412)
            , (xprv "address-number-9", Coin 35787)
            , (xprv "address-number-10", Coin 66745)
            , (xprv "address-number-11", Coin 142141)
            , (xprv "address-number-12", Coin 314142)
            , (xprv "address-number-13", Coin 666666)
            , (xprv "address-number-14", Coin 1389571)
            , (xprv "address-number-15", Coin 8589934592)
            , (xprv "address-number-16", Coin 1)
            , (xprv "address-number-17", Coin 1)
            , (xprv "address-number-18", Coin 1)
            , (xprv "address-number-19", Coin 1)
            , (xprv "address-number-20", Coin 1)
            , (xprv "address-number-21", Coin 1)
            , (xprv "address-number-22", Coin 1)
            , (xprv "address-number-23", Coin 1)
            , (xprv "address-number-24", Coin 1)
            , (xprv "address-number-25", Coin 1)
            ] goldenTestnet__1_25

{-------------------------------------------------------------------------------
                                Properties
-------------------------------------------------------------------------------}

prop_rebalanceChangeOutputs
    :: CoinSelection
    -> OnDanglingChange
    -> Property
prop_rebalanceChangeOutputs sel onDangling = do
    let (sel', fee') = rebalanceChangeOutputs opts sel
    let prop = case onDangling of
            PayAndBalance ->
                fee' /= Fee 0 || Fee (delta sel') == estimateFee opts sel'
            SaveMoney ->
                fee' /= Fee 0 || Fee (delta sel') >= estimateFee opts sel'
    property prop
        & counterexample (unlines
            [ "selection (before):", pretty sel
            , "selection (after):", pretty sel'
            , "delta (before): " <> show (delta sel)
            , "delta (after):  " <> show (delta sel')
            , "remaining fee:  " <> show (getFee fee')
            ])
  where
    delta s = inputBalance s - (outputBalance s + changeBalance s)
    opts = FeeOptions
        { estimateFee = computeFee feePolicy . estimateSize tlayer
        , dustThreshold = minBound
        , onDanglingChange = onDangling
        }
      where
        tlayer =
            newTransactionLayer @'Mainnet @ByronKey Proxy mainnetMagic
        feePolicy =
            LinearFee (Quantity 155381) (Quantity 43) (Quantity 0)

propSizeEstimation
    :: forall n k.
       ( WalletKey k
       , MaxSizeOf Address n ByronKey
       )
    => ProtocolMagic
    -> Gen CoinSelection
    -> (Int -> Gen [Address])
    -> Property
propSizeEstimation pm genSel genChngAddrs =
    withMaxSuccess 1000 $
    forAllShrinkBlind genSel shrinkSelection $ \sel -> let nc = length (change sel) in
    forAllBlind (genChngAddrs nc) $ \chngAddrs ->
    let
        calcSize = estimateSize (newTransactionLayer @n @k Proxy pm) sel
        cbor = fromCoinSelection sel chngAddrs
        size = fromIntegral $ BS.length $ CBOR.toStrictByteString cbor

        -- As we have
        -- maxSizeOf Icarus w/ Mainnet = 43
        -- maxSizeOf Random w/ Mainnet = 76
        -- The difference is 33, and we always go for the higher bound for
        -- change address payload's size, so, we may end up with up to
        -- 33 + 12 = 45 extra bytes per change address in our estimation.
        -- For Icarus addresses, we can be as good as 4 bytes per change address
        -- because there's no variance due to the derivation path encoded as
        -- attributes (this only happens on random addresses).
        margin = 45 * fromIntegral (length $ change sel)
        realSizeSup = Quantity (size + margin)
        realSizeInf = Quantity size
    in
        property (calcSize >= realSizeInf && calcSize <= realSizeSup)
            & counterexample ("Raw Transaction:" <> CBOR.prettyHexEnc cbor)
            & counterexample ("Coin Selection:\n" <> pretty sel)
            & counterexample (unlines
                [ "real size inf:   " <> show size
                , "real size sup:   " <> show (size + margin)
                , "estimated size:  " <> show (getQuantity calcSize)
                ])
  where
    fromCoinSelection :: CoinSelection -> [Address] -> CBOR.Encoding
    fromCoinSelection (CoinSelection inps outs chngs) chngAddrs =
        CBOR.encodeSignedTx (fst <$> inps, outs <> outs') wits
      where
        dummySig =
            BS.replicate 64 0
        Right dummyXPub =
            xpub $ BS.replicate 64 0
        outs' =
            zipWith TxOut (take (length chngs) chngAddrs) chngs
        wits =
            replicate (length inps)
                $ CBOR.toStrictByteString
                $ CBOR.encodePublicKeyWitness dummyXPub dummySig

newtype LegacyAddress (n :: NetworkDiscriminant) (k :: Depth -> * -> *) =
    LegacyAddress (Proxy n, Proxy k, Address)

instance (PaymentAddress n ByronKey) => Arbitrary (LegacyAddress n ByronKey)
  where
    arbitrary = do
        bytes <- BS.pack <$> vector 64
        addrIx <- toEnum . fromIntegral <$> choose (0, 10 :: Word32)
        let (Right key) = xpub bytes
        let pw = Passphrase $ BA.convert $ BS.replicate 32 0
        pure $ LegacyAddress
            ( Proxy
            , Proxy
            , paymentAddress @n $ ByronKey key (minBound, addrIx) pw
            )

instance (PaymentAddress n IcarusKey) => Arbitrary (LegacyAddress n IcarusKey)
  where
    arbitrary = do
        bytes <- BS.pack <$> vector 64
        let (Right key) = xpub bytes
        pure $ LegacyAddress
            ( Proxy
            , Proxy
            , paymentAddress @n $ IcarusKey key
            )

instance Arbitrary OnDanglingChange
  where
    arbitrary = elements [ PayAndBalance, SaveMoney ]

genLegacyAddress
    :: forall (n :: NetworkDiscriminant) k.
        ( Arbitrary (LegacyAddress n k)
        )
    => Gen Address
genLegacyAddress = do
    LegacyAddress (_ :: Proxy n, _ :: Proxy k, addr) <- arbitrary
    pure addr

-- 'CoinSelection' are generated by running the @largestFirst@ algorithm on an
-- arbitrary UTxO and to cover an arbitrary set of 'TxOut'.
genSelection
    :: forall (n :: NetworkDiscriminant) k.
        ( Arbitrary (LegacyAddress n k)
        )
    => Gen CoinSelection
genSelection = do
    outs <- choose (1, 10) >>= \n -> vectorOf n genCoin >>= genTxOut @n @k
    genSelectionFor (NE.fromList outs)
  where
    genSelectionFor :: NonEmpty TxOut -> Gen CoinSelection
    genSelectionFor outs = do
        utxo <- vectorOf (NE.length outs * 3) genCoin >>= genUTxO @n @k
        case runIdentity $ runExceptT $ largestFirst opts outs utxo of
            Left _ -> genSelectionFor outs
            Right (s,_) -> return s

    opts :: CoinSelectionOptions ()
    opts = CoinSelectionOptions
        { maximumNumberOfInputs = const 100
        , validate = const $ Right ()
        }

shrinkSelection :: CoinSelection -> [CoinSelection]
shrinkSelection sel@(CoinSelection inps outs chgs) = case (inps, outs, chgs) of
    ([_], [_], []) ->
        []
    _ ->
        let
            inps' = take (max 1 (length inps `div` 2)) inps
            outs' = take (max 1 (length outs `div` 2)) outs
            chgs' = take (length chgs `div` 2) chgs
            inps'' = if length inps > 1 then drop 1 inps else inps
            outs'' = if length outs > 1 then drop 1 outs else outs
            chgs'' = drop 1 chgs
        in
            filter (\s -> s /= sel && isValidSelection s)
                [ CoinSelection inps' outs' chgs'
                , CoinSelection inps' outs chgs
                , CoinSelection inps outs chgs'
                , CoinSelection inps outs' chgs
                , CoinSelection inps'' outs'' chgs''
                , CoinSelection inps'' outs chgs
                , CoinSelection inps outs'' chgs
                , CoinSelection inps outs chgs''
                ]

isValidSelection :: CoinSelection -> Bool
isValidSelection (CoinSelection i o c) =
    let
        oAmt = sum $ map (fromIntegral . getCoin . coin) o
        cAmt = sum $ map (fromIntegral . getCoin) c
        iAmt = sum $ map (fromIntegral . getCoin . coin . snd) i
    in
        (iAmt :: Integer) >= (oAmt + cAmt)

genTxIn :: Gen TxIn
genTxIn = TxIn
    <$> genTxId
    <*> scale (`mod` 3) arbitrary -- No need for a high indexes

genTxId :: Gen (Hash "Tx")
genTxId =
    Hash . BS.pack <$> vector 32

genCoin :: Gen Coin
genCoin = Coin
    <$> choose (1, 200000)

genUTxO
    :: forall (n :: NetworkDiscriminant) k.  ( Arbitrary (LegacyAddress n k))
    => [Coin]
    -> Gen UTxO
genUTxO coins = do
    let n = length coins
    inps <- vectorOf n genTxIn
    outs <- genTxOut @n @k coins
    return $ UTxO $ Map.fromList $ zip inps outs

genTxOut
    :: forall (n :: NetworkDiscriminant) k.
        ( Arbitrary (LegacyAddress n k)
        )
    => [Coin]
    -> Gen [TxOut]
genTxOut coins = do
    let n = length coins
    outs <- vectorOf n $ genLegacyAddress @n @k
    return $ zipWith TxOut outs coins

{-------------------------------------------------------------------------------
                                Golden Tests
-------------------------------------------------------------------------------}

xprv :: ByteString -> IcarusKey depth XPrv
xprv seed =
    IcarusKey $ generateNew seed (mempty :: ByteString) (mempty :: ByteString)

goldenTestSignedTx
    :: forall (n :: NetworkDiscriminant) k.
        ( k ~ IcarusKey
        , MaxSizeOf Address n ByronKey
        , PaymentAddress n k
        )
    => Proxy n
    -> ProtocolMagic
    -> Int
        -- ^ Number of outputs
    -> [(k 'AddressK XPrv, Coin)]
        -- ^ (Address Private Keys, Output value)
    -> ByteString
        -- ^ Expected result, in Base16
    -> SpecWith ()
goldenTestSignedTx proxy pm nOuts xprvs expected = it title $ do
    let addrs = first (paymentAddress @n @k . publicKey) <$> xprvs
    let s = Map.fromList (zip (fst <$> addrs) (fst <$> xprvs))
    let keyFrom a = (,mempty) <$> Map.lookup a s
    let inps = mkInput <$> zip addrs [0..]
    let outs = take nOuts $ mkOutput <$> cycle addrs
    let res = mkStdTx (newTransactionLayer proxy pm) keyFrom inps outs
    case res of
        Left e -> fail (show e)
        Right (_tx, SealedTx bytes) ->
            convertToBase Base16 bytes `shouldBe` expected
  where
    title :: String
    title = mempty
        <> show (length xprvs) <> " inputs & "
        <> show nOuts <> " outputs"

    -- | Inputs are constructed from _resolved inputs_, i.e., inputs for which
    -- we have the associated address; This address is used for signing as we
    -- have, in order to submit a valid transaction, to provide evidence for
    -- ownership of these addresses
    mkInput :: ((Address, Coin), Word32) -> (TxIn, TxOut)
    mkInput (out, ix) =
        ( TxIn faucetTx ix
        , mkOutput out
        )

    -- | Arbitrary output too, we do re-use the same addresses here but this is
    -- purely arbitrary.
    mkOutput :: (Address, Coin) -> TxOut
    mkOutput =
        uncurry TxOut

    -- | An arbitrary source transaction for inputs (see 'TxIn'). This could be
    -- anything in practice and isn't relevant to our signing code.
    faucetTx :: Hash "Tx"
    faucetTx = Hash $ unsafeFromHex
        "3B40265111D8BB3C3C608D95B3A0BF83461ACE32D79336579A1939B3AAD1C0B7"

goldenMainnet__1_1 :: ByteString
goldenMainnet__1_1 =
    "82839f8200d81858248258203b40265111d8bb3c3c608d95b3a0bf83461ace3\
    \2d79336579a1939b3aad1c0b700ff9f8282d818582183581cb0e693cbc97272\
    \bd42aacf3b443ecac68b96b4fa20159186a4a38883a0001a497c15c6182affa\
    \0818200d81858858258408e620235b2a427259cc1a1c63af9ef179cb3b6e396\
    \da182e01d65dd3ed7be94be25db39fb78e74d4b53fb51776d0f5eb360e62d09\
    \b853f3a87ac25bf834ee1fb58403684501a21346ba3c19a6dc3479180e80a14\
    \e5edc0ffb4b1469abe0332e7e1caf291d50cb8d2c47ee37f6b44b7fbfcc5a1b\
    \01bc262038e93969b9a728a2b3500"

goldenMainnet__2_2 :: ByteString
goldenMainnet__2_2 =
    "82839f8200d81858248258203b40265111d8bb3c3c608d95b3a0bf83461ace32\
    \d79336579a1939b3aad1c0b7008200d81858248258203b40265111d8bb3c3c60\
    \8d95b3a0bf83461ace32d79336579a1939b3aad1c0b701ff9f8282d818582183\
    \581cb0e693cbc97272bd42aacf3b443ecac68b96b4fa20159186a4a38883a000\
    \1a497c15c6182a8282d818582183581c5d9254c4b41dbdc10f9c3b4517630497\
    \6e5e210decc38792ab2b50c9a0001ab537acfc0effa0828200d8185885825840\
    \8e620235b2a427259cc1a1c63af9ef179cb3b6e396da182e01d65dd3ed7be94b\
    \e25db39fb78e74d4b53fb51776d0f5eb360e62d09b853f3a87ac25bf834ee1fb\
    \5840a214bef8191aca0b41c109101c420deb131f5eff63394285dc18fc39726f\
    \b24a7bc34adce5a6e234360aae398b431a47d9b9d9a71cdcea6bd21bac7ed816\
    \53068200d81858858258402cf36a269802da7cc0408308bda9a0015e7e819121\
    \17013c93edeefa9b62bbb61bbf1c6081545b1ab140578d7b5c035bf904d05dd8\
    \e9b79b34d3160f86206bfc58403cb09514f2d27f5dab76480ec010b4e89c0c50\
    \f583c00bca487e0a45643e4c2854842c57d87f93961efad7037be8de5100d0fc\
    \e2383e8a11e20cb9762731da0c"

goldenMainnet__25_1 :: ByteString
goldenMainnet__25_1 =
    "82839f8200d81858248258203b40265111d8bb3c3c608d95b3a0bf83461ace32\
    \d79336579a1939b3aad1c0b700ff9f8282d818582183581cb0e693cbc97272bd\
    \42aacf3b443ecac68b96b4fa20159186a4a38883a0001a497c15c60e8282d818\
    \582183581cb0e693cbc97272bd42aacf3b443ecac68b96b4fa20159186a4a388\
    \83a0001a497c15c60e8282d818582183581cb0e693cbc97272bd42aacf3b443e\
    \cac68b96b4fa20159186a4a38883a0001a497c15c60e8282d818582183581cb0\
    \e693cbc97272bd42aacf3b443ecac68b96b4fa20159186a4a38883a0001a497c\
    \15c60e8282d818582183581cb0e693cbc97272bd42aacf3b443ecac68b96b4fa\
    \20159186a4a38883a0001a497c15c60e8282d818582183581cb0e693cbc97272\
    \bd42aacf3b443ecac68b96b4fa20159186a4a38883a0001a497c15c60e8282d8\
    \18582183581cb0e693cbc97272bd42aacf3b443ecac68b96b4fa20159186a4a3\
    \8883a0001a497c15c60e8282d818582183581cb0e693cbc97272bd42aacf3b44\
    \3ecac68b96b4fa20159186a4a38883a0001a497c15c60e8282d818582183581c\
    \b0e693cbc97272bd42aacf3b443ecac68b96b4fa20159186a4a38883a0001a49\
    \7c15c60e8282d818582183581cb0e693cbc97272bd42aacf3b443ecac68b96b4\
    \fa20159186a4a38883a0001a497c15c60e8282d818582183581cb0e693cbc972\
    \72bd42aacf3b443ecac68b96b4fa20159186a4a38883a0001a497c15c60e8282\
    \d818582183581cb0e693cbc97272bd42aacf3b443ecac68b96b4fa20159186a4\
    \a38883a0001a497c15c60e8282d818582183581cb0e693cbc97272bd42aacf3b\
    \443ecac68b96b4fa20159186a4a38883a0001a497c15c60e8282d81858218358\
    \1cb0e693cbc97272bd42aacf3b443ecac68b96b4fa20159186a4a38883a0001a\
    \497c15c60e8282d818582183581cb0e693cbc97272bd42aacf3b443ecac68b96\
    \b4fa20159186a4a38883a0001a497c15c60e8282d818582183581cb0e693cbc9\
    \7272bd42aacf3b443ecac68b96b4fa20159186a4a38883a0001a497c15c60e82\
    \82d818582183581cb0e693cbc97272bd42aacf3b443ecac68b96b4fa20159186\
    \a4a38883a0001a497c15c60e8282d818582183581cb0e693cbc97272bd42aacf\
    \3b443ecac68b96b4fa20159186a4a38883a0001a497c15c60e8282d818582183\
    \581cb0e693cbc97272bd42aacf3b443ecac68b96b4fa20159186a4a38883a000\
    \1a497c15c60e8282d818582183581cb0e693cbc97272bd42aacf3b443ecac68b\
    \96b4fa20159186a4a38883a0001a497c15c60e8282d818582183581cb0e693cb\
    \c97272bd42aacf3b443ecac68b96b4fa20159186a4a38883a0001a497c15c60e\
    \8282d818582183581cb0e693cbc97272bd42aacf3b443ecac68b96b4fa201591\
    \86a4a38883a0001a497c15c60e8282d818582183581cb0e693cbc97272bd42aa\
    \cf3b443ecac68b96b4fa20159186a4a38883a0001a497c15c60e8282d8185821\
    \83581cb0e693cbc97272bd42aacf3b443ecac68b96b4fa20159186a4a38883a0\
    \001a497c15c60e8282d818582183581cb0e693cbc97272bd42aacf3b443ecac6\
    \8b96b4fa20159186a4a38883a0001a497c15c60effa0818200d8185885825840\
    \8e620235b2a427259cc1a1c63af9ef179cb3b6e396da182e01d65dd3ed7be94b\
    \e25db39fb78e74d4b53fb51776d0f5eb360e62d09b853f3a87ac25bf834ee1fb\
    \58402b01895a50681f18418da48bf6cf9d70ad465018bb9276f6f0aa9b58ff2d\
    \2f273d584e4f27e228cd57a6f96009d5c27525fd92647db708185e488713d40d\
    \d402"


goldenMainnet__1_25 :: ByteString
goldenMainnet__1_25 =
    "82839f8200d81858248258203b40265111d8bb3c3c608d95b3a0bf83461ace32d\
    \79336579a1939b3aad1c0b7008200d81858248258203b40265111d8bb3c3c608d\
    \95b3a0bf83461ace32d79336579a1939b3aad1c0b7018200d81858248258203b4\
    \0265111d8bb3c3c608d95b3a0bf83461ace32d79336579a1939b3aad1c0b70282\
    \00d81858248258203b40265111d8bb3c3c608d95b3a0bf83461ace32d79336579\
    \a1939b3aad1c0b7038200d81858248258203b40265111d8bb3c3c608d95b3a0bf\
    \83461ace32d79336579a1939b3aad1c0b7048200d81858248258203b40265111d\
    \8bb3c3c608d95b3a0bf83461ace32d79336579a1939b3aad1c0b7058200d81858\
    \248258203b40265111d8bb3c3c608d95b3a0bf83461ace32d79336579a1939b3a\
    \ad1c0b7068200d81858248258203b40265111d8bb3c3c608d95b3a0bf83461ace\
    \32d79336579a1939b3aad1c0b7078200d81858248258203b40265111d8bb3c3c6\
    \08d95b3a0bf83461ace32d79336579a1939b3aad1c0b7088200d8185824825820\
    \3b40265111d8bb3c3c608d95b3a0bf83461ace32d79336579a1939b3aad1c0b70\
    \98200d81858248258203b40265111d8bb3c3c608d95b3a0bf83461ace32d79336\
    \579a1939b3aad1c0b70a8200d81858248258203b40265111d8bb3c3c608d95b3a\
    \0bf83461ace32d79336579a1939b3aad1c0b70b8200d81858248258203b402651\
    \11d8bb3c3c608d95b3a0bf83461ace32d79336579a1939b3aad1c0b70c8200d81\
    \858248258203b40265111d8bb3c3c608d95b3a0bf83461ace32d79336579a1939\
    \b3aad1c0b70d8200d81858248258203b40265111d8bb3c3c608d95b3a0bf83461\
    \ace32d79336579a1939b3aad1c0b70e8200d81858248258203b40265111d8bb3c\
    \3c608d95b3a0bf83461ace32d79336579a1939b3aad1c0b70f8200d8185824825\
    \8203b40265111d8bb3c3c608d95b3a0bf83461ace32d79336579a1939b3aad1c0\
    \b7108200d81858248258203b40265111d8bb3c3c608d95b3a0bf83461ace32d79\
    \336579a1939b3aad1c0b7118200d81858248258203b40265111d8bb3c3c608d95\
    \b3a0bf83461ace32d79336579a1939b3aad1c0b7128200d81858248258203b402\
    \65111d8bb3c3c608d95b3a0bf83461ace32d79336579a1939b3aad1c0b7138200\
    \d81858248258203b40265111d8bb3c3c608d95b3a0bf83461ace32d79336579a1\
    \939b3aad1c0b7148200d81858248258203b40265111d8bb3c3c608d95b3a0bf83\
    \461ace32d79336579a1939b3aad1c0b7158200d81858248258203b40265111d8b\
    \b3c3c608d95b3a0bf83461ace32d79336579a1939b3aad1c0b7168200d8185824\
    \8258203b40265111d8bb3c3c608d95b3a0bf83461ace32d79336579a1939b3aad\
    \1c0b7178200d81858258258203b40265111d8bb3c3c608d95b3a0bf83461ace32\
    \d79336579a1939b3aad1c0b718188200d81858258258203b40265111d8bb3c3c6\
    \08d95b3a0bf83461ace32d79336579a1939b3aad1c0b71819ff9f8282d8185821\
    \83581cb0e693cbc97272bd42aacf3b443ecac68b96b4fa20159186a4a38883a00\
    \01a497c15c60effa0981a8200d81858858258408e620235b2a427259cc1a1c63a\
    \f9ef179cb3b6e396da182e01d65dd3ed7be94be25db39fb78e74d4b53fb51776d\
    \0f5eb360e62d09b853f3a87ac25bf834ee1fb5840fd0bbc24ade743630a5f8aa8\
    \ec3c306260c26783fd92049680cbbc532dfe41473dd55d6c04149bb5d6253b749\
    \8755dd8d31b7ec7e459abcd7483d31a4c3486038200d81858858258402cf36a26\
    \9802da7cc0408308bda9a0015e7e81912117013c93edeefa9b62bbb61bbf1c608\
    \1545b1ab140578d7b5c035bf904d05dd8e9b79b34d3160f86206bfc5840e64c39\
    \fafd472b4be20fc1b70be2d910a6c9f05f329595902a1e633f99dfa4be3a77abd\
    \7cf7bb236c3148951b366d82d4460e3b3a24f081da681b8eeefdd83028200d818\
    \5885825840d5ac2de87f73943e2af170a34be58db3997b9c5b6685e8cc4e33d37\
    \71798a0d5850235a0cafb25f5324da599db055ae743efd205b99c91586dd8e7b6\
    \25f04581584051981e339afcedf67a7e446a72623c7ba114794c307fab57922a1\
    \76c5636afc22ec4d4f46bee5b1784f4c321ea77743f2294d405afe0e64e4bb57d\
    \5d2d30af078200d81858858258408a75ede1f39317d26fa80755807a11ebd4b18\
    \beba63d6b7de2dae1d1b01f9bddc58090ed0fd50fa95d12a5c93e29d1eca37019\
    \c3ec25890aba3bb77aa20be5105840dbed9eb37398a97f2e23c8cc04c5d795588\
    \c74c3b35a9c052c2bc962a7fa6dbc1759abed7a326eacd6b9bafe8f275867232f\
    \0aed592de2dda9e3c91c10dbbf048200d8185885825840353cbf7e535c6a74fc1\
    \aabe6a554650d87b9625da43dbe4a8c55bb81a95668862d15dea850a7b2d127b0\
    \ccd6e98f929e544ca4f5198cb92ed8fd1a342dfbba4a58400dd03ea6212416b30\
    \8e503fc227e66b3edd3e6b1643d01364c7ade22c488129229483d6ad85a93b01b\
    \128fc07c049845e085423b467d3d499cfab0c037157c0d8200d81858858258407\
    \0e046565dc76736332f886d918d6fd78bdb21aaa9b60e54ffacfec921c6f49c01\
    \bb7822abaf3e9711c9ed5e5e0c2b293f7759caa483eab05651fdf227e584e3584\
    \08b45210f2f18af85fe42cdda10c7f2a62846061c560660b1db2a4ded7dd497ff\
    \bddadd488e3b226172652e931d9278afebe557c6daa6bff6f40594bfa29efc038\
    \200d81858858258403907e9f5dd48253f86947f557e4b8a4178be4ba40268ebcb\
    \d3f377c35c75530a0f05dbace6b2b8363d4f7b71f7264369487e13d4ee634c989\
    \0c4554ea6ae2c7a584069e9b5040f17e9530ae270fdc6ec76c4fff8db7743dd16\
    \8a34b52ba1db9884e905fede8540a70991c9a3e6da3d4de21dbfb365c59d0a693\
    \35a35a3dff69893048200d8185885825840110ad78e56e38da42e37033710af91\
    \da5a76c58db9eccff83059b3b135d2c6692f9e995c2d73d5fb4edcdfa7f3fc737\
    \cc7a2debdff413b5b8336302a8b66592658407ba0af525be47be95a395ac5771d\
    \9cb97f90d8907789900fb456503d27c657f85d63449c36444add3bfe56b01fa78\
    \45008628d5d1c79cfafb5f6d83e22e432018200d818588582584094b01b4ea67f\
    \f5457a30e599aa2a5695ba9e48b9e6e45fa694baafe3f204231be79f7d04c19be\
    \9ff153a79db0f5cb39cd2ad469ba62f65d26a05df49cdbfe85758406b52bba616\
    \a8a3699a9f0f193cd06b2ab19fd04048908cbe6f920f9e9cc7286cd0e0b911fa5\
    \4523c5e6774ff27c550ef3b34a818eb00e24f9cfed1fa17d93a008200d8185885\
    \8258406baf92d3ad25656d677ba9f8b1d9f977d3781a26b83899022b47fa29074\
    \2fab11c1b1a743f6b8ece3e06212b41153c7cf0eff1521d26d027af2c01c68c66\
    \0b7f58406b5190c93a84c0ae8e78ee87d1637c2abf916f7a7c44b272d1b32b7ca\
    \35a78bec55325f47158db6f1e96237d5d5f3f02a43cbd7fa8772734e2f8cd660e\
    \eefb0b8200d8185885825840bb022193d349a54cab81ffa12ad179e149f1e543b\
    \6f00802b820fd9abe48862c29f96692a0da19899f064e8752a89f4468f5bef4a6\
    \2de97179cd565c54c697d05840d9084543eee65b0672957de5180fb96afe44ee2\
    \145b0234b12a0ef6e3f4420f34b1ddf15617189cd12d7a4a9b5e40c85b6e6aa03\
    \876014472ce820ada7e318088200d8185885825840b937f13c458951d1d2308ac\
    \ab0ddc1233cfae660de288cbdeb262855ad3eb0d3cd75d2b09694b1bfdd307faa\
    \023e20523c7a103d7b09cf37a6a98abc5ce87ce85840609e8c9cf09b10f4756e2\
    \e14a1e278c71a6c59b2a5d742eeb6f936a3c607cd1696825762b6e2387da7ede3\
    \2341e347c9e80fb76612296c70ada7ba3ccdf8290f8200d8185885825840d79b3\
    \42a6ab8f789a9c0e73f45d38d026f880b7df296dd01ab485cfe366053aea3ab23\
    \b04f2d31e1c40a4468dcd61c43cdc7d3fb332be7bbbf16dcb2bbc432115840d66\
    \9baa8ba33e54c61af833e2ceba1f74a6b410905a565cf547e94d5d9aef3719952\
    \0df540eadb0752d133cffd7052d1b2063db625c544e5cab359bdfc1575048200d\
    \8185885825840dcfc39775d9141b762a832e2f4e1b1de4a0b54c818ee2939216b\
    \db8e5608970a2c7d7c4074d7a2d032cf9bca9eeec3b749d78d2a8282cb7e2797b\
    \c12a6e8ddbe584091da83811f89117115a9c9a16aaeae36c7bf689a0e25e57d97\
    \d5cf1adf91c6b35bdc1daf244b7474864fa1b003e4f2f8a451d10013525c689ac\
    \a2db224c273078200d8185885825840007e90cc7f9c3966fca29b678c303c5cac\
    \d7a5e8d2b0a49fcbda46226ad375b65d99d9a287966421cc796e8590ccd2b3f7b\
    \c7ba6772f10ca5d48e4ad45c35e76584059330969346a435083bbc2df7c84a826\
    \7b34bb44d01ee87f70b0e8f37562414d88ff5b588c10c522e78ceebd99108c2aa\
    \3e0ac15940efe6637189eccf5e2db0a8200d81858858258405d3ffff5833ffdf7\
    \deaa6d0df930bd6e96f79074e5a639b0924079dd3266e4cd23282e426386f3b3b\
    \8608e83c8b057085aac7e729fa7722953746a3837b2da3c58402e80535ce39ab8\
    \a9310be52aba8d92f96089067b82088271f94a263e813e8bb504f51319c926c09\
    \c7911df2723f8736046553a051b29185609803554a7c7e0098200d81858858258\
    \405aec9001e5741167a93133ab51c4d43e8feed103a25d129213a91abb54096e6\
    \4fee1e72add16cec87984fe2e2218c8891f19895de8523dca4e2d818a036a8329\
    \584013ca954743179f7ecb1c4255c10c78ee59c1e0620fe88a1d03d03f919488b\
    \867a11b478e6c527a8c5b17d2e04a2f391c41e67c72a87d9eb6e90bf5ad30e7c0\
    \048200d81858858258409c8265c168c89b5ee50ee9e89aa80bfa93ac8386cc699\
    \f59f356f1731b9a2cdbf7cf7d837261da103b96178a7a9b7c610a9937ea0cc6b6\
    \a87b9a5e2de469151e58404498e473d854ed3be3a637040f73c9b8b1fb9deeb08\
    \84aae8dfa2c37b8a32a9e52c1c386dc041036e5aae7bb9df640b598a7bf978fb8\
    \5eccae7ca33f59fb0c008200d81858858258408bf5a5ca84baaf90c780ab4beea\
    \3119124bebebeb85a8aa416099395bb9d2a8f90b4747e68afa85a982bb4876eb8\
    \e7ca100b6f165f6aa6ff2887d042b87f99e55840e90037edefdf6e0e90d9db3c0\
    \40ca1e62742ab5b115eb0da73257c1ffa5465fff8588b1933a032902bc3e5c101\
    \b3b2303b6ac0061432b17238513f069713410d8200d8185885825840daa255b74\
    \77fe6dcc5a250c86c643223d3f6acff902396e691062c80b1e80d2dfcbe5ae322\
    \a22bacb1fe9dc980ef3906637520d9785d4fa0c32215d1d90ce4a85840a42b0e3\
    \7c0c441b9acc1f271cd8bb5d0511ba2f662ae21acc175cc75ceeb229397c66842\
    \15c4a2fe4f09bc62cdc012b8b03a0c3fa31401927c23286c6b44e2058200d8185\
    \8858258401435ec592be6059437379a5ef137bdd2cfb9606fd439e7973df5c6df\
    \955083f8b8e64085a68fca27406b23b12ae245c68b25cb623e4f59453658cddf8\
    \e79bbe45840af639752afb9e55e83bf5f84d89ba47a4125f276f8e22a40820ce1\
    \3f1b93e0287e283aeff36cd90819f70f13c913495b67be35f4cc0bf23171976c2\
    \7336229088200d8185885825840a954e9ae6343371585c518963f49616e00c0e7\
    \47911e5f663f1df23766a04938968459b2c4e2dffc46b4b8096c40dc8706a4f88\
    \c7f5184a9febf43ec88f383e65840eb883eba71d547758d9645be5d8b16ab9ca6\
    \79cef7949ecd787686873fa3e43bb1ebff938541cddd88e15d73b2d6375245d77\
    \c3f063f61cbef65dbdb65f2950f8200d818588582584099c980094af7f5a48e67\
    \9ab7035a3c0724cefe5d9856712ce5e50b1e9c574b0999086f441b85856b32987\
    \e70392b8664ce9607014d19dbb3e797b1f5b7c7d2835840005366b2f9a9e7d56b\
    \a70e5e13896ea9c04abbf4af0d23cf85a1f8ffed6b14beaf93c47bdbc10bbac57\
    \1bc08df551cd72a4c5196c819241d7b31590999f5fc098200d8185885825840be\
    \8f915d9fe21f1aef6de5eb06c7647d3620181e89d6054ceee3527d46ea813f5ff\
    \a25a64b41785f8aa9b4b667bcdff7ee35b434cabebb979225df612c7a38685840\
    \e72ab2bf79ae17b4e3b3742bc64ab4bc423a08330974eab01818654c734892304\
    \2ad262a3d874ad9887214d3548e2b532ff4fe6bbd5ec78eac3c5962e1394b0482\
    \00d81858858258402060e83a04b86755077b3f7c4a13d355ab14045167d1073c3\
    \34cb46c4a391a5ea20cb7f20dda78e77fa7e1670a869268fa8b1fc7533827c666\
    \189ffb95038d295840d92232bcf86f089e42a352b8ff2876b455afb5c58ade4c9\
    \b3100c69670f9fbbc31ba6a21ef721a54ea9cf5ae90e58a971eb7282b29ce82d9\
    \7b4907ddb3553c0f8200d81858858258402e02134c56a2cf1c59edd8472181a56\
    \e897ae71091f8d73236a3cebf1e566548f9a6713fb5516ff6c15e9aefebb006d0\
    \2d1a0e0f7ed81e11b8b68f1e358c8e9558405b205311607d0bec9df287715f060\
    \29ec8f11b71e8ad620a028bd925c50dac1505ff0443efb6e823159227e9dad4d0\
    \06dda5c2ed885ae1d68f9e220835d58707"

goldenTestnet__1_1 :: ByteString
goldenTestnet__1_1 =
    "82839f8200d81858248258203b40265111d8bb3c3c608d95b3a0bf83461ace32d7\
    \9336579a1939b3aad1c0b700ff9f8282d818582883581c946480eb45fa900e84d1\
    \f25141589468ca1d38bbb4e6c3abec7d1ea0a102451a4170cb17001a16bbe0ab18\
    \2affa0818200d81858858258408e620235b2a427259cc1a1c63af9ef179cb3b6e3\
    \96da182e01d65dd3ed7be94be25db39fb78e74d4b53fb51776d0f5eb360e62d09b\
    \853f3a87ac25bf834ee1fb584081b55eff4aa66b01c339ba2ec04dbad9d14bd049\
    \f6e8ae46211a2870c839966bfc721ca80c3b71d3719e468dcd16c877ff68a3cff8\
    \887ac9f3546da99e6bba00"

goldenTestnet__2_2 :: ByteString
goldenTestnet__2_2 =
    "82839f8200d81858248258203b40265111d8bb3c3c608d95b3a0bf83461ace32d7\
    \9336579a1939b3aad1c0b7008200d81858248258203b40265111d8bb3c3c608d95\
    \b3a0bf83461ace32d79336579a1939b3aad1c0b701ff9f8282d818582883581c94\
    \6480eb45fa900e84d1f25141589468ca1d38bbb4e6c3abec7d1ea0a102451a4170\
    \cb17001a16bbe0ab182a8282d818582883581c48a08edee8a9f586263d5f4e2288\
    \ac1a6e33656bfa2570fb8618c5c4a102451a4170cb17001ab30d9d270effa08282\
    \00d81858858258408e620235b2a427259cc1a1c63af9ef179cb3b6e396da182e01\
    \d65dd3ed7be94be25db39fb78e74d4b53fb51776d0f5eb360e62d09b853f3a87ac\
    \25bf834ee1fb5840fb812923b3b5a15a9d7005ef0b2ef714ed188c48f0ec16a7ef\
    \5e3661f92aebb203d3717fafbb10b4de6bf4fa6ad064b5ad65434a449983741182\
    \1af434d0280f8200d81858858258402cf36a269802da7cc0408308bda9a0015e7e\
    \81912117013c93edeefa9b62bbb61bbf1c6081545b1ab140578d7b5c035bf904d0\
    \5dd8e9b79b34d3160f86206bfc5840a57a996c2a1185f117db5e21aed47c54eccf\
    \263bf1e978114b1a2731086b4e8d189700c918b1ce04cd77316a15fb22f3a2cbed\
    \7de16fb4fc83d4cc4b11a3860b"


goldenTestnet__25_1 :: ByteString
goldenTestnet__25_1 =
    "82839f8200d81858248258203b40265111d8bb3c3c608d95b3a0bf83461ace32d7\
    \9336579a1939b3aad1c0b700ff9f8282d818582883581c946480eb45fa900e84d1\
    \f25141589468ca1d38bbb4e6c3abec7d1ea0a102451a4170cb17001a16bbe0ab0e\
    \8282d818582883581c946480eb45fa900e84d1f25141589468ca1d38bbb4e6c3ab\
    \ec7d1ea0a102451a4170cb17001a16bbe0ab0e8282d818582883581c946480eb45\
    \fa900e84d1f25141589468ca1d38bbb4e6c3abec7d1ea0a102451a4170cb17001a\
    \16bbe0ab0e8282d818582883581c946480eb45fa900e84d1f25141589468ca1d38\
    \bbb4e6c3abec7d1ea0a102451a4170cb17001a16bbe0ab0e8282d818582883581c\
    \946480eb45fa900e84d1f25141589468ca1d38bbb4e6c3abec7d1ea0a102451a41\
    \70cb17001a16bbe0ab0e8282d818582883581c946480eb45fa900e84d1f2514158\
    \9468ca1d38bbb4e6c3abec7d1ea0a102451a4170cb17001a16bbe0ab0e8282d818\
    \582883581c946480eb45fa900e84d1f25141589468ca1d38bbb4e6c3abec7d1ea0\
    \a102451a4170cb17001a16bbe0ab0e8282d818582883581c946480eb45fa900e84\
    \d1f25141589468ca1d38bbb4e6c3abec7d1ea0a102451a4170cb17001a16bbe0ab\
    \0e8282d818582883581c946480eb45fa900e84d1f25141589468ca1d38bbb4e6c3\
    \abec7d1ea0a102451a4170cb17001a16bbe0ab0e8282d818582883581c946480eb\
    \45fa900e84d1f25141589468ca1d38bbb4e6c3abec7d1ea0a102451a4170cb1700\
    \1a16bbe0ab0e8282d818582883581c946480eb45fa900e84d1f25141589468ca1d\
    \38bbb4e6c3abec7d1ea0a102451a4170cb17001a16bbe0ab0e8282d81858288358\
    \1c946480eb45fa900e84d1f25141589468ca1d38bbb4e6c3abec7d1ea0a102451a\
    \4170cb17001a16bbe0ab0e8282d818582883581c946480eb45fa900e84d1f25141\
    \589468ca1d38bbb4e6c3abec7d1ea0a102451a4170cb17001a16bbe0ab0e8282d8\
    \18582883581c946480eb45fa900e84d1f25141589468ca1d38bbb4e6c3abec7d1e\
    \a0a102451a4170cb17001a16bbe0ab0e8282d818582883581c946480eb45fa900e\
    \84d1f25141589468ca1d38bbb4e6c3abec7d1ea0a102451a4170cb17001a16bbe0\
    \ab0e8282d818582883581c946480eb45fa900e84d1f25141589468ca1d38bbb4e6\
    \c3abec7d1ea0a102451a4170cb17001a16bbe0ab0e8282d818582883581c946480\
    \eb45fa900e84d1f25141589468ca1d38bbb4e6c3abec7d1ea0a102451a4170cb17\
    \001a16bbe0ab0e8282d818582883581c946480eb45fa900e84d1f25141589468ca\
    \1d38bbb4e6c3abec7d1ea0a102451a4170cb17001a16bbe0ab0e8282d818582883\
    \581c946480eb45fa900e84d1f25141589468ca1d38bbb4e6c3abec7d1ea0a10245\
    \1a4170cb17001a16bbe0ab0e8282d818582883581c946480eb45fa900e84d1f251\
    \41589468ca1d38bbb4e6c3abec7d1ea0a102451a4170cb17001a16bbe0ab0e8282\
    \d818582883581c946480eb45fa900e84d1f25141589468ca1d38bbb4e6c3abec7d\
    \1ea0a102451a4170cb17001a16bbe0ab0e8282d818582883581c946480eb45fa90\
    \0e84d1f25141589468ca1d38bbb4e6c3abec7d1ea0a102451a4170cb17001a16bb\
    \e0ab0e8282d818582883581c946480eb45fa900e84d1f25141589468ca1d38bbb4\
    \e6c3abec7d1ea0a102451a4170cb17001a16bbe0ab0e8282d818582883581c9464\
    \80eb45fa900e84d1f25141589468ca1d38bbb4e6c3abec7d1ea0a102451a4170cb\
    \17001a16bbe0ab0e8282d818582883581c946480eb45fa900e84d1f25141589468\
    \ca1d38bbb4e6c3abec7d1ea0a102451a4170cb17001a16bbe0ab0effa0818200d8\
    \1858858258408e620235b2a427259cc1a1c63af9ef179cb3b6e396da182e01d65d\
    \d3ed7be94be25db39fb78e74d4b53fb51776d0f5eb360e62d09b853f3a87ac25bf\
    \834ee1fb5840e0f9591bfd7b67738db6b689d514bba1fe8c730491f9bdad7fbe05\
    \58974e57c1ef62c331da2ad528ff829b5406638d384a0b4d0b40b378f4063acb73\
    \851d3906"

goldenTestnet__1_25 :: ByteString
goldenTestnet__1_25 =
    "82839f8200d81858248258203b40265111d8bb3c3c608d95b3a0bf83461ace32d\
    \79336579a1939b3aad1c0b7008200d81858248258203b40265111d8bb3c3c608d\
    \95b3a0bf83461ace32d79336579a1939b3aad1c0b7018200d81858248258203b4\
    \0265111d8bb3c3c608d95b3a0bf83461ace32d79336579a1939b3aad1c0b70282\
    \00d81858248258203b40265111d8bb3c3c608d95b3a0bf83461ace32d79336579\
    \a1939b3aad1c0b7038200d81858248258203b40265111d8bb3c3c608d95b3a0bf\
    \83461ace32d79336579a1939b3aad1c0b7048200d81858248258203b40265111d\
    \8bb3c3c608d95b3a0bf83461ace32d79336579a1939b3aad1c0b7058200d81858\
    \248258203b40265111d8bb3c3c608d95b3a0bf83461ace32d79336579a1939b3a\
    \ad1c0b7068200d81858248258203b40265111d8bb3c3c608d95b3a0bf83461ace\
    \32d79336579a1939b3aad1c0b7078200d81858248258203b40265111d8bb3c3c6\
    \08d95b3a0bf83461ace32d79336579a1939b3aad1c0b7088200d8185824825820\
    \3b40265111d8bb3c3c608d95b3a0bf83461ace32d79336579a1939b3aad1c0b70\
    \98200d81858248258203b40265111d8bb3c3c608d95b3a0bf83461ace32d79336\
    \579a1939b3aad1c0b70a8200d81858248258203b40265111d8bb3c3c608d95b3a\
    \0bf83461ace32d79336579a1939b3aad1c0b70b8200d81858248258203b402651\
    \11d8bb3c3c608d95b3a0bf83461ace32d79336579a1939b3aad1c0b70c8200d81\
    \858248258203b40265111d8bb3c3c608d95b3a0bf83461ace32d79336579a1939\
    \b3aad1c0b70d8200d81858248258203b40265111d8bb3c3c608d95b3a0bf83461\
    \ace32d79336579a1939b3aad1c0b70e8200d81858248258203b40265111d8bb3c\
    \3c608d95b3a0bf83461ace32d79336579a1939b3aad1c0b70f8200d8185824825\
    \8203b40265111d8bb3c3c608d95b3a0bf83461ace32d79336579a1939b3aad1c0\
    \b7108200d81858248258203b40265111d8bb3c3c608d95b3a0bf83461ace32d79\
    \336579a1939b3aad1c0b7118200d81858248258203b40265111d8bb3c3c608d95\
    \b3a0bf83461ace32d79336579a1939b3aad1c0b7128200d81858248258203b402\
    \65111d8bb3c3c608d95b3a0bf83461ace32d79336579a1939b3aad1c0b7138200\
    \d81858248258203b40265111d8bb3c3c608d95b3a0bf83461ace32d79336579a1\
    \939b3aad1c0b7148200d81858248258203b40265111d8bb3c3c608d95b3a0bf83\
    \461ace32d79336579a1939b3aad1c0b7158200d81858248258203b40265111d8b\
    \b3c3c608d95b3a0bf83461ace32d79336579a1939b3aad1c0b7168200d8185824\
    \8258203b40265111d8bb3c3c608d95b3a0bf83461ace32d79336579a1939b3aad\
    \1c0b7178200d81858258258203b40265111d8bb3c3c608d95b3a0bf83461ace32\
    \d79336579a1939b3aad1c0b718188200d81858258258203b40265111d8bb3c3c6\
    \08d95b3a0bf83461ace32d79336579a1939b3aad1c0b71819ff9f8282d8185828\
    \83581c946480eb45fa900e84d1f25141589468ca1d38bbb4e6c3abec7d1ea0a10\
    \2451a4170cb17001a16bbe0ab0effa0981a8200d81858858258408e620235b2a4\
    \27259cc1a1c63af9ef179cb3b6e396da182e01d65dd3ed7be94be25db39fb78e7\
    \4d4b53fb51776d0f5eb360e62d09b853f3a87ac25bf834ee1fb5840d4d96fdda7\
    \84ecb321517ff5d73224420851c44c34177e2f19f642a638e9a08089127f27eb8\
    \8eb7e561d0516605d84af5a6174ffdd973f97bc652f159706070a8200d8185885\
    \8258402cf36a269802da7cc0408308bda9a0015e7e81912117013c93edeefa9b6\
    \2bbb61bbf1c6081545b1ab140578d7b5c035bf904d05dd8e9b79b34d3160f8620\
    \6bfc58400627b3393ddf4f8c27c3c9996c50d373439a1cc8eca69a03f475eccd1\
    \64790739cde814ba965e6d9053d36b08b33806f986ac07bbf4c61fdc4637a87d2\
    \502a098200d8185885825840d5ac2de87f73943e2af170a34be58db3997b9c5b6\
    \685e8cc4e33d3771798a0d5850235a0cafb25f5324da599db055ae743efd205b9\
    \9c91586dd8e7b625f045815840d73ae3f14540968e003ec7a83fa97660efb5087\
    \60591882804b5d67fe5e0209e4c624551640340126d9d88a0f5a917f388305c1a\
    \866b7402f24dee188bb2d00d8200d81858858258408a75ede1f39317d26fa8075\
    \5807a11ebd4b18beba63d6b7de2dae1d1b01f9bddc58090ed0fd50fa95d12a5c9\
    \3e29d1eca37019c3ec25890aba3bb77aa20be5105840e75eb79a6c4ff85f2e299\
    \ba8f57486f35fd889e256fa7d51ddee0ef438d3b2156d6db25c1312ac5e8383a9\
    \81a31d097c1062723c01aafa2645fbfa695bec8b028200d8185885825840353cb\
    \f7e535c6a74fc1aabe6a554650d87b9625da43dbe4a8c55bb81a95668862d15de\
    \a850a7b2d127b0ccd6e98f929e544ca4f5198cb92ed8fd1a342dfbba4a5840c1f\
    \3f88b64056e89920413ffa7ea6cdf00b24cb250e4577d1899b3ddf4b4e62ff6fb\
    \5b0b7fd02d27fae53e7673ecaebd1bb4eacef9a745681948d3fb3288b0008200d\
    \818588582584070e046565dc76736332f886d918d6fd78bdb21aaa9b60e54ffac\
    \fec921c6f49c01bb7822abaf3e9711c9ed5e5e0c2b293f7759caa483eab05651f\
    \df227e584e35840e1a5f87c9c579164cda9e35a9687c25f5f3d21394d47f13dee\
    \26082468030424d938b09107be9184f385310b86c513507ad09ac327692a3cb5f\
    \caa56315834088200d81858858258403907e9f5dd48253f86947f557e4b8a4178\
    \be4ba40268ebcbd3f377c35c75530a0f05dbace6b2b8363d4f7b71f7264369487\
    \e13d4ee634c9890c4554ea6ae2c7a5840a52ad2fdc5eb882bb82cac3a10667bd9\
    \da141980ec9bae951e4001f53642f845b94da39b01d054a378a4cde1ba8ada325\
    \a48ca321a90c7eb4f0cf568a737e40c8200d8185885825840110ad78e56e38da4\
    \2e37033710af91da5a76c58db9eccff83059b3b135d2c6692f9e995c2d73d5fb4\
    \edcdfa7f3fc737cc7a2debdff413b5b8336302a8b66592658406c7219f7792e40\
    \8869e57a5d5f40f75a5533c954ac2031e3cea8f13f243b174b028ba11e222fee1\
    \e454d18c0bb5cf35fa651b565b68eacfefc989dbfe03eac098200d81858858258\
    \4094b01b4ea67ff5457a30e599aa2a5695ba9e48b9e6e45fa694baafe3f204231\
    \be79f7d04c19be9ff153a79db0f5cb39cd2ad469ba62f65d26a05df49cdbfe857\
    \5840031ada77198b54b62c23c2ced8942a952f3ac95932db63f0f6d5408c124ee\
    \fb3a1b5babd6966cef95abbeb904a330500fdf56cc372443b78eb4b6444efb982\
    \0d8200d81858858258406baf92d3ad25656d677ba9f8b1d9f977d3781a26b8389\
    \9022b47fa290742fab11c1b1a743f6b8ece3e06212b41153c7cf0eff1521d26d0\
    \27af2c01c68c660b7f5840bb4cad9f0a4d239dc94ecc74fe3eb7f18988ec375fd\
    \9c712ba1a6c62e9e0527cb3c585dec267de3e4c52427900d23be97dae87eb2eb2\
    \67433a68c257221ebe0e8200d8185885825840bb022193d349a54cab81ffa12ad\
    \179e149f1e543b6f00802b820fd9abe48862c29f96692a0da19899f064e8752a8\
    \9f4468f5bef4a62de97179cd565c54c697d0584051d21bce67ec1d878e987db6d\
    \4bf34247109dce62cc9e98d96255710b995ce75f6b4ee85076109041681f5050b\
    \a01a9ed2eb1187ef1c3eae38c8284fe55af1048200d8185885825840b937f13c4\
    \58951d1d2308acab0ddc1233cfae660de288cbdeb262855ad3eb0d3cd75d2b096\
    \94b1bfdd307faa023e20523c7a103d7b09cf37a6a98abc5ce87ce858401dee085\
    \8c3188097a803da95c9bc5ee60bd08c43a7abc3b030d3ac3432b95990414d4c76\
    \9eb51a1bf60ee4b314e4c3f8e6f83b873b0d8141874ce016d4842c0e8200d8185\
    \885825840d79b342a6ab8f789a9c0e73f45d38d026f880b7df296dd01ab485cfe\
    \366053aea3ab23b04f2d31e1c40a4468dcd61c43cdc7d3fb332be7bbbf16dcb2b\
    \bc432115840cbe9ab8ea49c9973ac71e3f141db6d78fb29a39c4fe81844f6b97c\
    \10d5286ee7a3cd862b92552a0b5642ea0534145a47b2a6072a6887f17eaa6d8da\
    \237e851028200d8185885825840dcfc39775d9141b762a832e2f4e1b1de4a0b54\
    \c818ee2939216bdb8e5608970a2c7d7c4074d7a2d032cf9bca9eeec3b749d78d2\
    \a8282cb7e2797bc12a6e8ddbe58400832ed5b0da646c57b650c14a38321f45b9f\
    \b239eabe62ec0defeb796cbb2252a39945f1ff87c00af4e0355c5f45952b38677\
    \0b19d20b4109217e23febc8820d8200d8185885825840007e90cc7f9c3966fca2\
    \9b678c303c5cacd7a5e8d2b0a49fcbda46226ad375b65d99d9a287966421cc796\
    \e8590ccd2b3f7bc7ba6772f10ca5d48e4ad45c35e765840d1d1a9690347ec81d7\
    \971c3b70e81496d0b2226f94c9fc1c0b0a5d8fe9d7c34ad0ec3acebc37a3d717a\
    \30215df0c3d84315e2d0f2401891835c8c6586ed085098200d81858858258405d\
    \3ffff5833ffdf7deaa6d0df930bd6e96f79074e5a639b0924079dd3266e4cd232\
    \82e426386f3b3b8608e83c8b057085aac7e729fa7722953746a3837b2da3c5840\
    \392e4a0a6ef7dbf3795ead01bd06c060bc87f864e52fbe25a33215e7f11009713\
    \d705e60caa59dce91ca2332a34fa0c4a3e4d3fe182d98fa1c1ddb89e23ac30982\
    \00d81858858258405aec9001e5741167a93133ab51c4d43e8feed103a25d12921\
    \3a91abb54096e64fee1e72add16cec87984fe2e2218c8891f19895de8523dca4e\
    \2d818a036a832958401e5e4131f8a3bc3057a434711311fa7a3d21a53cc06afa4\
    \6f861bb922721c9635833d44c65e5a2bd3e4d7c9795235c7b76e4894aacf77edc\
    \95172248f446b6008200d81858858258409c8265c168c89b5ee50ee9e89aa80bf\
    \a93ac8386cc699f59f356f1731b9a2cdbf7cf7d837261da103b96178a7a9b7c61\
    \0a9937ea0cc6b6a87b9a5e2de469151e58402ee10ba0653fd7610653e14cb92df\
    \10a8b35f540d51fb5713a27e2d91009e58701ca62a2fb60beabefd45add063f6d\
    \45dd48616edad07decc1ed9a42112abc088200d81858858258408bf5a5ca84baa\
    \f90c780ab4beea3119124bebebeb85a8aa416099395bb9d2a8f90b4747e68afa8\
    \5a982bb4876eb8e7ca100b6f165f6aa6ff2887d042b87f99e55840f688b3e2513\
    \b910ccb7d58c7d5aac113d31451312be9eb8837e7ce2a18d4d7b0f8638eba3d3d\
    \dc6731363a54a4a1ba4736e88ea84bf12354b31f8beec298e4028200d81858858\
    \25840daa255b7477fe6dcc5a250c86c643223d3f6acff902396e691062c80b1e8\
    \0d2dfcbe5ae322a22bacb1fe9dc980ef3906637520d9785d4fa0c32215d1d90ce\
    \4a85840c1bc90d612d637094709e05591689e7faee2f0d26afe1875edeb13be61\
    \bbd4cbe14295754154109f095cdcc2fff9d25d31ffa7158df23002065e726071b\
    \45a058200d81858858258401435ec592be6059437379a5ef137bdd2cfb9606fd4\
    \39e7973df5c6df955083f8b8e64085a68fca27406b23b12ae245c68b25cb623e4\
    \f59453658cddf8e79bbe45840bf46abc4e377a7dee6f6ef615ce7a6916bdd98a4\
    \d2f28cd70e126e04a21fa13341785a610a46f429a5a00e65ecd4f9c1a7e62e662\
    \59f290fda1a39da2935fb0e8200d8185885825840a954e9ae6343371585c51896\
    \3f49616e00c0e747911e5f663f1df23766a04938968459b2c4e2dffc46b4b8096\
    \c40dc8706a4f88c7f5184a9febf43ec88f383e6584063f08a61ef2141fc5d1a16\
    \79fa633b4cbd794cb9a6fd1d70be3e7026a979a8f2d21355cba1435fe501f4f55\
    \da2604cee2d49b8b887f95e0c34c94789b8845b0b8200d818588582584099c980\
    \094af7f5a48e679ab7035a3c0724cefe5d9856712ce5e50b1e9c574b0999086f4\
    \41b85856b32987e70392b8664ce9607014d19dbb3e797b1f5b7c7d28358402055\
    \676117c5c3ae7f17b91faa27ddbbe7f5fd5c5606fa943ea605575bab7906dbbc3\
    \acd51b830cbed7a81e84bf22bdcaa0681ea27a7f698376a07f39f3591078200d8\
    \185885825840be8f915d9fe21f1aef6de5eb06c7647d3620181e89d6054ceee35\
    \27d46ea813f5ffa25a64b41785f8aa9b4b667bcdff7ee35b434cabebb979225df\
    \612c7a38685840878c7512614326917ab7a1b5b33c982b8471ba59916bd741369\
    \b155a5c5ced98b40ad15b2a6175411dc1e393837f156fa4adf0df4b5303662ca5\
    \670b4285690e8200d81858858258402060e83a04b86755077b3f7c4a13d355ab1\
    \4045167d1073c334cb46c4a391a5ea20cb7f20dda78e77fa7e1670a869268fa8b\
    \1fc7533827c666189ffb95038d2958402f77aa3e985a8d6e813c79c84eb10e520\
    \c65a93d80e1b07c91af5241198173141aa3b7e6f6c0dba1d3b112444b98b890b5\
    \fe7a499a7f01d479e7d39a9bf8200b8200d81858858258402e02134c56a2cf1c5\
    \9edd8472181a56e897ae71091f8d73236a3cebf1e566548f9a6713fb5516ff6c1\
    \5e9aefebb006d02d1a0e0f7ed81e11b8b68f1e358c8e9558402ca25b0147477da\
    \188472599009a7791671bccac2d7eda55f359dd6226a206f6227f3dc37c93b7f3\
    \675681d04fe85c1b3e522923b98b4dd29d1c629cef728a01"

{- NOTE: The above golden tests were obtained from 'cardano-sl@3.0.1', using the
    following code:

module Main where

--  build-depends:
--      base
--      bytestring
--      cardano-crypto
--      cardano-sl
--      cardano-sl-binary
--      cardano-sl-chain
--      cardano-sl-core
--      cardano-sl-crypto
--      cardano-wallet
--      containers
--      memory
--      universum

import           Universum

import           Cardano.Wallet.Kernel.Transactions (mkStdTx)

import           Pos.Binary.Class
import           Pos.Chain.Txp
import           Pos.Core.Common
import           Pos.Core.NetworkMagic
import           Pos.Crypto.Configuration
import           Pos.Crypto.Hashing
import           Pos.Crypto.Signing

import           System.IO.Unsafe (unsafePerformIO)

import qualified Cardano.Crypto.Wallet as CC
import qualified Data.ByteArray.Encoding as BA
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map

main :: IO ()
main = do
    genGoldenTest mainnet 1
        [ (genESK "addr-0", Coin 42) ]

    -- and so forth ...

-- | Generate an encrypted key from a seed, with an empty passphrase
genESK
    :: ByteString
    -> EncryptedSecretKey
genESK seed = unsafePerformIO $
    mkEncSecretUnsafe mempty (CC.generateNew seed pwd pwd)
  where
    pwd :: ByteString
    pwd = mempty

-- | Generate a golden test containing a signed transaction
genGoldenTest
    :: (NetworkMagic, ProtocolMagic)
        -- ^ Protocol parameters
    -> Int
        -- ^ Number of outputs
    -> [(EncryptedSecretKey, Coin)]
        -- ^ (Address Private Keys, Output value)
    -> IO ()
genGoldenTest (nm, pm) nOuts xprvs = do
    let addrs = first (makePubKeyAddressBoot nm . encToPublic) <$> xprvs
    let res = mkStdTx pm shuffler signer inps outs []
          where
            shuffler = return
            signer addr = maybe
                (Left ()) (\(esk,_) -> Right $ SafeSigner esk mempty) (Map.lookup addr m)
              where
                m = Map.fromList (zip (fst <$> addrs) xprvs)
            inps = NE.fromList $ mkInput <$> zip [0..] addrs
            outs = NE.fromList $ take nOuts $ mkOutput <$> (cycle addrs)
    case res of
        Left _ -> fail $ "genGoldenTest: failed to sign tx"
        Right tx -> do
            let bytes = CBOR.toLazyByteString $ encode tx
            B8.putStrLn $
                B8.pack (show nm)
                <> " " <> show nOuts <> " ouputs"
                <> " " <> show (length xprvs) <> " inputs"
            print $ (B8.unpack . addrToBase58 . fst) <$> addrs
            -- NOTE Dropping first 4 bytes of 'TxAux' wrapper, not actually
            -- present on chain.
            B8.putStrLn $ BS.drop 4 $ BA.convertToBase BA.Base16 $ BL.toStrict bytes
            B8.putStrLn ""
  where
    mkInput (ix, addr) =
        let
            txId = unsafeHash @String "arbitrary"
        in
            ( TxInUtxo txId ix
            , mkOutput addr
            )
    mkOutput (addr, c) =
        TxOutAux $ TxOut addr c
-}
