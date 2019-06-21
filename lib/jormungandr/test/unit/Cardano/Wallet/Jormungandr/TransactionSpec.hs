{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.Jormungandr.TransactionSpec
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Jormungandr.Binary
    ( Message (Transaction), getMessage, putMessage, runGet, runPut )
import Cardano.Wallet.Jormungandr.Compatibility
    ( Jormungandr )
import Cardano.Wallet.Jormungandr.Environment
    ( KnownNetwork (..), Network (..) )
import Cardano.Wallet.Jormungandr.Primitive.Types
    ( Tx (..) )
import Cardano.Wallet.Jormungandr.Transaction
    ( newTransactionLayer )
import Cardano.Wallet.Primitive.AddressDerivation
    ( ChangeChain (..)
    , Passphrase (..)
    , deriveAccountPrivateKey
    , deriveAddressPrivateKey
    , generateKeyFromSeed
    , keyToAddress
    , publicKey
    , serializeXPrv
    )
import Cardano.Wallet.Primitive.CoinSelection
    ( CoinSelection (..) )
import Cardano.Wallet.Primitive.CoinSelection.LargestFirst
    ( largestFirst )
import Cardano.Wallet.Primitive.Mnemonic
    ( entropyToBytes, mkMnemonic, mnemonicToEntropy )
import Cardano.Wallet.Primitive.Types
    ( Address (..)
    , Coin (..)
    , EncodeAddress (..)
    , Hash (..)
    , ShowFmt (..)
    , TxIn (..)
    , TxOut (..)
    , UTxO (..)
    , txId
    )
import Cardano.Wallet.Transaction
    ( TransactionLayer (..) )
import Control.Arrow
    ( second )
import Control.Monad.Trans.Except
    ( runExceptT )
import Data.ByteArray.Encoding
    ( Base (Base16), convertFromBase, convertToBase )
import Data.ByteString
    ( ByteString )
import Data.Functor.Identity
    ( Identity (runIdentity) )
import Data.List.NonEmpty
    ( NonEmpty )
import Data.Proxy
    ( Proxy (..) )
import Data.Quantity
    ( Quantity (..) )
import Data.Text
    ( Text )
import Test.Hspec
    ( Spec, describe, it, shouldBe )
import Test.QuickCheck
    ( Arbitrary (..)
    , Gen
    , Property
    , choose
    , property
    , scale
    , vectorOf
    , withMaxSuccess
    , (===)
    )

import qualified Cardano.Wallet.Primitive.CoinSelection as CS
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

spec :: Spec
spec = do
    estimateSizeSpec
    mkStdTxSpec

estimateSizeSpec :: Spec
estimateSizeSpec = do
    describe "estimateSize" $ do
        it "Estimated size is zero"
            (withMaxSuccess 2500 $ property $ propSizeEstimation $ Proxy @'Mainnet)
        it "Estimated size is zero"
            (withMaxSuccess 2500 $ property $ propSizeEstimation $ Proxy @'Testnet)

{-------------------------------------------------------------------------------
                                Size Estimation
-------------------------------------------------------------------------------}

propSizeEstimation
    :: forall n. (KnownNetwork n)
    => Proxy n
    -> (ShowFmt CoinSelection)
    -> Property
propSizeEstimation _ (ShowFmt sel) =
    let
        blockHash =
            Hash {getHash = "\216OY\rX\199\234\188.<O\\\244Y\211\210\254\224`i\216\DC3\167\132\139\154\216\161T\174\247\155"}
        tl = newTransactionLayer blockHash :: TransactionLayer (Jormungandr n)
        calcSize = estimateSize tl sel
    in calcSize === Quantity 0

instance Arbitrary CoinSelection where
    shrink sel@(CoinSelection inps outs chgs) = case (inps, outs, chgs) of
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
    arbitrary = do
        outs <- choose (1, 10)
            >>= \n -> vectorOf n arbitrary
            >>= genTxOut
        genSelection (NE.fromList outs)

deriving instance Arbitrary a => Arbitrary (ShowFmt a)

-- Check whether a selection is valid
isValidSelection :: CoinSelection -> Bool
isValidSelection (CoinSelection i o c) =
    let
        oAmt = sum $ map (fromIntegral . getCoin . coin) o
        cAmt = sum $ map (fromIntegral . getCoin) c
        iAmt = sum $ map (fromIntegral . getCoin . coin . snd) i
    in
        (iAmt :: Integer) >= (oAmt + cAmt)

genTxOut :: [Coin] -> Gen [TxOut]
genTxOut coins = do
    let n = length coins
    outs <- vectorOf n arbitrary
    return $ zipWith TxOut outs coins

genSelection :: NonEmpty TxOut -> Gen CoinSelection
genSelection outs = do
    let opts = CS.CoinSelectionOptions 100
    utxo <- vectorOf (NE.length outs * 3) arbitrary >>= genUTxO
    case runIdentity $ runExceptT $ largestFirst opts outs utxo of
        Left _ -> genSelection outs
        Right (s,_) -> return s

genUTxO :: [Coin] -> Gen UTxO
genUTxO coins = do
    let n = length coins
    inps <- vectorOf n arbitrary
    outs <- genTxOut coins
    return $ UTxO $ Map.fromList $ zip inps outs

instance Arbitrary Coin where
    shrink (Coin c) = Coin <$> shrink (fromIntegral c)
    arbitrary = Coin <$> choose (1, 200000)

instance Arbitrary Address where
    shrink _ = []
    arbitrary =
        pure $ Address "\131\&3$\195xi\193\"h\154\&5\145}\245:O\"\148\163\165/h^\ENQ\245\248\229;\135\231\234E/"

instance Arbitrary TxIn where
    shrink _ = []
    arbitrary = TxIn
        <$> arbitrary
        <*> scale (`mod` 3) arbitrary -- No need for a high indexes

instance Arbitrary (Hash "Tx") where
    shrink _ = []
    arbitrary = do
        bytes <- BS.pack <$> vectorOf 32 arbitrary
        pure $ Hash bytes

{-------------------------------------------------------------------------------
                               mkStdTx
-------------------------------------------------------------------------------}


mkStdTxSpec :: Spec
mkStdTxSpec = do
    describe "mkStdTx" $ do
        it "should work" $
            let
                mw =
                    either (error . show) id $ mkMnemonic @15 ["tattoo","potato","foil","mutual","slab","path","forward","pencil","suit","marble","hill","meat","close","garden","bird"]
                (seed, pwd) =
                    (Passphrase $ entropyToBytes $ mnemonicToEntropy mw, mempty)
                rootXPrv =
                    generateKeyFromSeed (seed, mempty) pwd
                accXPrv =
                    deriveAccountPrivateKey pwd rootXPrv minBound
                addrXPrv =
                    deriveAddressPrivateKey pwd accXPrv ExternalChain minBound
                chngXPrv =
                    deriveAddressPrivateKey pwd accXPrv InternalChain minBound
                addr =
                    keyToAddress @(Jormungandr 'Testnet) (publicKey addrXPrv)
                chngAddr =
                    keyToAddress @(Jormungandr 'Testnet) (publicKey chngXPrv)
                keystore = \case
                    x | x == addr -> Just (addrXPrv, pwd)
                      | x == chngAddr -> Just (chngXPrv, pwd)
                      | otherwise -> Nothing
            in do

                let block0TxId = Hash "fi\132\222\196\188\SI\241\136\139\233{\254\ACK\148\169k5\197\141\STXT\ENQ\234\213\GS\\\199*0\EM\244"

                TIO.putStrLn $ "       Addr: " <> toBech32 addr
                TIO.putStrLn $ "Change Addr: " <> toBech32 chngAddr
                TIO.putStrLn $ "     Block0: " <> (toHex . getHash $ block0Hash)
                TIO.putStrLn $ " block0txId: " <> (toHex . getHash $ block0TxId)
                let h = fst . serializeXPrv $ (addrXPrv, Hash "")
                TIO.putStrLn $ "len adrXPrv: " <> (T.pack . show $ BS.length h)
                TIO.putStrLn $ " a  ddrXPrv: " <> T.pack (B8.unpack $ BS.take 128 h)

                let tl = newTransactionLayer @'Testnet block0Hash

                -- Same address
                let ownedIns = [((TxIn block0TxId 1), TxOut addr $ Coin 1000000000000)]
                let outs = [TxOut addr $ Coin 1000000000000 ]

                let tx = Tx (map (second coin) ownedIns) outs
                let tid = txId @(Jormungandr 'Testnet) tx

                TIO.putStrLn $ "       txId: " <> (toHex . getHash $ tid)

                let (Right stx) = mkStdTx tl keystore ownedIns outs
                let msg = (BL.toStrict . runPut $ putMessage $ Transaction stx)
                --TIO.putStrLn $ "=> signedtx: " <> msg

                print stx

                msg `shouldBe` (fromHex "009602010101000000e8d4a51000666984dec4bc0ff1888be97bfe0694a96b35c58d025405ead51d5cc72a3019f483054bba3fcb7b5f2491861b6e2b8bf0a3f9257cdf2fdf41ca5af7072e77c70f3a000000e8d4a510000198e9d483b1228f0d92ca57da3948d981ddaccfdfca1698effbadac5108424158df7b7ad8c2ecc2f6acb7f83315ec04f15c6282824ef8a3d2924acc954accc106")
                let ss = BL.fromStrict (fromHex "009602010101000000e8d4a51000666984dec4bc0ff1888be97bfe0694a96b35c58d025405ead51d5cc72a3019f483054bba3fcb7b5f2491861b6e2b8bf0a3f9257cdf2fdf41ca5af7072e77c70f3a000000e8d4a510000198e9d483b1228f0d92ca57da3948d981ddaccfdfca1698effbadac5108424158df7b7ad8c2ecc2f6acb7f83315ec04f15c6282824ef8a3d2924acc954accc106")
                runGet getMessage ss `shouldBe` (Transaction stx)

block0Hash :: Hash "Block0Hash"
block0Hash = Hash "\DC3\195\216\&5\197:\EM\143|\133\DC3\176M\153\238\178<t\\\ns6L/\SO\128/\163\142\236\157\186"

toHex :: ByteString -> Text
toHex =  T.pack . B8.unpack . convertToBase Base16

fromHex :: Text -> ByteString
fromHex = either (error . show) id
    . convertFromBase Base16
    . B8.pack
    . T.unpack

toBech32 :: Address -> Text
toBech32 = encodeAddress (Proxy @(Jormungandr 'Testnet))
