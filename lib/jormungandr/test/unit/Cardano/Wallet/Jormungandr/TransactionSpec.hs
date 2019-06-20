{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.Jormungandr.TransactionSpec
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Jormungandr.Compatibility
    ( Jormungandr )
import Cardano.Wallet.Jormungandr.Environment
    ( KnownNetwork (..), Network (..) )
import Cardano.Wallet.Jormungandr.Transaction
    ( newTransactionLayer )
import Cardano.Wallet.Primitive.CoinSelection
    ( CoinSelection (..) )
import Cardano.Wallet.Primitive.CoinSelection.LargestFirst
    ( largestFirst )
import Cardano.Wallet.Primitive.Types
    ( Address (..)
    , Coin (..)
    , Hash (..)
    , ShowFmt (..)
    , TxIn (..)
    , TxOut (..)
    , UTxO (..)
    )
import Cardano.Wallet.Transaction
    ( TransactionLayer (..) )
import Control.Monad.Trans.Except
    ( runExceptT )
import Data.Functor.Identity
    ( Identity (runIdentity) )
import Data.List.NonEmpty
    ( NonEmpty )
import Data.Proxy
    ( Proxy (..) )
import Data.Quantity
    ( Quantity (..) )
import Test.Hspec
    ( Spec, describe, it )
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
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map

spec :: Spec
spec = do
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
