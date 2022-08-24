module Cardano.Wallet.DB.Store.CBOR.ModelSpec
    ( spec, genDeltas , genDeltasConstrained) where

import Prelude

import Cardano.Wallet.DB.Arbitrary
    ()
import Cardano.Wallet.DB.Sqlite.Types
    ( TxId (..) )
import Cardano.Wallet.DB.Store.CBOR.Model
    ( DeltaTxCBOR (..), TxCBORHistory (..) )
import Control.Monad
    ( forM )
import Test.Hspec
    ( Spec )
import Test.QuickCheck
    ( Gen, arbitrary, frequency, shuffle, sized, sublistOf, vectorOf )

import qualified Data.Map.Strict as Map

spec :: Spec
spec = pure ()

genDeltas
    :: TxCBORHistory
    -- ^ submitted ones
    -> Gen (DeltaTxCBOR)
genDeltas  old = genDeltasConstrained old Nothing

genDeltasConstrained
    ::  TxCBORHistory
    -- ^ submitted ones
    -> Maybe [TxId]
    -- ^ possible pool of txids
    -> Gen (DeltaTxCBOR)
genDeltasConstrained (TxCBORHistory old) txids = frequency $
    [(1, sized $ \n -> do
        tids <- genTxIds n txids
        locals <- forM tids $ \txId'@(TxId txId) -> do
                txcbor  <- arbitrary
                pure (txId', txcbor)
        pure $ Expand . TxCBORHistory $ Map.fromList locals)
    ] <>
    [(2, Prune <$> sublistOf (Map.keys old) ) | not (null old)]

genTxIds :: Int -> Maybe [TxId] -> Gen [TxId]
genTxIds n Nothing = fmap TxId <$> vectorOf n arbitrary
genTxIds n (Just base) = take n <$> shuffle base
