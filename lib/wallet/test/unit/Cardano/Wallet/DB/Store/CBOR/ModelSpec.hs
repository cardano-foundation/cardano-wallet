module Cardano.Wallet.DB.Store.CBOR.ModelSpec
    ( spec, genDeltas , genDeltasConstrained) where

import Prelude

import Cardano.Wallet.DB.Arbitrary
    ()
import Cardano.Wallet.DB.Sqlite.Types
    ( TxId (..) )
import Cardano.Wallet.DB.Store.CBOR.Model
    ( DeltaTxCBOR (..), TxCBORSet (..) )
import Control.Monad
    ( forM )
import Test.Hspec
    ( Spec )
import Test.QuickCheck
    ( Gen, arbitrary, elements, frequency, shuffle, sized, vectorOf )

import qualified Data.Map.Strict as Map

spec :: Spec
spec = pure ()

genDeltas
    :: TxCBORSet
    -- ^ submitted ones
    -> Gen (DeltaTxCBOR)
genDeltas  old = genDeltasConstrained old Nothing

genDeltasConstrained
    ::  TxCBORSet
    -- ^ submitted ones
    -> Maybe [TxId]
    -- ^ possible pool of txids
    -> Gen (DeltaTxCBOR)
genDeltasConstrained (TxCBORSet old) txids = frequency $
    [(1, sized $ \n -> do
        tids <- genTxIds n txids
        locals <- forM tids $ \txId' -> do
                txcbor  <- arbitrary
                pure (txId', txcbor)
        pure $ Append . TxCBORSet $ Map.fromList locals)
    ] <>
    [(3, DeleteTx <$> elements (Map.keys old) ) | not (null old)]

genTxIds :: Int -> Maybe [TxId] -> Gen [TxId]
genTxIds n Nothing = fmap TxId <$> vectorOf n arbitrary
genTxIds n (Just base) = take n <$> shuffle base
