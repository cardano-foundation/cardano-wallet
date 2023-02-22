{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{- |
 Copyright: © 2018-2022 IOHK
 License: Apache-2.0

Unit tests for `Cardano.Wallet.DB.Store.Wallets.Model`.
-}
module Cardano.Wallet.DB.Store.Wallets.ModelSpec
    ( spec
    ) where

import Prelude

import Cardano.Wallet.DB.Arbitrary
    ()
import Cardano.Wallet.DB.Sqlite.Schema
    ( TxMeta (..) )
import Cardano.Wallet.DB.Sqlite.Types
    ( TxId (..) )
import Control.Monad
    ( replicateM )
import Data.Delta
    ( apply )
import Data.DeltaMap
    ( DeltaMap (Adjust) )
import Test.Hspec
    ( Spec, describe, it )
import Test.QuickCheck
    ( Arbitrary, Gen, Property, arbitrary, property, (===) )

import qualified Cardano.Wallet.DB.Store.Meta.Model as TxMetas
import qualified Cardano.Wallet.DB.Store.Transactions.Model as Txs
import qualified Cardano.Wallet.DB.Store.Wallets.Model as TxWallets
import qualified Cardano.Wallet.Primitive.Types as W
import qualified Cardano.Wallet.Primitive.Types.Tx as W
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Test.QuickCheck as Q

spec :: Spec
spec = do
    describe "wallets-transactions" $ do
        it "rollback collects the right amount of garbage" $
            property prop_RollbackCollectsGarbage

{-----------------------------------------------------------------------------
    Properties
------------------------------------------------------------------------------}
prop_RollbackCollectsGarbage :: GenRollback -> Property
prop_RollbackCollectsGarbage = \GenRollback{wid,slot,history} ->
        rollbackA wid slot history
    === rollbackB wid slot history
  where
    rollbackMetas wid slot = apply (Adjust wid change)
      where
        change
            = TxMetas.Manipulate
            $ TxMetas.RollBackTxMetaHistory slot

    rollbackA wid slot (x, wmetas) =
        fst $ TxWallets.garbageCollectTxWalletsHistory
            ( x
            , TxWallets.garbageCollectEmptyWallets
                $ rollbackMetas wid slot wmetas
            )

    rollbackB wid slot (Txs.TxSet x, wmetas) =
        Txs.TxSet $ Map.withoutKeys x (Set.fromList deletions)
      where
        deletions = TxWallets.transactionsToDeleteOnRollback wid slot wmetas

{-----------------------------------------------------------------------------
    Generators
------------------------------------------------------------------------------}
instance Arbitrary GenRollback where
    arbitrary = genRollback

data GenRollback = GenRollback
    { wid :: W.WalletId
    , slot :: W.SlotNo
    , history :: TxWallets.TxWalletsHistory
    } deriving (Show)

genRollback :: Gen GenRollback
genRollback = do
    (wid, wid2) <- arbitrary
    slot <- genSlotNo
    wmetas <- fmap mkTxMetaHistory
        <$> genMapWithKeys [wid,wid2] genTxMeta
    tx <- mkTxRelation <$> arbitrary
    let txIdSet = TxWallets.walletsLinkedTransactions wmetas
        txSet = Map.fromSet (const tx) txIdSet
        history = (Txs.TxSet txSet, wmetas)
    pure GenRollback{wid,slot,history}
  where
    mkTxRelation :: W.Tx -> Txs.TxRelation
    mkTxRelation tx = snd $ Map.findMin m
      where
        Txs.TxSet m = Txs.mkTxSet [tx]

mkTxMetaHistory :: [TxMeta] -> TxMetas.TxMetaHistory
mkTxMetaHistory =
    TxMetas.TxMetaHistory . Map.fromList . fmap (\x -> (txMetaTxId x,x))

genTxMeta :: Gen TxMeta
genTxMeta = do
    meta <- arbitrary
    slot' <- genSlotNo
    pure $ meta{txMetaSlot=slot'}

instance Arbitrary TxMeta where
    arbitrary = TxMeta
        <$> (TxId <$> arbitrary)
        <*> arbitrary
        <*> arbitrary
        <*> pure W.Outgoing
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary

genSlotNo :: Gen W.SlotNo
genSlotNo = W.SlotNo <$> Q.choose (1,100)

-- | Generate a 'Map' from a small list of keys.
genMapWithKeys :: Ord key => [key] -> Gen v -> Gen (Map.Map key [v])
genMapWithKeys keys genValue = do
    n <- Q.getSize
    Map.unionsWith (<>) <$> replicateM n genOne
  where
    one x = [x]
    genOne = Map.singleton <$> Q.elements keys <*> (one <$> genValue)
