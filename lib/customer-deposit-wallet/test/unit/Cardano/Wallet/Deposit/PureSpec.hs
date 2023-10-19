module Cardano.Wallet.Deposit.PureSpec
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Primitive.Types.Tx.TxSeq
    ( toTxList )
import Cardano.Wallet.Primitive.Types.Tx.TxSeq.Gen
    ( genTxSeq, getTxSeq )
import Cardano.Wallet.Primitive.Types.UTxO.Gen
    ( genUTxO )
import Data.Maybe
    ( isJust )
import Test.Hspec
    ( Spec, describe, it )
import Test.QuickCheck
    ( Gen, Property, cover, forAll, property, suchThat )

import qualified Cardano.Wallet.Deposit.Pure as Wallet
import qualified Cardano.Wallet.Deposit.Read as Read

spec :: Spec
spec = do
    describe "rollForwardOne" $
        it "adds initial UTxO"
            prop_rollForwardOne_UTxO

{-----------------------------------------------------------------------------
    Properties
------------------------------------------------------------------------------}
prop_rollForwardOne_UTxO
    :: Property
prop_rollForwardOne_UTxO =
    forAll genBlock $ \block ->
        -- The wallet has a nonzero balance most of the time
        -- FIXME: Should have all the time?
        cover 50 (hasFunds $ Wallet.rollForwardOne block w0)
            "has balance" (property True)
  where
    w0 = Wallet.fromGenesisUTxO mempty
    hasFunds w1 = mempty /= Wallet.availableBalance w1

hasOutputs :: Read.Tx -> Bool
hasOutputs tx =
    if Read.txScriptInvalid tx
        then isJust $ Read.collateralOutput tx
        else not . null $ Read.outputs tx

haveSomeOutputs :: Read.Block -> Bool
haveSomeOutputs = any hasOutputs . Read.transactions

genBlock :: Gen Read.Block
genBlock =
    (mkBlock <$> genTxs) `suchThat` haveSomeOutputs
  where
    genTxs = toTxList . getTxSeq <$> genTxSeq genUTxO genAddress
    mkBlock transactions =
        Read.Block
            { Read.blockHeader = Read.dummyBHeader
            , Read.transactions = transactions
            }

genAddress :: Gen Read.Address
genAddress = pure Read.dummyAddress
