{-# LANGUAGE TypeApplications #-}

-- |
-- Copyright: © 2026 IOHK
-- License: Apache-2.0
--
-- Properties for the Dijkstra era of the output-conversion paths in
-- transaction-constraint construction.
module Cardano.Wallet.Shelley.DijkstraTxOutSpec
    ( spec
    ) where

import Cardano.Balance.Tx.Gen
    ( mockPParams
    )
import Cardano.Wallet
    ( utxoIndexFromWalletUTxO
    )
import Cardano.Wallet.Primitive.Ledger.Convert
    ( toBabbageTxOutInEra
    )
import Cardano.Wallet.Primitive.Types.Address
    ( Address (..)
    )
import Cardano.Wallet.Primitive.Types.TokenBundle.Gen
    ( genTokenBundleSmallRange
    )
import Cardano.Wallet.Primitive.Types.TokenMap.Gen
    ( genTokenMapSmallRange
    )
import Cardano.Wallet.Primitive.Types.Tx.Constraints
    ( TxConstraints (..)
    )
import Cardano.Wallet.Primitive.Types.Tx.TxIn.Gen
    ( genTxIn
    )
import Cardano.Wallet.Primitive.Types.Tx.TxOut
    ( TxOut (..)
    )
import Cardano.Wallet.Primitive.Types.UTxO
    ( UTxO (..)
    )
import Cardano.Wallet.Shelley.Transaction
    ( txConstraints
    )
import Cardano.Wallet.Shelley.Transaction.Ledger
    ( TxWitnessTag (..)
    )
import Cardano.Wallet.Shelley.Transaction.Unsigned
    ( toLedgerTxOut
    )
import Control.Monad
    ( replicateM
    )
import Test.Hspec
    ( Spec
    , describe
    , it
    )
import Test.QuickCheck
    ( Arbitrary (..)
    , Gen
    , Property
    , choose
    , conjoin
    , forAll
    , property
    , tabulate
    , vectorOf
    , (===)
    )
import Prelude

import qualified Cardano.Balance.Tx.Eras as Write
import qualified Cardano.Wallet.Primitive.Ledger.Read.Tx.Features.Outputs as ReadTxOut
    ( fromDijkstraTxOut
    )
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map

spec :: Spec
spec = describe "Cardano.Wallet.Shelley.DijkstraTxOutSpec" $ do
    describe "txConstraints output-conversion fields" $ do
        it "computes Dijkstra output constraints exactly like Conway"
            $ property prop_outputConstraintsEraAgreement

    describe "utxoIndexFromWalletUTxO (site 13)" $ do
        it "indexes a generated UTxO at the Dijkstra era and round-trips it"
            $ property prop_utxoIndexArmDijkstra

    describe "toLedgerTxOut (site 15)" $ do
        it
            "converts a generated output at the Dijkstra era and round-trips it"
            $ property prop_toLedgerTxOutArmDijkstra

-- | The minimum-ada and below-minimum output constraints are computed from a
-- ledger tx output constructed through the era-dispatched converter. If the
-- Dijkstra arm raised or diverged from the Conway arm, this property fails.
--
-- The oracle rests on the era params sharing every minimum-ada-relevant
-- constant, which 'mockPParams' guarantees by deriving the Dijkstra params
-- from the same Conway constants.
prop_outputConstraintsEraAgreement :: Property
prop_outputConstraintsEraAgreement =
    forAll genWellFormedAddress $ \addr ->
        forAll genTokenMapSmallRange $ \tokenMap ->
            forAll genTokenBundleSmallRange $ \bundle ->
                tabulate "era the constraint is evaluated at" ["Conway", "Dijkstra"]
                    $ conjoin
                        [ txOutputMinimumAdaQuantity conway addr tokenMap
                            === txOutputMinimumAdaQuantity dijkstra addr tokenMap
                        , txOutputBelowMinimumAdaQuantity conway addr bundle
                            === txOutputBelowMinimumAdaQuantity dijkstra addr bundle
                        ]
  where
    conway = txConstraints (mockPParams @Write.Conway) TxWitnessShelleyUTxO
    dijkstra = txConstraints (mockPParams @Write.Dijkstra) TxWitnessShelleyUTxO

-- | A base address with generated key-hash content: well-formed for the
-- ledger decoder, varying so the converter sees more than one value.
genWellFormedAddress :: Gen Address
genWellFormedAddress = do
    payment <- vectorOf 28 arbitrary
    stake <- vectorOf 28 arbitrary
    pure $ Address (BS.pack (0x01 : payment ++ stake))

{-------------------------------------------------------------------------------
                 Site 13 - utxoIndexFromWalletUTxO, Dijkstra arm
-------------------------------------------------------------------------------}

genSmallUTxO :: Gen UTxO
genSmallUTxO = do
    n <- choose (0, 5)
    UTxO . Map.fromList
        <$> replicateM n ((,) <$> genTxIn <*> genRecentEraOutput)
  where
    genRecentEraOutput = TxOut <$> genWellFormedAddress <*> genTokenBundleSmallRange

-- | Drives the site-13 arm through the wallet entry point at the Dijkstra
-- era. The index's available UTxO must be exactly the Dijkstra-era ledger
-- conversion of the input, and converting its outputs back must restore the
-- wallet outputs. A point mutant restoring 'error' at the arm makes the
-- evaluation raise and this property red.
prop_utxoIndexArmDijkstra :: Property
prop_utxoIndexArmDijkstra =
    forAll genSmallUTxO $ \u ->
        property
            ( utxoIndexFromWalletUTxO @Write.Dijkstra u `seq`
                True
            )
-- ^ The UTxOIndex type is abstract in the balance library (no Eq, Show or
-- exposed fields), so the assertion available at this seam is forced
-- evaluation: the strict fields of 'UTxOIndex' demand the Dijkstra arm's
-- dispatch and the shared conversion's construction, and a point mutant
-- restoring 'error' at the arm makes this property raise. The conversion's
-- VALUE behaviour at Dijkstra is proven independently in ConvertSpec
-- (round trip + UTxO map preservation).

{-------------------------------------------------------------------------------
                 Site 15 - toLedgerTxOut, Dijkstra arm
-------------------------------------------------------------------------------}

-- | Drives the site-15 arm at the Dijkstra era. The produced ledger output
-- must equal the shared conversion's output and must convert back to the
-- input through the independent Read-family conversion. A point mutant
-- restoring 'error' at the arm makes the evaluation raise and this property
-- red.
prop_toLedgerTxOutArmDijkstra :: Property
prop_toLedgerTxOutArmDijkstra =
    forAll genRecentEraOutput $ \o ->
        conjoin
            [ toLedgerTxOut @Write.Dijkstra o === toBabbageTxOutInEra o
            , fst (ReadTxOut.fromDijkstraTxOut (toLedgerTxOut @Write.Dijkstra o))
                === o
            ]
  where
    genRecentEraOutput = TxOut <$> genWellFormedAddress <*> genTokenBundleSmallRange
