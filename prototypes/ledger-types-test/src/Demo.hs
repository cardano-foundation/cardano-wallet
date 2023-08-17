module Demo where

import Prelude

import Elaborate
import Module
import Parser
import Typ

import qualified Data.Map as Map

{-----------------------------------------------------------------------------
    Demonstration
    of the current ledger types.
------------------------------------------------------------------------------}
main :: IO ()
main = do
    Just babbage <- parseLedgerTypes <$> readFile "data/Babbage.txt"
    Just elaborated <-
        parseLedgerTypes <$> readFile "data/BabbageTxOut.txt"

    let txout1 = moduleDeclarations babbage Map.! "TxOut"
        txout1' = resolveVars (moduleDeclarations babbage) txout1
        txout2 = moduleDeclarations elaborated Map.! "TxOut"
        txout2' = resolveVars (moduleDeclarations elaborated) txout2

    print $ txout2' `elaborates` txout1'
    print $ collectNotInScope babbage
    print $ collectNotInScope elaborated
