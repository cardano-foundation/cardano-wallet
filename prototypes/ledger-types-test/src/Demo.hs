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

    print $ collectNotInScope babbage
    print $ collectNotInScope elaborated
