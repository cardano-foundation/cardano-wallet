module Cardano.Wallet.Read.Tx.Gen.TxParameters
    ( TxParameters (..)
    , Lovelace (..)
    , Address (..)
    , Index (..)
    , exampleTxParameters
    )
where

import Cardano.Wallet.Read.Tx.TxId
    ( TxId
    , txIdFromHash
    )
import Data.ByteString
    ( ByteString
    )
import Data.List.NonEmpty
    ( NonEmpty (..)
    )
import Data.Maybe
    ( fromJust
    )
import Numeric.Natural
    ( Natural
    )
import Prelude

import Cardano.Wallet.Read.Hash qualified as Hash
import Data.ByteString.Char8 qualified as B8

newtype Lovelace = Lovelace Integer
    deriving (Eq, Show)

data Address
    = ByronAddress ByteString
    | ShelleyAddress ByteString
    deriving (Eq, Show)

newtype Index = Index Natural
    deriving (Eq, Show)

data TxParameters = TxParameters
    { txInputs :: NonEmpty (Index, TxId)
    , txOutputs :: NonEmpty (Address, Lovelace)
    }
    deriving (Eq, Show)

exampleTxParameters :: TxParameters
exampleTxParameters =
    TxParameters
        { txInputs =
            (Index 0, txIdx 'a') :| [(Index 1, txIdx 'b')]
        , txOutputs =
            (ByronAddress "a", Lovelace 1) :| [(ShelleyAddress "b", Lovelace 2)]
        }

txIdx :: Char -> TxId
txIdx =
    txIdFromHash . fromJust . Hash.hashFromBytes . B8.pack . replicate 32
