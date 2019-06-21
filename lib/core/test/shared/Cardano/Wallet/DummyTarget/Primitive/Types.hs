{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Wallet.DummyTarget.Primitive.Types
    ( DummyTarget
    , Tx (..)
    , block0
    ) where

import Prelude

import Cardano.Crypto.Wallet
    ( unXPub )
import Cardano.Wallet.Primitive.AddressDerivation
    ( KeyToAddress (..), getKey )
import Cardano.Wallet.Primitive.AddressDiscovery
    ( SeqState (..) )
import Cardano.Wallet.Primitive.Types
    ( Address (..)
    , Block (..)
    , BlockHeader (..)
    , DecodeAddress (..)
    , DefineTx
    , EncodeAddress (..)
    , Hash (..)
    , SlotId (..)
    , TxIn (..)
    , TxOut (..)
    )
import Control.DeepSeq
    ( NFData )
import Data.Bifunctor
    ( bimap )
import Data.ByteArray.Encoding
    ( Base (Base16), convertFromBase, convertToBase )
import Data.Text.Class
    ( TextDecodingError (..) )
import GHC.Generics
    ( Generic )

import qualified Cardano.Wallet.Primitive.Types as W
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text.Encoding as T

data DummyTarget

data Tx = Tx
    { inputs :: ![TxIn]
    , outputs :: ![TxOut]
    } deriving (Show, Generic, Ord, Eq)

instance NFData Tx

instance KeyToAddress DummyTarget where
    keyToAddress = Address . unXPub . getKey

instance EncodeAddress DummyTarget where
    encodeAddress _ = T.decodeUtf8 . convertToBase Base16 . unAddress

instance DecodeAddress DummyTarget where
    decodeAddress _ = bimap decodingError Address
        . convertFromBase Base16
        . T.encodeUtf8
      where
        decodingError _ = TextDecodingError
            "Unable to decode Address: expected Base16 encoding"

deriving instance Eq (SeqState DummyTarget)

instance DefineTx DummyTarget where
    type Tx DummyTarget = Tx
    txId = Hash . B8.pack . show
    inputs = inputs
    outputs = outputs

block0 :: Block Tx
block0 = Block
    { header = BlockHeader
        { slotId = SlotId 0 0
        , prevBlockHash = Hash "genesis"
        }
    , transactions = []
    }
