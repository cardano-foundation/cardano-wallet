{-# LANGUAGE QuasiQuotes #-}

module Cardano.Wallet.UI.Lib.Address
where

import Prelude

import Cardano.Wallet.Deposit.Read
    ( Address
    )
import Cardano.Wallet.Read.Address
    ( toShortByteString
    )
import Codec.Binary.Bech32
    ( dataPartFromBytes
    )
import Data.Text
    ( Text
    )

import qualified Codec.Binary.Bech32 as Bech32
import qualified Codec.Binary.Bech32.TH as Bech32
import qualified Data.ByteString.Short as B8

encodeMainnetAddress :: Address -> Text
encodeMainnetAddress addr = bech32
  where
    bytes = B8.fromShort $ toShortByteString addr
    bech32 = Bech32.encodeLenient hrp (dataPartFromBytes bytes)
    hrp = [Bech32.humanReadablePart|addr|]
