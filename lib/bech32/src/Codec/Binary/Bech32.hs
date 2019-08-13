-- |
-- Copyright: © 2017 Marko Bencun, 2018-2019 IOHK
-- License: Apache-2.0
--
-- Implementation of the [Bech32]
-- (https://github.com/bitcoin/bips/blob/master/bip-0173.mediawiki)
-- address format.
--
-- From an original implementation by Marko Bencun:
--
-- [sipa/bech32](https://github.com/sipa/bech32/tree/bdc264f84014c234e908d72026b7b780122be11f/ref/haskell)

module Codec.Binary.Bech32
    (
      -- * Encoding & Decoding
      encode
    , encodeLenient
    , EncodingError (..)
    , decode
    , decodeLenient
    , DecodingError (..)

      -- * Data Part
    , DataPart
    , dataPartFromBytes
    , dataPartFromText
    , dataPartToBytes
    , dataPartToText

      -- * Human-Readable Part
    , HumanReadablePart
    , HumanReadablePartError (..)
    , humanReadablePartFromText
    , humanReadablePartToText
    ) where

import Codec.Binary.Bech32.Internal
