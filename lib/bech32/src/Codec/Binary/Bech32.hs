-- |
-- Copyright: © 2017 Marko Bencun, 2018-2019 IOHK
-- License: MIT
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
    , decode

      -- * Data Part
    , DataPart
    , dataPartFromBytes
    , dataPartToBytes

      -- * Human-Readable Part
    , HumanReadablePart
    , humanReadablePartFromBytes
    , humanReadablePartToBytes

    ) where

import Codec.Binary.Bech32.Internal

