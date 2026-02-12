module Cryptography.Core
    ( CryptoError (..)
    , CryptoFailable (..)
    , eitherCryptoError
    , MonadRandom (..)
    , genSalt
    ) where

import Crypto.Error
    ( CryptoError (..)
    , CryptoFailable (..)
    , eitherCryptoError
    )
import Crypto.Random.Types
    ( MonadRandom (..)
    )
import Data.ByteString
    ( ByteString
    )
import Prelude

genSalt :: MonadRandom m => Int -> m ByteString
genSalt = getRandomBytes
