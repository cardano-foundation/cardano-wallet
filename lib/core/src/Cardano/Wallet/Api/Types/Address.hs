{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Copyright: © 2018-2022 IOHK
-- License: Apache-2.0

module Cardano.Wallet.Api.Types.Address
    ( DecodeAddress (..)
    , DecodeStakeAddress (..)
    , EncodeAddress (..)
    , EncodeStakeAddress (..)
    )
    where

import Prelude

import Cardano.Wallet.Primitive.AddressDerivation
    ( NetworkDiscriminant (..) )
import Cardano.Wallet.Primitive.Types.Address
    ( Address (..) )
import Data.Text
    ( Text )
import Data.Text.Class
    ( TextDecodingError (..) )

import qualified Cardano.Wallet.Primitive.Types.RewardAccount as W

-- | An abstract class to allow encoding of addresses depending on the target
-- backend used.
class EncodeAddress (n :: NetworkDiscriminant) where
    encodeAddress :: Address -> Text

instance EncodeAddress 'Mainnet => EncodeAddress ('Staging pm) where
    encodeAddress = encodeAddress @'Mainnet

-- | An abstract class to allow decoding of addresses depending on the target
-- backend used.
class DecodeAddress (n :: NetworkDiscriminant) where
    decodeAddress :: Text -> Either TextDecodingError Address

instance DecodeAddress 'Mainnet => DecodeAddress ('Staging pm) where
    decodeAddress = decodeAddress @'Mainnet

class EncodeStakeAddress (n :: NetworkDiscriminant) where
    encodeStakeAddress :: W.RewardAccount -> Text

instance EncodeStakeAddress 'Mainnet => EncodeStakeAddress ('Staging pm) where
    encodeStakeAddress = encodeStakeAddress @'Mainnet

class DecodeStakeAddress (n :: NetworkDiscriminant) where
    decodeStakeAddress :: Text -> Either TextDecodingError W.RewardAccount

instance DecodeStakeAddress 'Mainnet => DecodeStakeAddress ('Staging pm) where
    decodeStakeAddress = decodeStakeAddress @'Mainnet
