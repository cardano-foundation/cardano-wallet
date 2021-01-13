module Cardano.Wallet.Primitive.Types.Address.Gen
    ( genAddressSmallRange
    , shrinkAddressSmallRange
    )
    where

import Prelude

import Cardano.Wallet.Primitive.Types.Address
    ( Address (..) )
import Test.QuickCheck
    ( Gen, elements )

import qualified Data.ByteString.Char8 as B8

--------------------------------------------------------------------------------
-- Addresses chosen from a small range (to allow collisions)
--------------------------------------------------------------------------------

genAddressSmallRange :: Gen (Address)
genAddressSmallRange = elements addresses

shrinkAddressSmallRange :: Address -> [Address]
shrinkAddressSmallRange a = filter (< a) addresses

addresses :: [Address]
addresses = mkAddress <$> ['0' .. '7']

--------------------------------------------------------------------------------
-- Internal utilities
--------------------------------------------------------------------------------

mkAddress :: Char -> Address
mkAddress c = Address $ "ADDR" `B8.snoc` c
