module Cardano.Wallet.Primitive.Types.Address.Gen
    ( genAddress
    , shrinkAddress
    )
    where

import Prelude

import Cardano.Wallet.Primitive.Types.Address
    ( Address (..) )
import Test.QuickCheck
    ( Gen, elements, sized )

import qualified Data.ByteString.Char8 as B8

--------------------------------------------------------------------------------
-- Addresses generated according to the size parameter
--------------------------------------------------------------------------------

genAddress :: Gen (Address)
genAddress = sized $ \size -> elements $ take (max 1 size) addresses

shrinkAddress :: Address -> [Address]
shrinkAddress a
    | a == simplest = []
    | otherwise = [simplest]
  where
    simplest = head addresses

addresses :: [Address]
addresses = mkAddress <$> ['0' ..]

--------------------------------------------------------------------------------
-- Internal utilities
--------------------------------------------------------------------------------

mkAddress :: Char -> Address
mkAddress c = Address $ "ADDR" `B8.snoc` c
