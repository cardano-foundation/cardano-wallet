module Cardano.Wallet.Spec.Data.Mnemonic
    ( Mnemonic
    , toWords
    , fromWords
    , unsafeFromList
    ) where

import qualified Data.List.NonEmpty as NE

newtype Mnemonic = Mnemonic (NonEmpty Text)
    deriving stock (Show)
    deriving newtype (Eq, Ord)

toWords :: Mnemonic -> NonEmpty Text
toWords (Mnemonic mnemonicWords) = mnemonicWords

fromWords :: NonEmpty Text -> Mnemonic
fromWords = Mnemonic

unsafeFromList :: [Text] -> Mnemonic
unsafeFromList = fromWords . NE.fromList
