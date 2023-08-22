module Cardano.Wallet.Spec.Data.Mnemonic
    ( Mnemonic
    , toText
    , toWords
    , fromWords
    , unsafeFromList
    ) where

import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T

import Prelude hiding
    ( toText )

newtype Mnemonic = Mnemonic (NonEmpty Text)
    deriving stock (Show)
    deriving newtype (Eq, Ord)

toText :: Mnemonic -> Text
toText = T.intercalate "." . toList . toWords

toWords :: Mnemonic -> NonEmpty Text
toWords (Mnemonic mnemonicWords) = mnemonicWords

fromWords :: NonEmpty Text -> Mnemonic
fromWords = Mnemonic

unsafeFromList :: [Text] -> Mnemonic
unsafeFromList = fromWords . NE.fromList
