module Cardano.Mnemonic.Extended
  ( module Reexport
  , someMnemonicToWords
  , someMnemonicToSentence
  ) where

import Cardano.Mnemonic as Reexport
import Prelude

import qualified Data.List.NonEmpty as NE

import Data.Foldable
    ( fold
    )
import Data.List.NonEmpty
    ( NonEmpty
    )
import Data.Text
    ( Text
    )

someMnemonicToWords :: SomeMnemonic -> NonEmpty Text
someMnemonicToWords (SomeMnemonic m) = NE.fromList (mnemonicToText m)

someMnemonicToSentence :: SomeMnemonic -> Text
someMnemonicToSentence = fold . NE.intersperse " " . someMnemonicToWords
