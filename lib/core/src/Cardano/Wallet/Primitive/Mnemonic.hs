{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- This module provides mnemonic (backup phrase) creation, and conversion of a
-- mnemonic to seed for wallet restoration.
--
-- The module uses a lot of type-level machinery to ensure that entropy and
-- mnemonic sizes are all compatible and legit. Therefore, it isn't possible to
-- generate an invalid seed by using the smart constructors below, and trying to
-- generate an entropy of an invalid size will result in a runtime error.

module Cardano.Wallet.Primitive.Mnemonic
    (
      -- * Creating @Mnemonic@ (resp. @Entropy@)
      -- $constructors
      Mnemonic
    , Entropy
    , mkEntropy
    , mkMnemonic
    , genEntropy

      -- * Errors
    , MnemonicError(..)
    , MnemonicException(..)

      -- * Converting from and to @Mnemonic@ (resp. @Entropy@)
    , mnemonicToEntropy
    , entropyToMnemonic
    , entropyToBytes
    , mnemonicToText

      -- * Re-export from @Crypto.Encoding.BIP39@
    , EntropyError(..)
    , DictionaryError(..)
    , MnemonicWordsError(..)
    , ValidEntropySize
    , ValidChecksumSize
    , ValidMnemonicSentence
    , ConsistentEntropy
    , CheckSumBits
    , EntropySize
    , MnemonicWords

    ) where

import Prelude

import Basement.Sized.List
    ( unListN )
import Control.Arrow
    ( left )
import Control.Monad.Catch
    ( throwM )
import Crypto.Encoding.BIP39
    ( CheckSumBits
    , ConsistentEntropy
    , DictionaryError (..)
    , Entropy
    , EntropyError (..)
    , EntropySize
    , MnemonicSentence
    , MnemonicWords
    , MnemonicWordsError (..)
    , ValidChecksumSize
    , ValidEntropySize
    , ValidMnemonicSentence
    , dictionaryIndexToWord
    , entropyRaw
    , entropyToWords
    , mnemonicPhrase
    , mnemonicPhraseToMnemonicSentence
    , mnemonicSentenceToListN
    , toEntropy
    , wordsToEntropy
    )
import Data.ByteArray
    ( ScrubbedBytes )
import Data.ByteString
    ( ByteString )
import Data.Proxy
    ( Proxy (..) )
import Data.Text
    ( Text )
import Data.Typeable
    ( Typeable )
import GHC.TypeLits
    ( KnownNat, Nat, natVal )

import qualified Basement.Compat.Base as Basement
import qualified Basement.String as Basement
import qualified Crypto.Encoding.BIP39.English as Dictionary
import qualified Crypto.Random.Entropy as Crypto
import qualified Data.ByteArray as BA
import qualified Data.Text as T

-- | A backup-phrase in the form of a non-empty of Mnemonic words
-- Constructor isn't exposed.
data Mnemonic (mw :: Nat) = Mnemonic
    { mnemonicToEntropy  :: Entropy (EntropySize mw)
    , mnemonicToSentence :: MnemonicSentence mw
    } deriving (Eq, Show)

-- | This wraps EntropyError of "Cardano.Encoding.BIP39"
newtype MnemonicException csz =
    UnexpectedEntropyError (EntropyError csz)
    -- ^ Invalid entropy length or checksum
    deriving (Show, Typeable)

-- | This wraps errors from "Cardano.Encoding.BIP39"
data MnemonicError csz
    = ErrMnemonicWords MnemonicWordsError
      -- ^ Wrong number of words in mnemonic.
    | ErrEntropy (EntropyError csz)
      -- ^ Invalid entropy length or checksum.
    | ErrDictionary DictionaryError
      -- ^ Invalid word in mnemonic.
    deriving (Eq, Show)

deriving instance Eq (EntropyError czs)
deriving instance Eq MnemonicWordsError
deriving instance Eq DictionaryError

-- $constructors
--
-- Type families 'EntropySize', 'MnemonicWords' and 'CheckSumBits' can be used
-- to disambiguate calls to smart constructors in a readable way.
--
-- __TroubleShooting__:
--
-- - @Natural XX is out of bounds for Int@:
--   This usually occurs when ones is trying to specify an invalid size for an
--   'Entropy' or 'Mnemonic'. For example:
--
--   >>> genEntropy @42
--   error:
--     • Natural CheckSumBits 42 is out of bounds for Int
--
--   This could be the case as well when forgetting to use an adequate type
--   application:
--
--   >>> mkEntropy mempty
--   error:
--     • Natural ent is out of bounds for Int

-- | Smart-constructor for the Entropy
--
-- >>> mkEntropy @(EntropySize 15) bytes
-- Entropy {} :: Entropy 160
mkEntropy
    :: forall ent csz. (ValidEntropySize ent, ValidChecksumSize ent csz)
    => ByteString
    -> Either (EntropyError csz) (Entropy ent)
mkEntropy = toEntropy

-- | Generate Entropy of a given size using a random seed.
--
-- Example:
--
-- >>> genEntropy @(EntropySize 12)
-- Entropy {} :: Entropy 128
genEntropy
    :: forall ent csz. (ValidEntropySize ent, ValidChecksumSize ent csz)
    => IO (Entropy ent)
genEntropy =
    let
        size =
            fromIntegral $ natVal @ent Proxy
        eitherToIO =
            either (throwM . UnexpectedEntropyError) return
    in
        (eitherToIO . mkEntropy) =<< Crypto.getEntropy (size `div` 8)

-- | Smart-constructor for 'Mnemonic'. Requires a type application to
-- disambiguate the mnemonic size:
--
-- >>> mkMnemonic @15 sentence
-- Mnemonic {} :: Mnemonic 15
--
-- See also [FromMnemonic](Cardano-Wallet-Primitive-AddressDerivation.html#t:FromMnemonic)
-- to build a 'Mnemonic' from lists of words of variable sizes.
--
-- __Property__:
--
-- prop> mkMnemonic (mnemonicToText mnemonic) == Right mnemonic
mkMnemonic
    :: forall mw ent csz.
     ( ConsistentEntropy ent mw csz
     , EntropySize mw ~ ent
     )
    => [Text]
    -> Either (MnemonicError csz) (Mnemonic mw)
mkMnemonic wordsm = do
    phrase <- left ErrMnemonicWords
        $ mnemonicPhrase @mw (toUtf8String <$> wordsm)

    sentence <- left ErrDictionary
        $ mnemonicPhraseToMnemonicSentence Dictionary.english phrase

    entropy <- left ErrEntropy
        $ wordsToEntropy sentence

    pure Mnemonic
        { mnemonicToEntropy  = entropy
        , mnemonicToSentence = sentence
        }

-- | Convert an Entropy to a corresponding Mnemonic Sentence. Since 'Entropy'
-- and 'Mnemonic' can only be created through smart-constructors, this function
-- cannot fail and is total.
entropyToMnemonic
    :: forall mw ent csz.
     ( ValidMnemonicSentence mw
     , ValidEntropySize ent
     , ValidChecksumSize ent csz
     , ent ~ EntropySize mw
     , mw ~ MnemonicWords ent
     )
    => Entropy ent
    -> Mnemonic mw
entropyToMnemonic entropy = Mnemonic
    { mnemonicToSentence = entropyToWords entropy
    , mnemonicToEntropy  = entropy
    }

-- | Convert 'Entropy' to a plain bytes.
entropyToBytes
    :: Entropy n
    -> ScrubbedBytes
entropyToBytes = BA.convert . entropyRaw

toUtf8String
    :: Text
    -> Basement.String
toUtf8String = Basement.fromString . T.unpack

fromUtf8String
    :: Basement.String
    -> Text
fromUtf8String = T.pack . Basement.toList

instance (KnownNat csz) => Basement.Exception (MnemonicException csz)

-- | Convert a 'Mnemonic' to a sentence of English mnemonic words.
mnemonicToText
    :: Mnemonic mw
    -> [Text]
mnemonicToText =
    map (fromUtf8String . dictionaryIndexToWord Dictionary.english)
    . unListN
    . mnemonicSentenceToListN
    . mnemonicToSentence
