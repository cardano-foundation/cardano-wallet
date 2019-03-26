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
-- Copyright: © 2018-2019 IOHK
-- License: MIT
--
-- This module provides mnemonic (backup phrase) creation, and conversion of a
-- mnemonic to seed for wallet restoration.

module Cardano.Wallet.Primitive.Mnemonic
    (
      -- * Types
      Mnemonic
    , Entropy
    , EntropySize
    , MnemonicWords
    , ValidEntropySize
    , ValidChecksumSize
    , ConsistentEntropy
    , CheckSumBits

      -- * Errors
    , MnemonicError(..)
    , MnemonicException(..)
    , EntropyError(..)
    , DictionaryError(..)
    , MnemonicWordsError(..)

      -- * Creating @Mnemonic@ (resp. @Entropy@)
    , mkEntropy
    , mkMnemonic
    , genEntropy

      -- * Converting from and to @Mnemonic@ (resp. @Entropy@)
    , mnemonicToEntropy
    , entropyToMnemonic
    , entropyToBytes

    , ambiguousNatVal
    , mnemonicToText
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

-- | Smart-constructor for the Entropy
mkEntropy
    :: forall n csz. (ValidEntropySize n, ValidChecksumSize n csz)
    => ByteString
    -> Either (EntropyError csz) (Entropy n)
mkEntropy = toEntropy

ambiguousNatVal
    :: forall n . (KnownNat n)
    => Integer
ambiguousNatVal = natVal @n Proxy

-- | Generate Entropy of a given size using a random seed.
--
-- Example:
--     do
--       ent <- genEntropy :: IO (Entropy 12)
genEntropy
    :: forall n csz. (ValidEntropySize n, ValidChecksumSize n csz)
    => IO (Entropy n)
genEntropy =
    let
        size =
            fromIntegral $ ambiguousNatVal @n
        eitherToIO =
            either (throwM . UnexpectedEntropyError) return
    in
        (eitherToIO . mkEntropy) =<< Crypto.getEntropy (size `div` 8)

-- | Smart-constructor for the Mnemonic
mkMnemonic
    :: forall mw n csz.
     ( ConsistentEntropy n mw csz
     , EntropySize mw ~ n
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

-- | Convert an Entropy to a corresponding Mnemonic Sentence
entropyToMnemonic
    :: forall mw n csz.
     ( ValidMnemonicSentence mw
     , ValidEntropySize n
     , ValidChecksumSize n csz
     , n ~ EntropySize mw
     , mw ~ MnemonicWords n
     )
    => Entropy n
    -> Mnemonic mw
entropyToMnemonic entropy = Mnemonic
    { mnemonicToSentence = entropyToWords entropy
    , mnemonicToEntropy  = entropy
    }

-- | Convert 'Entropy' to a raw 'ByteString'
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

mnemonicToText
    :: Mnemonic mw
    -> [Text]
mnemonicToText =
    map (fromUtf8String . dictionaryIndexToWord Dictionary.english)
    . unListN
    . mnemonicSentenceToListN
    . mnemonicToSentence
