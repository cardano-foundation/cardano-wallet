{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.Primitive.MnemonicSpec
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Primitive.Mnemonic
    ( Entropy
    , EntropyError
    , EntropySize
    , Mnemonic
    , MnemonicError (..)
    , MnemonicException (..)
    , MnemonicWords
    , ambiguousNatVal
    , entropyToBytes
    , entropyToMnemonic
    , genEntropy
    , mkEntropy
    , mkMnemonic
    , mnemonicToEntropy
    , mnemonicToText
    )
import Control.Monad
    ( forM_ )
import Crypto.Encoding.BIP39
    ( DictionaryError (..)
    , EntropyError (..)
    , MnemonicWordsError (..)
    , ValidChecksumSize
    , ValidEntropySize
    , ValidMnemonicSentence
    , toEntropy
    )
import Data.ByteString
    ( ByteString )
import Data.Either
    ( isRight )
import Data.Function
    ( on )
import Data.Text
    ( Text )
import Test.Hspec
    ( Spec, describe, it, shouldBe, shouldReturn, shouldSatisfy )
import Test.Hspec.QuickCheck
    ( prop )
import Test.QuickCheck
    ( Arbitrary, arbitrary, vectorOf, (===) )

import qualified Cardano.Crypto.Wallet as CC
import qualified Data.ByteArray as BA
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text as T

-- | By default, private keys aren't comparable for security reasons (timing
-- attacks). We allow it here for testing purpose which is fine.
instance Eq CC.XPrv where
    (==) = (==) `on` CC.unXPrv

data TestVector = TestVector
    {
      -- | Text
      string :: Text

      -- | Corresponding Entropy
    , entropy :: Entropy (EntropySize 12)

      -- | Corresponding Mnemonic
    , mnemonic :: Mnemonic 12
    }


spec :: Spec
spec = do

    prop "(9) entropyToMnemonic . mnemonicToEntropy == identity" $ \e ->
        (mnemonicToEntropy @9 . entropyToMnemonic @9 @(EntropySize 9)) e == e

    prop "(12) entropyToMnemonic . mnemonicToEntropy == identity" $ \e ->
        (mnemonicToEntropy @12 . entropyToMnemonic @12 @(EntropySize 12)) e == e

    prop "(15) entropyToMnemonic . mnemonicToEntropy == identity" $ \e ->
        (mnemonicToEntropy @15 . entropyToMnemonic @15 @(EntropySize 15)) e == e

    prop "(18) entropyToMnemonic . mnemonicToEntropy == identity" $ \e ->
        (mnemonicToEntropy @18 . entropyToMnemonic @18 @(EntropySize 18)) e == e

    prop "(21) entropyToMnemonic . mnemonicToEntropy == identity" $ \e ->
        (mnemonicToEntropy @21 . entropyToMnemonic @21 @(EntropySize 21)) e == e

    prop "(24) entropyToMnemonic . mnemonicToEntropy == identity" $ \e ->
        (mnemonicToEntropy @24 . entropyToMnemonic @24 @(EntropySize 24)) e == e

    prop "(9) mkMnemonic . mnemonicToText == pure" $
        \(mw :: Mnemonic 9) -> (mkMnemonic @9 . mnemonicToText) mw === pure mw

    prop "(12) mkMnemonic . mnemonicToText == pure" $
        \(mw :: Mnemonic 12) -> (mkMnemonic @12 . mnemonicToText) mw === pure mw

    prop "(15) mkMnemonic . mnemonicToText == pure" $
        \(mw :: Mnemonic 15) -> (mkMnemonic @15 . mnemonicToText) mw === pure mw

    prop "(18) mkMnemonic . mnemonicToText == pure" $
        \(mw :: Mnemonic 18) -> (mkMnemonic @18 . mnemonicToText) mw === pure mw

    prop "(21) mkMnemonic . mnemonicToText == pure" $
        \(mw :: Mnemonic 21) -> (mkMnemonic @21 . mnemonicToText) mw === pure mw

    prop "(24) mkMnemonic . mnemonicToText == pure" $
        \(mw :: Mnemonic 24) -> (mkMnemonic @24 . mnemonicToText) mw === pure mw


    describe "golden tests" $ do
        it "No empty mnemonic" $
            mkMnemonic @15 [] `shouldBe`
                Left (ErrMnemonicWords (ErrWrongNumberOfWords 0 15))

        it "No 1 word mnemonic" $
            mkMnemonic @15 ["material"] `shouldBe`
                Left (ErrMnemonicWords (ErrWrongNumberOfWords 1 15))

        it "No too long fake mnemonic" $ do
            let sentence =
                    [ "squirrel","material","silly","twice","direct"
                    , "slush","pistol","razor","become","twice"
                    ]
            mkMnemonic @9 sentence `shouldBe`
                Left (ErrMnemonicWords (ErrWrongNumberOfWords 10 9))

        it "No empty entropy" $
            mkEntropy @(EntropySize 12) "" `shouldBe`
                Left (ErrInvalidEntropyLength 0 128)

        it "No too short entropy" $
            mkEntropy @(EntropySize 15) "000000" `shouldBe`
                Left (ErrInvalidEntropyLength 48 160)

        it "No too long entropy" $
            mkEntropy @(EntropySize 15) "1234512345123451234512345" `shouldBe`
                Left (ErrInvalidEntropyLength 200 160)

        it "Can make entropy" $
            mkEntropy @(EntropySize 15) "12345123451234512345" `shouldSatisfy`
                isRight

        it "Can generate 96 bits entropy" $
            (BA.length . entropyToBytes <$> genEntropy @96) `shouldReturn` 12

        it "Can generate 128 bits entropy" $
            (BA.length . entropyToBytes <$> genEntropy @128) `shouldReturn` 16

        it "Can generate 160 bits entropy" $
            (BA.length . entropyToBytes <$> genEntropy @160) `shouldReturn` 20

        it "Can generate 192 bits entropy" $
            (BA.length . entropyToBytes <$> genEntropy @192) `shouldReturn` 24

        it "Can generate 224 bits entropy" $
            (BA.length . entropyToBytes <$> genEntropy @224) `shouldReturn` 28

        it "Can generate 256 bits entropy" $
            (BA.length . entropyToBytes <$> genEntropy @256) `shouldReturn` 32

        it "Mnemonic to Text" $ forM_ testVectors $ \TestVector{..} ->
            mnemonicToText mnemonic `shouldBe` extractWords string

        it "Mnemonic from Text" $ forM_ testVectors $ \TestVector{..} ->
            (mkMnemonic @12 . extractWords) string `shouldBe` pure mnemonic

        it "Mnemonic to Entropy" $ forM_ testVectors $ \TestVector{..} ->
            mnemonicToEntropy mnemonic `shouldBe` entropy

        it "Mnemonic from Api is invalid" $ do
            let mnemonicFromApi =
                    "[squirrel,material,silly,twice,direct,slush,pistol,razor,\
                    \become,junk,kingdom,flee,squirrel,silly,twice]"
            (mkMnemonic @15 . extractWords) mnemonicFromApi `shouldSatisfy`
                isErrInvalidEntropyChecksum

        it "Mnemonic 2nd factor from Api is invalid" $ do
            let mnemonicFromApi =
                    "[squirrel,material,silly,twice,direct,slush,pistol,razor,\
                    \become]"
            (mkMnemonic @9 . extractWords) mnemonicFromApi `shouldSatisfy`
                isErrInvalidEntropyChecksum

        it "15 long mnemonics not valid for mkMnemonic @12" $ do
            let mnemonicFromApi =
                    "[trigger,artwork,lab,raw,confirm,visual,energy,double,\
                    \coral,fat,hen,ghost,phone,yellow,bag]"
            (mkMnemonic @12 . extractWords) mnemonicFromApi `shouldBe`
                Left (ErrMnemonicWords (ErrWrongNumberOfWords 15 12))

        it "15 long mnemonics not valid for mkMnemonic @24" $ do
            let mnemonicFromApi =
                    "[trigger,artwork,lab,raw,confirm,visual,energy,double,\
                    \coral,fat,hen,ghost,phone,yellow,bag]"
            (mkMnemonic @24 . extractWords) mnemonicFromApi `shouldBe`
                Left (ErrMnemonicWords (ErrWrongNumberOfWords 15 24))

        it "Non-English mnemonics don't work" $ do
            let mnemonicFromApi =
                    "[むしば,いてん,ぜんりゃく,になう,きあい,よっか,けんま,\
                    \げきげん,きおん,こふん,しゅらば,しあさって,てんし,わかめ,\
                    \いわば]"
            (mkMnemonic @15 . extractWords) mnemonicFromApi `shouldBe`
                Left (ErrDictionary (ErrInvalidDictionaryWord "むしば"))
  where
    testVectors :: [TestVector]
    testVectors =
        [ TestVector
            "[abandon,abandon,abandon,abandon,abandon,abandon,abandon,abandon,\
            \abandon,abandon,abandon,about]"
          (orFail $ mkEntropy'
              "\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\
              \\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL")
          (orFail $ mkMnemonic
              [ "abandon", "abandon", "abandon", "abandon", "abandon", "abandon"
              , "abandon", "abandon", "abandon", "abandon", "abandon", "about"
              ])
        , TestVector
            "[letter,advice,cage,absurd,amount,doctor,acoustic,avoid,letter,\
            \advice,cage,above]"
            (orFail $ mkEntropy'
                "\128\128\128\128\128\128\128\128\
                \\128\128\128\128\128\128\128\128")
            (orFail $ mkMnemonic
                [ "letter", "advice", "cage", "absurd", "amount", "doctor"
                , "acoustic", "avoid", "letter", "advice", "cage", "above"
                ])
        , TestVector
            "[zoo,zoo,zoo,zoo,zoo,zoo,zoo,zoo,zoo,zoo,zoo,wrong]"
            (orFail $ mkEntropy'
                "\255\255\255\255\255\255\255\255\
                \\255\255\255\255\255\255\255\255")
            (orFail $ mkMnemonic
                [ "zoo", "zoo", "zoo", "zoo", "zoo", "zoo"
                , "zoo", "zoo", "zoo", "zoo", "zoo", "wrong" ])
        ]
      where
        orFail
            :: Show e
            => Either e a
            -> a
        orFail =
            either (error . (<>) "Failed to create golden Mnemonic: " . show) id

        mkEntropy'
            :: ByteString
            -> Either (EntropyError 4) (Entropy 128)
        mkEntropy' = toEntropy @128 @4 @ByteString

    extractWords :: Text -> [Text]
    extractWords =
        T.splitOn ","
      . T.dropAround (\c -> c == '[' || c == ']')

    isErrInvalidEntropyChecksum :: Either (MnemonicError e) b -> Bool
    isErrInvalidEntropyChecksum = \case
        Left (ErrEntropy ErrInvalidEntropyChecksum{}) -> True
        _ -> False

-- | The initial seed has to be vector or length multiple of 4 bytes and shorter
-- than 64 bytes. Note that this is good for testing or examples, but probably
-- not for generating truly random Mnemonic words.
--
-- See 'Crypto.Random.Entropy (getEntropy)'
instance
    ( ValidEntropySize n
    , ValidChecksumSize n csz
    ) => Arbitrary (Entropy n) where
    arbitrary =
        let
            size = fromIntegral $ ambiguousNatVal @n
            entropy =
                mkEntropy  @n . B8.pack <$> vectorOf (size `quot` 8) arbitrary
        in
            either (error . show . UnexpectedEntropyError) id <$> entropy

-- | Same remark from 'Arbitrary Entropy' applies here.
instance
    ( n ~ EntropySize mw
    , mw ~ MnemonicWords n
    , ValidChecksumSize n csz
    , ValidEntropySize n
    , ValidMnemonicSentence mw
    , Arbitrary (Entropy n)
    ) => Arbitrary (Mnemonic mw) where
    arbitrary =
        entropyToMnemonic <$> arbitrary @(Entropy n)
