{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.CLISpec
    ( spec
    ) where

import Prelude

import Cardano.BM.Data.Severity
    ( Severity (..) )
import Cardano.CLI
    ( OptionValue (..), Port (..), hGetLine, hGetSensitiveLine )
import Control.Concurrent
    ( forkFinally )
import Control.Concurrent.MVar
    ( newEmptyMVar, putMVar, takeMVar )
import Control.Monad
    ( mapM_ )
import Data.Proxy
    ( Proxy (..) )
import Data.Text
    ( Text )
import Data.Text.Class
    ( FromText (..), TextDecodingError (..), toText )
import System.IO
    ( Handle, IOMode (..), hClose, openFile )
import Test.Hspec
    ( Spec, describe, it, shouldBe )
import Test.QuickCheck
    ( Arbitrary (..)
    , Large (..)
    , arbitraryBoundedEnum
    , checkCoverage
    , cover
    , (===)
    )
import Test.QuickCheck.Arbitrary.Generic
    ( genericArbitrary, genericShrink )
import Test.Text.Roundtrip
    ( textRoundtrip )

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

spec :: Spec
spec = do
    describe "Can perform roundtrip textual encoding & decoding" $ do
        textRoundtrip $ Proxy @(Port "test")
        textRoundtrip $ Proxy @(OptionValue Severity)

    describe "Port decoding from text" $ do
        let err = TextDecodingError
                $ "expected a TCP port number between "
                <> show (getPort minBound)
                <> " and "
                <> show (getPort maxBound)

        it "decode valid numbers to TCP Port, fail otherwise" $ checkCoverage $
            \(Large p) ->
                let
                    result :: Either TextDecodingError (Port "")
                    result = fromText (toText p)
                in
                        if p >= getPort minBound && p <= getPort maxBound
                            then cover 3 True "Right" $ result === Right (Port p)
                            else cover 90 True "Left" $ result === Left err

        mapM_ (\p -> it (T.unpack p) $ fromText @(Port "") p === Left err)
            [ "not-a-int"
            , "14.42"
            , ""
            , "[]"
            , "0x1337"
            , "0"
            ]

    describe "getLine" $ do
        it "Normal usage" $ test hGetLine $ GetLineTest
            { prompt = "Prompt: "
            , input = "warrior toilet word\n"
            , expectedStdout = "Prompt: "
            , expectedResult = "warrior toilet word" :: Text
            }

        it "Parser with failure" $ test hGetLine $ GetLineTest
            { prompt = "Prompt: "
            , input = "patate\n14\n"
            , expectedStdout =
                "Prompt: Int is an \
                \integer number between "
                <> T.pack (show $ minBound @Int)
                <> " and "
                <> T.pack (show $ maxBound @Int)
                <> ".\nPrompt: "
            , expectedResult = 14 :: Int
            }

    describe "getSensitiveLine" $ do
        it "Normal usage" $ test hGetSensitiveLine $ GetLineTest
            { prompt = "Prompt: "
            , input = "password\n"
            , expectedStdout = "Prompt: ********\n"
            , expectedResult = "password" :: Text
            }

        it "Parser with failure" $ test hGetSensitiveLine $ GetLineTest
            { prompt = "Prompt: "
            , input = "patate\n14\n"
            , expectedStdout =
                "Prompt: ******\nInt is an \
                \integer number between "
                <> T.pack (show $ minBound @Int)
                <> " and "
                <> T.pack (show $ maxBound @Int)
                <> ".\nPrompt: **\n"
            , expectedResult = 14 :: Int
            }

        it "With backspaces" $ test hGetSensitiveLine $ GetLineTest
            { prompt = "Prompt: "
            , input = backspace <> "patate" <> backspace <> backspace <> "14\n"
            , expectedStdout = "Prompt: ******\ESC[1D \ESC[1D\ESC[1D \ESC[1D**\n"
            , expectedResult = "pata14" :: Text
            }
  where
    backspace :: Text
    backspace = T.singleton (toEnum 127)

{-------------------------------------------------------------------------------
                                hGetSensitiveLine
-------------------------------------------------------------------------------}

data GetLineTest a = GetLineTest
    { prompt :: Text
    , input :: Text
    , expectedStdout :: Text
    , expectedResult :: a
    }

test
    :: (FromText a, Show a, Eq a)
    =>  (  (Handle, Handle)
        -> Text
        -> (Text -> Either TextDecodingError a)
        -> IO (a, Text)
        )
    -> GetLineTest a
    -> IO ()
test fn (GetLineTest prompt_ input_ output expected) = do
    -- Setup
    let fstdin = "/tmp/cardano-wallet-cli-stdin"
    let fstdout = "/tmp/cardano-wallet-cli-stdout"
    TIO.writeFile fstdin input_ *> writeFile fstdout mempty
    stdin <- openFile fstdin ReadWriteMode
    stdout <- openFile fstdout ReadWriteMode

    -- Action
    mvar <- newEmptyMVar
    let action = fn (stdin, stdout) prompt_ fromText
    _ <- forkFinally action (handler mvar)
    res <- takeMVar mvar
    hClose stdin *> hClose stdout
    content <- TIO.readFile fstdout

    -- Expectations
    (fst <$> res) `shouldBe` Just expected
    content `shouldBe` output
  where
    handler mvar = \case
        Left _ ->
            putMVar mvar Nothing
        Right a ->
            putMVar mvar (Just a)

{-------------------------------------------------------------------------------
                               Arbitrary Instances
-------------------------------------------------------------------------------}

instance Arbitrary (Port "test") where
    arbitrary = arbitraryBoundedEnum
    shrink p
        | p == minBound = []
        | otherwise = [pred p]

instance Arbitrary a => Arbitrary (OptionValue a) where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary Severity where
    arbitrary = arbitraryBoundedEnum
    shrink = genericShrink
