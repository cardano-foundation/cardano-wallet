{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.CLISpec
    ( spec
    ) where

import Prelude

import Cardano.CLI
    ( Port, hGetSensitiveLine )
import Control.Concurrent
    ( forkFinally )
import Control.Concurrent.MVar
    ( newEmptyMVar, putMVar, takeMVar )
import Data.Proxy
    ( Proxy (..) )
import Data.Text
    ( Text )
import Data.Text.Class
    ( FromText (..) )
import System.IO
    ( IOMode (..), hClose, openFile )
import Test.Hspec
    ( Spec, describe, it, shouldBe )
import Test.QuickCheck
    ( Arbitrary (..) )
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

    describe "getSensitiveLine" $ do
        it "No special separator" $ getSensitiveLineTest $ GetSensitiveLineTest
            { prompt = "Prompt: "
            , separator = Nothing
            , input = "password\n"
            , expectedStdout = "Prompt: ********\n"
            , expectedResult = "password" :: Text
            }

        it "Space separated" $ getSensitiveLineTest $ GetSensitiveLineTest
            { prompt = "Prompt: "
            , separator = Just ' '
            , input = "toilet soldier word\n"
            , expectedStdout = "Prompt: ****** ******* ****\n"
            , expectedResult = "toilet soldier word" :: Text
            }

        it "Parser with failure" $ getSensitiveLineTest $ GetSensitiveLineTest
            { prompt = "Prompt: "
            , separator = Nothing
            , input = "patate\n14\n"
            , expectedStdout =
                "Prompt: ******\n\ESC[91minput does not \
                \start with a digit\n\ESC[mPrompt: **\n"
            , expectedResult = 14 :: Int
            }

        it "With backspaces" $ getSensitiveLineTest $ GetSensitiveLineTest
            { prompt = "Prompt: "
            , separator = Nothing
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

data GetSensitiveLineTest a = GetSensitiveLineTest
    { prompt :: Text
    , separator :: Maybe Char
    , input :: Text
    , expectedStdout :: Text
    , expectedResult :: a
    }

getSensitiveLineTest
    :: (FromText a, Show a, Eq a)
    => GetSensitiveLineTest a
    -> IO ()
getSensitiveLineTest (GetSensitiveLineTest prompt_ sep input_ output expected) = do
    -- Setup
    let fstdin = "/tmp/cardano-wallet-cli-stdin"
    let fstdout = "/tmp/cardano-wallet-cli-stdout"
    TIO.writeFile fstdin input_ *> writeFile fstdout mempty
    stdin <- openFile fstdin ReadWriteMode
    stdout <- openFile fstdout ReadWriteMode

    -- Action
    mvar <- newEmptyMVar
    let action = hGetSensitiveLine (stdin, stdout) prompt_ sep fromText
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
    arbitrary = genericArbitrary
    shrink = genericShrink
