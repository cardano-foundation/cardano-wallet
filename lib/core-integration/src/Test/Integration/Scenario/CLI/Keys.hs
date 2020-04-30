{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Test.Integration.Scenario.CLI.Keys where

import Prelude

import System.Process
    ( readProcess )
import Test.Hspec
    ( Spec, describe, it, shouldBe )
import Test.Integration.Framework.DSL
    ( KnownCommand (..) )

spec :: forall t. KnownCommand t => Spec
spec  = do
 describe "Piping and reading keys from stdin" $ do
    let mw = words "warfare subject disagree federal clock cube silly result sting tenant word defense choice kingdom awkward"

    -- The public key at path m/0'/0'/0
    let childPub = "xpub12vr8xuljuc0z4snfjjq6hcnx02t5qkegku6ygncqy7xkhx4s7gwq3ulfrded8x7n5ykegc3u2cvlenjpl5y35k0633m8yluke5fu8cg3dfa0p"
    let childPubHex = "53067373f2e61e2ac2699481abe2667a97405b28b734444f00278d6b9ab0f21c08f3e91b72d39bd3a12d94623c5619fcce41fd091a59fa8c76727f96cd13c3e1"

    it "derive bech32 public child key from mnemonic" $ do
        res <- key (["root", "--encoding", "bech32", "--"] ++ mw) ""
            >>= key ["child", "--path", "0H/0H"]
            >>= key ["public"]
            >>= key ["child", "--path", "0"]
        res `shouldBe` (childPub ++ "\n")
    it "derive hex public child key from mnemonic" $ do
        res <- key (["root", "--"] ++ mw) ""
            >>= key ["child", "--path", "0H/0H"]
            >>= key ["public"]
            >>= key ["child", "--path", "0"]
        res `shouldBe` (childPubHex ++ "\n")
    it "inspect the final result" $ do
        res <- key (["root", "--"] ++ mw) ""
            >>= key ["child", "--path", "0H/0H"]
            >>= key ["public"]
            >>= key ["child", "--path", "0"]
            >>= key ["inspect"]
        res `shouldBe` mconcat
            [ "extended public key: "
            , "53067373f2e61e2ac2699481abe2667a97405b28b734444f00278d6b9ab0f21c"
            , "\n"
            , "chain code: "
            , "08f3e91b72d39bd3a12d94623c5619fcce41fd091a59fa8c76727f96cd13c3e1"
            , "\n"
            ]
  where
    key
        :: [String]
        -- ^ Arguments
        -> String
        -- ^ Stdin
        -> IO String
        -- ^ Stdout
    key args = readProcess (commandName @t) ("key":args)


