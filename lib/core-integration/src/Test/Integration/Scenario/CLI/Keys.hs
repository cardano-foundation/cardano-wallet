{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Test.Integration.Scenario.CLI.Keys where

import Prelude

import System.Exit
    ( ExitCode (..) )
import System.Process
    ( readProcess, readProcessWithExitCode )
import Test.Hspec
    ( Spec, describe, it, shouldBe, shouldReturn )
import Test.Integration.Framework.DSL
    ( KnownCommand (..) )

spec :: forall t. KnownCommand t => Spec
spec  = do
    describe "Piping and reading keys from stdin" $ do
        let mw = words "warfare subject disagree federal clock cube silly result sting tenant word defense choice kingdom awkward"

        -- The public key at path m/0'/0'/0
        let childPub = "xpub12vr8xuljuc0z4snfjjq6hcnx02t5qkegku6ygncqy7xkhx4s7gwq3ulfrded8x7n5ykegc3u2cvlenjpl5y35k0633m8yluke5fu8cg3dfa0p"
        let childPubHex = "53067373f2e61e2ac2699481abe2667a97405b28b734444f00278d6b9ab0f21c08f3e91b72d39bd3a12d94623c5619fcce41fd091a59fa8c76727f96cd13c3e1"

        let expectedInspect = mconcat
                [ "extended public key: "
                , "53067373f2e61e2ac2699481abe2667a97405b28b734444f00278d6b9ab0f21c"
                , "\n"
                , "chain code: "
                , "08f3e91b72d39bd3a12d94623c5619fcce41fd091a59fa8c76727f96cd13c3e1"
                , "\n"
                ]

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
        it "inspect the final hex result" $ do
            res <- key (["root", "--"] ++ mw) ""
                >>= key ["child", "--path", "0H/0H"]
                >>= key ["public"]
                >>= key ["child", "--path", "0"]
                >>= key ["inspect"]
            res `shouldBe` expectedInspect
        it "inspect the final bech32 result" $ do
            res <- key (["root", "--encoding", "bech32", "--"] ++ mw) ""
                >>= key ["child", "--path", "0H/0H"]
                >>= key ["public"]
                >>= key ["child", "--path", "0"]
                >>= key ["inspect"]
            res `shouldBe` expectedInspect

    describe "key child" $ do
        let rootXPrv = "588102383ed9ecc5c44e1bfa18d1cf8ef19a7cf806a20bb4cbbe4e5\
                       \11666cf48d6fd7bec908e4c6ced5f0c4f0798b1b619d6b61e611049\
                       \2b5ebb430f570488f074a9fc9a22f0a61b2ab9b1f1a990e3f8dd6fb\
                       \ed4ad474371095c74db3d9c743a\n"
        it "golden" $ do
            res <- key ["child", "--path", "1852H/1815H/0H/0/0"] rootXPrv
            res `shouldBe`
                "5073cbc3e3f85b0099c67ed5b0344bfc0f15861ef05f41cde2a797352f66cf\
                \48ab59c46d040abb4b3e0623bb151362233e75cf1f923b6d5964780ebbcf3a\
                \2d7a3d9078e802011f1580465c80e7040f1e4d8e24f978d23f01c1d2cf18fc\
                \f741a7\n"

        -- This key does not have the "tweak" that newer keys are expected
        -- to have.
        let byronKey =
              "464f3a1316a3849a1ca49a7e3a8b9ab35379598ac4fbcd0ba2bc3a165185150a\
              \5c56ebf6d6d39fd6c070731a44133ebb083c42b949046d79aac48b7a1f52787c\
              \a5078d2194b78ccb6116d64f4d5a3fad3cd41e4748c20fc589d87a0e69583357"
        let encryptedKey =
              "9d41c6c66a0aaac73b31bfbf2522c63eea4e16e7df63ccf43e012b20a4606cbb\
              \e99a00cfed56e9516bc947f327a73e0849882a32a682932c51b42156055abb0b\
              \5d3661deb9064f2d0e03fe85d68070b2fe33b4916059658e28ac7f7f91ca4b12"

        it "byron keys does not fail anymore" $ do
            res <- key ["child", "--path", "0"] byronKey
            res `shouldBe`
                "460d95f55f8940c7145ab251da03f6a8767f2306b1f5345afd10090a558515\
                \0abb4902024903aa130d0ea3524968539071c0708a020c8181c913b9c7f306\
                \3b209bcb6f8af16317702127ab4169d2d7f11807d464b2f427b3ca45d5d9f0\
                \578288\n"

        it "encrypted byron keys does not fail anymore" $ do
            res <- key ["child", "--path", "0"] encryptedKey
            res `shouldBe`
                "7d0ef39b2f7de32e8ca24cfe7a9cf1663a860778c42c70ef41ad851fa6606c\
                \bbcfee7fbacf0e7141e449aa18e8d0c7206c0b7a71d3129751b516bf0a2abb\
                \2383ca7b962e3bc5aa7d049f33606c8851a7ba94432ef335e47d77575e2fce\
                \a9813c\n"

        it "fails when key is not 96 bytes" $ do
            keyStderr ["child", "--path", "0"] "5073"
                `shouldReturn`
                "Expected key to be 96 bytes in the\
                \ case of a private key and, 64 bytes for public keys. This\
                \ key is 2 bytes.\n"

        let pub1 =
              "20997b093a426804de5120fa2b6d2184a605274b364201ddc9f79307eae8dfed\
              \74a9fc9a22f0a61b2ab9b1f1a990e3f8dd6fbed4ad474371095c74db3d9c743a"

        let pub2 =
              "708792807c1e3959626cd0ab78b1db5ed1544a97f3addbb01457de56ee5a450f\
              \e932040a815c8994e44b7075ec61bdfc66e77671c59d14c76f3b33e8c534b15f"

        describe "public key derivation" $ do
            it "golden" $ do
                key ["child", "--path", "0"] pub1
                    `shouldReturn`
                    (pub2 <> "\n")

            it "fails for hardened index" $ do
                keyStderr ["child", "--path", "0H"] pub1
                    `shouldReturn`
                        "0H is a hardened index. Public key \
                        \derivation is only possible for soft indices. \nIf the\
                        \ index is correct, please use the corresponding \
                        \private key as input.\n"

    describe "key public" $ do
        let prv1 = "588102383ed9ecc5c44e1bfa18d1cf8ef19a7cf806a20bb4cbbe4e51166\
                   \6cf48d6fd7bec908e4c6ced5f0c4f0798b1b619d6b61e6110492b5ebb43\
                   \0f570488f074a9fc9a22f0a61b2ab9b1f1a990e3f8dd6fbed4ad4743710\
                   \95c74db3d9c743a"
        let pub1 = "20997b093a426804de5120fa2b6d2184a605274b364201ddc9f79307eae\
                   \8dfed74a9fc9a22f0a61b2ab9b1f1a990e3f8dd6fbed4ad474371095c74\
                   \db3d9c743a"

        it "golden (matching jcli)" $ do
            -- Verified manually with jcli.
            key ["public"] prv1 `shouldReturn` (pub1 ++ "\n")

        it "fails when input is a public key" $ do
            let err1 = "Input is already a public key."
            keyStderr ["public"] pub1 `shouldReturn` (err1 ++ "\n")

    describe "key inspect" $ do
        it "golden xprv" $ do
            let xprv =
                  "588102383ed9ecc5c44e1bfa18d1cf8e\
                  \f19a7cf806a20bb4cbbe4e511666cf48\
                  \d6fd7bec908e4c6ced5f0c4f0798b1b6\
                  \19d6b61e6110492b5ebb430f570488f0"
            let cc =
                  "74a9fc9a22f0a61b2ab9b1f1a990e3f8\
                  \dd6fbed4ad474371095c74db3d9c743a"
            key ["inspect"] (xprv ++ cc)
                `shouldReturn`
                mconcat [ "extended private key: "
                    , xprv
                    , "\n"
                    , "chain code: "
                    , cc
                    , "\n"
                    ]

        it "golden xpub" $ do
            let xpub =
                  "20997b093a426804de5120fa2b6d2184\
                  \a605274b364201ddc9f79307eae8dfed"
            let cc =
                  "74a9fc9a22f0a61b2ab9b1f1a990e3f8\
                  \dd6fbed4ad474371095c74db3d9c743a"
            key ["inspect"] (xpub ++ cc)
                `shouldReturn`
                mconcat [ "extended public key: "
                    , xpub
                    , "\n"
                    , "chain code: "
                    , cc
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

    keyStderr
        :: [String]
        -- ^ Arguments
        -> String
        -- ^ Stdin
        -> IO String
        -- ^ Stderr
    keyStderr args stdin = do
        (c, _out, err) <- readProcessWithExitCode (commandName @t) ("key":args) stdin
        c `shouldBe` (ExitFailure 1)
        return err
