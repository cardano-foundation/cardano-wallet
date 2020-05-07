{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Integration.Jormungandr.Scenario.CLI.Mnemonics
    ( spec
    ) where

import Prelude hiding
    ( lines )

import Cardano.Address.Derivation
    ( XPrv, getPublicKey )
import Cardano.Mnemonic
    ( MkSomeMnemonic (..), SomeMnemonic )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (..)
    , NetworkDiscriminant (..)
    , WalletKey (..)
    , deriveRewardAccount
    )
import Cardano.Wallet.Primitive.AddressDerivation.Shelley
    ( KnownNetwork (..), ShelleyKey (..), generateKeyFromSeed )
import Control.Monad
    ( forM_ )
import Data.Text
    ( Text )
import GHC.TypeLits
    ( Nat )
import System.Command
    ( Stdout (..) )
import System.Exit
    ( ExitCode (..) )
import System.IO
    ( Handle, hClose, hFlush, hPutStr )
import System.Process
    ( waitForProcess, withCreateProcess )
import Test.Hspec
    ( SpecWith, describe, it )
import Test.Hspec.Expectations.Lifted
    ( shouldBe, shouldContain )
import Test.Integration.Framework.DSL
    ( KnownCommand (..), generateMnemonicsViaCLI, proc' )
import Test.Integration.Framework.TestData
    ( errMsgNotInDictionary
    , invalidMnemonics15
    , japaneseMnemonics15
    , mnemonics6
    , russianWalletName
    , wildcardsWalletName
    )

import qualified Codec.Binary.Bech32 as Bech32
import qualified Codec.Binary.Bech32.TH as Bech32
import qualified Data.ByteString as BS
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

spec :: forall t. KnownCommand t => SpecWith ()
spec = do
    describe "CLI_MNEMONICS_REWARD_CREDENTIALS - \
        \address obtained with credentials matches JCLI" $ do
        let seedSizes = Nothing : (Just <$> [15,18,21,24 :: Int])
        let sndSizes = Nothing : (Just <$> [9,12 :: Int])
        let sizes = [ (s1, s2) | s1 <- seedSizes, s2 <- sndSizes ]

        let title seedSize sndSize = mconcat
                [ "--size=", maybe "default" show seedSize, " (seed), "
                , "--size=", maybe "default" show sndSize, " (snd factor)"
                ]

        forM_ sizes $ \(seedSize, sndSize) -> do
            it (title seedSize sndSize) $ do
                mseed <- do
                    let args = maybe [] ((["--size"]<>) . pure . show) seedSize
                    Stdout mSeed <- generateMnemonicsViaCLI @t args
                    pure (T.pack mSeed)

                msnd <- do
                    case sndSize of
                        Nothing -> pure "\n"
                        Just size -> do
                            let args = ["--size", show size]
                            Stdout msnd <- generateMnemonicsViaCLI @t args
                            pure (T.pack msnd)

                addr  <- mnemonicsToAccountAddress mseed msnd
                (c, out, err)  <- getRewardCredentialsViaCli @t mseed msnd
                c `shouldBe` ExitSuccess
                T.unpack err `shouldContain` "mnemonic"
                xprv <- getXPrv out
                xpub  <- jcliKeyToPublic xprv
                addr' <- jcliAddressAccount xpub
                addr `shouldBe` addr'

    it "CLI_MNEMONICS_REWARD_CREDENTIALS - \
        \cannot use 1st mnemonic < 15" $ do
        Stdout m1 <- generateMnemonicsViaCLI @t ["--size", "12"]
        (c, out, err)  <- getRewardCredentialsViaCli @t (T.pack m1) mempty
        c `shouldBe` ExitFailure 1
        T.unpack err `shouldContain`
            "Invalid number of words: 15, 18, 21 or 24 words are expected."
        T.unpack out `shouldContain` mempty

    it "CLI_MNEMONICS_REWARD_CREDENTIALS - \
        \cannot use 2nd mnemonic < 9" $ do
        Stdout m1 <- generateMnemonicsViaCLI @t []
        (c, out, err)  <- getRewardCredentialsViaCli @t (T.pack m1) (T.unwords mnemonics6)
        c `shouldBe` ExitFailure 1
        T.unpack out `shouldContain` mempty
        T.unpack err `shouldContain`
            "Invalid number of words: 9 or 12 words are expected."

    it "CLI_MNEMONICS_REWARD_CREDENTIALS - \
        \cannot use 2nd mnemonic > 12" $ do
        Stdout m1 <- generateMnemonicsViaCLI @t []
        Stdout m2 <- generateMnemonicsViaCLI @t ["--size", "18"]
        (c, out, err)  <- getRewardCredentialsViaCli @t (T.pack m1) (T.pack m2)
        c `shouldBe` ExitFailure 1
        T.unpack out `shouldContain` mempty
        T.unpack err `shouldContain`
            "Invalid number of words: 9 or 12 words are expected."

    it "CLI_MNEMONICS_REWARD_CREDENTIALS - \
        \non-english mnemonics show appropriate error" $ do
        (c, out, err)  <- getRewardCredentialsViaCli @t (T.unwords japaneseMnemonics15) mempty
        c `shouldBe` ExitFailure 1
        T.unpack out `shouldContain` mempty
        T.unpack err `shouldContain` errMsgNotInDictionary

    it "CLI_MNEMONICS_REWARD_CREDENTIALS - \
        \invalid mnemonics show appropriate error" $ do
        (c, out, err)  <- getRewardCredentialsViaCli @t (T.unwords invalidMnemonics15) mempty
        c `shouldBe` ExitFailure 1
        T.unpack out `shouldContain` mempty
        T.unpack err `shouldContain`
            "Invalid entropy checksum: please double-check \
            \the last word of your mnemonic sentence."

    it "CLI_MNEMONICS_REWARD_CREDENTIALS - \
        \adding just text shows appropriate error" $ do
        (c, out, err)  <- getRewardCredentialsViaCli @t (russianWalletName) mempty
        c `shouldBe` ExitFailure 1
        T.unpack out `shouldContain` mempty
        T.unpack err `shouldContain`
            "Invalid number of words: 15, 18, 21 or 24 words are expected."

    it "CLI_MNEMONICS_REWARD_CREDENTIALS - \
        \adding just wildcards shows appropriate error" $ do
        (c, out, err)  <- getRewardCredentialsViaCli @t (wildcardsWalletName) mempty
        c `shouldBe` ExitFailure 1
        T.unpack out `shouldContain` mempty
        T.unpack err `shouldContain`
            "Invalid number of words: 15, 18, 21 or 24 words are expected."

{-------------------------------------------------------------------------------

-------------------------------------------------------------------------------}

-- Like hGetContents but trim any extra newline or space
hGetTrimmedContents :: Handle -> IO Text
hGetTrimmedContents =
    fmap (T.filter (`notElem` ['\n', ' '])) . TIO.hGetContents

mnemonicsToAccountAddress
    :: Text
        -- ^ A mnemonic sentence for the seed
    -> Text
        -- ^ A mnemonic sentence as a second factor
    -> IO Text
        -- ^ An encoded address
mnemonicsToAccountAddress m1 m2 = do
    seed <- unsafeFromMnemonic  @'[15,18,21,24] m1
    sndFactor <- if m2 == "\n"
        then return Nothing
        else Just <$> (unsafeFromMnemonic @'[9,12] m2)
    pure $ mkAccountAddress $ generateKeyFromSeed (seed, sndFactor) mempty
  where
    unsafeFromMnemonic
        :: forall (mz :: [Nat]). (MkSomeMnemonic mz)
        => Text
        -> IO SomeMnemonic
    unsafeFromMnemonic = either (fail . show) pure . mkSomeMnemonic @mz . T.words

    -- Derive an account address corresponding to a reward account, from a root key
    mkAccountAddress
        :: ShelleyKey 'RootK XPrv
        -> Text
    mkAccountAddress =
        Bech32.encodeLenient hrp
        . Bech32.dataPartFromBytes
        . (BS.pack [addrAccount @('Testnet 0)] <>)
        . getPublicKey
        . getRawKey
        . publicKey
        . deriveRewardAccount mempty
      where
        hrp = [Bech32.humanReadablePart|addr|]

getRewardCredentialsViaCli
    :: forall t. KnownCommand t
    => Text
        -- ^ A mnemonic sentence for the seed
    -> Text
        -- ^ A mnemonic sentence as a second factor
    -> IO (ExitCode, Text, Text)
        -- ^ exit code, stdout and stderr of the command
getRewardCredentialsViaCli mseed msnd = do
    withCreateProcess
        (proc' (commandName @t)  ["mnemonic", "reward-credentials"])
        $ \(Just stdin) (Just stdout) (Just stderr) h -> do
            hPutStr stdin $ T.unpack mseed
            hPutStr stdin $ T.unpack msnd
            hFlush stdin
            hClose stdin
            c <- waitForProcess h
            out <- TIO.hGetContents stdout
            err <- TIO.hGetContents stderr
            return (c, out, err)

getXPrv :: Text -> IO Text
getXPrv t = do
    let lines = T.lines t
    let errNoKey = unwords
            [ "expected an encoded extended secret key in the output"
            , "but it's not there. Output is:"
            , T.unpack (T.unlines lines)
            ]
    maybe (fail errNoKey) pure $
        T.filter (/= ' ') <$> L.find (T.isInfixOf "ed25519e_sk") lines

jcliKeyToPublic
    :: Text
        -- ^ An encoded private key
    -> IO Text
        -- ^ An encoded public key
jcliKeyToPublic xprv = withCreateProcess
    ( proc' "jcli" ["key", "to-public"]
    ) $ \(Just stdin) (Just stdout) _ _ -> do
        hPutStr stdin (T.unpack xprv)
        hFlush stdin
        hClose stdin
        hGetTrimmedContents stdout

jcliAddressAccount
    :: Text
        -- ^ An encoded public key
    -> IO Text
        -- ^ An encoded address account
jcliAddressAccount xpub = withCreateProcess
    ( proc' "jcli"
        [ "address", "account", T.unpack xpub
        , "--testing"
        , "--prefix", "addr"]
    ) $ \_ (Just stdout) _ _ -> do
        hGetTrimmedContents stdout
