{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Integration.Jormungandr.Scenario.CLI.Mnemonics
    ( spec
    ) where

import Prelude hiding
    ( lines )

import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (..)
    , FromMnemonic (..)
    , NetworkDiscriminant (..)
    , WalletKey (..)
    , XPrv
    , XPub (..)
    , deriveRewardAccount
    )
import Cardano.Wallet.Primitive.AddressDerivation.Shelley
    ( KnownNetwork (..), ShelleyKey (..), generateKeyFromSeed )
import Data.Text
    ( Text )
import System.Command
    ( Stdout (..) )
import System.Exit
    ( ExitCode (..) )
import System.IO
    ( Handle, hClose, hFlush, hPutStr )
import System.Process
    ( waitForProcess, withCreateProcess )
import Test.Hspec
    ( SpecWith, it )
import Test.Hspec.Expectations.Lifted
    ( shouldBe, shouldContain )
import Test.Integration.Framework.DSL
    ( KnownCommand (..), generateMnemonicsViaCLI, proc' )
import Test.Integration.Framework.TestData
    ( invalidMnemonics15
    , japaneseMnemonics15
    , mnemonics6
    , russianWalletName
    , wildcardsWalletName
    )

import qualified Codec.Binary.Bech32 as Bech32
import qualified Data.ByteString as BS
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

spec :: forall t. KnownCommand t => SpecWith ()
spec = do
    it "CLI_MNEMONICS_REWARD_CREDENTIALS - \
        \address obtained with credentials matches JCLI" $ do
        Stdout m <- generateMnemonicsViaCLI @t []
        let mnemonics = T.words (T.pack m)
        addr  <- mnemonicsToAccountAddress mnemonics
        (c, out, err)  <- getRewardCredentialsViaCli @t (T.pack m) mempty
        c `shouldBe` ExitSuccess
        T.unpack err `shouldContain` "mnemonic"
        xprv <- getXprv out
        xpub  <- jcliKeyToPublic xprv
        addr' <- jcliAddressAccount xpub
        addr `shouldBe` addr'

    it "CLI_MNEMONICS_REWARD_CREDENTIALS - \
        \can use two mnemonic sentences" $ do
        Stdout m1 <- generateMnemonicsViaCLI @t []
        Stdout m2 <- generateMnemonicsViaCLI @t ["--size", "12"]
        (c, out, err)  <- getRewardCredentialsViaCli @t (T.pack m1) (T.pack m2)
        c `shouldBe` ExitSuccess
        T.unpack err `shouldContain` "mnemonic"
        T.unpack out `shouldContain` "ed25519e_sk"

    it "CLI_MNEMONICS_REWARD_CREDENTIALS - \
        \cannot use 1st mnemonic > 15" $ do
        Stdout m1 <- generateMnemonicsViaCLI @t ["--size", "18"]
        (c, out, err)  <- getRewardCredentialsViaCli @t (T.pack m1) mempty
        c `shouldBe` ExitFailure 1
        T.unpack err `shouldContain` "Invalid number of words: 15 words are expected."
        T.unpack out `shouldContain` mempty

    it "CLI_MNEMONICS_REWARD_CREDENTIALS - \
        \cannot use 1st mnemonic < 15" $ do
        Stdout m1 <- generateMnemonicsViaCLI @t ["--size", "12"]
        (c, out, err)  <- getRewardCredentialsViaCli @t (T.pack m1) mempty
        c `shouldBe` ExitFailure 1
        T.unpack err `shouldContain` "Invalid number of words: 15 words are expected."
        T.unpack out `shouldContain` mempty

    it "CLI_MNEMONICS_REWARD_CREDENTIALS - \
        \cannot use 2nd mnemonic < 9" $ do
        Stdout m1 <- generateMnemonicsViaCLI @t []
        (c, out, err)  <- getRewardCredentialsViaCli @t (T.pack m1) (T.unwords mnemonics6)
        c `shouldBe` ExitFailure 1
        T.unpack err `shouldContain` "Invalid number of words: 9 or 12 words are expected."
        T.unpack out `shouldContain` mempty

    it "CLI_MNEMONICS_REWARD_CREDENTIALS - \
        \cannot use 2nd mnemonic > 12" $ do
        Stdout m1 <- generateMnemonicsViaCLI @t []
        Stdout m2 <- generateMnemonicsViaCLI @t ["--size", "18"]
        (c, out, err)  <- getRewardCredentialsViaCli @t (T.pack m1) (T.pack m2)
        c `shouldBe` ExitFailure 1
        T.unpack err `shouldContain` "Invalid number of words: 9 or 12 words are expected."
        T.unpack out `shouldContain` mempty

    it "CLI_MNEMONICS_REWARD_CREDENTIALS - \
        \non-english mnemonics show appropriate error" $ do
        (c, out, err)  <- getRewardCredentialsViaCli @t (T.unwords japaneseMnemonics15) mempty
        c `shouldBe` ExitFailure 1
        T.unpack err `shouldContain` "Found invalid (non-English) word"
        T.unpack out `shouldContain` mempty

    it "CLI_MNEMONICS_REWARD_CREDENTIALS - \
        \invalid mnemonics show appropriate error" $ do
        (c, out, err)  <- getRewardCredentialsViaCli @t (T.unwords invalidMnemonics15) mempty
        c `shouldBe` ExitFailure 1
        T.unpack err `shouldContain` "Invalid entropy checksum: please \
            \double-check the last word of your mnemonic sentence."
        T.unpack out `shouldContain` mempty

    it "CLI_MNEMONICS_REWARD_CREDENTIALS - \
        \adding just text shows appropriate error" $ do
        (c, out, err)  <- getRewardCredentialsViaCli @t (russianWalletName) mempty
        c `shouldBe` ExitFailure 1
        T.unpack err `shouldContain` "Invalid number of words: 15 words are expected."
        T.unpack out `shouldContain` mempty

    it "CLI_MNEMONICS_REWARD_CREDENTIALS - \
        \adding just wildcards shows appropriate error" $ do
        (c, out, err)  <- getRewardCredentialsViaCli @t (wildcardsWalletName) mempty
        c `shouldBe` ExitFailure 1
        T.unpack err `shouldContain` "Invalid number of words: 15 words are expected."
        T.unpack out `shouldContain` mempty
{-------------------------------------------------------------------------------

-------------------------------------------------------------------------------}

-- Like hGetContents but trim any extra newline or space
hGetTrimmedContents :: Handle -> IO Text
hGetTrimmedContents =
    fmap (T.filter (`notElem` ['\n', ' '])) . TIO.hGetContents

mnemonicsToAccountAddress
    :: [Text]
        -- ^ A list of mnemonics
    -> IO Text
        -- ^ An encoded address
mnemonicsToAccountAddress mnemonics = do
    seed <- either (fail . show) pure $ fromMnemonic @'[15] mnemonics
    pure $ mkAccountAddress $ generateKeyFromSeed (seed, mempty) mempty
  where
    -- Derive an account address corresponding to a reward account, from a root key
    mkAccountAddress
        :: ShelleyKey 'RootK XPrv
        -> Text
    mkAccountAddress =
        Bech32.encodeLenient hrp
        . Bech32.dataPartFromBytes
        . (BS.pack [addrAccount @'Testnet] <>)
        . xpubPublicKey
        . getRawKey
        . publicKey
        . deriveRewardAccount mempty
      where
        hrp = Bech32.unsafeHumanReadablePartFromText "addr"

getRewardCredentialsViaCli
    :: forall t. KnownCommand t
    => Text
        -- ^ A list of mnemonics 1
    -> Text
        -- ^ A list of mnemonics 2
    -> IO (ExitCode, Text, Text)
        -- ^ exit code, stdout and stderr of the command
getRewardCredentialsViaCli m1 m2 = do
    withCreateProcess
        (proc' (commandName @t)  ["mnemonic", "reward-credentials"])
        $ \(Just stdin) (Just stdout) (Just stderr) h -> do
            hPutStr stdin $ T.unpack m1 ++ "\n"
            hPutStr stdin $ T.unpack m2 ++ "\n"
            hFlush stdin
            hClose stdin
            c <- waitForProcess h
            out <- TIO.hGetContents stdout
            err <- TIO.hGetContents stderr
            return (c, out, err)

getXprv :: Text -> IO Text
getXprv t = do
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
