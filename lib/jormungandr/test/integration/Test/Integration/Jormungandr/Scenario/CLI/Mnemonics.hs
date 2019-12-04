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
import System.IO
    ( Handle, hClose, hFlush, hPutStr )
import System.Process
    ( withCreateProcess )
import Test.Hspec
    ( SpecWith, it )
import Test.Hspec.Expectations.Lifted
    ( shouldBe )
import Test.Integration.Framework.DSL
    ( KnownCommand (..), generateMnemonicsViaCLI, proc' )

import qualified Codec.Binary.Bech32 as Bech32
import qualified Data.ByteString as BS
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

spec :: forall t. KnownCommand t => SpecWith ()
spec = do
    it "CLI_MNEMONICS_REWARD_CREDENTIALS - \
        \address obtained with credentials matches JCLI" $ do
        Stdout stdout <- generateMnemonicsViaCLI @t []
        let mnemonics = T.words (T.pack stdout)
        addr  <- mnemonicsToAccountAddress mnemonics
        xprv  <- walletRewardCredentials @t mnemonics
        xpub  <- jcliKeyToPublic xprv
        addr' <- jcliAddressAccount xpub
        addr `shouldBe` addr'

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

walletRewardCredentials
    :: forall t. KnownCommand t
    => [Text]
        -- ^ A list of mnemonics
    -> IO Text
        -- ^ An encoded extended private key
walletRewardCredentials mnemonics = do
    lines <- withCreateProcess
        (proc' (commandName @t)  ["mnemonic", "reward-credentials"])
        $ \(Just stdin) (Just stdout) _ _ -> do
            hPutStr stdin $ T.unpack (T.unwords mnemonics) ++ "\n"
            hPutStr stdin "\n"
            hFlush stdin
            hClose stdin
            T.lines <$> TIO.hGetContents stdout

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
