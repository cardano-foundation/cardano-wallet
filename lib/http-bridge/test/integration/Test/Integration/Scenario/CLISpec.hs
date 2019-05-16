{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module Test.Integration.Scenario.CLISpec
    ( specNoCluster
    , specWithCluster
    ) where

import Prelude

import Cardano.Wallet.Api.Types
    ( ApiWallet )
import Control.Monad
    ( forM_ )
import Data.List
    ( length )
import Data.Text
    ( Text )
import System.Command
    ( Stdout (..), command )
import Test.Hspec
    ( SpecWith, describe, it )
import Test.Hspec.Expectations.Lifted
    ( shouldBe, shouldContain, shouldNotContain )
import Test.Integration.Framework.DSL
    ( Context (..)
    , Headers (..)
    , Payload (..)
    , expectResponseCode
    , getFromResponse
    , json
    , request
    , walletId
    )

import qualified Data.Text as T
import qualified Network.HTTP.Types.Status as HTTP

specNoCluster :: SpecWith ()
specNoCluster = do

    it "CLI - Shows version" $  do
        Stdout out <- command [] "cardano-wallet" ["--version"]
        cabal <- readFile "../../cardano-wallet.cabal"
        let cabalVersion =  ( words $ (lines cabal) !! 1 ) !! 1
        let returnedVersion = T.unpack $ T.dropEnd 1 (T.pack out)
        returnedVersion `shouldBe` cabalVersion

    it "CLI - Can generate mnemonics with default size" $  do
        Stdout out <- command [] "cardano-wallet" ["mnemonic", "generate"]
        length (words out) `shouldBe` 15

    describe "CLI - Can generate mnemonics with different sizes" $  do
        forM_ [9, 12, 15, 18, 21, 24] $ \size -> it (show size) $ do
            Stdout out <- command [] "cardano-wallet"
                         ["mnemonic", "generate", "--size", show size]
            length (words out) `shouldBe` size

specWithCluster :: SpecWith Context
specWithCluster = do

    it "CLI - Can get a wallet" $ \ctx -> do
        walId <- createWallet ctx "1st CLI Wallet" mnemonics15
        Stdout out <- command [] "cardano-wallet"
                     ["wallet", "get", "--port", "1337", walId ]
        out `shouldContain` "1st CLI Wallet"

    it "CLI - Can list wallets" $ \ctx -> do
        _ <- createWallet ctx "1st CLI Wallet" mnemonics15
        _ <- createWallet ctx "2nd CLI Wallet" mnemonics18
        Stdout out <- command [] "cardano-wallet"
                     ["wallet", "list", "--port", "1337"]
        out `shouldContain` "1st CLI Wallet"
        out `shouldContain` "2nd CLI Wallet"

    it "CLI - Can update wallet name" $ \ctx -> do
        walId <- createWallet ctx "1st CLI Wallet" mnemonics15
        Stdout out <- command [] "cardano-wallet"
                     ["wallet", "update", "--port", "1337", walId, "--name"
                     , "Updated name" ]
        out `shouldContain` "Updated name"

    it "CLI - Can delete wallet" $ \ctx -> do
        walId <- createWallet ctx "CLI Wallet" mnemonics15
        Stdout out <- command [] "cardano-wallet"
                     ["wallet", "delete", "--port", "1337", walId ]
        out `shouldNotContain` "CLI Wallet"
 where

   createWallet :: Context -> Text -> [Text] -> IO String
   createWallet ctx name mnemonics = do
       let payload = Json [json| {
               "name": #{name},
               "mnemonic_sentence": #{mnemonics},
               "passphrase": "Secure Passphrase"
               } |]
       r <- request @ApiWallet ctx ("POST", "v2/wallets") Default payload
       expectResponseCode @IO HTTP.status202 r
       return (T.unpack $ getFromResponse walletId r)

   mnemonics15 :: [Text]
   mnemonics15 = ["network", "empty", "cause", "mean", "expire", "private",
       "finger", "accident", "session", "problem", "absurd", "banner", "stage",
       "void", "what"]

   mnemonics18 :: [Text]
   mnemonics18 = ["whisper", "control", "diary", "solid", "cattle", "salmon",
       "whale", "slender", "spread", "ice", "shock", "solve", "panel",
       "caution", "upon", "scatter", "broken", "tonight"]
