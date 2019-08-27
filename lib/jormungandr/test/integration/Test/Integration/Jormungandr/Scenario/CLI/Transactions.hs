{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Integration.Jormungandr.Scenario.CLI.Transactions
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Jormungandr.Binary
    ( MessageType (..) )
import Cardano.Wallet.Jormungandr.Primitive.Types
    ( Tx (..) )
import Cardano.Wallet.Primitive.Types
    ( DecodeAddress (..), EncodeAddress (..), Hash (..) )
import Data.ByteArray.Encoding
    ( Base (Base16), convertToBase )
import Numeric.Natural
    ( Natural )
import System.Command
    ( Exit (..), Stderr (..), Stdout (..) )
import System.Exit
    ( ExitCode (..) )
import Test.Hspec
    ( SpecWith, it )
import Test.Hspec.Expectations.Lifted
    ( shouldBe, shouldContain )
import Test.Integration.Framework.DSL
    ( Context (..), KnownCommand, postExternalTransactionViaCLI )
import Test.Integration.Framework.TestData
    ( errMsg404MalformedTxPayload )
import Test.Integration.Jormungandr.Scenario.API.Transactions
    ( encodeTx, fixtureExternalTx )

import qualified Data.Text as T
import qualified Data.Text.Encoding as T

spec :: forall t. (EncodeAddress t, DecodeAddress t, KnownCommand t)
     => SpecWith (Context t)
spec = do

    it "TRANS_EXTERNAL_CREATE_01 - proper single output transaction and \
       \proper binary format" $ \ctx -> do

        let toSend = 1 :: Natural
        (tx@(Tx txId _ _), wits) <- fixtureExternalTx ctx toSend
        let arg = T.unpack $ encodeTx (tx, wits) MsgTypeTransaction

        -- post external transaction
        (Exit code, Stdout out, Stderr err) <-
            postExternalTransactionViaCLI @t ctx [arg]
        let expectedTxId = T.decodeUtf8 . convertToBase Base16 . getHash $ txId

        err `shouldBe` "Ok.\n"
        out `shouldBe` "{\n    \"id\": " ++ show expectedTxId ++ "\n}\n"
        code `shouldBe` ExitSuccess

    it "TRANS_EXTERNAL_CREATE_02 - proper single output transaction and \
       \wrong binary format" $ \ctx -> do

        let toSend = 1 :: Natural
        (tx, wits) <- fixtureExternalTx ctx toSend
        let arg = T.unpack $ encodeTx (tx, wits) MsgTypeInitial

        -- post external transaction
        (Exit code, Stdout out, Stderr err) <-
            postExternalTransactionViaCLI @t ctx [arg]

        err `shouldContain` errMsg404MalformedTxPayload
        out `shouldBe` ""
        code `shouldBe` ExitFailure 1
