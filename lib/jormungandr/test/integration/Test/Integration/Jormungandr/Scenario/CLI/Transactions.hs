{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Integration.Jormungandr.Scenario.CLI.Transactions
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Api.Types
    ( ApiWallet )
import Cardano.Wallet.Jormungandr.Binary
    ( MessageType (..) )
import Cardano.Wallet.Jormungandr.Primitive.Types
    ( Tx (..) )
import Cardano.Wallet.Primitive.Types
    ( DecodeAddress (..), EncodeAddress (..), Hash (..) )
import Data.ByteArray.Encoding
    ( Base (Base16, Base64), convertToBase )
import Data.Generics.Internal.VL.Lens
    ( (^.) )
import Data.Proxy
    ( Proxy (..) )
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
    ( Context (..)
    , KnownCommand
    , balanceAvailable
    , balanceTotal
    , expectCliFieldEqual
    , expectEventually'
    , expectValidJSON
    , faucetAmt
    , getWalletViaCLI
    , postExternalTransactionViaCLI
    , verify
    , walletId
    )
import Test.Integration.Framework.TestData
    ( errMsg400MalformedTxPayload, errMsg400WronglyEncodedTxPayload )
import Test.Integration.Jormungandr.Scenario.API.Transactions
    ( ExternalTxFixture (..), encodeTx, fixtureExternalTx )

import qualified Data.Text as T
import qualified Data.Text.Encoding as T

spec
    :: forall t. (EncodeAddress t, DecodeAddress t, KnownCommand t)
    => SpecWith (Context t)
spec = do
    it "TRANS_EXTERNAL_CREATE_01 - proper single output transaction and \
       \proper binary format" $ \ctx -> do
        let toSend = 1 :: Natural
        (ExternalTxFixture wSrc wDest fee txWits@((Tx txId _ _), _)) <-
            fixtureExternalTx @t ctx toSend
        let baseOk = Base16
        let arg = T.unpack $ encodeTx txWits MsgTypeTransaction baseOk

        -- post external transaction
        (Exit code, Stdout out, Stderr err) <-
            postExternalTransactionViaCLI @t ctx [arg]
        let expectedTxId = T.decodeUtf8 . convertToBase Base16 . getHash $ txId
        err `shouldBe` "Ok.\n"
        out `shouldBe` "{\n    \"id\": " ++ show expectedTxId ++ "\n}\n"
        code `shouldBe` ExitSuccess

        -- verify balance on src wallet
        Stdout gOutSrc <- getWalletViaCLI @t ctx (T.unpack (wSrc ^. walletId))
        gJson <- expectValidJSON (Proxy @ApiWallet) gOutSrc
        verify gJson
            [ expectCliFieldEqual balanceTotal (faucetAmt - fee - toSend)
            ]

        expectEventually' ctx balanceAvailable toSend wDest
        expectEventually' ctx balanceTotal toSend wDest

        -- verify balance on dest wallet
        Stdout gOutDest <- getWalletViaCLI @t ctx (T.unpack (wDest ^. walletId))
        destJson <- expectValidJSON (Proxy @ApiWallet) gOutDest
        verify destJson
            [ expectCliFieldEqual balanceAvailable toSend
            , expectCliFieldEqual balanceTotal toSend
            ]

        let baseWrong = Base64
        let argWrong = T.unpack $ encodeTx txWits MsgTypeTransaction baseWrong
        -- post external transaction
        (Exit code1, Stdout out1, Stderr err1) <-
            postExternalTransactionViaCLI @t ctx [argWrong]
        err1 `shouldContain` errMsg400WronglyEncodedTxPayload
        out1 `shouldBe` ""
        code1 `shouldBe` ExitFailure 1

    it "TRANS_EXTERNAL_CREATE_02 - proper single output transaction and \
       \wrong binary format" $ \ctx -> do
        let toSend = 1 :: Natural
        (ExternalTxFixture _ _ _ txWits) <- fixtureExternalTx ctx toSend
        let baseOk = Base16
        let arg = T.unpack $ encodeTx txWits MsgTypeInitial baseOk

        -- post external transaction
        (Exit code, Stdout out, Stderr err) <-
            postExternalTransactionViaCLI @t ctx [arg]
        err `shouldContain` errMsg400MalformedTxPayload
        out `shouldBe` ""
        code `shouldBe` ExitFailure 1
