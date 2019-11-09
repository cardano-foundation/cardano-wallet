{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Integration.Jormungandr.Scenario.CLI.Transactions
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Api.Types
    ( ApiTxId (..), ApiWallet, getApiT )
import Cardano.Wallet.Jormungandr.Binary
    ( MessageType (..) )
import Cardano.Wallet.Primitive.AddressDerivation
    ( NetworkDiscriminant (..), hex )
import Cardano.Wallet.Primitive.Types
    ( Hash (..), Tx (..) )
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
    , deleteTransactionViaCLI
    , emptyWallet
    , expectCliFieldEqual
    , expectEventually'
    , expectValidJSON
    , fixtureRawTx
    , getWalletEp
    , getWalletViaCLI
    , listAddresses
    , postExternalTransactionViaCLI
    , verify
    , walletId
    )
import Test.Integration.Framework.TestData
    ( errMsg400MalformedTxPayload
    , errMsg400WronglyEncodedTxPayload
    , errMsg404CannotFindTx
    )
import Test.Integration.Jormungandr.Scenario.API.Transactions
    ( ExternalTxFixture (..), encodeTx, fixtureExternalTx )
import Web.HttpApiData
    ( ToHttpApiData (..) )

import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

spec
    :: forall t n. (n ~ 'Testnet, KnownCommand t)
    => SpecWith (Context t)
spec = do
    it "TRANS_EXTERNAL_CREATE_01x - \
        \single output tx signed via jcli" $ \ctx -> do
        w <- emptyWallet ctx
        addr:_ <- listAddresses ctx w
        let amt = 4321

        payload <- fixtureRawTx ctx (getApiT $ fst $ addr ^. #id, amt)
        (Exit code, Stdout out, Stderr err) <-
            postExternalTransactionViaCLI @t ctx
                [T.unpack $ T.decodeUtf8 $ hex $ BL.toStrict payload]
        err `shouldBe` "Ok.\n"
        out `shouldContain` "id"
        code `shouldBe` ExitSuccess

        expectEventually' ctx getWalletEp balanceAvailable amt w
        expectEventually' ctx getWalletEp balanceTotal amt w

    it "TRANS_EXTERNAL_CREATE_01 - proper single output transaction and \
       \proper binary format" $ \ctx -> do
        let toSend = 1 :: Natural
        (ExternalTxFixture _ wDest _ txWits@((Tx txid _ _), _)) <-
            fixtureExternalTx @t ctx toSend
        let baseOk = Base16
        let arg = T.unpack $ encodeTx txWits MsgTypeTransaction baseOk

        -- post external transaction
        (Exit code, Stdout out, Stderr err) <-
            postExternalTransactionViaCLI @t ctx [arg]
        let expectedTxId = T.decodeUtf8 . convertToBase Base16 . getHash $ txid
        err `shouldBe` "Ok.\n"
        out `shouldBe` "{\n    \"id\": " ++ show expectedTxId ++ "\n}\n"
        code `shouldBe` ExitSuccess

        expectEventually' ctx getWalletEp balanceAvailable toSend wDest
        expectEventually' ctx getWalletEp balanceTotal toSend wDest

        -- verify balance on dest wallet
        Stdout gOutDest <- getWalletViaCLI @t ctx (T.unpack (wDest ^. walletId))
        destJson <- expectValidJSON (Proxy @ApiWallet) gOutDest
        verify destJson
            [ expectCliFieldEqual balanceAvailable toSend
            , expectCliFieldEqual balanceTotal toSend
            ]

    it "TRANS_EXTERNAL_CREATE_02 - proper single output transaction and \
       \not hex-encoded binary format" $ \ctx -> do
        let toSend = 1 :: Natural
        (ExternalTxFixture _ _ _ txWits) <- fixtureExternalTx @t ctx toSend
        let baseWrong = Base64
        let argWrong = T.unpack $ encodeTx txWits MsgTypeTransaction baseWrong
        -- post external transaction
        (Exit code1, Stdout out1, Stderr err1) <-
            postExternalTransactionViaCLI @t ctx [argWrong]
        err1 `shouldContain` errMsg400WronglyEncodedTxPayload
        out1 `shouldBe` ""
        code1 `shouldBe` ExitFailure 1

    it "TRANS_EXTERNAL_CREATE_03 - proper single output transaction and \
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

    it "TRANS_DELETE_05 - Cannot forget external tx via CLI" $ \ctx -> do
        w <- emptyWallet ctx
        addr:_ <- listAddresses ctx w
        let amt = 11111

        -- post external tx
        payload <- fixtureRawTx ctx (getApiT $ fst $ addr ^. #id, amt)
        (Exit code, Stdout out, Stderr err) <-
            postExternalTransactionViaCLI @t ctx
                [T.unpack $ T.decodeUtf8 $ hex $ BL.toStrict payload]
        err `shouldBe` "Ok.\n"
        txJson <- expectValidJSON (Proxy @ApiTxId) out
        code `shouldBe` ExitSuccess
        let txid = T.unpack $ toUrlPiece (txJson ^. #id)

        -- Try to forget external tx
        (Exit c2, Stdout out2, Stderr err2) <-
            deleteTransactionViaCLI @t ctx (T.unpack $ w ^. walletId) txid
        err2 `shouldContain` errMsg404CannotFindTx (T.pack txid)
        out2 `shouldBe` ""
        c2 `shouldBe` ExitFailure 1

        -- funds eventually are on target wallet
        expectEventually' ctx getWalletEp balanceAvailable amt w
        expectEventually' ctx getWalletEp balanceTotal amt w
