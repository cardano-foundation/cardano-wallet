{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Integration.HttpBridge.Scenario.CLI.Transactions
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Api.Types
    ( getApiT )
import Cardano.Wallet.Primitive.Types
    ( DecodeAddress (..), EncodeAddress (..), encodeAddress )
import Data.Generics.Internal.VL.Lens
    ( (^.) )
import Data.Proxy
    ( Proxy (..) )
import System.Exit
    ( ExitCode (..) )
import Test.Hspec
    ( SpecWith, it )
import Test.Hspec.Expectations.Lifted
    ( shouldBe, shouldContain )
import Test.Integration.Framework.DSL
    ( Context (..)
    , emptyWallet
    , fixtureWallet
    , listAddresses
    , postTransactionViaCLI
    , walletId
    )
import Test.Integration.Framework.TestData
    ( errMsg403InvalidTransaction )

import qualified Data.Text as T

spec :: forall t. (EncodeAddress t, DecodeAddress t) => SpecWith (Context t)
spec = do
    it "TRANS_CREATE_09 - 0 amount transaction is forbidden on single output tx" $ \ctx -> do
        wSrc <- fixtureWallet ctx
        wDest <- emptyWallet ctx
        addrs:_ <- listAddresses ctx wDest
        let addr =
                encodeAddress (Proxy @t) (getApiT $ fst $ addrs ^. #id)
        let amt = "0"
        let args = T.unpack <$>
                [ wSrc ^. walletId
                , "--payment", amt <> "@" <> addr
                ]

        (c, out, err) <- postTransactionViaCLI ctx "cardano-wallet" args
        (T.unpack err) `shouldContain` errMsg403InvalidTransaction
        out `shouldBe` ""
        c `shouldBe` ExitFailure 1

    it "TRANS_CREATE_09 - 0 amount transaction is forbidden on multi-output tx" $ \ctx -> do
        wSrc <- fixtureWallet ctx
        wDest <- emptyWallet ctx
        addrs <- listAddresses ctx wDest
        let addr1 =
                encodeAddress (Proxy @t) (getApiT $ fst $ addrs !! 1 ^. #id)
        let addr2 =
                encodeAddress (Proxy @t) (getApiT $ fst $ addrs !! 2 ^. #id)
        let args = T.unpack <$>
                [ wSrc ^. walletId
                , "--payment", "0@" <> addr1
                , "--payment", "15@" <> addr2
                ]

        (c, out, err) <- postTransactionViaCLI ctx "cardano-wallet" args
        (T.unpack err) `shouldContain` errMsg403InvalidTransaction
        out `shouldBe` ""
        c `shouldBe` ExitFailure 1
