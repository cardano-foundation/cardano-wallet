{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

module Test.Integration.Scenario.CLI.Transactions
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Api.Types
    ( ApiAddress, ApiTransaction, getApiT )
import Cardano.Wallet.HttpBridge.Compatibility
    ( HttpBridge )
import Cardano.Wallet.Primitive.Types
    ( encodeAddress )
import Data.Generics.Internal.VL.Lens
    ( (^.) )
import Data.Proxy
    ( Proxy (..) )
import System.Exit
    ( ExitCode (..) )
import Test.Hspec
    ( SpecWith, it )
import Test.Hspec.Expectations.Lifted
    ( shouldBe )
import Test.Integration.Framework.DSL
    ( Context (..)
    , Payload (..)
    , emptyWallet
    , expectValidJSON
    , fixtureWallet
    , getAddresses
    , postTransactionViaCLI
    , unsafeRequest
    , walletId
    )

import qualified Data.Text as T

spec :: SpecWith Context
spec = do

    it "CLI - Can create transaction" $ \ctx -> do
        wSrc <- fixtureWallet ctx
        wDest <- emptyWallet ctx
        (_, addr:_) <- unsafeRequest @[ApiAddress HttpBridge] ctx (getAddresses wDest) Empty
        let addrStr =
                encodeAddress (Proxy @HttpBridge) (getApiT $ fst $ addr ^. #id)
        let args = T.unpack <$>
                [ wSrc ^. walletId
                , "--payment", "14@" <> addrStr
                ]
        (c, out, err) <- postTransactionViaCLI "cardano-wallet" args
        err `shouldBe` "Please enter a passphrase: **************\nOk.\n"
        expectValidJSON (Proxy @(ApiTransaction HttpBridge)) out
        c `shouldBe` ExitSuccess
