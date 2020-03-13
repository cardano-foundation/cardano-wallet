{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Integration.Scenario.CLI.HWWallets
    ( spec
    ) where

import Prelude

-- import Cardano.CLI
--     ( Port )
import Cardano.Wallet.Api.Types
    ( ApiTransaction, ApiWallet, encodeAddress, getApiT )
import Cardano.Wallet.Primitive.AddressDerivation
    ( NetworkDiscriminant (..) )
-- import Cardano.Wallet.Primitive.Types
--     ( SyncProgress (..), walletNameMaxLength, walletNameMinLength )
-- import Control.Monad
--     ( forM_ )
import Data.Generics.Internal.VL.Lens
    ( (^.) )
-- import Data.Generics.Product.Typed
--     ( typed )
import Data.Proxy
    ( Proxy (..) )
import Data.Quantity
    ( Quantity (..) )
-- import Data.Text
--     ( Text )
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
    , createWalletFromPublicKeyViaCLI
    , createWalletViaCLI
    , deleteWalletViaCLI
    , eventually
    , expectCliField
    , expectValidJSON
    , fixtureWallet
    , generateMnemonicsViaCLI
    , getWalletViaCLI
    , listAddresses
    , postTransactionViaCLI
    , pubKeyFromMnemonics
    , verify
    , walletId
    )
import Test.Integration.Framework.TestData
    ( cmdOk )

import qualified Data.Text as T

spec
    :: forall t n. (n ~ 'Testnet, KnownCommand t)
    => SpecWith (Context t)
spec = do

    it "HW_WALLETS_01 - Restoration from account public key preserves funds" $ \ctx -> do
        wSrc <- fixtureWallet ctx

        -- create a wallet
        Stdout m <- generateMnemonicsViaCLI @t []
        (c1, o1, e1) <- createWalletViaCLI @t ctx ["n"] m "\n" "secure-passphrase"
        c1 `shouldBe` ExitSuccess
        T.unpack e1 `shouldContain` cmdOk
        wDest <- expectValidJSON (Proxy @ApiWallet) o1
        verify wDest
            [ expectCliField
                    (#balance . #getApiT . #available) (`shouldBe` Quantity 0)
            , expectCliField
                    (#balance . #getApiT . #total) (`shouldBe` Quantity 0)
            ]

        --send transaction to the wallet
        let amount = 11
        addrs:_ <- listAddresses ctx wDest
        let addr = encodeAddress @n (getApiT $ fst $ addrs ^. #id)
        let args = T.unpack <$>
                [ wSrc ^. walletId
                , "--payment", T.pack (show amount) <> "@" <> addr
                ]

        (cp, op, ep) <- postTransactionViaCLI @t ctx "cardano-wallet" args
        T.unpack ep `shouldContain` cmdOk
        _ <- expectValidJSON (Proxy @(ApiTransaction n)) op
        cp `shouldBe` ExitSuccess

        eventually "Wallet balance is as expected" $ do
            Stdout og <- getWalletViaCLI @t ctx $ T.unpack (wDest ^. walletId)
            jg <- expectValidJSON (Proxy @ApiWallet) og
            expectCliField (#balance . #getApiT . #available)
                (`shouldBe` Quantity amount) jg
            expectCliField (#balance . #getApiT . #total)
                (`shouldBe` Quantity amount) jg

        -- delete wallet
        Exit cd <- deleteWalletViaCLI @t ctx $ T.unpack (wDest ^. walletId)
        cd `shouldBe` ExitSuccess

        -- restore wallet from account public key
        let accXPub = pubKeyFromMnemonics' (words m)
        (Exit c2, Stdout o2, Stderr e2) <-
            createWalletFromPublicKeyViaCLI @t ctx ["n"] accXPub
        c2 `shouldBe` ExitSuccess
        e2 `shouldContain` cmdOk
        wRestored <- expectValidJSON (Proxy @ApiWallet) o2

        -- make sure funds are there
        Stdout o3 <- getWalletViaCLI @t ctx $ T.unpack (wRestored ^. walletId)
        justRestored <- expectValidJSON (Proxy @ApiWallet) o3
        verify justRestored
            [ expectCliField
                    (#balance . #getApiT . #available) (`shouldBe` Quantity amount)
            , expectCliField
                    (#balance . #getApiT . #total) (`shouldBe` Quantity amount)
            ]

pubKeyFromMnemonics' :: [String] -> String
pubKeyFromMnemonics' m = T.unpack $ pubKeyFromMnemonics (T.pack <$> m)

--       expect (expEc, expOut, expErr) (ec, out, err) = do
--               ec `shouldBe` expEc
--               out `shouldBe` expOut
--               T.unpack err `shouldContain` expErr
--
-- emptyRandomWallet' :: Context t -> IO String
-- emptyRandomWallet' = fmap (T.unpack . view walletId) . emptyRandomWallet
--
-- emptyWallet' :: Context t -> IO String
-- emptyWallet' = fmap (T.unpack . view walletId) . emptyWallet
--
-- emptyWalletWith' :: Context t -> (Text, Text, Int) -> IO String
-- emptyWalletWith' ctx (name, pass, pg) =
--     fmap (T.unpack . view walletId) (emptyWalletWith ctx (name, pass, pg))
--
-- walletNames :: [(String, String)]
-- walletNames =
--         [ ( "Name min", replicate walletNameMinLength 'ź' )
--         , ( "Name max", replicate walletNameMaxLength 'ą' )
--         , ( "Name max - 1", replicate ( walletNameMaxLength - 1 ) 'ą' )
--         , ( "Name max + 1", replicate ( walletNameMinLength + 1 ) 'ź' )
--         , ( "Single space", " " )
--         , ( "Russian", "АаБбВвГгДдЕеЁёЖжЗз")
--         , ( "Polish", "aąbcćdeęfghijklłmnoóp" )
--         , ( "Kanji", "亜哀挨愛曖悪握圧扱宛嵐")
--         ]
