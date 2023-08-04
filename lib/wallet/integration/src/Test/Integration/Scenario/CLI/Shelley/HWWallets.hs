{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Integration.Scenario.CLI.Shelley.HWWallets
  ( spec
  )
where

import Cardano.Wallet.Address.Discovery.Sequential
  ( defaultAddressPoolGap
  , getAddressPoolGap
  )
import Cardano.Wallet.Api.Types
  ( ApiAddressWithPath
  , ApiFee
  , ApiTransaction
  , ApiUtxoStatistics
  , ApiWallet
  , apiAddress
  )
import Cardano.Wallet.Api.Types.SchemaMetadata
  ( TxMetadataSchema (..)
  )
import Cardano.Wallet.Primitive.NetworkId
  ( HasSNetworkId (..)
  )
import Cardano.Wallet.Primitive.Types.Address
  ( AddressState (..)
  )
import Cardano.Wallet.Shelley.Compatibility
  ( encodeAddress
  )
import Control.Monad
  ( forM_
  )
import Control.Monad.Trans.Resource
  ( ResourceT
  , runResourceT
  )
import Data.Generics.Internal.VL.Lens
  ( view
  , (^.)
  )
import Data.Proxy
  ( Proxy (..)
  )
import Data.Quantity
  ( Quantity (..)
  )
import Data.Text qualified as T
import Data.Text.Class
  ( ToText (..)
  )
import System.Command
  ( Exit (..)
  , Stderr (..)
  , Stdout (..)
  )
import System.Exit
  ( ExitCode (..)
  )
import Test.Hspec
  ( SpecWith
  , describe
  )
import Test.Hspec.Expectations.Lifted
  ( shouldBe
  , shouldContain
  )
import Test.Hspec.Extra
  ( it
  )
import Test.Integration.Framework.DSL
  ( Context (..)
  , createWalletFromPublicKeyViaCLI
  , createWalletViaCLI
  , deleteWalletViaCLI
  , emptyWallet
  , eventually
  , expectCliField
  , expectCliListField
  , expectValidJSON
  , expectWalletUTxO
  , fixturePassphrase
  , fixtureWallet
  , fixtureWalletWithMnemonics
  , generateMnemonicsViaCLI
  , getWalletUtxoStatisticsViaCLI
  , getWalletViaCLI
  , listAddresses
  , listAddressesViaCLI
  , listTransactionsViaCLI
  , listWalletsViaCLI
  , minUTxOValue
  , postTransactionFeeViaCLI
  , postTransactionViaCLI
  , pubKeyFromMnemonics
  , updateWalletNameViaCLI
  , updateWalletPassphraseViaCLI
  , verify
  , verifyMsg
  , walletId
  , (.>)
  )
import Test.Integration.Framework.TestData
  ( cmdOk
  , errMsg403NoRootKey
  )
import Test.Integration.Scenario.CLI.Shelley.Wallets
  ( walletNames
  , walletNamesInvalid
  )
import Prelude

spec
  :: forall n
   . HasSNetworkId n
  => SpecWith Context
spec = describe "SHELLEY_CLI_HW_WALLETS" $ do
  it "HW_WALLETS_01x - Restoration from account public key preserves funds" $ \ctx -> runResourceT $ do
    wSrc <- fixtureWallet ctx

    -- create a wallet
    Stdout m <- generateMnemonicsViaCLI []
    (c1, o1, e1) <- createWalletViaCLI ctx ["n"] m "\n" "secure-passphrase"
    c1 `shouldBe` ExitSuccess
    T.unpack e1 `shouldContain` cmdOk
    wDest <- expectValidJSON (Proxy @ApiWallet) o1
    verifyMsg
      "Wallet balance is as expected"
      wDest
      [ expectCliField (#balance . #available) (`shouldBe` Quantity 0)
      , expectCliField (#balance . #total) (`shouldBe` Quantity 0)
      ]

    -- send transaction to the wallet
    let
      amount = Quantity . minUTxOValue . _mainEra $ ctx
    addrs : _ <- listAddresses @n ctx wDest
    let
      addr = encodeAddress (sNetworkId @n) (apiAddress $ addrs ^. #id)
    let
      args =
        T.unpack
          <$> [ wSrc ^. walletId
              , "--payment"
              , toText amount <> "@" <> addr
              ]

    (cp, op, ep) <- postTransactionViaCLI ctx "cardano-wallet" args
    T.unpack ep `shouldContain` cmdOk
    _ <- expectValidJSON (Proxy @(ApiTransaction n)) op
    cp `shouldBe` ExitSuccess

    eventually "Wallet balance is as expected" $ do
      Stdout og <- getWalletViaCLI ctx $ T.unpack (wDest ^. walletId)
      jg <- expectValidJSON (Proxy @ApiWallet) og
      verify
        jg
        [ expectCliField (#balance . #available) (`shouldBe` amount)
        , expectCliField (#balance . #total) (`shouldBe` amount)
        ]

    -- delete wallet
    Exit cd <- deleteWalletViaCLI ctx $ T.unpack (wDest ^. walletId)
    cd `shouldBe` ExitSuccess

    -- restore wallet from account public key
    let
      accXPub = pubKeyFromMnemonics' (words m)
    (Exit c2, Stdout o2, Stderr e2) <-
      createWalletFromPublicKeyViaCLI ctx [restoredWalletName, accXPub]
    c2 `shouldBe` ExitSuccess
    e2 `shouldContain` cmdOk
    wRestored <- expectValidJSON (Proxy @ApiWallet) o2

    -- make sure funds are there
    eventually "Wallet balance is as expected on wallet from pubKey" $ do
      Stdout o3 <- getWalletViaCLI ctx $ T.unpack (wRestored ^. walletId)
      justRestored <- expectValidJSON (Proxy @ApiWallet) o3
      verify
        justRestored
        [ expectCliField (#balance . #available) (`shouldBe` amount)
        , expectCliField (#balance . #total) (`shouldBe` amount)
        ]

  describe "HW_WALLETS_03 - Cannot do operations requiring private key" $ do
    it "Cannot send tx" $ \ctx -> runResourceT $ do
      -- create wallet from pubKey with funds
      (w, mnemonics) <- fixtureWalletWithMnemonics (Proxy @"shelley") ctx
      let
        pubKey = T.unpack $ pubKeyFromMnemonics mnemonics
      Exit cd <- deleteWalletViaCLI ctx $ T.unpack (w ^. walletId)
      cd `shouldBe` ExitSuccess

      (Exit c2, Stdout o2, Stderr e2) <-
        createWalletFromPublicKeyViaCLI ctx [restoredWalletName, pubKey]
      c2 `shouldBe` ExitSuccess
      e2 `shouldContain` cmdOk
      wRestored <- expectValidJSON (Proxy @ApiWallet) o2

      eventually "pubKey wallet is restored and has balance" $ do
        Stdout o3 <- getWalletViaCLI ctx $ T.unpack (wRestored ^. walletId)
        justRestored <- expectValidJSON (Proxy @ApiWallet) o3
        verify
          justRestored
          [ expectCliField
              (#balance . #available)
              (.> Quantity 0)
          ]

      -- make sure you cannot send tx from wallet
      wDest <- emptyWallet ctx
      addrs : _ <- listAddresses @n ctx wDest
      let
        addr = encodeAddress (sNetworkId @n) (apiAddress $ addrs ^. #id)

      let
        amt = T.pack . show . minUTxOValue . _mainEra $ ctx
      let
        args =
          T.unpack
            <$> [ wRestored ^. walletId
                , "--payment"
                , amt <> "@" <> addr
                ]

      (c, out, err) <- postTransactionViaCLI ctx (T.unpack fixturePassphrase) args
      (T.unpack err)
        `shouldContain` errMsg403NoRootKey (wRestored ^. walletId)
      out `shouldBe` ""
      c `shouldBe` ExitFailure 1

    it "Cannot update pass" $ \ctx -> runResourceT $ do
      w <- emptyWalletFromPubKeyViaCLI ctx restoredWalletName

      let
        pass = "cardano-wallet"
      let
        wid = T.unpack $ w ^. walletId
      -- cannot update pass
      (exitCode, out, err) <-
        updateWalletPassphraseViaCLI ctx wid pass pass pass
      out `shouldBe` ""
      T.unpack err `shouldContain` errMsg403NoRootKey (w ^. walletId)
      exitCode `shouldBe` ExitFailure 1

  describe "HW_WALLETS_04 - Can manage HW wallet the same way as others" $ do
    it "Can update name" $ \ctx -> runResourceT $ do
      w <- emptyWalletFromPubKeyViaCLI ctx restoredWalletName

      -- can update wallet name
      let
        n = "new name"
      let
        args = T.unpack <$> [w ^. walletId, n]
      (Exit code, Stdout out, Stderr err) <-
        updateWalletNameViaCLI ctx args
      code `shouldBe` ExitSuccess
      err `shouldBe` cmdOk
      j <- expectValidJSON (Proxy @ApiWallet) out
      expectCliField
        (#name . #getApiT . #getWalletName)
        (`shouldBe` n)
        j

    it "Can get tx fee" $ \ctx -> runResourceT $ do
      -- create wallet from pubKey with funds
      (w, mnemonics) <- fixtureWalletWithMnemonics (Proxy @"shelley") ctx
      let
        pubKey = T.unpack $ pubKeyFromMnemonics mnemonics
      Exit cd <- deleteWalletViaCLI ctx $ T.unpack (w ^. walletId)
      cd `shouldBe` ExitSuccess

      (Exit c1, Stdout o1, Stderr e1) <-
        createWalletFromPublicKeyViaCLI ctx [restoredWalletName, pubKey]
      c1 `shouldBe` ExitSuccess
      e1 `shouldContain` cmdOk
      wRestored <- expectValidJSON (Proxy @ApiWallet) o1

      eventually "Wallet has funds" $ do
        Stdout og <- getWalletViaCLI ctx $ T.unpack (wRestored ^. walletId)
        expectValidJSON (Proxy @ApiWallet) og
          >>= flip
            verify
            [ expectCliField
                (#balance . #available)
                (.> Quantity 0)
            ]

      -- get fee
      wDest <- emptyWallet ctx
      addrs : _ <- listAddresses @n ctx wDest
      let
        addr = encodeAddress (sNetworkId @n) (apiAddress $ addrs ^. #id)
      let
        amt = minUTxOValue (_mainEra ctx)
      let
        args =
          T.unpack
            <$> [ wRestored ^. walletId
                , "--payment"
                , T.pack (show amt) <> "@" <> addr
                ]
      (Exit code, Stdout out, Stderr err) <- postTransactionFeeViaCLI ctx args
      err `shouldBe` cmdOk
      txJson <- expectValidJSON (Proxy @ApiFee) out
      verify
        txJson
        [ expectCliField (#estimatedMin . #getQuantity) (.> 0)
        , expectCliField (#estimatedMax . #getQuantity) (.> 0)
        ]
      code `shouldBe` ExitSuccess

    it "Can delete" $ \ctx -> runResourceT $ do
      w <- emptyWalletFromPubKeyViaCLI ctx restoredWalletName
      (Exit cd, Stdout od, Stderr ed) <-
        deleteWalletViaCLI ctx $ T.unpack (w ^. walletId)
      cd `shouldBe` ExitSuccess
      ed `shouldContain` cmdOk
      od `shouldBe` "\n"

    it "Can see utxo" $ \ctx -> runResourceT $ do
      w <- emptyWalletFromPubKeyViaCLI ctx restoredWalletName
      (Exit c, Stdout o, Stderr e) <-
        getWalletUtxoStatisticsViaCLI ctx $ T.unpack (w ^. walletId)
      c `shouldBe` ExitSuccess
      e `shouldBe` cmdOk
      utxoStats <- expectValidJSON (Proxy @ApiUtxoStatistics) o
      expectWalletUTxO [] (Right utxoStats)

    it "Can list addresses" $ \ctx -> runResourceT $ do
      w <- emptyWalletFromPubKeyViaCLI ctx restoredWalletName

      let
        g = fromIntegral $ getAddressPoolGap defaultAddressPoolGap
      (Exit c, Stdout out, Stderr err) <-
        listAddressesViaCLI ctx [T.unpack (w ^. walletId)]
      err `shouldBe` "Ok.\n"
      c `shouldBe` ExitSuccess
      json <- expectValidJSON (Proxy @[ApiAddressWithPath n]) out
      length json `shouldBe` g
      forM_ [0 .. (g - 1)] $ \addrNum -> do
        expectCliListField
          addrNum
          (#state . #getApiT)
          (`shouldBe` Unused)
          json

    it "Can have address pool gap" $ \ctx -> runResourceT $ do
      Stdout m <- generateMnemonicsViaCLI []
      let
        accXPub = pubKeyFromMnemonics' (words m)
      let
        addrPoolGap = 55 -- arbitrary but known
      let
        args =
          [ restoredWalletName
          , "--address-pool-gap"
          , show addrPoolGap
          , accXPub
          ]
      (Exit c, Stdout out, Stderr err) <-
        createWalletFromPublicKeyViaCLI ctx args
      c `shouldBe` ExitSuccess
      err `shouldContain` cmdOk
      j <- expectValidJSON (Proxy @ApiWallet) out
      expectCliField
        (#addressPoolGap . #getApiT . #getAddressPoolGap)
        (`shouldBe` addrPoolGap)
        j

    it "Can list transactions" $ \ctx -> runResourceT $ do
      w <- emptyWalletFromPubKeyViaCLI ctx restoredWalletName
      (Exit code, Stdout out, Stderr err) <-
        listTransactionsViaCLI
          ctx
          TxMetadataDetailedSchema
          [T.unpack $ w ^. walletId]
      err `shouldBe` cmdOk
      out `shouldBe` "[]\n"
      code `shouldBe` ExitSuccess

  describe "HW_WALLETS_05 - Wallet from pubKey is available" $ do
    it "The same account and mnemonic wallet can live side-by-side" $ \ctx -> runResourceT $ do
      Stdout m <- generateMnemonicsViaCLI []
      let
        pubKeyWalName = "pub key wallet"
      let
        mnemonicWalName = "mnemonic wallet"
      -- create wallet from mnemonics
      (c1, o1, e1) <-
        createWalletViaCLI ctx [mnemonicWalName] m "\n" "secure-passphrase"
      c1 `shouldBe` ExitSuccess
      T.unpack e1 `shouldContain` cmdOk
      mnemonicWal <- expectValidJSON (Proxy @ApiWallet) o1

      -- create wallet from pub key
      let
        accXPub = pubKeyFromMnemonics' (words m)
      (Exit c2, Stdout o2, Stderr e2) <-
        createWalletFromPublicKeyViaCLI ctx [pubKeyWalName, accXPub]
      c2 `shouldBe` ExitSuccess
      e2 `shouldContain` cmdOk
      pubKeyWal <- expectValidJSON (Proxy @ApiWallet) o2

      (Exit c, Stdout out, Stderr err) <- listWalletsViaCLI ctx
      wids <- map (view walletId) <$> expectValidJSON (Proxy @[ApiWallet]) out
      c `shouldBe` ExitSuccess
      err `shouldBe` cmdOk
      wids `shouldContain` [mnemonicWal ^. walletId]
      wids `shouldContain` [pubKeyWal ^. walletId]

  describe "HW_WALLETS_06 - Test parameters" $ do
    describe "Wallet names valid" $ do
      forM_ walletNames $ \(title, n) -> it title $ \ctx -> runResourceT $ do
        j <- emptyWalletFromPubKeyViaCLI ctx n
        expectCliField
          (#name . #getApiT . #getWalletName)
          (`shouldBe` T.pack n)
          j
    describe "Wallet names invalid" $ do
      forM_ walletNamesInvalid $ \(name, expects) -> it expects $ \ctx -> runResourceT $ do
        Stdout m <- generateMnemonicsViaCLI []
        let
          accXPub = pubKeyFromMnemonics' (words m)
        (Exit c, Stdout o, Stderr e) <-
          createWalletFromPublicKeyViaCLI ctx [name, accXPub]
        c `shouldBe` ExitFailure 1
        e `shouldContain` expects
        o `shouldBe` mempty
    describe "Pub Key invalid" $ do
      let
        pubKeysInvalid = ["", "1", replicate 128 'ś', replicate 129 '1']
      forM_ pubKeysInvalid $ \key -> it ("Pub key: " ++ key) $ \ctx -> runResourceT $ do
        (Exit c, Stdout o, Stderr e) <-
          createWalletFromPublicKeyViaCLI ctx [restoredWalletName, key]
        c `shouldBe` ExitFailure 1
        e
          `shouldContain` "Invalid account public key: expecting a hex-encoded value\
                          \ that is 64 bytes in length."
        o `shouldBe` mempty
    describe "Address pool gap invalid" $ do
      let
        addrPoolMin = fromIntegral @_ @Int $ getAddressPoolGap minBound
      let
        addrPoolMax = fromIntegral @_ @Int $ getAddressPoolGap maxBound

      let
        poolGapsInvalid = [-1, 0, addrPoolMin - 1, addrPoolMax + 1]
      forM_ poolGapsInvalid $ \pGap -> it ("Pool gap: " ++ show pGap) $ \ctx -> runResourceT $ do
        Stdout m <- generateMnemonicsViaCLI []
        let
          accXPub = pubKeyFromMnemonics' (words m)
        (Exit c, Stdout o, Stderr e) <-
          createWalletFromPublicKeyViaCLI
            ctx
            [ restoredWalletName
            , "--address-pool-gap"
            , show pGap
            , accXPub
            ]
        c `shouldBe` ExitFailure 1
        e
          `shouldContain` "option --address-pool-gap: An address pool gap must be a\
                          \ natural number between 10 and 100000."
        o `shouldBe` mempty

emptyWalletFromPubKeyViaCLI
  :: Context
  -> String
  -> ResourceT IO ApiWallet
emptyWalletFromPubKeyViaCLI ctx name = do
  Stdout m <- generateMnemonicsViaCLI []
  let
    accXPub = pubKeyFromMnemonics' (words m)
  (Exit c, Stdout o, Stderr e) <-
    createWalletFromPublicKeyViaCLI ctx [name, accXPub]
  c `shouldBe` ExitSuccess
  e `shouldContain` cmdOk
  expectValidJSON (Proxy @ApiWallet) o

pubKeyFromMnemonics' :: [String] -> String
pubKeyFromMnemonics' m = T.unpack $ pubKeyFromMnemonics (T.pack <$> m)

restoredWalletName :: String
restoredWalletName = "Wallet from pub key"
