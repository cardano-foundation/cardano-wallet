{-# LANGUAGE LambdaCase #-}

module Cardano.Wallet.Api.ServerSpec (spec) where

import Prelude

import Cardano.Wallet.Api.Server
    ( Listen (..), ListenError (..), withListeningSocket )
import Network.Socket
    ( SockAddr (..), getSocketName, tupleToHostAddress )
import Test.Hspec
    ( Spec, describe, it, shouldBe, shouldReturn )
import Test.Utils.Windows
    ( skipOnWindows )

spec :: Spec
spec = describe "API Server" $ do
    it "listens on the local interface" $ do
        withListeningSocket "127.0.0.1" ListenOnRandomPort $ \case
            Right (port, socket) -> do
                let lo = tupleToHostAddress (0x7f, 0, 0, 1)
                getSocketName socket `shouldReturn`
                    SockAddrInet (fromIntegral port) lo
            Left e -> fail (show e)

    it "can listen on any interface" $ do
        withListeningSocket "0.0.0.0" ListenOnRandomPort $ \case
            Right (port, socket) -> do
                getSocketName socket `shouldReturn`
                    SockAddrInet (fromIntegral port) 0
            Left e -> fail (show e)

    -- assuming there is no host "patate"
    it "handles bad host name" $ do
        withListeningSocket "patate" ListenOnRandomPort $ \res ->
            res `shouldBe` Left (ListenErrorHostDoesNotExist "patate")

    -- can't bind to link-local IPv6 address
    it "handles invalid address" $ do
        withListeningSocket "fe80::90c2:786f:431:b721" ListenOnRandomPort $ \res ->
            res `shouldBe` Left (ListenErrorInvalidAddress "fe80::90c2:786f:431:b721")

    -- assuming we are not running the tests as root
    it "handles privileged ports" $ do
        skipOnWindows "Impossible to uniquely detect this error case"
        withListeningSocket "127.0.0.1" (ListenOnPort 23) $ \res ->
            res `shouldBe` Left ListenErrorOperationNotPermitted

    it "handles port in use" $ do
        withListeningSocket "127.0.0.1" ListenOnRandomPort $ \case
            Right (port, _) ->
                withListeningSocket "127.0.0.1" (ListenOnPort port) $ \res ->
                    res `shouldBe` Left (ListenErrorAddressAlreadyInUse (Just port))
            Left e -> fail (show e)
