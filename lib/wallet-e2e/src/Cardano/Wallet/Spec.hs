module Cardano.Wallet.Spec
    ( walletSpec
    , effectsSpec
    ) where

import qualified Cardano.Wallet.Spec.Network.Local as Local
import qualified Cardano.Wallet.Spec.Network.Manual as Manual
import qualified Cardano.Wallet.Spec.Network.Preprod as Preprod

import Cardano.Wallet.Spec.Data.TestNetwork
    ( TestNetwork (..) )
import Cardano.Wallet.Spec.Interpreters.Effectfully
    ( story )
import Cardano.Wallet.Spec.Network.Config
    ( NetworkConfig )
import Cardano.Wallet.Spec.Stories.Wallet
    ( createdWalletHasZeroAda, createdWalletListed, createdWalletRetrievable )
import Cardano.Wallet.Spec.TimeoutSpec
    ( timeoutSpec )
import Path
    ( Abs, Dir, Path )
import Test.Syd
    ( Spec, aroundAll, describe, sequential )

walletSpec :: Path Abs Dir -> TestNetwork -> Spec
walletSpec stateDirectory network =
    aroundAll setupForNetwork do
        describe "Wallet Backend API" $ sequential do
            story "Created wallet is listed" createdWalletListed
            story "Created wallet can be retrieved by id" createdWalletRetrievable
            story "Created wallet has zero ADA balance" createdWalletHasZeroAda
  where
    setupForNetwork :: (NetworkConfig -> IO ()) -> IO ()
    setupForNetwork =
        case network of
            Manual -> Manual.nodeWalletSetup
            Local -> Local.nodeWalletSetup stateDirectory
            Preprod -> Preprod.nodeWalletSetup stateDirectory

effectsSpec :: Spec
effectsSpec = describe "Effect interpreters" do
    timeoutSpec
