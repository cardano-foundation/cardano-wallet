{-# LANGUAGE TemplateHaskell #-}

module Cardano.Wallet.Spec.Effect.Query where

import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Wallet.Common as WC
import qualified Wallet.Operations.DeleteWallet as DW
import qualified Wallet.Operations.ListWallets as LW
import qualified Wallet.Operations.PostWallet as PW

import Cardano.Wallet.Spec.Effect.Assert
    ( FxAssert, assert )
import Cardano.Wallet.Spec.Effect.Http
    ( FxHttp )
import Cardano.Wallet.Spec.Effect.Trace
    ( FxTrace, trace )
import Cardano.Wallet.Spec.Types
    ( Mnemonic (..), Wallet (..) )
import Effectful
    ( (:>), Eff, Effect )
import Effectful.Dispatch.Dynamic
    ( interpret, reinterpret )
import Effectful.Fail
    ( Fail )
import Effectful.State.Static.Local
    ( evalState, get, modify )
import Effectful.TH
    ( makeEffect )
import Network.HTTP.Client
    ( responseBody, responseStatus )
import Network.HTTP.Types
    ( created201, noContent204, ok200 )
import Prelude hiding
    ( evalState, get, gets, modify, trace )

data FxQuery :: Effect where
    ListKnownWallets :: FxQuery m (Set Wallet)
    CreateWalletFromMnemonic :: Text -> Mnemonic -> FxQuery m Wallet
    DeleteWallet :: Wallet -> FxQuery m ()

$(makeEffect ''FxQuery)

runQueryMock :: (FxTrace :> es) => Set Wallet -> Eff (FxQuery : es) a -> Eff es a
runQueryMock db0 = reinterpret (evalState db0) \_ -> \case
    ListKnownWallets -> do
        wallets <- get
        trace $ "Listing known wallets (" <> show (length wallets) <> ")"
        pure wallets
    CreateWalletFromMnemonic name (Mnemonic phrase) -> do
        let wallet =
                Wallet
                    { walletId = T.intercalate "." (toList phrase)
                    , walletName = name
                    }
        trace $ "Creating a wallet '" <> name <> "': " <> show wallet
        wallet <$ modify (Set.insert wallet)
    DeleteWallet wallet -> do
        trace $ "Deleting a wallet " <> show wallet
        modify (Set.delete wallet)

runQuery
    :: (FxHttp :> es, Fail :> es, FxAssert :> es, FxTrace :> es)
    => Eff (FxQuery : es) a
    -> Eff es a
runQuery = interpret \_ -> \case
    ListKnownWallets -> do
        resp <- WC.runWithConfiguration configuration LW.listWallets
        assert "ListKnownWallets response status"
            $ responseStatus resp == ok200
        let walletFromBody (LW.ListWalletsResponseBody200{..}) =
                Wallet
                    { walletId = listWalletsResponseBody200Id
                    , walletName = listWalletsResponseBody200Name
                    }
        knownWallets <- case responseBody resp of
            LW.ListWalletsResponse200 lwrbs ->
                pure $ Set.fromList $ map walletFromBody lwrbs
            respBody -> fail do
                "ListKnownWallets: unexpected response body: " <> show respBody
        trace $ "Listing known wallets (" <> show (length knownWallets) <> ")"
        pure knownWallets
    CreateWalletFromMnemonic name mnemonic -> do
        trace $ "Creating a wallet from mnemonic " <> show mnemonic
        resp <-
            WC.runWithConfiguration configuration
                $ PW.postWallet
                $ PW.PostWalletRequestBodyVariant1
                $ PW.mkPostWalletRequestBodyOneOf1
                    (toList (mnemonicWords mnemonic))
                    name
                    "Secure Passphrase"
        assert "PostWallet response status is 201 Created"
            $ responseStatus resp == created201
        case responseBody resp of
            PW.PostWalletResponse201 PW.PostWalletResponseBody201{..} -> do
                pure
                    Wallet
                        { walletId = postWalletResponseBody201Id
                        , walletName = postWalletResponseBody201Name
                        }
            respBody -> fail do
                "CreateWalletFromMnemonic: unexpected response body: "
                    <> show respBody
    DeleteWallet wallet@(Wallet{walletId}) -> do
        trace $ "Deleting a wallet " <> show wallet
        resp <- WC.runWithConfiguration configuration (DW.deleteWallet walletId)
        assert "DeleteWallet response status is 204 NoContent"
            $ responseStatus resp == noContent204
        case responseBody resp of
            DW.DeleteWalletResponse204 -> pass
            respBody -> fail do
                "DeleteWallet ("
                    <> show wallet
                    <> "): unexpected response body: "
                    <> show respBody

configuration :: WC.Configuration
configuration =
    WC.Configuration
        { WC.configBaseURL = "http://localhost:8090/v2"
        , WC.configSecurityScheme = WC.anonymousSecurityScheme
        , WC.configIncludeUserAgent = False
        , WC.configApplicationName = ""
        }
