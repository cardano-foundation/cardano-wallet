{-# LANGUAGE TemplateHaskell #-}

module Cardano.Wallet.Spec.Effect.Query where

import qualified Cardano.Wallet.Spec.Data.Mnemonic as Mnemonic
import qualified Cardano.Wallet.Spec.Data.Network.NodeStatus as NodeStatus
import qualified Cardano.Wallet.Spec.Data.WalletId as WalletId
import qualified Cardano.Wallet.Spec.Data.WalletName as WalletName
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as Set
import qualified Network.HTTP.Types as Http
import qualified Text.Show as TS
import qualified Wallet as W
import qualified Wallet.Common as WC
import qualified Wallet.Operations.DeleteWallet as DW
import qualified Wallet.Operations.GetWallet as GW
import qualified Wallet.Operations.ListWallets as LW
import qualified Wallet.Operations.PostWallet as PW

import Cardano.Wallet.Cli.Launcher
    ( walletInstanceApiUrl
    )
import Cardano.Wallet.Spec.Data.AdaBalance
    ( AdaBalance (..)
    )
import Cardano.Wallet.Spec.Data.Mnemonic
    ( Mnemonic
    )
import Cardano.Wallet.Spec.Data.Network.Info
    ( NetworkInfo (..)
    )
import Cardano.Wallet.Spec.Data.Network.NodeStatus
    ( NodeStatus (..)
    )
import Cardano.Wallet.Spec.Data.Wallet
    ( Wallet (..)
    )
import Cardano.Wallet.Spec.Data.WalletId
    ( WalletId (..)
    )
import Cardano.Wallet.Spec.Data.WalletName
    ( WalletName
    )
import Cardano.Wallet.Spec.Effect.Assert
    ( FxAssert
    , assert
    , assertEq
    )
import Cardano.Wallet.Spec.Effect.Http
    ( FxHttp
    )
import Cardano.Wallet.Spec.Effect.Trace
    ( FxTrace
    , trace
    )
import Cardano.Wallet.Spec.Network.Configured
    ( ConfiguredNetwork (..)
    )
import Effectful
    ( Eff
    , Effect
    , (:>)
    )
import Effectful.Dispatch.Dynamic
    ( interpret
    , reinterpret
    )
import Effectful.Error.Static
    ( runErrorNoCallStack
    )
import Effectful.Fail
    ( Fail
    )
import Effectful.State.Static.Local
    ( evalState
    , get
    , gets
    , modify
    )
import Effectful.TH
    ( makeEffect
    )
import Network.HTTP.Client
    ( responseBody
    , responseStatus
    )
import Network.HTTP.Types
    ( created201
    , noContent204
    , ok200
    )
import Prelude hiding
    ( evalState
    , get
    , gets
    , modify
    , trace
    )

data FxQuery :: Effect where
    ListKnownWallets :: FxQuery m (Set Wallet)
    CreateWalletFromMnemonic :: WalletName -> Mnemonic -> FxQuery m Wallet
    GetWallet :: WalletId -> FxQuery m Wallet
    DeleteWallet :: Wallet -> FxQuery m ()
    QueryNetworkInfo :: FxQuery m NetworkInfo

$(makeEffect ''FxQuery)

runQueryMock
    :: (FxTrace :> es, Fail :> es)
    => Set Wallet
    -> Eff (FxQuery : es) a
    -> Eff es a
runQueryMock db0 = reinterpret (evalState db0) \_ -> \case
    ListKnownWallets -> do
        wallets <- get
        trace $ "Listing known wallets (" <> show (length wallets) <> ")"
        pure wallets
    CreateWalletFromMnemonic name mnemonic -> do
        let wallet =
                Wallet
                    { walletId =
                        WalletId.fromNel
                            $ NE.intersperse "."
                            $ Mnemonic.toWords mnemonic
                    , walletName = name
                    , walletBalance =
                        AdaBalance
                            { adaAvailable = 0
                            , adaInRewards = 0
                            , adaTotal = 0
                            }
                    }
        trace
            $ fold
                [ "Creating a wallet '"
                , WalletName.toText name
                , "': "
                , show wallet
                ]
        wallet <$ modify (Set.insert wallet)
    GetWallet wid -> do
        trace $ "Getting a wallet " <> show wid
        gets (find ((== wid) . walletId) . Set.toList)
            >>= maybe
                (fail $ "GetWallet (" <> show wid <> ") not found by id")
                pure
    DeleteWallet wallet -> do
        trace $ "Deleting a wallet " <> show wallet
        modify (Set.delete wallet)
    QueryNetworkInfo -> do
        trace "Querying network info ..."
        pure NetworkInfo{nodeStatus = NodeIsSynced}

runQuery
    :: ( FxHttp :> es
       , Fail :> es
       , FxAssert :> es
       , FxTrace :> es
       )
    => ConfiguredNetwork
    -> Eff (FxQuery : es) a
    -> Eff es a
runQuery configuredNetwork = interpret \_ -> \case
    ListKnownWallets -> do
        resp <- runClient LW.listWallets
        assert "ListKnownWallets response status"
            $ responseStatus resp == ok200
        let walletFromBody (LW.ListWalletsResponseBody200{..}) =
                Wallet
                    { walletId =
                        WalletId.mkUnsafe listWalletsResponseBody200Id
                    , walletName =
                        WalletName.mkUnsafe listWalletsResponseBody200Name
                    , walletBalance =
                        AdaBalance
                            { adaAvailable =
                                fromIntegral
                                    ( LW.listWalletsResponseBody200BalanceAvailableQuantity
                                        ( LW.listWalletsResponseBody200BalanceAvailable
                                            listWalletsResponseBody200Balance
                                        )
                                    )
                            , adaInRewards =
                                fromIntegral
                                    ( LW.listWalletsResponseBody200BalanceRewardQuantity
                                        ( LW.listWalletsResponseBody200BalanceReward
                                            listWalletsResponseBody200Balance
                                        )
                                    )
                            , adaTotal =
                                fromIntegral
                                    ( LW.listWalletsResponseBody200BalanceTotalQuantity
                                        ( LW.listWalletsResponseBody200BalanceTotal
                                            listWalletsResponseBody200Balance
                                        )
                                    )
                            }
                    }
        knownWallets <- case responseBody resp of
            LW.ListWalletsResponse200 lwrbs ->
                pure $ Set.fromList $ map walletFromBody lwrbs
            respBody ->
                fail
                    $ "ListKnownWallets: unexpected response body: " <> show respBody
        trace $ "Listing known wallets (" <> show (length knownWallets) <> ")"
        pure knownWallets
    CreateWalletFromMnemonic name mnemonic -> do
        trace $ "Creating a wallet from mnemonic: " <> Mnemonic.toText mnemonic
        resp <-
            runClient
                $ PW.postWallet
                $ PW.PostWalletRequestBodyVariant1
                $ PW.mkPostWalletRequestBodyOneOf1
                    (toList (Mnemonic.toWords mnemonic))
                    (WalletName.toText name)
                    "Secure Passphrase"
        assertEq
            "PostWallet response status is 201 Created"
            (responseStatus resp)
            created201
        case responseBody resp of
            PW.PostWalletResponse201 PW.PostWalletResponseBody201{..} -> do
                pure
                    Wallet
                        { walletId =
                            WalletId.mkUnsafe postWalletResponseBody201Id
                        , walletName =
                            WalletName.mkUnsafe postWalletResponseBody201Name
                        , walletBalance =
                            AdaBalance
                                { adaAvailable =
                                    fromIntegral
                                        ( PW.postWalletResponseBody201BalanceAvailableQuantity
                                            ( PW.postWalletResponseBody201BalanceAvailable
                                                postWalletResponseBody201Balance
                                            )
                                        )
                                , adaInRewards =
                                    fromIntegral
                                        ( PW.postWalletResponseBody201BalanceRewardQuantity
                                            ( PW.postWalletResponseBody201BalanceReward
                                                postWalletResponseBody201Balance
                                            )
                                        )
                                , adaTotal =
                                    fromIntegral
                                        ( PW.postWalletResponseBody201BalanceTotalQuantity
                                            ( PW.postWalletResponseBody201BalanceTotal
                                                postWalletResponseBody201Balance
                                            )
                                        )
                                }
                        }
            respBody ->
                fail
                    $ "CreateWalletFromMnemonic: unexpected response body: "
                        <> show respBody
    GetWallet wid -> do
        trace $ "Getting a wallet " <> show wid
        resp <- runClient $ GW.getWallet (walletIdToText wid)
        assert "GetWallet response status is 200 Ok"
            $ responseStatus resp == ok200
        case responseBody resp of
            GW.GetWalletResponse200 GW.GetWalletResponseBody200{..} ->
                pure
                    Wallet
                        { walletId =
                            WalletId.mkUnsafe getWalletResponseBody200Id
                        , walletName =
                            WalletName.mkUnsafe getWalletResponseBody200Name
                        , walletBalance =
                            AdaBalance
                                { adaAvailable =
                                    fromIntegral
                                        ( GW.getWalletResponseBody200BalanceAvailableQuantity
                                            ( GW.getWalletResponseBody200BalanceAvailable
                                                getWalletResponseBody200Balance
                                            )
                                        )
                                , adaInRewards =
                                    fromIntegral
                                        ( GW.getWalletResponseBody200BalanceRewardQuantity
                                            ( GW.getWalletResponseBody200BalanceReward
                                                getWalletResponseBody200Balance
                                            )
                                        )
                                , adaTotal =
                                    fromIntegral
                                        ( GW.getWalletResponseBody200BalanceTotalQuantity
                                            ( GW.getWalletResponseBody200BalanceTotal
                                                getWalletResponseBody200Balance
                                            )
                                        )
                                }
                        }
            respBody ->
                fail
                    $ "GetWallet ("
                        <> show wid
                        <> "): unexpected response body: "
                        <> show respBody
    DeleteWallet wallet -> do
        trace $ "Deleting a wallet " <> show wallet
        resp <- runClient $ DW.deleteWallet (walletIdToText (walletId wallet))
        assert "DeleteWallet response status is 204 NoContent"
            $ responseStatus resp == noContent204
        case responseBody resp of
            DW.DeleteWalletResponse204 -> pass
            respBody ->
                fail
                    $ "DeleteWallet ("
                        <> show wallet
                        <> "): unexpected response body: "
                        <> show respBody
    QueryNetworkInfo -> do
        trace "Querying network info ..."
        W.getNetworkInformation
            & runClient
            & runErrorNoCallStack
            >>= \case
                Left (e :: SomeException) ->
                    environmentException
                        $ WalletNetworkInfoException configuredNetwork e
                Right resp | status <- responseStatus resp -> do
                    unless (status == ok200) do
                        environmentException
                            $ WalletNetworkInfoStatus configuredNetwork status
                    case responseBody resp of
                        W.GetNetworkInformationResponseError err ->
                            environmentException
                                $ WalletNetworkInfoError configuredNetwork (toText err)
                        W.GetNetworkInformationResponse406
                            W.GetNetworkInformationResponseBody406{..} ->
                                environmentException
                                    $ WalletNetworkInfoError
                                        configuredNetwork
                                        getNetworkInformationResponseBody406Message
                        W.GetNetworkInformationResponse200
                            W.GetNetworkInformationResponseBody200{..} -> do
                                nodeStatus <-
                                    NodeStatus.fromClientResponse getNetworkInformationResponseBody200Sync_progress
                                        & maybe
                                            ( environmentException
                                                $ WalletNetworkInfoUnknownNodeStatus configuredNetwork
                                            )
                                            pure
                                pure NetworkInfo{nodeStatus}
  where
    runClient :: WC.ClientT m a -> m a
    runClient =
        WC.runWithConfiguration
            WC.Configuration
                { WC.configBaseURL =
                    walletInstanceApiUrl (configuredNetworkWallet configuredNetwork)
                , WC.configSecurityScheme = WC.anonymousSecurityScheme
                , WC.configIncludeUserAgent = False
                , WC.configApplicationName = ""
                }

environmentException :: (Fail :> es) => ExecutionEnvironmentException -> Eff es a
environmentException = fail . displayException

data ExecutionEnvironmentException
    = WalletNetworkInfoException ConfiguredNetwork SomeException
    | WalletNetworkInfoStatus ConfiguredNetwork Http.Status
    | WalletNetworkInfoError ConfiguredNetwork Text
    | NodeIsNotReady ConfiguredNetwork
    | WalletNetworkInfoUnknownNodeStatus ConfiguredNetwork
    deriving anyclass (Exception)

instance Show ExecutionEnvironmentException where
    show = \case
        WalletNetworkInfoException configuredNetwork se ->
            requirement configuredNetwork
                <> "However, an exception happened when trying to retrieve \n\
                   \network information from the wallet backend: \n\n"
                <> displayException se
        WalletNetworkInfoUnknownNodeStatus configuredNetwork ->
            requirement configuredNetwork
        WalletNetworkInfoStatus configuredNetwork _ ->
            requirement configuredNetwork
        WalletNetworkInfoError configuredNetwork _ ->
            requirement configuredNetwork
        NodeIsNotReady configuredNetwork ->
            requirement configuredNetwork
      where
        requirement :: ConfiguredNetwork -> String
        requirement configuration =
            "E2E test suite requires a running cardano-wallet instance \n\
            \connected to a running cardano-node and listenting on "
                <> show
                    (walletInstanceApiUrl (configuredNetworkWallet configuration))
                <> "\n\n"
