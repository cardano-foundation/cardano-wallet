{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Test.Integration.Framework.DSL.Wallet
    ( createARandomWalletWithMnemonics
    , createWalletFromMnemonics
    , createARandomWallet
    , waitUntilStateIsReady
    , balanceIs
    , statusIs
    , Patch
    , deleteWallet
    , named
    , fundWallet
    , withApiWallet
    , xPubOfMnemonics
    , createWalletFromXPub
    , withRestorationMode
    ) where

import Prelude

import Cardano.Address.Derivation
    ( XPub
    , toXPub
    )
import Cardano.Mnemonic
    ( SomeMnemonic
    )
import Cardano.Wallet.Api.Clients.Testnet.Id
    ( Testnet42
    )
import Cardano.Wallet.Api.Types
    ( AccountPostData (..)
    , AddressAmount (..)
    , ApiAccountPublicKey (..)
    , ApiMnemonicT (..)
    , ApiT (..)
    , ApiTxId (..)
    , ApiWallet
    , PostTransactionOldData (..)
    , WalletOrAccountPostData (..)
    , WalletPostData (..)
    )
import Cardano.Wallet.Api.Types.Amount
    ( ApiAmount (..)
    )
import Cardano.Wallet.Api.Types.WalletAssets
    ( ApiWalletAssets (..)
    )
import Cardano.Wallet.Faucet
    ( Faucet (nextShelleyMnemonic)
    )
import Cardano.Wallet.Network.RestorationMode
    ( RestorationMode
    )
import Cardano.Wallet.Primitive.SyncProgress
    ( SyncProgress (..)
    )
import Cardano.Wallet.Primitive.Types
    ( WalletId
    , WalletName
    )
import Cardano.Wallet.Primitive.Types.Tx.TxMeta
    ( TxStatus (InLedger)
    )
import Cardano.Wallet.Unsafe
    ( unsafeFromText
    )
import Control.Lens
    ( view
    )
import Control.Monad.Reader
    ( MonadIO (..)
    , MonadReader (..)
    , asks
    , lift
    , withReaderT
    )
import Data.Generics.Internal.VL
    ( (.~)
    , (^.)
    )
import Data.Generics.Product
    ( HasField
    )
import Data.Text
    ( Text
    )
import Numeric.Natural
    ( Natural
    )
import Servant.Client
    ( ClientError
    )
import Test.Integration.Framework.DSL
    ( Context (..)
    , eventually
    , fixturePassphrase
    , shouldBe
    )
import Test.Integration.Framework.DSL.TestM
    ( Over
    , TestM
    , check
    , over
    , pattern Partial
    , request
    )

import qualified Cardano.Address.Style.Shelley as Address
import qualified Cardano.Faucet.Mnemonics as Mnemonics
import qualified Cardano.Wallet.Api.Clients.Testnet.Shelley as C

type AWallet = ApiT WalletId

type Patch a = a -> a

createWalletFromXPub
    :: XPub
    -> Patch AccountPostData
    -> TestM (Either ClientError AWallet)
createWalletFromXPub xpub refine = do
    apiWallet' <-
        request
            $ C.postWallet
            $ WalletOrAccountPostData
            $ Right
            $ refine
            $ AccountPostData
                { name = ApiT $ unsafeFromText "Wallet from mnemonic"
                , accountPublicKey = ApiAccountPublicKey $ ApiT xpub
                , addressPoolGap = Nothing
                , restorationMode = Nothing
                }
    pure $ fmap (view #id) apiWallet'

createWalletFromMnemonics
    :: SomeMnemonic
    -> Patch WalletPostData
    -> TestM (Either ClientError AWallet)
createWalletFromMnemonics m15 refine = do
    apiWallet' <-
        request
            $ C.postWallet
            $ WalletOrAccountPostData
            $ Left
            $ refine
            $ WalletPostData
                { addressPoolGap = Nothing
                , mnemonicSentence = ApiMnemonicT m15
                , mnemonicSecondFactor = Nothing
                , name = ApiT $ unsafeFromText "Wallet from mnemonic"
                , passphrase = ApiT $ unsafeFromText fixturePassphrase
                , oneChangeAddressMode = Nothing
                , restorationMode = Nothing
                }
    pure $ fmap (view #id) apiWallet'

createARandomWallet
    :: Patch WalletPostData -> TestM (Either ClientError AWallet)
createARandomWallet refine = fmap fst <$> createARandomWalletWithMnemonics refine

xPubOfMnemonics :: SomeMnemonic -> XPub
xPubOfMnemonics mnemonic =
    let rootKey = Address.genMasterKeyFromMnemonic mnemonic mempty
        accKey = Address.deriveAccountPrivateKey rootKey minBound
    in  toXPub $ Address.getKey accKey

createARandomWalletWithMnemonics
    :: Patch WalletPostData
    -- ^ Refine the wallet data
    -> TestM (Either ClientError (AWallet, SomeMnemonic))
createARandomWalletWithMnemonics refine = do
    m15 <- Mnemonics.generateSome Mnemonics.M15
    w <- createWalletFromMnemonics m15 refine
    pure $ (,m15) <$> w

deleteWallet :: Over AWallet ()
deleteWallet = do
    w <- ask
    Partial _ <- lift $ request $ C.deleteWallet w
    pure ()

waitUntilStateIsReady :: Over AWallet ()
waitUntilStateIsReady =
    eventually "Wallet state is ready" $ withApiWallet $ statusIs Ready

balanceIs :: Natural -> Over ApiWallet ()
balanceIs q = check
    $ \w -> w ^. #balance . #available . #toNatural `shouldBe` q

statusIs :: SyncProgress -> Over ApiWallet ()
statusIs expected = check
    $ \w -> w ^. #state . #getApiT `shouldBe` expected

named
    :: HasField "name" a a b (ApiT WalletName)
    => Text
    -> Patch a
named name' = #name .~ ApiT (unsafeFromText @WalletName name')

withRestorationMode
    :: HasField "restorationMode" a a b (Maybe (ApiT RestorationMode))
    => RestorationMode
    -> Patch a
withRestorationMode rm = #restorationMode .~ Just (ApiT rm)

aFaucetWallet :: TestM AWallet
aFaucetWallet = do
    faucet <- asks _faucet
    faucetMnemonic <- liftIO $ nextShelleyMnemonic faucet
    Partial faucetWalletId <-
        createWalletFromMnemonics faucetMnemonic $ named "Faucet wallet"
    over faucetWalletId waitUntilStateIsReady
    pure faucetWalletId

fundWallet :: Natural -> Over AWallet ()
fundWallet amt = do
    w <- ask
    lift $ do
        faucetWalletId <- aFaucetWallet
        Partial addrs <- request $ C.listAddresses w Nothing
        over faucetWalletId $ do
            let destination = case addrs of (a:_) -> a ^. #id; [] -> error "expected addresses"
                addressAmount =
                    AddressAmount
                        { address = destination
                        , amount = ApiAmount amt
                        , assets = ApiWalletAssets []
                        }
                payload =
                    PostTransactionOldData
                        { payments = pure addressAmount
                        , passphrase = ApiT $ unsafeFromText fixturePassphrase
                        , withdrawal = Nothing
                        , metadata = Nothing
                        , timeToLive = Nothing
                        }
            submitTx payload

-- | Submit a transaction and wait for it to be on the ledger
submitTx :: PostTransactionOldData Testnet42 -> Over AWallet ()
submitTx payload = do
    w <- ask
    lift $ do
        Partial tx <- request $ C.postTransaction w payload
        eventually "Transaction didn't make it" $ do
            Partial tx' <-
                request $ C.getTransaction w (ApiTxId $ tx ^. #id) False
            lift $ tx' ^. #status . #getApiT `shouldBe` InLedger

withApiWallet :: Over ApiWallet a -> Over AWallet a
withApiWallet action = do
    w <- ask
    Partial r <- lift $ request $ C.getWallet w
    withReaderT (const r) action
