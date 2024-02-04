{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.UI.Handlers.Wallet where

import Prelude hiding
    ( lookup
    )

import Cardano.Mnemonic
    ( MkSomeMnemonic (mkSomeMnemonic)
    )
import Cardano.Wallet.Address.Derivation.Shelley
    ( ShelleyKey (..)
    )
import Cardano.Wallet.Address.Discovery.Sequential
    ( SeqState
    )
import Cardano.Wallet.Api
    ( ApiLayer
    , PostData
    )
import Cardano.Wallet.Api.Types
    ( AllowedMnemonics
    , ApiMnemonicT (..)
    , ApiT (..)
    , ApiWallet
    , WalletOrAccountPostData (WalletOrAccountPostData)
    , WalletPostData (..)
    , WalletStyle (..)
    )
import Cardano.Wallet.Primitive.NetworkId
    ( HasSNetworkId (..)
    )
import Cardano.Wallet.Primitive.Types
    ( WalletId
    )
import Cardano.Wallet.UI.Handlers.Lib
    ( alerting
    , handleParseRequestError
    )
import Cardano.Wallet.UI.Html.Html
    ( RawHtml (..)
    )
import Cardano.Wallet.UI.Layer
    ( Push (..)
    , SessionLayer (..)
    , walletId
    )
import Control.Lens
    ( set
    , view
    , (^.)
    )
import Control.Monad
    ( replicateM
    )
import Control.Monad.Catch
    ( MonadCatch (..)
    , SomeException (..)
    )
import Control.Monad.Trans
    ( MonadIO (..)
    )
import Data.Aeson
    ( Value
    , decode
    , withObject
    )
import Data.Aeson.Types
    ( parseEither
    , (.:)
    )
import Data.Text
    ( Text
    )
import Data.Text.Class
    ( FromText (..)
    )
import Servant
    ( Handler
    , ServerError (..)
    , runHandler
    )
import System.Random.Stateful
    ( randomRIO
    )

import qualified Cardano.Wallet.Address.Derivation.Shelley as Shelley
import qualified Cardano.Wallet.Api.Http.Shelley.Server as Server
import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T

newWallet :: Text -> Text -> Text -> PostData ApiWallet
newWallet xs name' passphrase' =
    WalletOrAccountPostData
        $ Left
        $ WalletPostData
            Nothing
            (ApiMnemonicT $ fromRight $ mkSomeMnemonic @(AllowedMnemonics 'Shelley) $ T.words xs)
            Nothing
            (ApiT $ fromRight $ fromText name')
            (ApiT $ fromRight $ fromText passphrase')
            Nothing
            Nothing

postWallet
    :: HasSNetworkId n
    => SessionLayer
    -> (ApiLayer (SeqState n ShelleyKey))
    -> Value
    -> Handler RawHtml
postWallet SessionLayer{..} ctx v = do
    (mnemonic, name', password) <-
        handleParseRequestError
            $ parsePostWalletRequest v
    catch
        do
            ex <-
                liftIO
                    $ runHandler
                    $ Server.postWallet ctx Shelley.generateKeyFromSeed ShelleyKey
                    $ newWallet mnemonic name' password
            case ex of
                Left ServerError{..} ->
                    case decode errBody of
                        Nothing -> pure . RawHtml $ errBody
                        Just je -> pure . RawHtml . Aeson.encodePretty @Value $ je
                Right api -> do
                    liftIO $ do
                        sendSSE $ Sync "wallets"
                        update $ set walletId $ Just $ api ^. #id . #getApiT
                    pure $ RawHtml "Wallet created"
        do \(SomeException e) -> pure . RawHtml . BL.pack . show $ e

parsePostWalletRequest :: Value -> Either String (Text, Text, Text)
parsePostWalletRequest = parseEither
    . withObject "create wallet request"
    $ \o -> do
        mnemonic <- o .: "mnemonicSentence"
        name' <- o .: "name"
        password <- o .: "passphrase"
        pure (mnemonic, name', password)

fromRight :: Show a => Either a b -> b
fromRight (Right a) = a
fromRight (Left a) = error $ show a

pickMnemonic :: Int -> Maybe Bool -> IO (Maybe [Text])
pickMnemonic _n (Just True) = pure Nothing
pickMnemonic n _ = do
    dict <- fmap T.pack . words <$> readFile "specifications/mnemonic/english.txt"

    let loop = do
            xs <- replicateM n $ do
                i <- randomRIO (0, length dict - 1)
                pure $ dict !! i
            case mkSomeMnemonic @(AllowedMnemonics 'Shelley) xs of
                Left _ -> loop
                Right _ -> pure xs
    Just <$> loop

data UIWallet = UIWallet {id :: WalletId, name :: Text}

getWallet
    :: HasSNetworkId n
    => SessionLayer
    -> ApiLayer (SeqState n ShelleyKey)
    -> (BL.ByteString -> RawHtml) -- alert
    -> (ApiWallet -> RawHtml)
    -> Handler RawHtml
getWallet SessionLayer{..} ctx alert render = do
    let handle (SomeException e) = pure . alert . BL.pack . show $ e
    flip catch handle $ do
        liftIO $ do
            mwid <- view walletId <$> state
            case mwid of
                Nothing -> do
                    ls <- runHandler $ Server.listWallets ctx Server.mkShelleyWallet
                    alerting alert ls $ \case
                        [] -> pure $ alert "No wallets found"
                        (w, _) : _ -> do
                            update $ set walletId $ Just $ w ^. #id . #getApiT
                            sendSSE $ Sync "wallets"
                            pure $ render w
                Just wid -> do
                    result <-
                        runHandler
                            $ Server.getWallet
                                ctx
                                Server.mkShelleyWallet
                            $ ApiT wid
                    alerting alert result $ \case
                        (w, _) -> pure $ render w
