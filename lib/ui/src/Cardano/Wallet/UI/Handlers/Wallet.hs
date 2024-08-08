{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
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
    ( evenWithNoWallet
    , handleParseRequestError
    , withWallet
    )
import Cardano.Wallet.UI.Layer
    ( Push (..)
    , SessionLayer (..)
    , walletId
    )
import Control.Lens
    ( set
    , (^.)
    )
import Control.Monad
    ( replicateM
    )
import Control.Monad.Trans
    ( MonadIO (..)
    )
import Data.Aeson
    ( Value
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
import Paths_cardano_wallet_ui
    ( getDataFileName
    )
import Servant
    ( Handler
    , NoContent
    )
import System.Random.Stateful
    ( randomRIO
    )

import qualified Cardano.Wallet.Address.Derivation.Shelley as Shelley
import qualified Cardano.Wallet.Api.Http.Shelley.Server as Server
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T

newWallet :: Text -> Text -> Text -> PostData ApiWallet
newWallet xs name' passphrase' =
    WalletOrAccountPostData
        $ Left
        $ WalletPostData
            Nothing
            ( ApiMnemonicT
                $ fromRight
                $ mkSomeMnemonic @(AllowedMnemonics 'Shelley)
                $ T.words xs
            )
            Nothing
            (ApiT $ fromRight $ fromText name')
            (ApiT $ fromRight $ fromText passphrase')
            Nothing
            Nothing

postWallet
    :: HasSNetworkId n
    => SessionLayer
    -> ApiLayer (SeqState n ShelleyKey)
    -> (BL.ByteString -> html) -- problem report
    -> (ApiWallet -> html) -- success report
    -> Value
    -> Handler html
postWallet SessionLayer{..} ctx alert render v = do
    (mnemonic, name', password) <-
        handleParseRequestError
            $ parsePostWalletRequest v
    liftIO $ do
        evenWithNoWallet alert render $ do
            r <- Server.postWallet ctx Shelley.generateKeyFromSeed ShelleyKey
                    $ newWallet mnemonic name' password
            liftIO $ do
                sendSSE $ Push "wallets"
                update $ set walletId $ Just $ r ^. #id . #getApiT
            pure r

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
    wordsFile <- getDataFileName "data/english.txt"
    dict <- fmap T.pack . words <$> readFile wordsFile

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
    => SessionLayer -- session provider
    -> ApiLayer (SeqState n ShelleyKey) -- api provider
    -> (BL.ByteString -> html) -- problem report
    -> (ApiWallet -> html) -- success report
    -> Handler html
getWallet layer ctx alert render = liftIO $ do
    withWallet layer alert render $ \wid ->
        fmap fst $ Server.getWallet ctx Server.mkShelleyWallet $ ApiT wid

deleteWallet
    :: SessionLayer
    -> ApiLayer (SeqState n ShelleyKey)
    -> (BL.ByteString -> html)
    -> (NoContent -> html)
    -> Handler html
deleteWallet layer ctx alert render = liftIO $ do
    withWallet layer alert render $ \wid -> do
        r <- Server.deleteWallet ctx $ ApiT wid
        liftIO $ do
            update layer $ set walletId Nothing
            sendSSE layer $ Push "wallets"
            sendSSE layer $ Push "wallet"
        pure r

selectWallet :: SessionLayer -> WalletId -> Handler ()
selectWallet SessionLayer{..} wid = liftIO $ do
    update $ set walletId $ Just wid
    sendSSE $ Push "wallet"
    sendSSE $ Push "wallets"
    sendSSE $ Push "settings"
