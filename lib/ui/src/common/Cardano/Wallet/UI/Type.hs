{-# LANGUAGE RankNTypes #-}

module Cardano.Wallet.UI.Type
where

import Prelude

import Control.Monad.Morph
    ( MFunctor (..)
    )
import Control.Monad.Reader
    ( MonadReader (..)
    , Reader
    , runReader
    )
import Data.Functor.Identity
    ( Identity (..)
    )
import Lucid
    ( Html
    , HtmlT
    )

data WalletType = Deposit | Shelley

type WHtml a = HtmlT (Reader WalletType) a

runWHtml :: WalletType -> WHtml a -> Html a
runWHtml t = hoist (Identity . (`runReader` t))

onDeposit :: WHtml () -> WHtml ()
onDeposit h = do
    t <- ask
    case t of
        Deposit -> h
        Shelley -> pure ()

onShelley :: WHtml () -> WHtml ()
onShelley h = do
    t <- ask
    case t of
        Deposit -> pure ()
        Shelley -> h
