{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Faucet.Http.Api.Utils where

import Prelude

import Data.OpenApi
    ( Reference (..)
    )
import Data.Text
    ( Text
    )
import Data.Typeable
    ( Proxy (..)
    , Typeable
    , typeRep
    )

import qualified Data.Text as T

typeRef :: forall t. Typeable t => Reference
typeRef = Reference (typeName @t)

typeName :: forall t. Typeable t => Text
typeName = T.pack (show (typeRep (Proxy @t)))
