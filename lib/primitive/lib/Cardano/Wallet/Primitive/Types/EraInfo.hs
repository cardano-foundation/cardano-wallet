{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module Cardano.Wallet.Primitive.Types.EraInfo
    ( EraInfo (..)
    , emptyEraInfo
    )
where

import Prelude

import Cardano.Wallet.Primitive.Types.EpochNo
    ( EpochNo
    )
import Control.DeepSeq
    ( NFData
    )
import Fmt
    ( Buildable (..)
    , blockListF'
    )
import GHC.Generics
    ( Generic
    )

-- | Represents 'info' about the starting epoch/time of all possible eras.
--
-- Field values can be either:
-- - Just pastEpochBoundary - the network forked to this era in the past.
-- - Just futureEpochBoundary - the hard-fork to this era is confirmed, but it
--                              hasn't yet occured.
-- - Nothing - the hard-fork to this era is not yet confirmed.
--
-- Note: this type is not a practical way to tell what the current era is.
--
-- It is expected that there is an order, @byron, shelley, allegra, mary@, by
-- which the @Maybe@ fields are filled in.
--
-- It might be cumbersome to work with this type. /But/ we don't need to. A
-- product of @Maybe@ is both what we can query from the node, and
-- what we need to provide in the wallet API.
data EraInfo info = EraInfo
    { byron :: Maybe info
    , shelley :: Maybe info
    , allegra :: Maybe info
    , mary :: Maybe info
    , alonzo :: Maybe info
    , babbage :: Maybe info
    }
    deriving (Eq, Generic, Show, Functor)

emptyEraInfo :: EraInfo info
emptyEraInfo = EraInfo Nothing Nothing Nothing Nothing Nothing Nothing

instance NFData info => NFData (EraInfo info)

instance Buildable (EraInfo EpochNo) where
    build (EraInfo byron' shelley' allegra' mary' alonzo' babbage') =
        blockListF'
            "-"
            id
            [ "byron" <> boundF byron'
            , "shelley" <> boundF shelley'
            , "allegra" <> boundF allegra'
            , "mary" <> boundF mary'
            , "alonzo" <> boundF alonzo'
            , "babbage" <> boundF babbage'
            ]
      where
        boundF (Just e) = " from " <> build e
        boundF Nothing = " <not started>"
