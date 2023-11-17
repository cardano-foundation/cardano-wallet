{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}

module Cardano.Wallet.Primitive.Types.ExecutionUnitPrices
    ( ExecutionUnitPrices (..)
    )
where

import Prelude

import Control.DeepSeq
    ( NFData
    )
import Data.Aeson
    ( FromJSON (parseJSON)
    , KeyValue ((.=))
    , ToJSON (toJSON)
    , Value
    , object
    , withObject
    , (.:)
    )
import Data.Scientific
    ( fromRationalRepetendLimited
    )
import Fmt
    ( Buildable (..)
    )
import GHC.Generics
    ( Generic
    )

data ExecutionUnitPrices = ExecutionUnitPrices
    { pricePerStep :: Rational
    , pricePerMemoryUnit :: Rational
    }
    deriving (Eq, Generic, Show)

instance NFData ExecutionUnitPrices

instance Buildable ExecutionUnitPrices where
    build ExecutionUnitPrices{pricePerStep, pricePerMemoryUnit} =
        build
            $ mconcat
                [ show pricePerStep
                , " per step, "
                , show pricePerMemoryUnit
                , " per memory unit"
                ]

instance ToJSON ExecutionUnitPrices where
    toJSON ExecutionUnitPrices{pricePerStep, pricePerMemoryUnit} =
        object
            [ "step_price"
                .= toRationalJSON pricePerStep
            , "memory_unit_price"
                .= toRationalJSON pricePerMemoryUnit
            ]
      where
        toRationalJSON :: Rational -> Value
        toRationalJSON r = case fromRationalRepetendLimited 20 r of
            Right (s, Nothing) -> toJSON s
            _ -> toJSON r

instance FromJSON ExecutionUnitPrices where
    parseJSON = withObject "ExecutionUnitPrices" $ \o ->
        ExecutionUnitPrices <$> o .: "step_price" <*> o .: "memory_unit_price"
