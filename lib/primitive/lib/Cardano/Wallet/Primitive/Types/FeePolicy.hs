{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Cardano.Wallet.Primitive.Types.FeePolicy
    ( FeePolicy (..)
    , LinearFunction (..)
    )
where

import Prelude

import Control.Arrow
    ( left
    )
import Control.DeepSeq
    ( NFData
    )
import Data.Text.Class
    ( FromText (..)
    , TextDecodingError (TextDecodingError)
    , ToText (..)
    )
import GHC.Generics
    ( Generic
    )

import qualified Data.Text as T

-- | A linear equation of a free variable `x`. Represents the @\x -> a + b*x@
-- function where @x@ can be either a transaction size in bytes or
-- a number of inputs + outputs.
newtype FeePolicy = LinearFee (LinearFunction Double)
    deriving (Eq, Show, Generic)

instance NFData FeePolicy

instance ToText FeePolicy where
    toText (LinearFee LinearFunction{..}) =
        toText intercept
            <> " + "
            <> toText slope
            <> "x"

instance FromText FeePolicy where
    fromText txt = case T.splitOn " + " txt of
        [a, b]
            | T.takeEnd 1 b == "x" ->
                left (const err)
                    $ (LinearFee .) . LinearFunction
                        <$> fromText a
                        <*> fromText (T.dropEnd 1 b)
        _ -> Left err
      where
        err =
            TextDecodingError
                "Unable to decode FeePolicy: \
                \Linear equation not in expected format: a + bx \
                \where 'a' and 'b' are numbers"

data LinearFunction a = LinearFunction {intercept :: a, slope :: a}
    deriving (Eq, Show, Generic)

instance NFData a => NFData (LinearFunction a)
