{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.Primitive.Types.EpochNo
    ( EpochNo (..)
    , unsafeEpochNo
    , isValidEpochNo
    )
where

import Prelude

import Control.DeepSeq
    ( NFData (..)
    )
import Control.Monad
    ( (<=<)
    )
import Data.Text.Class
    ( FromText (..)
    , TextDecodingError (..)
    , ToText (..)
    )
import Data.Word
    ( Word32
    )
import Data.Word.Odd
    ( Word31
    )
import Fmt
    ( Buildable (..)
    )
import GHC.Generics
    ( Generic
    )
import GHC.Natural
    ( Natural
    )
import GHC.Stack
    ( HasCallStack
    )

import qualified Data.Text as T

newtype EpochNo = EpochNo {unEpochNo :: Word31}
    deriving stock (Show, Read, Eq, Ord, Generic)
    deriving newtype (Num, Bounded, Enum)

instance ToText EpochNo where
    toText = T.pack . show . unEpochNo

instance FromText EpochNo where
    fromText = validate <=< (fmap (EpochNo . fromIntegral) . fromText @Natural)
      where
        validate x
            | isValidEpochNo x =
                return x
            | otherwise =
                Left $ TextDecodingError "EpochNo value is out of bounds"

isValidEpochNo :: EpochNo -> Bool
isValidEpochNo c = c >= minBound && c <= maxBound

instance Buildable EpochNo where
    build (EpochNo e) = build $ fromIntegral @Word31 @Word32 e

instance NFData EpochNo where
    rnf (EpochNo !_) = ()

-- | Convert the specified value into an 'EpochNo', or fail if the value is
--   too large.
unsafeEpochNo :: HasCallStack => Word32 -> EpochNo
unsafeEpochNo epochNo
    | epochNo > maxEpochNo =
        error
            $ mconcat
                [ "unsafeEpochNo: epoch number ("
                , show epochNo
                , ") out of bounds ("
                , show (minBound @Word31)
                , ", "
                , show (maxBound @Word31)
                , ")."
                ]
    | otherwise =
        EpochNo $ fromIntegral epochNo
  where
    maxEpochNo :: Word32
    maxEpochNo = fromIntegral @Word31 $ unEpochNo maxBound
