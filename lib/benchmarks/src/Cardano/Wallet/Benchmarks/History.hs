{-# LANGUAGE DerivingStrategies #-}
module Cardano.Wallet.Benchmarks.History
    ( HarmonizedHistory (..)
    , History
    , IndexedSemantic (..)
    , HarmonizedRow (..)
    , harmonizeHistory
    , pastDays
    , renderHarmonizedHistoryCsv
    )
where

import Prelude

import Cardano.Wallet.Benchmarks.Collect
    ( Result (Result, resultUnits)
    , Semantic
    , Units (..)
    , convertUnits
    )
import Data.Bifunctor
    ( second
    )
import Data.Csv
    ( Header
    , ToNamedRecord (..)
    , namedRecord
    , (.=)
    )
import Data.Foldable
    ( Foldable (..)
    )
import Data.Functor
    ( (<&>)
    )
import Data.Map
    ( Map
    )
import Data.Map.Monoidal.Strict
    ( MonoidalMap (..)
    , assocs
    )
import Data.Semigroup
    ( First (..)
    )
import Data.Set
    ( Set
    )
import Data.Time
    ( Day
    , UTCTime (..)
    , addDays
    , getCurrentTime
    )

import qualified Data.ByteString.Char8 as B8
import qualified Data.Map as Map
import qualified Data.Map.Monoidal.Strict as MMap
import qualified Data.Text as T
import qualified Data.Vector as V

data IndexedSemantic = IndexedSemantic
    { semantic :: Semantic
    , index :: Int
    }
    deriving stock (Eq, Ord, Show)

type History = MonoidalMap IndexedSemantic (MonoidalMap Day (First Result))

data HarmonizedRow
    = Harmonized (Map Day Double) Units
    | FailedToHarmonize [(Day, Result)]
    deriving stock (Show)

harmonizeUnits :: [(Day, Result)] -> HarmonizedRow
harmonizeUnits rs =
    let
        u = biggestUnit $ fmap (resultUnits . snd) rs
    in
        case u of
            Nothing -> FailedToHarmonize rs
            Just u' ->
                Harmonized
                    (Map.fromList $ second (harmonizeResult u') <$> rs)
                    u'

-- dropping the iteration count ... for now
harmonizeResult :: Units -> Result -> Double
harmonizeResult u (Result v u' _i) = convertUnits u' u v

biggestUnit :: [Units] -> Maybe Units
biggestUnit = foldr go Nothing
  where
    go :: Units -> Maybe Units -> Maybe Units
    go u Nothing = Just u
    go u (Just u') = Just $ compareUnits u u'

compareUnits :: Units -> Units -> Units
compareUnits u u' = case (u, u') of
    (Seconds, Seconds) -> Seconds
    (Seconds, Milliseconds) -> Seconds
    (Seconds, Microseconds) -> Seconds
    (Seconds, Nanoseconds) -> Seconds
    (Milliseconds, Seconds) -> Seconds
    (Milliseconds, Milliseconds) -> Milliseconds
    (Milliseconds, Microseconds) -> Milliseconds
    (Milliseconds, Nanoseconds) -> Milliseconds
    (Microseconds, Seconds) -> Seconds
    (Microseconds, Milliseconds) -> Milliseconds
    (Microseconds, Microseconds) -> Microseconds
    (Microseconds, Nanoseconds) -> Microseconds
    (Nanoseconds, Seconds) -> Seconds
    (Nanoseconds, Milliseconds) -> Milliseconds
    (Nanoseconds, Microseconds) -> Microseconds
    (Nanoseconds, Nanoseconds) -> Nanoseconds
    (Bytes, Bytes) -> Bytes
    (Bytes, KiloBytes) -> KiloBytes
    (Bytes, MegaBytes) -> MegaBytes
    (Bytes, GigaBytes) -> GigaBytes
    (KiloBytes, Bytes) -> KiloBytes
    (KiloBytes, KiloBytes) -> KiloBytes
    (KiloBytes, MegaBytes) -> MegaBytes
    (KiloBytes, GigaBytes) -> GigaBytes
    (MegaBytes, Bytes) -> MegaBytes
    (MegaBytes, KiloBytes) -> MegaBytes
    (MegaBytes, MegaBytes) -> MegaBytes
    (MegaBytes, GigaBytes) -> GigaBytes
    (GigaBytes, Bytes) -> GigaBytes
    (GigaBytes, KiloBytes) -> GigaBytes
    (GigaBytes, MegaBytes) -> GigaBytes
    (GigaBytes, GigaBytes) -> GigaBytes
    (Count, Count) -> Count
    _ -> error "Invalid units"

data HarmonizedHistory = HarmonizedHistory
    { harmonizedHistory :: MonoidalMap IndexedSemantic HarmonizedRow
    , days :: Set Day
    }
    deriving stock (Show)

harmonizeDay :: MonoidalMap Day (First Result) -> HarmonizedRow
harmonizeDay = harmonizeUnits . fmap (second getFirst) . assocs

harmonizeHistory :: History -> HarmonizedHistory
harmonizeHistory h =
    HarmonizedHistory
        { harmonizedHistory = fmap harmonizeDay h
        , days = foldMap (Map.keysSet . getMonoidalMap) h
        }

pastDays :: Day -> IO [Day]
pastDays d = do
    today <- utctDay <$> getCurrentTime
    return $ reverse $ takeWhile (<= today) $ iterate (addDays 1) d

data Row = Row
    { rowSemantic :: Semantic
    , rowIndex :: Int
    , units :: Units
    , values :: [(Day, Maybe Double)]
    }
    deriving stock (Show)

instance ToNamedRecord Row where
    toNamedRecord (Row s i u vs) =
        namedRecord
            $ ("Semantic" .= s)
                : ("Index" .= T.pack (show i))
                : ("Units" .= u)
                : fmap (\(d, v) -> (B8.pack . show $ d) .= v) vs

renderHarmonizedHistoryCsv :: HarmonizedHistory -> (Header, [Row])
renderHarmonizedHistoryCsv (HarmonizedHistory h ds) = (header', rows)
  where
    header' =
        V.fromList
            $ "Semantic"
                : "Index"
                : "Units"
                : fmap (B8.pack . show) (toList ds)
    rows = do
        (IndexedSemantic s i, m) <- MMap.assocs h
        pure
            $ case m of
                FailedToHarmonize rs -> error $ "Failed to harmonize " <> show rs
                Harmonized m' u -> Row s i u $ do
                    toList ds <&> \d -> (d, Map.lookup d m')
