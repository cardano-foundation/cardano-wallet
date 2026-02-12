{-# LANGUAGE DerivingStrategies #-}

module Cardano.Wallet.Benchmarks.History
    ( HarmonizedHistory (..)
    , History
    , IndexedSemantic (..)
    , HarmonizedRow (..)
    , harmonizeHistory
    , pastDays
    , renderHarmonizedHistoryCsv
    , parseHistory
    , parseResults
    , historyFromResults
    )
where

import Cardano.Wallet.Benchmarks.Collect
    ( Benchmark (..)
    , Result (Result, resultUnit)
    , Semantic
    , Unit (..)
    , convertUnit
    )
import Control.Monad
    ( forM
    )
import Data.Bifunctor
    ( second
    )
import Data.Csv
    ( FromField (..)
    , FromNamedRecord (..)
    , Header
    , NamedRecord
    , Parser
    , ToNamedRecord (..)
    , decodeByName
    , namedRecord
    , (.:)
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
import Data.Maybe
    ( maybeToList
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
    , defaultTimeLocale
    , getCurrentTime
    , parseTimeM
    )
import Prelude

import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.HashMap.Strict as HMap
import qualified Data.Map as Map
import qualified Data.Map.Monoidal.Strict as MMap
import qualified Data.Text as T
import qualified Data.Vector as V

-- | A semantic, indexed by an integer which is the row number in the input CSV
-- file. The row number is a hack for same-semantics metrics.
data IndexedSemantic = IndexedSemantic
    { semantic :: Semantic
    , index :: Int
    }
    deriving stock (Eq, Ord, Show)

-- | A history of results, indexed by semantic and day.
type History =
    MonoidalMap IndexedSemantic (MonoidalMap Day (First Result))

-- | A row of unit-harmonized data, indexed by day.
data HarmonizedRow
    = Harmonized (Map Day Double) Unit
    deriving stock (Show)

-- | Unit-harmonize a list of results, returning the harmonized row if all results
-- have the same unit. Otherwise it fails with Nothing.
harmonizeUnit :: [(Day, Result)] -> Maybe HarmonizedRow
harmonizeUnit rs =
    let
        u = biggestUnit $ fmap (resultUnit . snd) rs
    in
        u <&> \u' ->
            Harmonized
                (Map.fromList $ second (harmonizeResult u') <$> rs)
                u'

-- dropping the iteration count ... for now
harmonizeResult :: Unit -> Result -> Double
harmonizeResult u (Result v u' _i) = convertUnit u' u v

-- | Find the biggest unit in a list of unit.
biggestUnit :: [Unit] -> Maybe Unit
biggestUnit = foldr go Nothing
  where
    go :: Unit -> Maybe Unit -> Maybe Unit
    go u Nothing = Just u
    go u (Just u') = compareUnit u u'

-- | Compare two unit and return the biggest one if they are compatible.
compareUnit :: Unit -> Unit -> Maybe Unit
compareUnit u u' = case (u, u') of
    (Seconds, Seconds) -> Just Seconds
    (Seconds, Milliseconds) -> Just Seconds
    (Seconds, Microseconds) -> Just Seconds
    (Seconds, Nanoseconds) -> Just Seconds
    (Milliseconds, Seconds) -> Just Seconds
    (Milliseconds, Milliseconds) -> Just Milliseconds
    (Milliseconds, Microseconds) -> Just Milliseconds
    (Milliseconds, Nanoseconds) -> Just Milliseconds
    (Microseconds, Seconds) -> Just Seconds
    (Microseconds, Milliseconds) -> Just Milliseconds
    (Microseconds, Microseconds) -> Just Microseconds
    (Microseconds, Nanoseconds) -> Just Microseconds
    (Nanoseconds, Seconds) -> Just Seconds
    (Nanoseconds, Milliseconds) -> Just Milliseconds
    (Nanoseconds, Microseconds) -> Just Microseconds
    (Nanoseconds, Nanoseconds) -> Just Nanoseconds
    (Bytes, Bytes) -> Just Bytes
    (Bytes, KiloBytes) -> Just KiloBytes
    (Bytes, MegaBytes) -> Just MegaBytes
    (Bytes, GigaBytes) -> Just GigaBytes
    (KiloBytes, Bytes) -> Just KiloBytes
    (KiloBytes, KiloBytes) -> Just KiloBytes
    (KiloBytes, MegaBytes) -> Just MegaBytes
    (KiloBytes, GigaBytes) -> Just GigaBytes
    (MegaBytes, Bytes) -> Just MegaBytes
    (MegaBytes, KiloBytes) -> Just MegaBytes
    (MegaBytes, MegaBytes) -> Just MegaBytes
    (MegaBytes, GigaBytes) -> Just GigaBytes
    (GigaBytes, Bytes) -> Just GigaBytes
    (GigaBytes, KiloBytes) -> Just GigaBytes
    (GigaBytes, MegaBytes) -> Just GigaBytes
    (GigaBytes, GigaBytes) -> Just GigaBytes
    (Count, Count) -> Just Count
    _ -> Nothing

-- | A history of unit-harmonized data, indexed by semantic and day.
-- The union of all days in the history is stored in the 'days' field.
data HarmonizedHistory = HarmonizedHistory
    { harmonizedHistory :: MonoidalMap IndexedSemantic HarmonizedRow
    , days :: Set Day
    }
    deriving stock (Show)

-- | Harmonize a row of results, returning the unit-harmonized row if all results
-- are harmonizable. Otherwise it fails with the list of unharmonizable results.
harmonizeDay
    :: MonoidalMap Day (First Result)
    -> Either [(Day, Result)] HarmonizedRow
harmonizeDay = f . fmap (second getFirst) . assocs
  where
    f rs = maybe (Left rs) Right $ harmonizeUnit rs

-- | Harmonize a history of results, returning the unit-harmonized history if all
-- rows are harmonizable. Otherwise it fails with the first unharmonizable row.
harmonizeHistory
    :: History
    -> Either [(Day, Result)] HarmonizedHistory
harmonizeHistory h =
    case traverse harmonizeDay h of
        Right h' ->
            Right
                $ HarmonizedHistory
                    { harmonizedHistory = h'
                    , days = foldMap (Map.keysSet . getMonoidalMap) h
                    }
        Left rs -> Left rs

-- | Generate a list of days from a given day up to today in reverse order.
pastDays :: Day -> IO [Day]
pastDays d = do
    today <- utctDay <$> getCurrentTime
    return $ reverse $ takeWhile (<= today) $ iterate (addDays 1) d

-- | A CSV row of data with semantic and unit
data Row = Row
    { rowSemantic :: Semantic
    , rowIndex :: Int
    , unit :: Unit
    , values :: [(Day, Maybe Double)]
    }
    deriving stock (Show)

rowToHarmonizedRow :: Row -> History
rowToHarmonizedRow (Row s i u vs) =
    MMap.singleton (IndexedSemantic s i)
        $ fold
        $ do
            (d, mv) <- vs
            v <- maybeToList mv
            pure $ MMap.singleton d $ First $ Result v u 1

instance ToNamedRecord Row where
    toNamedRecord (Row s i u vs) =
        namedRecord
            $ ("Semantic" .= s)
                : ("Index" .= T.pack (show i))
                : ("Unit" .= u)
                : fmap (\(d, v) -> (B8.pack . show $ d) .= v) vs

instance FromNamedRecord Row where
    parseNamedRecord r =
        Row
            <$> r .: "Semantic"
            <*> r .: "Index"
            <*> r .: "Unit"
            <*> parseDays r

parseDays :: NamedRecord -> Parser [(Day, Maybe Double)]
parseDays hm =
    let
        hm' =
            HMap.delete "Semantic"
                $ HMap.delete "Index"
                $ HMap.delete "Unit" hm
        fields = HMap.toList hm'
    in
        forM fields $ \(k, v) -> do
            d <- parseTimeM True defaultTimeLocale "%Y-%m-%d" (B8.unpack k)
            v' <- parseField v
            return (d, v')

parseHistory :: BL8.ByteString -> Either String History
parseHistory r = foldMap rowToHarmonizedRow . toList . snd <$> decodeByName r

-- | Render a harmonized history as a CSV file.
renderHarmonizedHistoryCsv :: HarmonizedHistory -> (Header, [Row])
renderHarmonizedHistoryCsv (HarmonizedHistory h ds) = (header', rows)
  where
    header' =
        V.fromList
            $ "Semantic"
                : "Index"
                : "Unit"
                : fmap (B8.pack . show) (toList ds)
    rows = do
        (IndexedSemantic s i, Harmonized m' u) <- MMap.assocs h
        pure $ Row s i u $ do
            toList ds <&> \d -> (d, Map.lookup d m')

-- | Parse benchmark CSV results into indexed history points.
parseResults
    :: BL8.ByteString
    -> Either String [(IndexedSemantic, Result)]
parseResults =
    fmap (fmap f . zip [0 ..] . toList . snd) . decodeByName
  where
    f (i, (Benchmark s r)) = (IndexedSemantic s i, r)

-- | Build a 'History' from parsed results for a given day.
historyFromResults
    :: Day -> [(IndexedSemantic, Result)] -> History
historyFromResults day rs = fold $ do
    (i, r) <- rs
    pure
        $ MMap.singleton i
        $ MMap.singleton day
        $ First r
