{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoMonoLocalBinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Wallet.Submissions.Generation
    ( prop_submission_history
    , SubmissionsHG (..)
    , txG
    , slotG
    , noSubmissions
    , Step (..)
    , P
    )
where

import Prelude

import Cardano.Wallet.Submissions.Properties.Common
    ( Step (..), newState )
import Cardano.Wallet.Submissions.Submissions
    ( Submissions (Submissions), transactionsL )
import Cardano.Wallet.Submissions.TxStatus
    ( HasTxId (..), _Expired, _InLedger, _InSubmission )
import Control.Arrow
    ( (&&&) )
import Control.Lens
    ( lastOf, (&), _2 )
import Control.Lens.Extras
    ( is )
import System.Random
    ( Random )
import Test.QuickCheck
    ( Arbitrary (arbitrary)
    , Gen
    , Positive (getPositive)
    , choose
    , elements
    , frequency
    , getSize
    )
import Test.QuickCheck.Property
    ( Property, conjoin, cover, forAllShrinkShow, mapSize )

import qualified Data.Map.Strict as Map
import qualified Data.Text.Lazy as T
import Text.Pretty.Simple
    ( pShow )

newtype Slot = Slot Int
    deriving (Eq, Show, Ord, Random, Enum, Num)

newtype Tx = Tx Int
    deriving (Eq, Show, Ord)

type P x = x Slot Tx

instance HasTxId Tx where
    type TxId Tx = Tx
    txId x = x

txG
    :: Int -- ^ unknown share
    -> Int -- ^ in submissions share
    -> Int -- ^ in ledger share
    -> Int -- ^ expired share
    -> P Submissions -- ^ source
    -> Gen Tx -- ^ choosen tx from source
txG new oldInS oldInL oldE (Submissions db _finality _tip) =
    frequency $
        [(new, Tx . getPositive <$> arbitrary)]
        <> include oldInS (is _InSubmission)
        <> include oldInL (is _InLedger)
        <> include oldE (is _Expired)
    where include n l = onNonEmpty n (Map.keys $ Map.filter l db)

onNonEmpty :: Int -> [a] -> [(Int, Gen a)]
onNonEmpty _ [] = []
onNonEmpty k xs = [(k, elements xs)]

slotG
    :: Int -- ^ before the finality share
  -> Int -- ^ between finality and tip share
  -> Int -- ^ after the tip share
  -> P Submissions -- ^ source of tip and finality
  -> Gen Slot -- ^ selected slot
slotG bf bft at (Submissions _db finality' tip') =
    frequency
        [ (bf, choose (finality' - 5, finality'))
        , (bft, choose (finality' + 1, tip'))
        , (at, choose (tip' + 1, tip' + 10))
        ]

-- | test parameters
data SubmissionsHG a = SubmissionsHG
    {
    -- | operations generation
    deltaG :: P Submissions -> Gen (P a)
    , -- | prSubmissionserties to check on every state change
    stepProperties ::  P (Step a) -> Property
    , -- | state transformation
    applyDelta :: P a -> P Submissions -> P Submissions
    }

submissionsGs
    :: SubmissionsHG a
    -> Gen [P (Step a)]
submissionsGs SubmissionsHG{..}
    = getSize >>= go noSubmissions -- (Submissions mempty (Slot (-1)) (Slot 100))
    where
        go _ 0 = pure []
        go x n = do
            d <- deltaG x
            let x' = applyDelta d x
            (Step x x' d :) <$> go x' (pred n)

arbitrarySubmissionH
    :: SubmissionsHG a
    -> Gen [(Property, P (Step a))]

arbitrarySubmissionH d@SubmissionsHG{..}
    = fmap (stepProperties &&& id) <$> submissionsGs d

shrinkByInit :: [a] -> [[a]]
shrinkByInit [] = []
shrinkByInit xs = [init xs]


prop_submission_history
    :: Show (a Slot Tx)
    =>  SubmissionsHG a
    -> Property
prop_submission_history d = mapSize (*30)
    $ forAllShrinkShow (arbitrarySubmissionH d) shrinkByInit
        (\xs -> T.unpack $ pShow $ last $ snd <$> xs)
        $ \xs ->
            let
                result = lastOf
                    (traverse . _2 . newState . transactionsL)
                    xs
                distribution l = maybe 0 (length . Map.filter l) result
            in
            "non trivial"
                & cover 50 ((length <$> result) > Just 1)
                $ "at least 1 in expired state"
                    & cover 10 (distribution (is _InSubmission) > 0)
                $ "at least 1 in in-ledger state"
                    & cover 10 (distribution (is _InLedger) > 0)
                $ "at least 1 in in-submission state"
                    & cover 10 (distribution (is _InSubmission) > 0)
                $ conjoin . fmap fst $ xs

noSubmissions :: Submissions Slot Tx
noSubmissions = Submissions mempty 0 0
