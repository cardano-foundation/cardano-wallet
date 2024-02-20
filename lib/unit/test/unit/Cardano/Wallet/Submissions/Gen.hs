{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoMonoLocalBinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Wallet.Submissions.Gen
    ( prop_submissionHistory
    , GenSubmissionsHistory (..)
    , genTx
    , genSlot
    , noSubmissions
    , Step (..)
    , P
    ) where

import Prelude

import Cardano.Wallet.Submissions.Properties.Common
    ( Step (..)
    , newState
    )
import Cardano.Wallet.Submissions.Submissions
    ( Submissions (..)
    , transactionsL
    , txStatus
    )
import Cardano.Wallet.Submissions.TxStatus
    ( HasTxId (..)
    , _Expired
    , _InLedger
    , _InSubmission
    , getTx
    )
import Control.Arrow
    ( (&&&)
    )
import Control.Lens
    ( _2
    , lastOf
    , to
    , view
    , (&)
    )
import Control.Lens.Extras
    ( is
    )
import Data.Foldable
    ( toList
    )
import Data.Maybe
    ( fromJust
    )
import System.Random
    ( Random
    )
import Test.QuickCheck
    ( Arbitrary (..)
    , Gen
    , choose
    , elements
    , frequency
    , getSize
    )
import Test.QuickCheck.Property
    ( Property
    , conjoin
    , cover
    , forAllShrinkShow
    , mapSize
    )
import Text.Pretty.Simple
    ( pShow
    )

import qualified Data.Map.Strict as Map
import qualified Data.Text.Lazy as T

newtype Slot = Slot Int
    deriving (Eq, Show, Ord, Random, Enum, Num, Arbitrary)

newtype Tx = Tx Int
    deriving (Eq, Show, Ord, Arbitrary)

type P x = x () Slot Tx

instance HasTxId Tx where
    type TxId Tx = Tx
    txId x = x

genTx
    :: Arbitrary tx
    => Int -- ^ unknown share
    -> Int -- ^ in submissions share
    -> Int -- ^ in ledger share
    -> Int -- ^ expired share
    -> Submissions meta slot tx -- ^ source
    -> Gen tx  -- ^ choosen tx from source
genTx new oldInS oldInL oldE (Submissions db _finality _tip) =
    frequency $
        [(new,  arbitrary)]
        <> include oldInS (is _InSubmission)
        <> include oldInL (is _InLedger)
        <> include oldE (is _Expired)
    where include n l = onNonEmpty n (fmap (fromJust . getTx)
            $ toList $ Map.filter l $ fmap (view txStatus) db)

onNonEmpty :: Int -> [a] -> [(Int, Gen a)]
onNonEmpty _ [] = []
onNonEmpty k xs = [(k, elements xs)]

genSlot
    :: (Random slot, Num slot) => Int -- ^ before the finality share
    -> Int -- ^ between finality and tip share
    -> Int -- ^ after the tip share
    -> Submissions meta slot tx -- ^ source of tip and finality
    -> Gen slot -- ^ selected slot
genSlot bf bft at (Submissions _db finality' tip') =
    frequency
        [ (bf, choose (finality' - 5, finality'))
        , (bft, choose (finality' + 1, tip'))
        , (at, choose (tip' + 1, tip' + 10))
        ]

-- | Parameters for generation of test history
data GenSubmissionsHistory delta = GenSubmissionsHistory
    {   -- | Generate changes.
      genDelta :: P Submissions -> Gen (P delta)
        -- | Submission properties to check on every state change.
    , stepProperties ::  P (Step delta) -> Property
        -- | State transformation.
    , applyDelta :: P delta -> P Submissions -> P Submissions
    }

genSubmissions
    :: GenSubmissionsHistory delta
    -> Gen [P (Step delta)]
genSubmissions GenSubmissionsHistory{..}
    = getSize >>= go noSubmissions -- (Submissions mempty (Slot (-1)) (Slot 100))
  where
    go _ 0 = pure []
    go x n = do
        d <- genDelta x
        let x' = applyDelta d x
        (Step x x' d :) <$> go x' (pred n)

arbitrarySubmissionHistory
    :: GenSubmissionsHistory delta
    -> Gen [(Property, P (Step delta))]
arbitrarySubmissionHistory d@GenSubmissionsHistory{stepProperties}
    = fmap (stepProperties &&& id) <$> genSubmissions d

shrinkByInit :: [a] -> [[a]]
shrinkByInit [] = []
shrinkByInit xs = [init xs]

prop_submissionHistory
    :: Show (delta () Slot Tx)
    => GenSubmissionsHistory delta
    -> Property
prop_submissionHistory d = mapSize (*10)
    $ forAllShrinkShow (arbitrarySubmissionHistory d) shrinkByInit
        (\xs -> T.unpack $ pShow $ last $ snd <$> xs)
        $ \xs ->
            let
                result = lastOf
                    (traverse . _2 . newState
                        . transactionsL . to (fmap $ view txStatus)
                    )
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

noSubmissions :: Submissions () Slot Tx
noSubmissions = Submissions mempty 0 0
