{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Copyright: © 2022 IOHK
-- License: Apache-2.0
module Control.Monad.UtilSpec
    ( spec
    )
where

import Control.Monad
    ( (<=<)
    )
import Control.Monad.Identity
    ( Identity (..)
    )
import Control.Monad.Util
    ( applyNM
    )
import Data.Function
    ( (&)
    )
import Data.Function.Utils
    ( applyN
    )
import Test.Hspec
    ( Spec
    , describe
    , it
    )
import Test.QuickCheck
    ( Fun (..)
    , NonNegative (..)
    , Property
    , applyFun
    , chooseInt
    , conjoin
    , forAll
    , property
    , (===)
    )
import Prelude

spec :: Spec
spec = describe "Control.Monad.UtilSpec" $ do
    describe "applyNM" $ do
        it "prop_applyNM_applyN @Int"
            $ prop_applyNM_applyN @Int
            & property
        it "prop_applyNM_iterate @Identity @Int"
            $ prop_applyNM_iterate @Identity @Int
            & property
        it "prop_applyNM_iterate @Maybe @Int"
            $ prop_applyNM_iterate @Maybe @Int
            & property
        it "prop_applyNM_iterate @[] @Int"
            $ prop_applyNM_iterate_list
            & property
        it "prop_applyNM_unit @Identity @Int"
            $ prop_applyNM_unit @Identity @Int
            & property
        it "prop_applyNM_unit @Maybe @Int"
            $ prop_applyNM_unit @Maybe @Int
            & property
        it "prop_applyNM_unit @[] @Int"
            $ prop_applyNM_unit_list
            & property

--------------------------------------------------------------------------------
-- applyNM
--------------------------------------------------------------------------------

prop_applyNM_applyN
    :: (Eq a, Show a) => Int -> Fun a a -> a -> Property
prop_applyNM_applyN n (applyFun -> f) a =
    applyNM n (Identity <$> f) a === Identity (applyN n f a)

prop_applyNM_iterate
    :: (Monad m, Eq (m a), Show (m a))
    => NonNegative Int
    -> Fun a (m a)
    -> a
    -> Property
prop_applyNM_iterate (getNonNegative -> n) (applyFun -> f) =
    prop_applyNM_iterate_with n f

-- In the list monad, iterating a function of result width @w@ to depth @n@
-- yields up to @w ^ n@ elements, so drawing both the function and the depth
-- freely generates exponentially large lists (#5309). The list properties
-- therefore bound both exponents:
--
-- - Width: the generated function's codomain is
--   @Maybe (Either Int (Int, Int))@, which 'boundedList' maps to lists of
--   width 0, 1 or 2. Wider results are unrepresentable, yet nothing
--   qualitative is lost: widths 0, 1 and 2 exercise failure, linear
--   chaining and branching, and every wider list arises as a composition
--   of binary branches.
--
-- - Depth: iteration counts are drawn from 0 to 5.
--
-- The largest generated case is therefore 2 ^ 5 = 32 elements.

prop_applyNM_iterate_list
    :: Fun Int (Maybe (Either Int (Int, Int)))
    -> Int
    -> Property
prop_applyNM_iterate_list (applyFun -> boundedF) a =
    forAll (chooseInt (0, 5)) $ \n ->
        prop_applyNM_iterate_with n (boundedList . boundedF) a

prop_applyNM_iterate_with
    :: (Monad m, Eq (m a), Show (m a)) => Int -> (a -> m a) -> a -> Property
prop_applyNM_iterate_with n f a =
    applyNM n f a === applyNM_iterate n f a
  where
    applyNM_iterate n' f' = (!! n') . iterate (f' =<<) . pure

prop_applyNM_unit
    :: (Monad m, Eq (m a), Show (m a)) => Fun a (m a) -> a -> Property
prop_applyNM_unit (applyFun -> f) = prop_applyNM_unit_with f

prop_applyNM_unit_list
    :: Fun Int (Maybe (Either Int (Int, Int))) -> Int -> Property
prop_applyNM_unit_list (applyFun -> boundedF) =
    prop_applyNM_unit_with (boundedList . boundedF)

prop_applyNM_unit_with
    :: (Monad m, Eq (m a), Show (m a)) => (a -> m a) -> a -> Property
prop_applyNM_unit_with f a =
    conjoin
        [ applyNM 0 f a === pure a
        , applyNM 1 f a === f a
        , applyNM 2 f a === (f <=< f) a
        , applyNM 3 f a === (f <=< f <=< f) a
        , applyNM 4 f a === (f <=< f <=< f <=< f) a
        ]

-- | Interpret the bounded codomain as a list of at most two elements.
boundedList :: Maybe (Either a (a, a)) -> [a]
boundedList Nothing = []
boundedList (Just (Left a)) = [a]
boundedList (Just (Right (a, b))) = [a, b]
