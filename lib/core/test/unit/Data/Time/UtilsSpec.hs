module Data.Time.UtilsSpec
    ( spec
    ) where

import Prelude

import Data.Time.Utils
    ( utcTimePred, utcTimeSucc )
import Test.Hspec
    ( Spec, describe, it )
import Test.Hspec.Extra
    ( parallel )
import Test.QuickCheck
    ( property, withMaxSuccess, (===) )
import Test.Utils.Time
    ( getUniformTime )

spec :: Spec
spec = parallel $ describe "Manipulation of time values." $ do

    parallel $ it "utcTimePred . utcTimeSucc == id" $
        withMaxSuccess 10000 $ property $ \t ->
            utcTimePred (utcTimeSucc (getUniformTime t)) === getUniformTime t

    parallel $ it "utcTimeSucc . utcTimePred == id" $
        withMaxSuccess 10000 $ property $ \t ->
            utcTimeSucc (utcTimePred (getUniformTime t)) === getUniformTime t

    parallel $ it "utcTimeSucc t > t" $
        withMaxSuccess 10000 $ property $ \t ->
            utcTimeSucc (getUniformTime t) > getUniformTime t

    parallel $ it "utcTimePred t < t" $
        withMaxSuccess 10000 $ property $ \t ->
            utcTimePred (getUniformTime t) < getUniformTime t
