module Data.Time.UtilsSpec
  ( spec
  )
where

import Data.Time.Utils
  ( utcTimePred
  , utcTimeSucc
  )
import Test.Hspec
  ( Spec
  , describe
  , it
  )
import Test.QuickCheck
  ( property
  , withMaxSuccess
  , (===)
  )
import Test.Utils.Time
  ( getUniformTime
  )
import Prelude

spec :: Spec
spec = describe "Manipulation of time values." $ do
  it "utcTimePred . utcTimeSucc == id"
    $ withMaxSuccess 10000
    $ property
    $ \t ->
      utcTimePred (utcTimeSucc (getUniformTime t)) === getUniformTime t

  it "utcTimeSucc . utcTimePred == id"
    $ withMaxSuccess 10000
    $ property
    $ \t ->
      utcTimeSucc (utcTimePred (getUniformTime t)) === getUniformTime t

  it "utcTimeSucc t > t"
    $ withMaxSuccess 10000
    $ property
    $ \t ->
      utcTimeSucc (getUniformTime t) > getUniformTime t

  it "utcTimePred t < t"
    $ withMaxSuccess 10000
    $ property
    $ \t ->
      utcTimePred (getUniformTime t) < getUniformTime t
