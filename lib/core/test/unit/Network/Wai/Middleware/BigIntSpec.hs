{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Copyright: © 2021-* IOHK
-- License: Apache-2.0

module Network.Wai.Middleware.BigIntSpec
    ( spec
    ) where

import Prelude

import Data.Aeson
    ( Value (..), toJSON )
import Data.Functor.Identity
    ( Identity (..) )
import Data.Text
    ( Text )
import GHC.Exts
    ( IsList (..) )
import Network.Wai.Middleware.BigInt
    ( traverseJSON )
import Test.Hspec
    ( Spec, describe )
import Test.Hspec.QuickCheck
    ( prop )
import Test.QuickCheck
    ( Arbitrary (..)
    , Property
    , arbitrary
    , choose
    , elements
    , listOf
    , oneof
    , vectorOf
    , (===)
    )

import qualified Data.Text as T

spec :: Spec
spec = describe "traverseJSON" $ do
    prop "traverseJSON preserves the structure" prop_traverseJSONStructure

prop_traverseJSONStructure :: Value -> Property
prop_traverseJSONStructure json =
    traverseJSON (pure . toJSON) (pure . toJSON) json === Identity json

--
-- Arbitrary
--

instance Arbitrary Value where
    shrink = \case
        Object o -> fmap (Object . fromList) $ shrink $ toList o
        Array xs -> fmap (Array . fromList) $ shrink $ toList xs
        _ -> []
    arbitrary = genValue (3 :: Int)
      where
        genValue = \case
            0 -> oneof
                [ pure Null
                , Bool <$> arbitrary
                , Number . fromInteger . toInteger <$> arbitrary @Int
                , String <$> arbitrary
                ]
            n -> oneof
                [ genValue 0
                , genArray n
                , genObject n
                ]
          where
            genArray n =
                Array  . fromList <$> listOf e
              where
                e = genValue (n-1)
            genObject n =
                Object . fromList <$> listOf ((,) <$> k <*> v)
              where
                k = arbitrary
                v = genValue (n-1)

instance Arbitrary Text where
    shrink "" = []
    shrink _  = [""]
    arbitrary = do
        n <- choose (0, 5)
        T.pack <$> vectorOf n (elements ['a'..'z'])
