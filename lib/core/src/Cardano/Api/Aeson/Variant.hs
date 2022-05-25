{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}

module Cardano.Api.Aeson.Variant where

import Cardano.Prelude
import Control.Monad.Fail
    ( fail )
import Control.Monad.Free ( Free(..), liftF )
import Data.Aeson
    ( FromJSON )
import Data.Aeson.Types
    ( FromJSON (parseJSON)
    , Object
    , Parser
    , Value (Object)
    , modifyFailure
    , withObject
    )
import Data.String
    ( String )

data Variant a x where
    Variant
      :: FromJSON b
      => String
      -> (Object -> Bool)
      -> (b -> a)
      -> x
      -> Variant a x

deriving instance Functor (Variant a)

variant
  :: FromJSON b
  => String
  -> (Object -> Bool)
  -> (b -> a)
  -> Free (Variant a) ()
variant t s p = liftF $ Variant t s p ()

variants :: String -> Free (Variant a) () -> Value -> Parser a
variants ctx f = withObject ctx run
    where
    run obj = go f mempty
        where
        go (Pure ()) xs = case xs of
            [] -> fail "not enough fields to select a variant"
            [p] -> p
            _ -> fail "multiple variants are possible"
        go (Free (Variant t s p q)) xs
            | s obj =
                let appended = ", " <>  t <> " variant"
                in go q $
                    modifyFailure (<> appended)
                        (p <$> parseJSON (Object obj)) : xs
            | otherwise = go q xs
