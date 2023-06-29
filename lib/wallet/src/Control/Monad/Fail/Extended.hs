{-# LANGUAGE DerivingVia #-}

module Control.Monad.Fail.Extended
    ( ReportFailure
    , reportFailure
    ) where

import Prelude

newtype ReportFailure a = ReportFailure {reportFailure :: Either String a}
    deriving (Functor, Applicative, Monad) via (Either String)

instance MonadFail ReportFailure where fail = ReportFailure . Left
