{-# LANGUAGE LambdaCase #-}

-- | work with Unchecked exceptions and SomeException
module Control.Monad.Exception.Unchecked
    ( throwUnchecked
    , catchUnchecked
    , throwSomeException
    ) where

import Prelude

import Control.Exception
    ( throw )
import Control.Monad.Catch
    ( Exception, MonadCatch (catch), SomeException (SomeException) )
import Control.Monad.Except
    ( ExceptT (..), runExceptT )
import Data.Typeable
    ( Typeable )

-- | An Unchecked exception is just any Typeable type
newtype Unchecked e = Unchecked e
    deriving (Eq, Show)

instance (Typeable e, Show e) => Exception (Unchecked e)

throwUnchecked :: (Monad m, Typeable e, Show e) => ExceptT e m b -> m b
throwUnchecked x =
    runExceptT x >>= \case
        Right a -> pure a
        Left e -> throw $ Unchecked e

catchUnchecked :: (MonadCatch m, Typeable e, Show e) => m a -> ExceptT e m a
catchUnchecked m =
    ExceptT $
        (Right <$> m) `catch` (\(Unchecked e) -> pure $ Left e)

throwSomeException :: Either SomeException t -> (t -> p) -> p
throwSomeException (Left (SomeException e)) _ = throw e
throwSomeException (Right x) f = f x
