{-# LANGUAGE LambdaCase #-}

module Control.Monad.Exception.Unchecked where 

import Prelude

import Control.Exception
    ( Exception, SomeException (SomeException), catch, throw )
import Control.Monad.Except
    ( ExceptT (..), runExceptT )
import Data.Typeable
    ( Typeable )
newtype Unchecked e = Unchecked e
    deriving (Eq, Show)

instance (Typeable e, Show e) => Exception (Unchecked e)

throwInIO :: (Show e, Typeable e) => ExceptT e IO a -> IO a
throwInIO x =
    runExceptT x >>= \case
        Right a -> pure a
        Left e -> throw $ Unchecked e

catchFromIO :: (Show e, Typeable e) => IO a -> ExceptT e IO a
catchFromIO m =
    ExceptT $
        (Right <$> m) `catch` (\(Unchecked e) -> pure $ Left e)

throwSomeException :: Either SomeException t -> (t -> p) -> p
throwSomeException (Left (SomeException e)) _ = throw e
throwSomeException (Right x) f = f x


