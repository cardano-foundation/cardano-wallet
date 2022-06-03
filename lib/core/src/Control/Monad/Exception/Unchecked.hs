{-# LANGUAGE LambdaCase #-}

module Control.Monad.Exception.Unchecked where 

import Control.Exception
    ( Exception, catch, throwIO )
import Control.Monad.Except
    ( ExceptT (..), runExceptT )
import Data.Typeable
    ( Typeable )
import Prelude

throwInIO :: (Show e, Typeable e) => ExceptT e IO a -> IO a
throwInIO x =
    runExceptT x >>= \case
        Right a -> pure a
        Left e -> throwIO $ Unchecked e

catchFromIO :: (Show e, Typeable e) => IO a -> ExceptT e IO a
catchFromIO m =
    ExceptT $
        (Right <$> m) `catch` (\(Unchecked e) -> pure $ Left e)

newtype Unchecked e = Unchecked e
    deriving (Eq, Show)

instance (Typeable e, Show e) => Exception (Unchecked e)
