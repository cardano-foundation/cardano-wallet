{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module Data.MaybeK
    ( MaybeK (..)
    , IsMaybe (..)
    , maybeFromMaybeK
    )
where

import Prelude

-- | A type level tag to indicate whether a value is present or not.
data IsMaybe = IsJust | IsNothing

-- | A Maybe carring the type level tag.
data MaybeK k a where
    NothingK :: MaybeK IsNothing a
    JustK :: a -> MaybeK IsJust a

deriving instance Show a => Show (MaybeK k a)
deriving instance Eq a => Eq (MaybeK k a)
deriving instance Functor (MaybeK k)

-- | Convert a MaybeK to a Maybe, dropping the type level tag.
maybeFromMaybeK :: MaybeK k a -> Maybe a
maybeFromMaybeK NothingK = Nothing
maybeFromMaybeK (JustK x) = Just x
