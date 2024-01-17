{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}

module Cardano.Wallet.Launch.Cluster.FileOf
    ( FileOf (..)
    , changeFileOf
    )
where

import Prelude

import GHC.TypeLits
    ( Symbol
    )

newtype FileOf (s :: Symbol) = FileOf {pathOf :: FilePath}
    deriving stock (Show)
    deriving newtype (Eq, Ord)

changeFileOf :: FileOf a -> FileOf b
changeFileOf (FileOf fp) = FileOf fp
