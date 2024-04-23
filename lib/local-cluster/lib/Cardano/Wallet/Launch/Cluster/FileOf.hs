{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}

module Cardano.Wallet.Launch.Cluster.FileOf
    ( FileOf (..)
    , DirOf (..)
    , RelDirOf (..)
    , changeFileOf
    , absFilePathOf
    , mkRelDirOf
    , toFilePath
    , newAbsolutizer
    , Absolutizer (..)
    , absolutize
    )
where

import Prelude

import GHC.TypeLits
    ( Symbol
    )
import System.Directory
    ( getCurrentDirectory
    )
import System.Path
    ( AbsDir
    , AbsFile
    , Path
    , RelDir
    , absDir
    , dynamicMakeAbsolute
    , relDir
    , toString
    )
import System.Path.Part
    ( Abs
    , AbsRel
    )

import qualified System.Path.PartClass as C

-- | An absolute path with a type-level tag
newtype FileOf (s :: Symbol) = FileOf {absFileOf :: AbsFile}
    deriving stock (Show)
    deriving newtype (Eq, Ord)

-- | Shortcut to get string filepath from a FileOf
absFilePathOf :: FileOf a -> FilePath
absFilePathOf (FileOf fp) = toFilePath fp

-- | An absolute directory with a type-level tag
newtype DirOf (s :: Symbol) = DirOf {absDirOf :: AbsDir}
    deriving stock (Show)
    deriving newtype (Eq, Ord)

-- | A relative directory with a type-level tag
newtype RelDirOf (s :: Symbol) = RelDirOf {relDirOf :: RelDir}
    deriving stock (Show)
    deriving newtype (Eq, Ord)

-- | Shortcut to build a RelDirOf from a string filepath
mkRelDirOf :: FilePath -> RelDirOf s
mkRelDirOf = RelDirOf . relDir

changeFileOf :: FileOf a -> FileOf b
changeFileOf (FileOf fp) = FileOf fp

-- | De-type a path
toFilePath :: (C.AbsRel ar, C.FileDir r) => Path ar r -> FilePath
toFilePath = toString

-- | A function to turn relative paths into absolute paths in IO
absolutize :: Path AbsRel t -> IO (Path Abs t)
absolutize x = do
    Absolutizer f <- newAbsolutizer
    pure $ f x

-- | A facility to turn relative paths into absolute paths.
newtype Absolutizer = Absolutizer
    { runAbsolutizer :: forall t. Path AbsRel t -> Path Abs t
    }

-- | Use the current working directory to create an 'Absolutizer'
newAbsolutizer :: IO Absolutizer
newAbsolutizer = do
    cwd <- absDir <$> getCurrentDirectory
    pure $ Absolutizer $ dynamicMakeAbsolute cwd
