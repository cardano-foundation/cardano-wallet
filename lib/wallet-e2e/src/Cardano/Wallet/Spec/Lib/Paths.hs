module Cardano.Wallet.Spec.Lib.Paths where

import Data.Tagged
    ( Tagged (..)
    , untag
    )
import Path
    ( Abs
    , Dir
    , Path
    , SomeBase (..)
    )
import Path.IO
    ( AnyPath (..)
    )

type DirOf x = Tagged x (Path Abs Dir)
type SomeDirOf x = Tagged x (SomeBase Dir)

makeDirAbsolute :: SomeDirOf x -> IO (DirOf x)
makeDirAbsolute = fmap Tagged . makeAbsolute' . untag

makeAbsolute' :: SomeBase Dir -> IO (Path Abs Dir)
makeAbsolute' = \case
    Abs absDir -> pure absDir
    Rel relDir -> makeAbsolute relDir
