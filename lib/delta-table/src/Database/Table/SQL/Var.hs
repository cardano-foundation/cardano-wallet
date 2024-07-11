{-# LANGUAGE TupleSections #-}

{- |
Copyright: © 2024 Cardano Foundation
License: Apache-2.0

Variables and bindings.
-}
module Database.Table.SQL.Var
    (
    -- * Variables and Bindings
    -- ** Variable names
    VarName
    , renderVarName
    , Fresh
    , newVarName

    -- ** Bindings
    , Value
    , Bindings
    , Lets
    , freshVarName
    , bindValue

    -- ** Rendering
    , Rendering (..)
    , render
    ) where

import Prelude

import Control.Monad
    ( ap
    )
import Control.Monad.Trans.State.Strict
    ( State
    , evalState
    , get
    , put
    )
import Data.Bifunctor
    ( first
    )
import Data.Text
    ( Text
    )
import Numeric.Natural
    ( Natural
    )

import qualified Data.Map.Strict as Map
import qualified Data.Text as T

import qualified Database.SQLite.Simple as Sqlite

{-------------------------------------------------------------------------------
    Name allocation
-------------------------------------------------------------------------------}
-- | Name for a variable.
newtype VarName = Var Text
    deriving (Eq, Ord)

-- | Render a variable name in a format that SQLite recognizes as variable.
renderVarName :: VarName -> Text
renderVarName (Var v) = v

type VarCounter = Natural

-- | Monad for allocating fresh variable names.
type Fresh = State VarCounter

-- | Create a variable name from the current counter.
mkVarName :: VarCounter -> VarName
mkVarName = Var . T.pack . (":var" <>) . show

-- | Allocate a fresh variable name.
newVarName :: Fresh VarName
newVarName = do
    c <- get
    put $! (c+1)
    pure $ mkVarName c

{-------------------------------------------------------------------------------
    Bindings
-------------------------------------------------------------------------------}
-- | Value that can be bound to a variable.
type Value = Sqlite.SQLData

-- | Bindings of variables to names.
type Bindings = Map.Map VarName Value

-- | Monad for creating values that also contain variables and bindings.
newtype Lets a = Lets {runLets :: Fresh (a, Bindings)}

instance Functor Lets where
    fmap f (Lets x) = Lets (first f <$> x)

instance Applicative Lets where
    pure a = Lets $ pure (a, mempty)
    (<*>) = ap

instance Monad Lets where
    Lets mx >>= g = Lets $ do
        (x, xs) <- mx
        (y, ys) <- runLets (g x)
        pure (y, xs <> ys)

instance Semigroup a => Semigroup (Lets a) where
    a <> b = (<>) <$> a <*> b

instance Monoid a => Monoid (Lets a) where
    mempty = pure mempty

-- | Allocate a new variable name, but without binding to it.
freshVarName :: Lets VarName
freshVarName = Lets $ (,mempty) <$> newVarName

-- | Bind a given value to a variable name.
bindValue :: Value -> Lets VarName
bindValue value =
    Lets $ (\name -> (name, Map.singleton name value)) <$> newVarName

{-------------------------------------------------------------------------------
    Rendering
-------------------------------------------------------------------------------}
-- | A value together with variable bindings.
data Rendering a = Rendering
    { val :: a
    , bindings :: Bindings
    }

-- | Instantiate all variable names.
render :: Lets a -> Rendering a
render (Lets mf) = uncurry Rendering $ evalState mf 0
