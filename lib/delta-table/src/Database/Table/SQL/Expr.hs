{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- |
-- Copyright: © 2024 Cardano Foundation
-- License: Apache-2.0
--
-- SQL expressions (for use in WHERE clauses)
module Database.Table.SQL.Expr
    ( -- * SQL expressions

      -- ** Expressions
      Expr
    , true
    , false
    , not_
    , (&&.)
    , and_
    , (||.)
    , or_
    , (==.)
    , (/=.)
    , (<.)
    , (<=.)
    , (>.)
    , (>=.)

      -- ** Internal
    , renderExpr
    , parens
    , text
    , var
    ) where

import Data.Text
    ( Text
    )
import Database.Table.Schema
    ( Col
    , IsColumnName
    , getColumnName
    )
import Prelude

import qualified Database.SQLite.Simple as Sqlite
import qualified Database.SQLite.Simple.ToField as Sqlite
import qualified Database.Table.SQL.Var as Var

{-------------------------------------------------------------------------------
    Expressions
    Type
-------------------------------------------------------------------------------}
type Identifier = Text

-- | SQL expressions corresponding to the Haskell type @a@.
data Expr a where
    Value :: Sqlite.SQLData -> Expr Identifier
    Zero :: Expr0 a -> Expr a
    One :: Op1 a b -> Expr a -> Expr b
    Two :: Op2 a b c -> Expr a -> Expr b -> Expr c

data Expr0 a where
    TrueE :: Expr0 Bool
    FalseE :: Expr0 Bool
    ColumnName :: Text -> Expr0 Identifier

data Op1 a b where
    Not :: Op1 Bool Bool

data Op2 a b c where
    And :: Op2 Bool Bool Bool
    Or :: Op2 Bool Bool Bool
    Eq :: Op2 a a Bool
    Lt :: Op2 a a Bool
    Leq :: Op2 a a Bool
    Gt :: Op2 a a Bool
    Geq :: Op2 a a Bool

infix 4 ==., /=., <., <=., >=., >.
infixr 3 &&.
infixr 2 ||.

-- | SQL @true@ literal.
true :: Expr Bool
true = Zero TrueE

-- | SQL @false@ literal.
false :: Expr Bool
false = Zero FalseE

-- | SQL @NOT@, negation.
not_ :: Expr Bool -> Expr Bool
not_ = One Not

-- | SQL @AND@, conjunction.
(&&.) :: Expr Bool -> Expr Bool -> Expr Bool
(&&.) = Two And

-- | Combine multiple 'Expr' with '(&&.)'.
and_ :: [Expr Bool] -> Expr Bool
and_ [] = true
and_ (x : xs) = foldr (&&.) x xs

-- | SQL @OR@, disjunction.
(||.) :: Expr Bool -> Expr Bool -> Expr Bool
(||.) = Two Or

-- | Combine multiple 'Expr' with '(||.)'.
or_ :: [Expr Bool] -> Expr Bool
or_ [] = false
or_ (x : xs) = foldr (||.) x xs

-- | Comparing a column against a given value.
columnOpValue
    :: forall n a c
     . (IsColumnName n, Sqlite.ToField a)
    => Op2 Identifier Identifier c -> Col n a -> a -> Expr c
columnOpValue op2 col a =
    Two
        op2
        (Zero . ColumnName $ getColumnName col)
        (Value $ Sqlite.toField a)

-- | Test whether the value in a given column equals a given value.
(==.)
    :: forall n a
     . (IsColumnName n, Sqlite.ToField a)
    => Col n a -> a -> Expr Bool
(==.) = columnOpValue Eq

(/=.)
    :: forall n a
     . (IsColumnName n, Sqlite.ToField a)
    => Col n a -> a -> Expr Bool
(/=.) col a = not_ (col ==. a)

(<.)
    :: forall n a
     . (IsColumnName n, Sqlite.ToField a, Ord a)
    => Col n a -> a -> Expr Bool
(<.) = columnOpValue Lt

(<=.)
    :: forall n a
     . (IsColumnName n, Sqlite.ToField a, Ord a)
    => Col n a -> a -> Expr Bool
(<=.) = columnOpValue Leq

(>.)
    :: forall n a
     . (IsColumnName n, Sqlite.ToField a, Ord a)
    => Col n a -> a -> Expr Bool
(>.) = columnOpValue Gt

(>=.)
    :: forall n a
     . (IsColumnName n, Sqlite.ToField a, Ord a)
    => Col n a -> a -> Expr Bool
(>=.) = columnOpValue Geq

{-------------------------------------------------------------------------------
    Expressions
    Rendering
-------------------------------------------------------------------------------}

-- | Render an expression as SQL source code.
renderExpr :: Expr a -> Var.Lets Text
renderExpr (Value value) =
    Var.bindValue value >>= var
renderExpr (Zero e) =
    text (render0 e)
renderExpr (One op1 ea) =
    text (renderOp1 op1)
        <> sp
        <> parens (renderExpr ea)
renderExpr (Two op2 ea eb) =
    parens (renderExpr ea)
        <> sp
        <> text (renderOp2 op2)
        <> sp
        <> parens (renderExpr eb)

-- | A single whitespace character.
sp :: Var.Lets Text
sp = text " "

mkIdentifier :: Text -> Text
mkIdentifier name = "\"" <> name <> "\""

render0 :: Expr0 a -> Text
render0 TrueE = "true"
render0 FalseE = "false"
render0 (ColumnName name) = mkIdentifier name

renderOp1 :: Op1 a b -> Text
renderOp1 Not = "NOT"

renderOp2 :: Op2 a b c -> Text
renderOp2 And = "AND"
renderOp2 Or = "OR"
renderOp2 Eq = "="
renderOp2 Lt = "<"
renderOp2 Leq = "<="
renderOp2 Gt = ">"
renderOp2 Geq = ">="

-- | Render a fixed text.
text :: Text -> Var.Lets Text
text = pure

-- | Render a variable name
var :: Var.VarName -> Var.Lets Text
var = pure . Var.renderVarName

-- | Wrap a rendering in parentheses
parens :: Var.Lets Text -> Var.Lets Text
parens = fmap $ \x -> "(" <> x <> ")"
