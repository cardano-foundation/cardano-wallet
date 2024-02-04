{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Wallet.UI.Html.Pages.ListOf where

import Prelude

import Control.Monad.Operational
    ( Program
    , ProgramView
    , ProgramViewT (Return, (:>>=))
    , view
    )

data Cons e a where
    Elem :: e -> Cons e ()

type ListOf e = Program (Cons e) ()

listOf :: ListOf a -> [a]
listOf = reverse . ($ []) . interpret

interpret :: forall e. ListOf e -> [e] -> [e]
interpret = eval . view
  where
    eval :: ProgramView (Cons e) () -> [e] -> [e]
    eval (Elem x :>>= is) stack = interpret (is ()) (x : stack)
    eval (Return _a) stack = stack
