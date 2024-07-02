{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

{- |
Copyright: © 2024 Cardano Foundation
License: Apache-2.0

Demonstration for 'Table' type.

-}
module Demo.Database where

import Prelude

import Data.Proxy
    ( Proxy (..)
    )
import Data.Text
    ( Text
    )
import Database.Table
    ( Col
    , Row
    , Table
    , (:.)
    )

{-----------------------------------------------------------------------------
    Test
------------------------------------------------------------------------------}
type TablePerson =
    Table "person"
        :. Col "name" Text
        :. Col "birthyear" Int

tablePerson :: Proxy TablePerson
tablePerson = Proxy
