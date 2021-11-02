{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Copyright: © 2021 IOHK
-- License: Apache-2.0
--
-- Generic functions and types relating to fields of data types.
--
module Data.Generic.Fields
    (
    -- * Generic constraints for data types with fields
      HasFields1
    , HasFields2
    , HasFields3
    , HasFields4
    , HasFields5
    , HasFields6
    , HasFields7
    , HasFields8
    , HasFields9

    -- * Generic conversion of values to tuples
    , toTuple1
    , toTuple2
    , toTuple3
    , toTuple4
    , toTuple5
    , toTuple6
    , toTuple7
    , toTuple8
    , toTuple9
    )
    where

import Data.Generics.Internal.VL.Lens
    ( (^.) )
import Data.Generics.Product.Positions
    ( HasPosition', position' )

--------------------------------------------------------------------------------
-- Generic constraints for data types with fields
--------------------------------------------------------------------------------

type HasFields1 r a =
    (HasPosition' 1 r a)

type HasFields2 r a b =
    (HasFields1 r a, HasPosition' 2 r b)

type HasFields3 r a b c =
    (HasFields2 r a b, HasPosition' 3 r c)

type HasFields4 r a b c d =
    (HasFields3 r a b c, HasPosition' 4 r d)

type HasFields5 r a b c d e =
    (HasFields4 r a b c d, HasPosition' 5 r e)

type HasFields6 r a b c d e f =
    (HasFields5 r a b c d e, HasPosition' 6 r f)

type HasFields7 r a b c d e f g =
    (HasFields6 r a b c d e f, HasPosition' 7 r g)

type HasFields8 r a b c d e f g h =
    (HasFields7 r a b c d e f g, HasPosition' 8 r h)

type HasFields9 r a b c d e f g h i =
    (HasFields8 r a b c d e f g h, HasPosition' 9 r i)

--------------------------------------------------------------------------------
-- Generic conversion of values to tuples
--------------------------------------------------------------------------------

toTuple1 :: HasFields1 r a => r -> (a)
toTuple1 r = (r ^. position' @1)

toTuple2 :: HasFields2 r a b => r -> (a, b)
toTuple2 r = (a, r ^. position' @2)
  where
    (a) = toTuple1 r

toTuple3 :: HasFields3 r a b c => r -> (a, b, c)
toTuple3 r = (a, b, r ^. position' @3)
  where
    (a, b) = toTuple2 r

toTuple4 :: HasFields4 r a b c d => r -> (a, b, c, d)
toTuple4 r = (a, b, c, r ^. position' @4)
  where
    (a, b, c) = toTuple3 r

toTuple5 :: HasFields5 r a b c d e => r -> (a, b, c, d, e)
toTuple5 r = (a, b, c, d, r ^. position' @5)
  where
    (a, b, c, d) = toTuple4 r

toTuple6 :: HasFields6 r a b c d e f => r -> (a, b, c, d, e, f)
toTuple6 r = (a, b, c, d, e, r ^. position' @6)
  where
    (a, b, c, d, e) = toTuple5 r

toTuple7 :: HasFields7 r a b c d e f g => r -> (a, b, c, d, e, f, g)
toTuple7 r = (a, b, c, d, e, f, r ^. position' @7)
  where
    (a, b, c, d, e, f) = toTuple6 r

toTuple8 :: HasFields8 r a b c d e f g h => r -> (a, b, c, d, e, f, g, h)
toTuple8 r = (a, b, c, d, e, f, g, r ^. position' @8)
  where
    (a, b, c, d, e, f, g) = toTuple7 r

toTuple9 :: HasFields9 r a b c d e f g h i => r -> (a, b, c, d, e, f, g, h, i)
toTuple9 r = (a, b, c, d, e, f, g, h, r ^. position' @9)
  where
    (a, b, c, d, e, f, g, h) = toTuple8 r
