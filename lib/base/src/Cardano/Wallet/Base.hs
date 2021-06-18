{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Copyright: © 2018-2021 IOHK
-- License: Apache-2.0
--
-- Base Prelude for cardano-wallet packages.

module Cardano.Wallet.Base
    ( -- * Reexports from base
      module Prelude
    , idFunc
    , (&)
    , (<&>)
    , toList
    , bool
    , first
    , second
    , bimap
    , on
    , isNothing
    , isJust
    , fromMaybe
    , mapMaybe
    , guard
    , foldl'
    , fold
    , for
    , (<=<)
    , (>=>)
    , ($>)
    , fromRight
    , isLeft
    , isRight
    , void
    , join
    , when
    , unless
    , forM
    , forM_
    , coerce
    , Generic
    , NFData
    , Natural
    , NonEmpty (..)
    , Word16
    , Word8
    , Word32
    , Word64
    , Alternative (..)
    , Coercible
    , Typeable
    , Proxy (..)
    , Type
    -- * Lens
    , Lens'
    , lens
    , (^.)
    , (^?)
    , view
    , (.~)
    , set
    , over
    -- * IO classes
    , MonadUnliftIO (..)
    , MonadIO (..)
    , lift
    -- * Exceptions
    , Exception
    , throwIO
    , throwString
    , HasCallStack
    -- * Text
    , Text
    , showText
    -- * Formatting
    , Buildable (..)
    , pretty
    , fmt
    , blockListF
    , ordinalF
    , (+|), (+||), (|+), (||+)

    -- * Extra
    , eitherToMaybe

    -- * Tracing
    , Tracer (..)
    , contramap
    , traceWith
    , nullTracer
    , say
    , sayErr

    -- * Debugging
    , traceShowId
    , trace

    -- * GHCi
    , pPrint
    ) where

import Prelude

-- base packages
import Control.Applicative
    ( Alternative (..), Const (..) )
import Control.DeepSeq
    ( NFData (..) )
import Control.Monad
    ( forM, forM_, guard, join, unless, void, when, (<=<), (>=>) )
import Control.Monad.Trans.Class
    ( lift )
import Data.Bifunctor
    ( bimap, first, second )
import Data.Bool
    ( bool )
import Data.Coerce
    ( Coercible, coerce )
import Data.Either
    ( fromRight, isLeft, isRight )
import Data.Foldable
    ( fold, toList )
import Data.Function
    ( on, (&) )
import Data.Functor
    ( ($>), (<&>) )
import Data.Kind
    ( Type )
import Data.List
    ( foldl' )
import Data.List.NonEmpty
    ( NonEmpty (..) )
import Data.Maybe
    ( fromMaybe, isJust, isNothing, mapMaybe )
import Data.Monoid
    ( First (..) )
import Data.Profunctor.Unsafe
    ( ( #. ) )
import Data.Proxy
    ( Proxy (..) )
import Data.Text
    ( Text )
import Data.Traversable
    ( for )
import Data.Typeable
    ( Typeable )
import Data.Word
    ( Word16, Word32, Word64, Word8 )
import Debug.Trace
    ( trace, traceShowId )
import GHC.Generics
    ( Generic )
import GHC.Natural
    ( Natural )
import GHC.Stack
    ( HasCallStack )

import qualified Data.Text as T

-- other packages
import Control.Monad.IO.Unlift
    ( MonadIO (..), MonadUnliftIO (..) )
import Data.Either.Extra
    ( eitherToMaybe )
import Data.Generics.Internal.VL
    ( over, (.~) )
import Data.Generics.Internal.VL.Lens
    ( Lens', lens, set, view, (^.) )
import Data.Generics.Labels
    ()
import Fmt
    ( blockListF, fmt, ordinalF, pretty, (+|), (+||), (|+), (||+) )
import Formatting.Buildable
    ( Buildable (..) )
import Say
    ( say, sayErr )
import Text.Pretty.Simple
    ( pPrint )
import UnliftIO.Exception
    ( Exception, throwIO, throwString )

-- iohk packages
import Control.Tracer
    ( Tracer (..), contramap, nullTracer, traceWith )

showText :: Show a => a -> Text
showText = T.pack . show

-- | Identity function. Use this when you need the name 'id' for another
-- variable.
--
-- > idFunc x = x
idFunc :: a -> a
idFunc x =  x

-- | Get the first item of a traversal, if it exists.
infixl 8 ^?
(^?) :: s -> ((a -> Const (First a) a) -> s -> Const (First a) s) -> Maybe a
s ^? l = getFirst (fmof l (First #. Just) s)
  where fmof l' f = getConst #. l' (Const #. f)
