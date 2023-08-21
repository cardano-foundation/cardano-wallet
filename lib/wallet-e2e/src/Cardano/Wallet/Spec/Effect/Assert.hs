{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TemplateHaskell #-}

module Cardano.Wallet.Spec.Effect.Assert
    ( -- * Effect
      FxAssert
    , assert
    , assertException
    , assertFail
    , assertFailException

      -- ** Handlers
    , runAssertError
    , filterOutAssertions
    , FilterOut (..)
    )
where

import qualified Effectful.Error.Static as Effect

import Cardano.Wallet.Spec.Effect.Trace
    ( FxTrace, trace )
import Control.Exception
    ( AssertionFailed (..) )
import Data.Type.Equality
    ( TestEquality (testEquality) )
import Effectful
    ( (:>), Eff, Effect )
import Effectful.Dispatch.Dynamic
    ( interpose, interpret, send )
import Effectful.Error.Static
    ( throwError )
import Effectful.TH
    ( makeEffect )
import Prelude hiding
    ( trace )
import Type.Reflection
    ( TypeRep, typeOf )

data FxAssert :: Effect where
    Assert :: Text -> Bool -> FxAssert m ()
    AssertException :: Exception a => a -> Bool -> FxAssert m ()

$(makeEffect ''FxAssert)

assertFail :: (FxAssert :> es) => Text -> Eff es ()
assertFail label = assert label False

assertFailException :: (FxAssert :> es, Exception a) => a -> Eff es ()
assertFailException x = assertException x False

runAssertError
    :: (Effect.Error SomeException :> es, FxTrace :> es)
    => Eff (FxAssert : es) a
    -> Eff es a
runAssertError = interpret \_ ->
    \case
        (Assert msg truth) -> do
            trace $ "Asserting that " <> msg
            unless truth $ throwError $ toException $ AssertionFailed $ toString msg
        (AssertException a truth) -> do
            trace $ "Asserting that " <> show a
            unless truth $ throwError $ SomeException a

data FilterOut = forall a. Typeable a => FilterOut (Proxy a)

filterOutAssertions
    :: forall a es
     . [FilterOut]
    -> Eff (FxAssert : es) a
    -> Eff (FxAssert : es) a
filterOutAssertions filters = interpose $ \_ -> \case
    AssertException a truth -> case find (filterOut a) filters of
        Nothing -> send $ AssertException a truth
        Just _ -> pure ()
    Assert msg truth -> send $ Assert msg truth

filterOut :: Typeable a => a -> FilterOut -> Bool
filterOut a (FilterOut p) = isJust $ testEquality (proxyType p) (typeOf a)

proxyType :: forall a. Typeable a => Proxy a -> TypeRep a
proxyType _ = typeOf @a $ error "proxyType: this should never be evaluated"
