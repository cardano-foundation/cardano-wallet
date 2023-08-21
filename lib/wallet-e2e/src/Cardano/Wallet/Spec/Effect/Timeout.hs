{-# LANGUAGE TemplateHaskell #-}

module Cardano.Wallet.Spec.Effect.Timeout
    ( -- * Effect
      FxTimeout
    , within

      -- ** Handlers
    , runTimeout
    , TimeoutAssertion (..)
    ) where

import Cardano.Wallet.Spec.Effect.Assert
    ( FxAssert, assertFailException )
import Control.Exception
    ( catch )
import Data.Time.TimeSpan
    ( TimeSpan, timeoutTS )
import Effectful
    ( (:>), Eff, Effect, IOE )
import Effectful.Dispatch.Dynamic
    ( interpret, localSeqLift, localSeqUnliftIO )
import Effectful.TH
    ( makeEffect )
import System.Timeout
    ( Timeout )

-- | An effect for timing out computations.
data FxTimeout :: Effect where
    Within :: TimeSpan -> m a -> FxTimeout m a

makeEffect ''FxTimeout

data TimeoutAssertion = TimeoutAssertion
    deriving stock (Show, Typeable)
    deriving anyclass (Exception)

runTimeout
    :: (FxAssert :> es, IOE :> es)
    => Eff (FxTimeout : es) a
    -> Eff es a
runTimeout =
    interpret $ \env -> \case
        Within ts action ->
            localSeqLift env $ \localLift ->
                localSeqUnliftIO env $ \unlift -> do
                    result <- catch
                        do timeoutTS ts $ unlift action
                        do \(_ :: Timeout) -> pure Nothing
                    case result of
                        Nothing -> unlift do
                            localLift $ assertFailException TimeoutAssertion
                            -- next is what happens if the interpreter of the
                            -- FxAssert effect is not bailing out of the
                            -- computation
                            within ts action
                        Just a -> pure a
