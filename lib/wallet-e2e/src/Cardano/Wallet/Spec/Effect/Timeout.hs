{-# LANGUAGE TemplateHaskell #-}

module Cardano.Wallet.Spec.Effect.Timeout
    ( -- * Effect
      FxTimeout
    , within

      -- ** Handlers
    , runTimeout
    ) where

import Cardano.Wallet.Spec.Effect.Assert
    ( FxAssert, assertFail )
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

$(makeEffect ''FxTimeout)

runTimeout :: (FxAssert :> es, IOE :> es) => Eff (FxTimeout : es) a -> Eff es a
runTimeout = interpret \env (Within timeSpan action) ->
    localSeqLift env \toLocalEs ->
        localSeqUnliftIO env \fromLocalEs -> do
            result <-
                timeoutTS timeSpan (fromLocalEs action)
                    `catch` \(_ :: Timeout) -> pure Nothing
            case result of
                Just a -> pure a
                Nothing -> do
                    let msg = "Computation timed out (" <> show timeSpan <> ")"
                    fromLocalEs $ toLocalEs $ assertFail msg
                    fail $ toString msg
