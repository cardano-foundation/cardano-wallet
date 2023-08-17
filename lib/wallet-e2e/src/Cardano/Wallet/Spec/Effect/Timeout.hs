{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Cardano.Wallet.Spec.Effect.Timeout
    ( -- * Effect
      FxTimeout

      -- ** Handlers
    , runTimeout

      -- ** Operations
    , within
    , within_
    ) where

import Data.Time.TimeSpan
    ( TimeSpan, timeoutTS )
import Effectful
    ( (:>), Dispatch (Static), DispatchOf, Eff, Effect, IOE )
import Effectful.Dispatch.Static
    ( SideEffects (WithSideEffects)
    , StaticRep
    , evalStaticRep
    , unsafeLiftMapIO
    )

-- | An effect for timing out computations.
data FxTimeout :: Effect

type instance DispatchOf FxTimeout = Static WithSideEffects
data instance StaticRep FxTimeout = FxTimeout

-- | Run the 'FxTimeout' effect.
runTimeout :: (IOE :> es) => Eff (FxTimeout : es) a -> Eff es a
runTimeout = evalStaticRep FxTimeout

within :: (FxTimeout :> es) => TimeSpan -> Eff es a -> Eff es (Maybe a)
within = unsafeLiftMapIO . timeoutTS

within_ :: (FxTimeout :> es) => TimeSpan -> Eff es () -> Eff es ()
within_ timeSpan action = fromMaybe () <$> within timeSpan action
