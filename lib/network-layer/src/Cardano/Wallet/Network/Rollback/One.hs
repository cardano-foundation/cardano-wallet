module Cardano.Wallet.Network.Rollback.One
    ( oneHistory
    )
where

import Prelude

import Cardano.Wallet.Network.Rollback.ChainPoints
    ( ChainPoints (..)
    )
import Cardano.Wallet.Read
    ( ChainPoint (..)
    )
import Control.Monad.Fix
    ( fix
    )
import Data.Map.Lazy
    ( Map
    )

import qualified Data.Map.Lazy as Map

newtype OneHistory b = OneHistory (Map ChainPoint b)

-- | Create a 'ChainPoints' store with a maximum number of slots to keep in
-- history. It will keep at least one state at the given distance from the
-- tip.
oneHistory
    :: b
    -- ^ Initial value
    -> ChainPoints b
oneHistory b0 =
    ($ start)
        $ fix
        $ \go (OneHistory actual) ->
            ChainPoints
                { rollback = \cp ->
                    let
                        (good, _bad) = Map.split cp actual
                    in
                        go $ OneHistory good
                , feed = \cp b -> go $ OneHistory $ Map.insert cp b actual
                , current = case Map.maxView actual of
                    Nothing -> b0
                    Just (b, _) -> b
                }
  where
    start = OneHistory $ Map.insert GenesisPoint b0 mempty
