{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Wallet.Spec.Effect.Random where

import qualified Data.List.NonEmpty as NE
import qualified Effectful.State.Static.Local as State

import Cardano.Mnemonic
    ( entropyToMnemonic, mnemonicToText )
import Cardano.Wallet.Spec.Effect.Trace
    ( FxTrace, trace )
import Cardano.Wallet.Spec.Types
    ( Mnemonic (..) )
import Crypto.Encoding.BIP39
    ( EntropyError, toEntropy )
import Effectful
    ( (:>), Eff, Effect )
import Effectful.Dispatch.Dynamic
    ( interpret, reinterpret )
import Effectful.Fail
    ( Fail )
import Effectful.State.Static.Local
    ( State, evalState )
import Effectful.TH
    ( makeEffect )
import Prelude hiding
    ( State, evalState, get, trace )
import System.Random
    ( StdGen )
import System.Random.Stateful
    ( StateGenM (..), uniformByteStringM )

data FxRandom :: Effect where
    RandomMnemonic :: FxRandom m Mnemonic

$(makeEffect ''FxRandom)

runRandomMock
    :: Mnemonic
    -> (FxTrace :> es)
    => Eff (FxRandom : es) a
    -> Eff es a
runRandomMock mnemonic = interpret \_ RandomMnemonic -> do
    trace "Generating a [mock] random mnemonic"
    pure mnemonic

instance (State StdGen :> es) => MonadState StdGen (Eff es) where
    state = State.state

runRandom
    :: (FxTrace :> es, Fail :> es)
    => StdGen
    -> Eff (FxRandom : es) a
    -> Eff es a
runRandom gen = reinterpret (evalState gen) \_ RandomMnemonic -> do
    trace "Generating a random mnemonic"
    randomByteString <- uniformByteStringM 32 (StateGenM :: StateGenM StdGen)
    entropy <-
        toEntropy @256 randomByteString
            & either (fail . show . FxRandomExceptionEntropy) pure
    let phrase = NE.fromList $ mnemonicToText $ entropyToMnemonic entropy
    pure $ Mnemonic phrase

newtype FxRandomException = FxRandomExceptionEntropy (EntropyError 8)
    deriving stock (Show)
    deriving anyclass (Exception)
