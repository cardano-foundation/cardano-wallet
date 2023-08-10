{-# LANGUAGE TemplateHaskell #-}

module Cardano.Wallet.Spec.Effect.Random where

import Cardano.Mnemonic
    ( entropyToMnemonic, mnemonicToText )
import Cardano.Wallet.Spec.Effect.Trace
    ( FxTrace, trace )
import Cardano.Wallet.Spec.Types
    ( Mnemonic (..) )
import Control.Exception
    ( throwIO )
import Crypto.Encoding.BIP39
    ( EntropyError, toEntropy )
import Data.ByteArray
    ( ScrubbedBytes )
import Effectful
    ( (:>), Eff, Effect, IOE )
import Effectful.Dispatch.Dynamic
    ( interpret )
import Effectful.TH
    ( makeEffect )
import Prelude hiding
    ( trace )
import System.Random
    ( StdGen, uniformR )

import qualified Data.ByteArray as BA
import qualified Data.List.NonEmpty as NE

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

runRandom
    :: (FxTrace :> es, IOE :> es)
    => StdGen
    -> Eff (FxRandom : es) a
    -> Eff es a
runRandom stdGen = interpret \_ RandomMnemonic -> do
    trace "Generating a random mnemonic"
    let randomBytes = unfoldr (Just . uniformR (minBound :: Word8, maxBound))
    entropy <-
        toEntropy @256 (BA.pack @ScrubbedBytes (take 32 (randomBytes stdGen)))
            & either (liftIO . throwIO . FxRandomExceptionEntropy) pure
    let phrase = NE.fromList $ mnemonicToText $ entropyToMnemonic entropy
    pure $ Mnemonic phrase

newtype FxRandomException = FxRandomExceptionEntropy (EntropyError 8)
    deriving stock (Show)
    deriving anyclass (Exception)
