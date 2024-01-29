{-# LANGUAGE TemplateHaskell #-}

module Cardano.Wallet.Spec.Effect.Faucet where

import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T

import Cardano.Mnemonic
    ( SomeMnemonic
    )
import Cardano.Wallet.Spec.Effect.Trace
    ( FxTrace
    , trace
    )
import Effectful
    ( Eff
    , Effect
    , (:>)
    )
import Effectful.Dispatch.Dynamic
    ( reinterpret
    )
import Effectful.Fail
    ( Fail
    )
import Effectful.State.Static.Shared
    ( evalState
    , get
    , put
    )
import Effectful.TH
    ( makeEffect
    )
import Prelude hiding
    ( State
    , evalState
    , get
    , gets
    , put
    , trace
    )

data FxFaucet :: Effect where
    UseFaucetMnemonic :: FxFaucet m SomeMnemonic

$(makeEffect ''FxFaucet)

runFaucetPure
    :: (FxTrace :> es, Fail :> es)
    => NonEmpty SomeMnemonic
    -> Eff (FxFaucet : es) a -> Eff es a
runFaucetPure faucetMnemonics = reinterpret (evalState faucetMnemonics) $
    \ _localEnv UseFaucetMnemonic -> do
      mnemonics <- get
      let (mnemonic, maybeRemainingMnemonics) = NE.uncons mnemonics
      case maybeRemainingMnemonics of
        Nothing -> fail "No more faucet mnemonics"
        Just remainingMnemonics -> do
          trace $ "Used faucet mnemonic: " <> T.pack (show mnemonic)
          put remainingMnemonics
          pure mnemonic
