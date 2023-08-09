module Cardano.Wallet.Spec where

import qualified Cardano.Wallet.Spec.Effect.Assert as Assert

import qualified Data.Set as Set

import Cardano.Wallet.Spec.Effect.Assert
    ( FxAssert, assert, runAssertError )
import Cardano.Wallet.Spec.Effect.Query
    ( FxQuery
    , createWalletFromMnemonic
    , deleteWallet
    , listKnownWallets
    , runQueryMock
    )
import Cardano.Wallet.Spec.Effect.Random
    ( FxRandom, randomMnemonic, runRandomMock )
import Cardano.Wallet.Spec.Effect.Trace
    ( FxTrace, recordTraceLog, runTracePure )
import Cardano.Wallet.Spec.Types
    ( Mnemonic (..) )
import Data.Set.Unicode
    ( (∈), (∉) )
import Effectful
    ( Eff, runPureEff )
import Effectful.Error.Dynamic
    ( Error, runErrorNoCallStack )
import Test.Syd
    ( Spec, TestDefM, describe, expectationFailure, it )

spec :: Spec
spec = do
    describe "Wallet Backend API" do
        describe "Wallets" do
            pureStory "Created wallet is known" do
                mnemonic <- randomMnemonic
                wallet <- createWalletFromMnemonic mnemonic
                assert "the new wallet is known" . (wallet ∈) =<< listKnownWallets
                deleteWallet wallet
                assert "the wallet is forgotten" . (wallet ∉) =<< listKnownWallets

--------------------------------------------------------------------------------
-- Helpers ---------------------------------------------------------------------

type Story = Eff [FxQuery, FxRandom, FxAssert, FxTrace, Error Assert.Error] ()

pureStory :: String -> Story -> TestDefM outers () ()
pureStory label story =
    it label $
        interpretStoryPure story
            & either (expectationFailure . show) (recordTraceLog label)
  where
    interpretStoryPure :: Story -> Either Assert.Error (Seq Text)
    interpretStoryPure =
        runQueryMock Set.empty
            >>> runRandomMock (Mnemonic $ "foo" :| ["bar", "baz"])
            >>> runAssertError
            >>> runTracePure
            >>> runErrorNoCallStack @Assert.Error
            >>> runPureEff
            >>> fmap snd
