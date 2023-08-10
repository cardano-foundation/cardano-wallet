module Cardano.Wallet.Spec.Interpreters.Effectfully
    ( Story
    , story
    ) where

import Cardano.Wallet.Spec.Effect.Assert
    ( Error, FxAssert, runAssertError )
import Cardano.Wallet.Spec.Effect.Query
    ( FxQuery, runQueryMock )
import Cardano.Wallet.Spec.Effect.Random
    ( FxRandom, runRandom )
import Cardano.Wallet.Spec.Effect.Trace
    ( FxTrace, recordTraceLog, runTracePure )
import Effectful
    ( Eff, IOE, runEff )
import Test.Syd
    ( TestDefM, expectationFailure, it )

import qualified Data.Set as Set
import qualified Effectful.Error.Dynamic as E
import System.Random
    ( newStdGen )

type Story a =
    Eff
        [ FxQuery
        , FxRandom
        , FxAssert
        , FxTrace
        , E.Error Error
        , IOE
        ]
        a

story :: String -> Story () -> TestDefM outers () ()
story label story' =
    it label do
        interpretStory story' >>= \case
            Left err -> expectationFailure (show err)
            Right (_unit :: (), log) -> recordTraceLog label log

interpretStory :: Story a -> IO (Either Error (a, Seq Text))
interpretStory story' = do
    stdGen <- newStdGen
    story'
        & runQueryMock Set.empty
        & runRandom stdGen
        & runAssertError
        & runTracePure
        & E.runErrorNoCallStack @Error
        & runEff
