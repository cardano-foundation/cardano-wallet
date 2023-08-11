module Cardano.Wallet.Spec.Interpreters.Effectfully
    ( Story
    , story
    ) where

import qualified Effectful.Error.Dynamic as E

import Cardano.Wallet.Spec.Effect.Assert
    ( Error, FxAssert, runAssertError )
import Cardano.Wallet.Spec.Effect.Http
    ( FxHttp, runHttpClient )
import Cardano.Wallet.Spec.Effect.Query
    ( FxQuery, runQuery )
import Cardano.Wallet.Spec.Effect.Random
    ( FxRandom, runRandom )
import Cardano.Wallet.Spec.Effect.Trace
    ( FxTrace, recordTraceLog, runTracePure )
import Effectful
    ( Eff, IOE, runEff )
import Effectful.Fail
    ( Fail, runFailIO )
import Prelude hiding
    ( State, evalState )
import System.Random
    ( newStdGen )
import Test.Syd
    ( TestDefM, expectationFailure, it )

type Story a =
    Eff
        [ FxQuery
        , FxHttp
        , FxRandom
        , Fail
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
        & runQuery
        & runHttpClient
        & runRandom stdGen
        & runFailIO
        & runAssertError
        & runTracePure
        & E.runErrorNoCallStack @Error
        & runEff
