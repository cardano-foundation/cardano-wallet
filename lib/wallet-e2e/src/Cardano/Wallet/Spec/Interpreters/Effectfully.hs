module Cardano.Wallet.Spec.Interpreters.Effectfully
    ( Story
    , story
    ) where

import qualified Effectful.Error.Static as E
import qualified Network.HTTP.Client as Http

import Cardano.Wallet.Spec.Effect.Assert
    ( FxAssert, runAssertFailsFast )
import Cardano.Wallet.Spec.Effect.Http
    ( FxHttp, runHttpClient )
import Cardano.Wallet.Spec.Effect.Query
    ( FxQuery, runQuery )
import Cardano.Wallet.Spec.Effect.Random
    ( FxRandom, runRandom )
import Cardano.Wallet.Spec.Effect.Timeout
    ( FxTimeout, runTimeout )
import Cardano.Wallet.Spec.Effect.Trace
    ( FxTrace, recordTraceLog, runTracePure )
import Effectful
    ( Eff, IOE, runEff )
import Effectful.Fail
    ( Fail, runFailIO )
import Network.HTTP.Client
    ( ManagerSettings (managerResponseTimeout) )
import Prelude hiding
    ( Show, State, evalState, show )
import System.Random
    ( newStdGen )
import Test.Syd
    ( TestDefM, expectationFailure, it )
import Text.Show
    ( show )

type Story a =
    Eff
        [ FxQuery
        , FxHttp
        , FxRandom
        , FxTimeout
        , FxAssert
        , FxTrace
        , E.Error SomeException
        , Fail
        , IOE
        ]
        a

story :: String -> Story () -> TestDefM outers () ()
story label story' =
    it label do
        interpretStory story' >>= \case
            Left err -> expectationFailure (show err)
            Right (_unit :: (), log) -> recordTraceLog label log

interpretStory :: Story a -> IO (Either SomeException (a, Seq Text))
interpretStory story' = do
    connectionManager <-
        Http.newManager
            Http.defaultManagerSettings
                { managerResponseTimeout = Http.responseTimeoutNone
                }
    stdGen <- newStdGen
    story'
        & runQuery
        & runHttpClient connectionManager
        & runRandom stdGen
        & runTimeout
        & runAssertFailsFast
        & runTracePure
        & E.runErrorNoCallStack
        & runFailIO
        & runEff
