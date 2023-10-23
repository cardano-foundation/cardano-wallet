module Cardano.Wallet.Spec.Interpreters.Effectfully
    ( Story
    , story
    ) where

import qualified Network.HTTP.Client as Http

import Cardano.Wallet.Spec.Effect.Assert
    ( FxAssert
    , runAssertFailsFast
    )
import Cardano.Wallet.Spec.Effect.Http
    ( FxHttp
    , runHttpClient
    )
import Cardano.Wallet.Spec.Effect.Query
    ( FxQuery
    , runQuery
    )
import Cardano.Wallet.Spec.Effect.Random
    ( FxRandom
    , runRandom
    )
import Cardano.Wallet.Spec.Effect.Timeout
    ( FxTimeout
    , runTimeout
    )
import Cardano.Wallet.Spec.Effect.Trace
    ( FxTrace
    , recordTraceLog
    , runTracePure
    )
import Cardano.Wallet.Spec.Network.Configured
    ( ConfiguredNetwork
    )
import Effectful
    ( Eff
    , IOE
    , runEff
    )
import Effectful.Fail
    ( Fail
    , runFail
    )
import Network.HTTP.Client
    ( ManagerSettings (managerResponseTimeout)
    )
import Prelude hiding
    ( Show
    , State
    , evalState
    , show
    )
import System.Random
    ( initStdGen
    )
import Test.Syd
    ( TestDefM
    , expectationFailure
    , itWithOuter
    )

type Story a =
    Eff
        [ FxQuery
        , FxHttp
        , FxRandom
        , FxTimeout
        , FxAssert
        , Fail
        , FxTrace
        , IOE
        ]
        a

story :: String -> Story () -> TestDefM '[ConfiguredNetwork] () ()
story label story' =
    itWithOuter label \network -> do
        interpretStory network story' >>= \(result, log) -> do
            recordTraceLog label log
            case result of
                Left err -> expectationFailure err
                Right () -> pass

interpretStory
    :: ConfiguredNetwork
    -> Story a
    -> IO (Either String a, Seq Text)
interpretStory configuredNetwork story' = do
    connectionManager <-
        Http.newManager
            Http.defaultManagerSettings
                { managerResponseTimeout = Http.responseTimeoutNone
                }
    stdGen <- initStdGen
    story'
        & runQuery configuredNetwork
        & runHttpClient connectionManager
        & runRandom stdGen
        & runTimeout
        & runAssertFailsFast
        & runFail
        & runTracePure
        & runEff
