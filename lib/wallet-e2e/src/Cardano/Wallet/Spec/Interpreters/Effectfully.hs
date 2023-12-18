module Cardano.Wallet.Spec.Interpreters.Effectfully
    ( Story
    , story
    ) where

import qualified Network.HTTP.Client as Http

import qualified Cardano.Faucet.Mnemonics as Mnemonics
import Cardano.Mnemonic
    ( SomeMnemonic (..)
    )
import Cardano.Wallet.Spec.Effect.Assert
    ( FxAssert
    , runAssertFailsFast
    )
import Cardano.Wallet.Spec.Effect.Faucet
    ( FxFaucet
    , runFaucetPure
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
import Cardano.Wallet.Spec.Interpreters.Config
    ( TraceConfiguration
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
    ( HList (..)
    , TestDefM
    , expectationFailure
    , itWithAll
    )

type Story a =
    Eff
        [ FxQuery
        , FxFaucet
        , FxHttp
        , FxRandom
        , FxTimeout
        , FxAssert
        , Fail
        , FxTrace
        , IOE
        ]
        a

type StoryConfig =
    '[ ConfiguredNetwork
     , TraceConfiguration
     ]

onStoryConfig
    :: HList StoryConfig
    -> (ConfiguredNetwork -> TraceConfiguration -> a)
    -> a
onStoryConfig (HCons network (HCons traceOutput HNil)) f = f network traceOutput

story
    :: String
    -> Story ()
    -> TestDefM StoryConfig () ()
story label story' =
    itWithAll label \config () -> onStoryConfig config
        $ \network traceOutput -> do
            (result, log) <- interpretStory network story'
            recordTraceLog traceOutput label log
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
        & runFaucetPure (SomeMnemonic Mnemonics.preregKeyWallet :| [])
        & runHttpClient connectionManager
        & runRandom stdGen
        & runTimeout
        & runAssertFailsFast
        & runFail
        & runTracePure
        & runEff
