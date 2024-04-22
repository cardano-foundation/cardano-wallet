module Cardano.Wallet.Spec.Interpreters.Pure
    ( pureStory
    , PureStory
    ) where

import qualified Data.Set as Set
import qualified Effectful.Error.Static as E

import Cardano.Mnemonic
    ( SomeMnemonic (..)
    )
import Cardano.Wallet.Launch.Cluster.FileOf
    ( DirOf (..)
    )
import Cardano.Wallet.Spec.Effect.Assert
    ( FxAssert
    , runAssertFailsFast
    )
import Cardano.Wallet.Spec.Effect.Query
    ( FxQuery
    , runQueryMock
    )
import Cardano.Wallet.Spec.Effect.Random
    ( FxRandom
    , runRandomMock
    )
import Cardano.Wallet.Spec.Effect.Trace
    ( FxTrace
    , recordTraceLog
    , runTracePure
    )
import Cardano.Wallet.Spec.Interpreters.Config
    ( TraceConfiguration (..)
    )
import Cardano.Wallet.Unsafe
    ( unsafeMkMnemonic
    )
import Effectful
    ( Eff
    , runPureEff
    )
import Effectful.Fail
    ( Fail
    , runFail
    )
import GHC.IO
    ( unsafePerformIO
    )
import System.Path
    ( absRel
    , dynamicMakeAbsoluteFromCwd
    )
import Test.Syd
    ( TestDefM
    , expectationFailure
    , it
    )

type PureStory a =
    Eff
        [ FxQuery
        , FxRandom
        , FxAssert
        , FxTrace
        , Fail
        , E.Error SomeException
        ]
        a

defaultTraceConfiguration :: TraceConfiguration
defaultTraceConfiguration =
    TraceConfiguration
        { traceConfigurationDir =
            DirOf @"tracing-dir"
                $ unsafePerformIO
                $ dynamicMakeAbsoluteFromCwd
                $ absRel "lib/wallet-e2e/test-output"
        }

pureStory :: String -> PureStory a -> TestDefM outers () ()
pureStory label story =
    it label do
        interpretStoryPure story & \case
            Left err -> expectationFailure (show err)
            Right (Left err) -> expectationFailure err
            Right (Right (_unit :: a, log)) ->
                recordTraceLog defaultTraceConfiguration label log

interpretStoryPure
    :: PureStory a
    -> (Either SomeException (Either String (a, Seq Text)))
interpretStoryPure =
    runQueryMock Set.empty
        >>> runRandomMock predefinedMnemonic
        >>> runAssertFailsFast
        >>> runTracePure
        >>> runFail
        >>> E.runErrorNoCallStack
        >>> runPureEff
  where
    predefinedMnemonic =
        SomeMnemonic
            $ unsafeMkMnemonic @15
                [ "vintage"
                , "poem"
                , "topic"
                , "machine"
                , "hazard"
                , "cement"
                , "dune"
                , "glimpse"
                , "fix"
                , "brief"
                , "account"
                , "badge"
                , "mass"
                , "silly"
                , "business"
                ]
