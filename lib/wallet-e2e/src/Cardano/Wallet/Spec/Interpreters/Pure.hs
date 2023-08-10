module Cardano.Wallet.Spec.Interpreters.Pure
    ( pureStory
    , PureStory
    ) where

import Cardano.Wallet.Spec.Effect.Assert
    ( Error, FxAssert, runAssertError )
import Cardano.Wallet.Spec.Effect.Query
    ( FxQuery, runQueryMock )
import Cardano.Wallet.Spec.Effect.Random
    ( FxRandom, runRandomMock )
import Cardano.Wallet.Spec.Effect.Trace
    ( FxTrace, recordTraceLog, runTracePure )
import Cardano.Wallet.Spec.Types
    ( Mnemonic (..) )
import Effectful
    ( Eff, runPureEff )
import Test.Syd
    ( TestDefM, expectationFailure, it )

import qualified Data.Set as Set
import qualified Effectful.Error.Dynamic as E

type PureStory a =
    Eff
        [ FxQuery
        , FxRandom
        , FxAssert
        , FxTrace
        , E.Error Error
        ]
        a

pureStory :: String -> PureStory a -> TestDefM outers () ()
pureStory label story =
    it label do
        interpretStoryPure story & \case
            Left err -> expectationFailure (show err)
            Right (_unit :: a, log) -> recordTraceLog label log

interpretStoryPure :: PureStory a -> Either Error (a, Seq Text)
interpretStoryPure =
    runQueryMock Set.empty
        >>> runRandomMock (Mnemonic $ "foo" :| ["bar", "baz"])
        >>> runAssertError
        >>> runTracePure
        >>> E.runErrorNoCallStack @Error
        >>> runPureEff
