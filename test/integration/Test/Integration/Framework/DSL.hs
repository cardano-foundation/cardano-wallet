{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module Test.Integration.Framework.DSL
    (
    -- * Scenario
      scenario
    , xscenario
    , pendingWith
    , Scenarios
    , Context(..)

    -- * Steps
    , request
    , request_
    , successfulRequest
    , verify

    -- * Expectations
    , expectSuccess
    , expectError
    , RequestException(..)

    -- * Helpers
    , ($-)
    ) where

import Prelude hiding
    ( fail )

import Control.Concurrent.MVar
    ( MVar )
import Control.Monad.Fail
    ( MonadFail (..) )
import Control.Monad.IO.Class
    ( MonadIO, liftIO )
import Data.Function
    ( (&) )
import Data.Text
    ( Text )
import GHC.Generics
    ( Generic )
import Network.HTTP.Client
    ( Manager )
import Test.Hspec.Core.Spec
    ( SpecM, it, xit )

import qualified Test.Hspec.Core.Spec as H

import Test.Integration.Framework.Request
    ( RequestException (..), request, request_, successfulRequest, ($-) )
import Test.Integration.Framework.Scenario
    ( Scenario )

--
-- SCENARIO
--

data Context = Context
    { _hlint :: ()
        -- ^ Something to stop hlint complaining
    , _manager
        :: (Text, Manager)
        -- ^ The underlying BaseUrl and Manager used by the Wallet Client
    } deriving (Generic)


-- | Just a type-alias to 'SpecM', like 'scenario'. Ultimately, everything is
-- made in such way that we can use normal (albeit lifted) HSpec functions and
-- utilities if needed (and rely on its CLI as well when needed).
type Scenarios ctx = SpecM (MVar ctx) ()

-- | Just a slightly-specialized alias for 'it' to help lil'GHC.
scenario
    :: String
    -> Scenario Context IO ()
    -> Scenarios Context
scenario = it

xscenario
    :: String
    -> Scenario Context IO ()
    -> Scenarios Context
xscenario = xit

-- | Lifted version of `H.pendingWith` allowing for temporarily skipping
-- scenarios from execution with a reason, like:
--
--      scenario title $ do
--          pendingWith "This test fails due to bug #213"
--          test
pendingWith
    :: (MonadIO m, MonadFail m)
    => String
    -> m ()
pendingWith = liftIO . H.pendingWith

-- | Apply 'a' to all actions in sequence
verify :: (Monad m) => a -> [a -> m ()] -> m ()
verify a = mapM_ (a &)


-- | Expect an errored response, without any further assumptions
expectError
    :: (MonadIO m, MonadFail m, Show a)
    => Either RequestException a
    -> m ()
expectError = \case
    Left _  -> return ()
    Right a -> wantedErrorButSuccess a


-- | Expect a successful response, without any further assumptions
expectSuccess
    :: (MonadIO m, MonadFail m, Show a)
    => Either RequestException a
    -> m ()
expectSuccess = \case
    Left e  -> wantedSuccessButError e
    Right _ -> return ()

wantedSuccessButError
    :: (MonadFail m, Show e)
    => e
    -> m void
wantedSuccessButError =
    fail . ("expected a successful response but got an error: " <>) . show

wantedErrorButSuccess
    :: (MonadFail m, Show a)
    => a
    -> m void
wantedErrorButSuccess =
    fail . ("expected an error but got a successful response: " <>) . show
