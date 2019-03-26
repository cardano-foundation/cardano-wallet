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
    , request'
    , request_
    , successfulRequest
    , verify

    -- * Expectations
    , expectSuccess
    , expectError
    , expectResponseCode
    , RequestException(..)

    -- * Helpers
    , ($-)
    , (</>)
    , (!!)
    , json
    , printInfo
    ) where

import Prelude hiding
    ( fail )

import Control.Concurrent.MVar
    ( MVar )
import Control.Monad.Fail
    ( MonadFail (..) )
import Control.Monad.IO.Class
    ( MonadIO, liftIO )
import Data.Aeson.QQ
    ( aesonQQ )
import Data.ByteString.Lazy
    ( ByteString )
import Data.Function
    ( (&) )
import Data.List
    ( (!!) )
import Data.Text
    ( Text )
import GHC.Generics
    ( Generic )
import Language.Haskell.TH.Quote
    ( QuasiQuoter )
import Network.HTTP.Client
    ( Manager, Request, Response, responseStatus )
import Network.HTTP.Types.Status
    ( Status )
import Test.Hspec.Core.Spec
    ( SpecM, it, xit )
import Test.Hspec.Expectations.Lifted
    ( shouldBe )
import Test.Integration.Framework.Request
    ( RequestException (..)
    , request
    , request'
    , request_
    , successfulRequest
    , ($-)
    )
import Test.Integration.Framework.Scenario
    ( Scenario )
import Web.HttpApiData
    ( ToHttpApiData (..) )

import qualified Test.Hspec.Core.Spec as H
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

-- | printInfo (e.g. response) for debugging purposes
printInfo
    :: (MonadIO m, MonadFail m)
    => String
    -> m ()
printInfo = liftIO . print

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

expectResponseCode
    :: (MonadIO m, MonadFail m)
    => Status
    -> Either RequestException (Request, Response ByteString)
    -> m ()
expectResponseCode c = \case
    Left e   -> wantedSuccessButError e
    Right a -> (responseStatus (snd a)) `shouldBe` c

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


--
-- HELPERS
--

json :: QuasiQuoter
json = aesonQQ

infixr 5 </>
(</>) :: ToHttpApiData a => Text -> a -> Text
base </> next = mconcat [base, "/", toQueryParam next]
