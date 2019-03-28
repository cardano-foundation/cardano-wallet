module Test.Integration.Framework.DSL
    ( Context(..)

    -- * Steps
    , request
    , unsafeRequest

    -- * Expectations
    , expectSuccess
    , expectError
    , expectResponseCode
    , RequestException(..)

    -- * Helpers
    , (</>)
    , (!!)
    , json
    ) where

import Prelude hiding
    ( fail )

import Control.Monad.Fail
    ( MonadFail (..) )
import Control.Monad.IO.Class
    ( MonadIO )
import Data.Aeson.QQ
    ( aesonQQ )
import Data.List
    ( (!!) )
import Data.Text
    ( Text )
import Language.Haskell.TH.Quote
    ( QuasiQuoter )
import Test.Hspec.Expectations.Lifted
    ( shouldBe )
import Test.Integration.Framework.Request
    ( Context (..), RequestException (..), request, unsafeRequest )
import Web.HttpApiData
    ( ToHttpApiData (..) )

import qualified Network.HTTP.Types.Status as HTTP


-- | Expect an errored response, without any further assumptions
expectError
    :: (MonadIO m, MonadFail m, Show a)
    => (s, Either RequestException a)
    -> m ()
expectError (_, res) = case res of
    Left _  -> return ()
    Right a -> wantedErrorButSuccess a

-- | Expect a successful response, without any further assumptions
expectSuccess
    :: (MonadIO m, MonadFail m, Show a)
    => (s, Either RequestException a)
    -> m ()
expectSuccess (_, res) = case res of
    Left e  -> wantedSuccessButError e
    Right _ -> return ()

-- | Expect a given response code on the response
expectResponseCode
    :: (MonadIO m, MonadFail m)
    => HTTP.Status
    -> (HTTP.Status, a)
    -> m ()
expectResponseCode want (got, _) =
    got `shouldBe` want

--
-- HELPERS
--

json :: QuasiQuoter
json = aesonQQ

infixr 5 </>
(</>) :: ToHttpApiData a => Text -> a -> Text
base </> next = mconcat [base, "/", toQueryParam next]

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
