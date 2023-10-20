{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Copyright: © 2020-2021 IOHK
-- License: Apache-2.0
--
-- General utility functions.
--
module Cardano.Wallet.Util
    ( -- * Partial functions for "impossible" situations
      HasCallStack
    , internalError
    , tina

    -- ** Handling errors for "impossible" situations.
    , isInternalError
    , tryInternalError

    -- * String formatting
    , ShowFmt (..)
    , mapFirst

    -- * StateT
    , modifyM

    -- * HTTP(S) URIs
    , uriToText
    , parseURI
    ) where

import Prelude

import Control.DeepSeq
    ( NFData (..)
    )
import Control.Error.Util
    ( (??)
    )
import Control.Exception
    ( ErrorCall
    , displayException
    )
import Control.Monad.IO.Unlift
    ( MonadUnliftIO
    )
import Control.Monad.Trans.Class
    ( lift
    )
import Control.Monad.Trans.Except
    ( runExceptT
    , throwE
    )
import Control.Monad.Trans.State.Strict
    ( StateT
    , get
    , put
    )
import Data.Foldable
    ( asum
    )
import Data.Functor.Identity
    ( runIdentity
    )
import Data.List
    ( isPrefixOf
    )
import Data.Maybe
    ( fromMaybe
    , isNothing
    )
import Data.Text
    ( Text
    )
import Data.Text.Class
    ( TextDecodingError (..)
    )
import Fmt
    ( Buildable (..)
    , Builder
    , fmt
    , (+|)
    )
import GHC.Generics
    ( Generic
    )
import GHC.Stack
    ( HasCallStack
    )
import Network.URI
    ( URI (..)
    , parseAbsoluteURI
    , uriQuery
    , uriScheme
    , uriToString
    )
import UnliftIO.Exception
    ( evaluate
    , tryJust
    )

import qualified Data.Text as T

-- | Calls the 'error' function, which will usually crash the program.
internalError :: HasCallStack => Builder -> a
internalError msg = error $ fmt $ "INTERNAL ERROR: "+|msg

isInternalErrorMsg :: String -> Bool
isInternalErrorMsg msg = "INTERNAL ERROR" `isPrefixOf` msg

-- | Take the first 'Just' from a list of 'Maybe', or die trying.
-- There is no alternative.
tina :: HasCallStack => Builder -> [Maybe a] -> a
tina msg = fromMaybe (internalError msg) . asum

-- | Effectfully modify the state of a state-monad transformer stack.
modifyM  :: forall m s. (Monad m) => (s -> m s) -> StateT s m ()
modifyM fn = get >>= lift . fn >>= put

-- | Tests whether an 'Exception' was caused by 'internalError'.
isInternalError :: ErrorCall -> Maybe String
isInternalError (displayException -> msg)
    | isInternalErrorMsg msg = Just msg
    | otherwise = Nothing

-- | Evaluates a pure expression to WHNF and handles any occurrence of
-- 'internalError'.
--
-- This is intended for use in testing. Don't use this in application code --
-- that's what normal IO exceptions are for.
tryInternalError :: MonadUnliftIO m => a -> m (Either String a)
tryInternalError = tryJust isInternalError . evaluate

{-------------------------------------------------------------------------------
                               Formatting helpers
-------------------------------------------------------------------------------}

-- | A polymorphic wrapper type with a custom show instance to display data
-- through 'Buildable' instances.
newtype ShowFmt a = ShowFmt { unShowFmt :: a }
    deriving (Generic, Eq, Ord)

instance NFData a => NFData (ShowFmt a)

instance Buildable a => Show (ShowFmt a) where
    show (ShowFmt a) = fmt (build a)

-- | Map a function to the first element of a list. Does nothing if the list is
-- empty.
mapFirst :: (a -> a) -> [a] -> [a]
mapFirst _     [] = []
mapFirst fn (h:q) = fn h:q

{-------------------------------------------------------------------------------
                                  HTTP(S) URIs
-------------------------------------------------------------------------------}

uriToText :: URI -> Text
uriToText uri = T.pack $ uriToString id uri ""

parseURI :: Text -> Either TextDecodingError URI
parseURI (T.unpack -> uri) = runIdentity $ runExceptT $ do
    uri' <- parseAbsoluteURI uri ??
        (TextDecodingError "Not a valid absolute URI.")
    let res = case uri' of
            (URI {uriAuthority, uriScheme, uriPath, uriQuery, uriFragment})
                | uriScheme `notElem` ["http:", "https:"] ->
                    Left "Not a valid URI scheme, only http/https is supported."
                | isNothing uriAuthority ->
                    Left "URI must contain a domain part."
                | not ((uriPath == "" || uriPath == "/")
                && uriQuery == "" && uriFragment == "") ->
                    Left "URI must not contain a path/query/fragment."
            _ -> Right uri'
    either (throwE . TextDecodingError) pure res
