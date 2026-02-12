{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.Wallet.UI.Cookies
    ( CookieRequest
    , CookieResponse
    , Cookied
    , RequestCookies (..)
    , SessionKey (..)
    , cookieName
    , sessioning
    , withSession
    , withSessionRead
    )
where

import Control.Monad
    ( replicateM
    )
import Control.Monad.IO.Class
    ( liftIO
    )
import Data.ByteString
    ( ByteString
    )
import Servant
    ( FromHttpApiData (parseHeader, parseQueryParam)
    , Handler
    , Header
    , Headers
    , addHeader
    , type (:>)
    )
import System.Random
    ( randomRIO
    )
import Web.Cookie
    ( Cookies
    , SetCookie (..)
    , defaultSetCookie
    , parseCookies
    )
import Prelude

import qualified Data.ByteString.Char8 as B8
import qualified Data.Text.Encoding as T

-- | A type representing a request that may contain cookies.
type CookieRequest = Header "Cookie" RequestCookies

-- | A type representing a response that may contain cookies.
type CookieResponse a = Headers '[Header "Set-Cookie" SetCookie] a

-- | A type representing a request and response that may contain cookies.
type Cookied b a = CookieRequest :> b (CookieResponse a)

-- | Cookies in a request.
newtype RequestCookies = RequestCookies Cookies

instance FromHttpApiData RequestCookies where
    parseHeader = return . RequestCookies . parseCookies
    parseQueryParam = return . RequestCookies . parseCookies . T.encodeUtf8

-- | The name of the cookie used to store the session key.
cookieName :: ByteString
cookieName = "wallet-UI"

-- | A 'SetCookie' value for a given session key.
setCookie :: SessionKey -> SetCookie
setCookie (SessionKey v) =
    defaultSetCookie
        { setCookieName = cookieName
        , setCookieValue = v
        }

newtype SessionKey = SessionKey ByteString
    deriving (Eq, Ord, Show)

sessioning
    :: Handler a
    -> Maybe RequestCookies
    -> Handler (CookieResponse a)
sessioning action = withSession (const action)

withSession
    :: (SessionKey -> Handler a)
    -> Maybe RequestCookies
    -> Handler (CookieResponse a)
withSession action mc = do
    c <- case mc of
        Nothing -> createCookie
        Just (RequestCookies cs) ->
            maybe createCookie (pure . SessionKey) (lookup cookieName cs)
    addHeader (setCookie c) <$> action c

withSessionRead
    :: (SessionKey -> Handler a)
    -> Maybe RequestCookies
    -> Handler a
withSessionRead action mc = do
    c <- case mc of
        Nothing -> createCookie
        Just (RequestCookies cs) ->
            maybe createCookie (pure . SessionKey) (lookup cookieName cs)
    action c

-- | Create a new session key.
createCookie :: Handler SessionKey
createCookie =
    liftIO
        $ fmap (SessionKey . B8.pack)
        $ replicateM 16
        $ randomRIO ('a', 'z')
