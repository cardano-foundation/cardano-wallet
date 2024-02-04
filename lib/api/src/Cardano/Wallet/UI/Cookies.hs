{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.Wallet.UI.Cookies where

import Prelude

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
import System.Random
    ( randomRIO
    )
import Web.Cookie
    ( Cookies
    , SetCookie (..)
    , defaultSetCookie
    , parseCookies
    )

import qualified Data.ByteString.Char8 as B8
import qualified Data.Text.Encoding as T

type CookieRequest = Header "Cookie" RequestCookies
type CookieResponse a = Headers '[Header "Set-Cookie" SetCookie] a

type Cookied b a = CookieRequest :> b (CookieResponse a)

newtype RequestCookies = RequestCookies Cookies -- type Cookies = [(BS.ByteString, BS.ByteString)]

instance FromHttpApiData RequestCookies where
    parseHeader = return . RequestCookies . parseCookies
    parseQueryParam = return . RequestCookies . parseCookies . T.encodeUtf8

cookieName :: ByteString
cookieName = "wallet-UI"

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
    -> Handler (Headers '[Header "Set-Cookie" SetCookie] a)
sessioning action = withSession (const action)

withSession
    :: (SessionKey -> Handler a)
    -> Maybe RequestCookies
    -> Handler (Headers '[Header "Set-Cookie" SetCookie] a)
withSession action mc = do
    c <- case mc of
        Nothing -> createCookie
        Just (RequestCookies cs) ->
            maybe createCookie (pure . SessionKey) (lookup cookieName cs)
    addHeader (setCookie c) <$> action c

withSessionRead :: (SessionKey -> Handler a) -> Maybe RequestCookies -> Handler a
withSessionRead action mc = do
    c <- case mc of
        Nothing -> createCookie
        Just (RequestCookies cs) ->
            maybe createCookie (pure . SessionKey) (lookup cookieName cs)
    action c

createCookie :: Handler SessionKey
createCookie =
    liftIO
        $ fmap (SessionKey . B8.pack)
        $ replicateM 16
        $ randomRIO ('a', 'z')
