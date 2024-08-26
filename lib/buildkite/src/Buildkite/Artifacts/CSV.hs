{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Buildkite.Artifacts.CSV
    ( fetchCSVArtifactContent
    )
where

import Prelude

import Buildkite.API
    ( GetArtifact
    , WithAuthPipeline
    , withLimitsLock
    )
import Data.Proxy
    ( Proxy (..)
    )
import Data.Text
    ( Text
    )
import Network.HTTP.Media
    ( (//)
    )
import Servant.API.ContentTypes
    ( Accept (contentType)
    , MimeRender (..)
    , MimeUnrender (mimeUnrender)
    )
import Servant.Client
    ( ClientM
    , client
    )

import qualified Data.ByteString.Lazy.Char8 as BL8

data CSV

instance Accept CSV where
    contentType _ = "text" // "csv"

instance Show a => MimeRender CSV a where
    mimeRender _ val = BL8.pack $ show val

instance MimeUnrender CSV BL8.ByteString where
    mimeUnrender _ = Right

fetchCSVArtifactContent
    :: WithAuthPipeline (Int -> Text -> Text -> ClientM BL8.ByteString)
fetchCSVArtifactContent l ma o r a b c =
    withLimitsLock l
        $ client (Proxy :: Proxy (GetArtifact CSV BL8.ByteString)) ma o r a b c
