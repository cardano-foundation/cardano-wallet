module Cardano.Launcher.Logging (traceHandle) where

import Control.Monad.IO.Class
    ( MonadIO (..)
    )
import Control.Tracer
    ( Tracer
    , traceWith
    )
import Data.Text
    ( Text
    )
import Prelude
import System.IO
    ( Handle
    , hIsEOF
    )

import qualified Data.Text.IO as T

traceHandle :: MonadIO m => Tracer m Text -> Handle -> m ()
traceHandle tr h = do
    eof <- liftIO $ hIsEOF h
    if eof
        then pure ()
        else do
            line <- liftIO $ T.hGetLine h
            traceWith tr line
            traceHandle tr h
