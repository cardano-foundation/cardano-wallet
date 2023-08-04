module Test.Utils.Startup
  ( withLineBuffering
  , withNoBuffering
  )
where

import Control.Monad
  ( void
  )
import Control.Monad.IO.Unlift
  ( MonadUnliftIO
  )
import UnliftIO.Exception
  ( IOException
  , bracket
  , tryJust
  )
import UnliftIO.IO
  ( BufferMode (..)
  , hGetBuffering
  , hSetBuffering
  , stderr
  , stdout
  )
import Prelude

withLineBuffering, withNoBuffering :: MonadUnliftIO m => m a -> m a
withLineBuffering = withBuffering LineBuffering
withNoBuffering = withBuffering NoBuffering

withBuffering :: MonadUnliftIO m => BufferMode -> m a -> m a
withBuffering mode = bracket before after . const
  where
    before = do
      prev <- (,) <$> getBuf stdout <*> getBuf stderr
      setBuf stdout (Just mode)
      setBuf stderr (Just mode)
      pure prev
    after (prevOut, prevErr) = do
      setBuf stdout prevOut
      setBuf stderr prevErr

    getBuf = tryErr . hGetBuffering
    setBuf h = maybe (pure ()) (void . tryErr . hSetBuffering h)

    -- Swallow any IO errors
    tryErr = fmap (either (const Nothing) Just) . tryJust isAlright
    isAlright :: IOException -> Maybe ()
    isAlright = const (Just ())
