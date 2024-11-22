{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_HADDOCK hide #-}

module System.IO.Extra
    (
    -- * I/O Helpers
      prettyIOException
    , progName
    ) where

import Prelude

import Control.Exception
    ( IOException
    )
import System.Environment
    ( getProgName
    )
import System.Exit
    ( exitFailure
    )
import System.IO
    ( stderr
    )
import System.IO.Unsafe
    ( unsafePerformIO
    )


import qualified Data.ByteString.Char8 as B8
import qualified Data.Text as T
import qualified Data.Text.Encoding as T


-- | Fail with a colored red error message.
prettyIOException :: IOException -> IO a
prettyIOException e = do
    B8.hPutStrLn stderr $ T.encodeUtf8 $ T.pack $ show e
    exitFailure

-- | Get program name to avoid hard-coding it in documentation excerpt.
progName :: String
progName = unsafePerformIO getProgName
{-# NOINLINE progName #-}
