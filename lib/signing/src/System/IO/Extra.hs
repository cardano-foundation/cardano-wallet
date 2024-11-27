{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_HADDOCK hide #-}

module System.IO.Extra
    (
    -- * I/O Helpers
      prettyIOException
    , progName
    , hGetBytes
    , hPutString
    ) where

import Prelude

import Codec.Binary.Encoding
    ( AbstractEncoding (..)
    , detectEncoding
    , fromBase16
    , fromBase58
    , fromBech32
    )
import Control.Exception
    ( IOException
    )
import Data.ByteString
    ( ByteString
    )
import System.Console.ANSI
    ( Color (..)
    , ColorIntensity (..)
    , ConsoleLayer (..)
    , SGR (..)
    , setSGRCode
    )
import System.Environment
    ( getProgName
    )
import System.Exit
    ( exitFailure
    )
import System.IO
    ( Handle
    , stderr
    )
import System.IO.Unsafe
    ( unsafePerformIO
    )


import qualified Data.ByteString.Char8 as B8
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

-- | Read some bytes from the console, and decode them if the encoding is recognized.
hGetBytes :: Handle -> IO ByteString
hGetBytes h = do
    raw <- B8.filter noNewline <$> B8.hGetContents h
    case detectEncoding (T.unpack $ T.decodeUtf8 raw) of
        Just (EBase16  ) -> decodeBytes fromBase16 raw
        Just (EBech32{}) -> decodeBytes (fmap snd . fromBech32 markCharsRedAtIndices) raw
        Just (EBase58  ) -> decodeBytes fromBase58 raw
        Nothing          -> fail
            "Couldn't detect input encoding? Data on stdin must be encoded as \
            \bech16, bech32 or base58."

-- | Print string to the console.
hPutString :: Handle -> String -> IO ()
hPutString h =
    B8.hPutStrLn h . T.encodeUtf8 . T.pack

decodeBytes
    :: (bin -> Either String result)
    -> bin
    -> IO result
decodeBytes from = either fail pure . from

-- | Fail with a colored red error message.
prettyIOException :: IOException -> IO a
prettyIOException e = do
    B8.hPutStrLn stderr $ T.encodeUtf8 $ T.pack $ show e
    exitFailure

-- | Mark all characters from a given string as red (in a console).
markCharsRedAtIndices :: Integral i => [i] -> String -> String
markCharsRedAtIndices ixs = go 0 (L.sort $ L.nub ixs)
  where
    go _c [] [] = mempty
    go c (i:is) (s:ss)
        | c == i    = red ++ s:def ++ go (c + 1) is ss
        | otherwise = s : go (c + 1) (i:is) ss
    go _ [] ss = ss
    go _ _ [] = [] -- NOTE: Really an error case.

    red = setSGRCode [SetColor Foreground Vivid Red]
    def = setSGRCode [Reset]

noNewline :: Char -> Bool
noNewline = (`notElem` ['\n', '\r'])

-- | Get program name to avoid hard-coding it in documentation excerpt.
progName :: String
progName = unsafePerformIO getProgName
{-# NOINLINE progName #-}
