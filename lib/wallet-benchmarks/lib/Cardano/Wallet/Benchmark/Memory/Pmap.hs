module Cardano.Wallet.Benchmark.Memory.Pmap
    ( pmapParser
    , lineParser
    , topLineParser
    , bottomLineParser
    , pmap
    , Pmap (..)
    , Line (..)
    , Header (..)
    ) where

import Control.Monad
    ( void
    )
import Data.Attoparsec.ByteString.Char8
    ( Parser
    , char
    , decimal
    , endOfLine
    , hexadecimal
    , inClass
    , many1
    , parseOnly
    , skipSpace
    , string
    , takeTill
    )
import System.Process
    ( getPid
    )
import UnliftIO
    ( liftIO
    )
import UnliftIO.Process
    ( ProcessHandle
    , readProcess
    )
import Prelude

import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.ByteString.Char8 as B8

-- | Represents a line of the pmap command
data Line = Line
    { address :: Int
    , memory :: Int
    , permissions :: String
    , command :: String
    }
    deriving (Show, Eq)

-- | Represents the header of the pmap command
data Header = Header
    { pid :: Int
    , commandCall :: String
    }
    deriving (Show, Eq)

-- | Represents the footer of the pmap command
newtype Footer = Footer
    { total :: Int
    }
    deriving (Show, Eq)

-- | Represents the output of the pmap command
data Pmap = Pmap
    { header :: Header
    , pmapLines :: [Line]
    , footer :: Footer
    }
    deriving (Show, Eq)

pmapParser :: Parser Pmap
pmapParser = do
    t <- topLineParser
    ls <- many1 lineParser
    Pmap t ls <$> bottomLineParser

topLineParser :: Parser Header
topLineParser = do
    pid <- decimal
    void $ char ':'
    skipSpace
    Header pid . B8.unpack <$> lastPart

lineParser :: Parser Line
lineParser = do
    addr <- hexadecimal
    skipSpace
    mem <- decimal <* char 'K'
    skipSpace
    perms <- A.takeWhile (inClass "rwxp-")
    skipSpace
    Line addr mem (B8.unpack perms) . B8.unpack <$> lastPart

bottomLineParser :: Parser Footer
bottomLineParser = do
    skipSpace
    void $ string "total"
    skipSpace
    tot <- decimal
    void $ char 'K'
    void lastPart
    pure $ Footer tot

lastPart :: Parser B8.ByteString
lastPart = takeTill (== '\n') <* endOfLine

-- | Get the pmap output for a given process
pmap :: ProcessHandle -> IO Pmap
pmap p = do
    Just pid <- liftIO $ getPid p
    output <- liftIO $ readProcess "pmap" [show pid] ""
    case parseOnly pmapParser $ B8.pack output of
        Right x -> pure x
        Left _ -> error "Failed to parse pmap output"
