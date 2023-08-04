-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
-- Portability: POSIX
module Cardano.Startup.POSIX
  ( installSignalHandlers
  , setDefaultFilePermissions
  , restrictFileMode
  , killProcess
  )
where

import Control.Monad
  ( void
  )
import Data.Bits
  ( (.|.)
  )
import System.Posix.Files
  ( groupModes
  , otherModes
  , ownerReadMode
  , setFileCreationMask
  , setFileMode
  )
import System.Posix.Signals
  ( Handler (..)
  , installHandler
  , keyboardSignal
  , raiseSignal
  , sigKILL
  , signalProcess
  , softwareTermination
  )
import System.Process
  ( Pid
  )
import Prelude

-- | Convert any SIGTERM received to SIGINT, for which the runtime system has
-- handlers that will correctly clean up sub-processes.
installSignalHandlers :: IO () -> IO ()
installSignalHandlers notify =
  void
    $ installHandler softwareTermination termHandler Nothing
  where
    termHandler = CatchOnce $ do
      notify
      raiseSignal keyboardSignal

-- | Restricts the process umask so that any files created are only readable by
-- their owner.
setDefaultFilePermissions :: IO ()
setDefaultFilePermissions = void $ setFileCreationMask mask
  where
    mask = groupModes .|. otherModes

-- | Changes permissions of an existing file so that only the owner can read
-- them.
restrictFileMode :: FilePath -> IO ()
restrictFileMode f = setFileMode f ownerReadMode

-- | Kill a process with signal 9. This is used only after previous attempts to
-- terminate the process did not work.
killProcess :: Pid -> IO ()
killProcess = signalProcess sigKILL
