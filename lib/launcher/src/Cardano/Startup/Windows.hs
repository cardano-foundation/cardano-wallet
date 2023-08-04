-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
-- Portability: Windows
module Cardano.Startup.Windows
  ( installSignalHandlers
  , setDefaultFilePermissions
  , restrictFileMode
  , killProcess
  )
where

import System.Process
  ( Pid
  )
import Prelude

-- | Stub function for windows.
installSignalHandlers :: IO () -> IO ()
installSignalHandlers _ = pure ()

-- | Stub function for windows.
setDefaultFilePermissions :: IO ()
setDefaultFilePermissions = pure ()

-- | Stub function for windows.
restrictFileMode :: FilePath -> IO ()
restrictFileMode _ = pure ()

-- | Stub function for windows. Under windows, the default behaviour of
-- 'terminateProcess' is to kill, so this isn't needed.
killProcess :: Pid -> IO ()
killProcess _ = pure ()
