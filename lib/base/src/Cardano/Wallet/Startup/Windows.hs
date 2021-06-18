-- |
-- Copyright: © 2018-2021 IOHK
-- License: Apache-2.0
-- Portability: Windows
--

module Cardano.Wallet.Startup.Windows
    ( installSignalHandlers
    , setDefaultFilePermissions
    , restrictFileMode
    , killProcess
    ) where

import Cardano.Wallet.Base

import System.Process
    ( Pid )

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
