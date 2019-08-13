-- |
-- Copyright: © 2018-2019 IOHK
-- License: Apache-2.0
-- Portability: POSIX
--

module Cardano.Launcher.POSIX
    ( installSignalHandlers
    ) where

import Prelude

import Control.Monad
    ( void )
import Say
    ( sayErr )
import System.Posix.Signals
    ( Handler (..)
    , installHandler
    , keyboardSignal
    , raiseSignal
    , softwareTermination
    )

-- | Convert any SIGTERM received to SIGINT, for which the runtime system has
-- handlers that will correctly clean up sub-processes.
installSignalHandlers :: IO ()
installSignalHandlers = void $
    installHandler softwareTermination termHandler Nothing
    where
        termHandler = CatchOnce $ do
            sayErr "Terminated by signal."
            raiseSignal keyboardSignal
