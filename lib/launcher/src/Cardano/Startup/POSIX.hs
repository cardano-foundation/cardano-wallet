-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
-- Portability: POSIX
--

module Cardano.Startup.POSIX
    ( installSignalHandlers
    ) where

import Prelude

import Control.Monad
    ( void )
import System.Posix.Signals
    ( Handler (..)
    , installHandler
    , keyboardSignal
    , raiseSignal
    , softwareTermination
    )

-- | Convert any SIGTERM received to SIGINT, for which the runtime system has
-- handlers that will correctly clean up sub-processes.
installSignalHandlers :: IO () -> IO ()
installSignalHandlers notify = void $
    installHandler softwareTermination termHandler Nothing
    where
        termHandler = CatchOnce $ do
            notify
            raiseSignal keyboardSignal
