-- |
-- Copyright: © 2018-2019 IOHK
-- License: Apache-2.0
-- Portability: POSIX
--

module Cardano.Launcher.POSIX
    ( installSignalHandlers
    ) where

import Prelude

import Cardano.BM.Trace
    ( Trace, logNotice )
import Control.Monad
    ( void )
import Data.Text
    ( Text )
import System.Posix.Signals
    ( Handler (..)
    , installHandler
    , keyboardSignal
    , raiseSignal
    , softwareTermination
    )

-- | Convert any SIGTERM received to SIGINT, for which the runtime system has
-- handlers that will correctly clean up sub-processes.
installSignalHandlers :: Trace IO Text -> IO ()
installSignalHandlers tr = void $
    installHandler softwareTermination termHandler Nothing
    where
        termHandler = CatchOnce $ do
            logNotice tr "Terminated by signal."
            raiseSignal keyboardSignal
