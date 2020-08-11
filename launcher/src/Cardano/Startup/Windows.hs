-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
-- Portability: Windows
--

module Cardano.Startup.Windows
    ( installSignalHandlers
    ) where

import Prelude

-- | Stub function for windows.
installSignalHandlers :: IO () -> IO ()
installSignalHandlers _ = pure ()
