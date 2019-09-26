-- |
-- Copyright: © 2018-2019 IOHK
-- License: Apache-2.0
-- Portability: Windows
--

module Cardano.Launcher.Windows
    ( installSignalHandlers
    ) where

import Prelude

import Cardano.BM.Trace
    ( Trace )
import Data.Text
    ( Text )

-- | Stub function for windows.
installSignalHandlers :: Trace IO Text -> IO ()
installSignalHandlers _ = pure ()
