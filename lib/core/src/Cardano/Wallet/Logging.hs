module Cardano.Wallet.Logging
    ( Logger (..)
    , Severity (..)
    , nullLogger
    ) where

import Cardano.BM.Data.Severity
    ( Severity (..) )
import Data.Text
    ( Text )

import Prelude

-- | Provides functionality relating to logging.
newtype Logger = Logger
    { log :: Severity -> Text -> IO () }

-- | A logger that consumes everything and outputs nothing.
nullLogger :: Logger
nullLogger = Logger $ \_ _ -> pure ()
