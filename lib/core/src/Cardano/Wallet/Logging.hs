-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- This module contains utility functions for logging and mapping trace data.

module Cardano.Wallet.Logging
    ( transformTextTrace
    , logTrace
    ) where

import Prelude

import Cardano.BM.Data.Tracer
    ( DefinePrivacyAnnotation (..), DefineSeverity (..) )
import Cardano.BM.Trace
    ( Trace, traceNamedItem )
import Control.Monad.IO.Class
    ( MonadIO (..) )
import Control.Tracer
    ( contramap )
import Data.Text
    ( Text )
import Data.Text.Class
    ( ToText (..) )

-- | Converts a 'Text' trace into any other type of trace that has a 'ToText'
-- instance.
transformTextTrace :: ToText a => Trace IO Text -> Trace IO a
transformTextTrace = contramap (fmap toText)

-- | Traces some data.
logTrace
    :: (MonadIO m, DefinePrivacyAnnotation a, DefineSeverity a)
    => Trace m a
    -> a
    -> m ()
logTrace tr msg = traceNamedItem tr priv sev msg
  where
    priv = definePrivacyAnnotation msg
    sev = defineSeverity msg
