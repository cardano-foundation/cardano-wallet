-- |
-- Copyright: © 2018-2021 IOHK
-- License: Apache-2.0
--
-- Prelude for cardano-wallet application.

module Cardano.Wallet.Prelude
    ( module Cardano.Wallet.Base
    , module Data.Text.Class
    , Severity (..)
    , PrivacyAnnotation (..)
    , HasSeverityAnnotation (..)
    , HasPrivacyAnnotation (..)
    ) where

import Cardano.Wallet.Base
import Data.Text.Class

import Cardano.BM.Data.LogItem
    ( PrivacyAnnotation (..) )
import Cardano.BM.Data.Severity
    ( Severity (..) )
import Cardano.BM.Data.Tracer
    ( HasPrivacyAnnotation (..), HasSeverityAnnotation (..) )
