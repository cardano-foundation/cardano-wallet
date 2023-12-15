{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}

-- |
-- Copyright: © 2022 IOHK
-- License: Apache-2.0
--
-- Module containing extra 'Cardano.Api' functionality needed by the wallet.
module Cardano.Api.Extra
    ( inAnyCardanoEra
    ) where

import Cardano.Api
    ( InAnyCardanoEra (..)
    , IsCardanoEra (cardanoEra)
    , Tx
    )

-- | Helper function for more easily creating an existential
-- @InAnyCardanoEra Tx@.
inAnyCardanoEra :: IsCardanoEra era => Tx era -> InAnyCardanoEra Tx
inAnyCardanoEra = InAnyCardanoEra cardanoEra
