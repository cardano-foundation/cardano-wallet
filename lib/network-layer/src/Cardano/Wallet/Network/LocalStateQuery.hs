-- |
-- Copyright: © 2024 Cardano Foundation
-- License: Apache-2.0
--
-- Root of module hierarchy about local state queries.
--
module Cardano.Wallet.Network.LocalStateQuery
    ( currentEra
    , module Cardano.Wallet.Network.LocalStateQuery.PParams
    , module Cardano.Wallet.Network.LocalStateQuery.RewardAccount
    , module Cardano.Wallet.Network.LocalStateQuery.StakeDistribution
    ) where

import Cardano.Wallet.Network.LocalStateQuery.Extra
    ( currentEra
    )
import Cardano.Wallet.Network.LocalStateQuery.PParams
    ( protocolParams
    , protocolParamsLegacy
    , slottingParamsLegacy
    )
import Cardano.Wallet.Network.LocalStateQuery.RewardAccount
    ( fetchRewardAccounts
    )
import Cardano.Wallet.Network.LocalStateQuery.StakeDistribution
    ( stakeDistribution
    )
