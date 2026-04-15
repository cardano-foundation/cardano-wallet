{-# LANGUAGE GADTs #-}

-- |
-- Copyright: © 2024 Cardano Foundation
-- License: Apache-2.0
--
-- A local state query that looks up UTxOs based on TxIns.
module Cardano.Wallet.Network.LocalStateQuery.UTxO
    ( getUTxOByTxIn
    ) where

import Cardano.Balance.Tx.Eras
    ( MaybeInRecentEra (..)
    )
import Cardano.Ledger.State
    ( UTxO
    )
import Cardano.Ledger.TxIn
    ( TxIn
    )
import Cardano.Wallet.Network.Implementation.Ouroboros
    ( LSQ (..)
    )
import Cardano.Wallet.Network.LocalStateQuery.Extra
    ( onAnyEra
    )
import Data.Set
    ( Set
    )
import Ouroboros.Consensus.Cardano
    ( CardanoBlock
    )
import Ouroboros.Consensus.Shelley.Eras
    ( StandardCrypto
    )
import Prelude

import qualified Ouroboros.Consensus.Shelley.Ledger as Shelley

{-----------------------------------------------------------------------------
    Local State Query for GetUTxOByTxIn
------------------------------------------------------------------------------}
--
type LSQ' m = LSQ (CardanoBlock StandardCrypto) m

getUTxOByTxIn
    :: Set TxIn -> LSQ' m (MaybeInRecentEra UTxO)
getUTxOByTxIn ins =
    onAnyEra
        (pure InNonRecentEraByron)
        (pure InNonRecentEraShelley)
        (pure InNonRecentEraAllegra)
        (pure InNonRecentEraMary)
        (pure InNonRecentEraAlonzo)
        (pure InNonRecentEraBabbage)
        (InRecentEraConway <$> LSQry (Shelley.GetUTxOByTxIn ins))
        (InRecentEraDijkstra <$> LSQry (Shelley.GetUTxOByTxIn ins))
