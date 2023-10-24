{- |
Copyright: © 2022 IOHK
License: Apache-2.0

The 'Block' type represents blocks as they are read from the mainnet ledger.
It is compatible with the era-specific types from @cardano-ledger@.
-}
module Cardano.Wallet.Read.Block
    ( ConsensusBlock
    ) where

import qualified Ouroboros.Consensus.Cardano.Block as O

{-------------------------------------------------------------------------------
    Block type
-------------------------------------------------------------------------------}
-- | Type synonym for 'CardanoBlock' with cryptography as used on mainnet.
type ConsensusBlock = O.CardanoBlock O.StandardCrypto
