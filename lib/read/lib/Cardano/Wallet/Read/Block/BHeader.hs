{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-|
Copyright: © 2024 Cardano Foundation
-- License: Apache-2.0
--
-- The 'BHeader' type represents a block header.
-}
module Cardano.Wallet.Read.Block.BHeader
    ( BHeader (..)
    , BHeaderT
    , getEraBHeader
    ) where

import Prelude

import Cardano.Ledger.Api
    ( StandardCrypto
    )
import Cardano.Ledger.Block
    ( bheader
    )
import Cardano.Wallet.Read.Block.Block
    ( Block (..)
    )
import Cardano.Wallet.Read.Eras
    ( Allegra
    , Alonzo
    , Babbage
    , Byron
    , Conway
    , Mary
    , Shelley
    )
import Cardano.Wallet.Read.Eras.KnownEras
    ( Era (..)
    , IsEra (..)
    )
import GHC.Generics
    ( Generic
    )
import Ouroboros.Consensus.Block.Abstract
    ( getHeader
    )
import Ouroboros.Consensus.Protocol.Praos.Header
    ( Header
    )
import Ouroboros.Consensus.Shelley.Protocol.Praos
    ()
import Ouroboros.Consensus.Shelley.Protocol.TPraos
    ()

import qualified Cardano.Protocol.TPraos.BHeader as TPraos
import qualified Ouroboros.Consensus.Byron.Ledger.Block as Byron
import qualified Ouroboros.Consensus.Shelley.Ledger.Block as O

type family BHeaderT era where
    BHeaderT Byron = Byron.Header Byron.ByronBlock
    BHeaderT Shelley = TPraos.BHeader StandardCrypto
    BHeaderT Allegra = TPraos.BHeader StandardCrypto
    BHeaderT Mary = TPraos.BHeader StandardCrypto
    BHeaderT Alonzo = TPraos.BHeader StandardCrypto
    BHeaderT Babbage = Header StandardCrypto
    BHeaderT Conway = Header StandardCrypto

newtype BHeader era = BHeader {unBHeader :: BHeaderT era}
    deriving (Generic)

deriving instance Show (BHeaderT era) => Show (BHeader era)
deriving instance Eq (BHeaderT era) => Eq (BHeader era)

{-# INLINABLE getEraBHeader #-}
getEraBHeader :: forall era. IsEra era => Block era -> BHeader era
getEraBHeader = case theEra :: Era era of
    Byron -> \(Block block) -> BHeader $ getHeader block
    Shelley -> \(Block (O.ShelleyBlock block _)) -> BHeader $ bheader block
    Allegra -> \(Block (O.ShelleyBlock block _)) -> BHeader $ bheader block
    Mary -> \(Block (O.ShelleyBlock block _)) -> BHeader $ bheader block
    Alonzo -> \(Block (O.ShelleyBlock block _)) -> BHeader $ bheader block
    Babbage -> \(Block (O.ShelleyBlock block _)) -> BHeader $ bheader block
    Conway -> \(Block (O.ShelleyBlock block _)) -> BHeader $ bheader block
