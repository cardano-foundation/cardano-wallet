{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.Wallet.Read.Block.Gen.Babbage
    ( mkBabbageBlock
    )
where

import Cardano.Crypto.DSIGN
    ( DSIGNAlgorithm (..)
    )
import Cardano.Crypto.Hash
    ( ByteString
    )
import Cardano.Crypto.VRF
    ( CertifiedVRF (CertifiedVRF)
    , VRFAlgorithm (..)
    )
import Cardano.Ledger.BaseTypes
    ( ProtVer (..)
    )
import Cardano.Ledger.Binary
    ( Version
    )
import Cardano.Ledger.Keys
    ( VKey (..)
    )
import Cardano.Protocol.Crypto
    ( StandardCrypto
    )
import Cardano.Protocol.TPraos.BHeader
    ( PrevHash (..)
    )
import Cardano.Read.Ledger.Block.BlockNo
    ( BlockNo (..)
    )
import Cardano.Read.Ledger.Block.SlotNo
    ( SlotNo (..)
    )
import Cardano.Wallet.Read
    ( TxT
    )
import Cardano.Wallet.Read.Block.Gen.BlockParameters
    ( BlockParameters (..)
    )
import Cardano.Wallet.Read.Block.Gen.Shelley
    ( HeaderEra
    , bodyHash
    , hashHeader
    , mkAnyAfterShelleyBlock
    , mkKeyDSIGN'
    , mkKeyVRF'
    , mkSignedKES
    , oCertamente
    )
import Ouroboros.Consensus.Protocol.Praos
    ( Praos
    )
import Ouroboros.Consensus.Protocol.Praos.Header
    ( Header (..)
    , HeaderBody (..)
    )
import Prelude

import Cardano.Ledger.Core qualified as L
import Cardano.Ledger.Slot qualified as L
import Ouroboros.Consensus.Shelley.Ledger qualified as O

mkBabbageBlock
    :: ( L.EraBlockBody era
       , HeaderEra era ~ Header StandardCrypto
       , TxT cardano_era ~ L.Tx era
       )
    => Version
    -> BlockParameters cardano_era
    -> O.ShelleyBlock (Praos StandardCrypto) era
mkBabbageBlock v BlockParameters{blockNumber, slotNumber, txs} =
    mkAnyAfterShelleyBlock txs $ babbageHeader v slotNumber' blockNumber'
  where
    slotNumber' = L.SlotNo $ fromIntegral $ unSlotNo slotNumber
    blockNumber' = L.BlockNo $ fromIntegral $ unBlockNo blockNumber

babbageHeader
    :: Version
    -> L.SlotNo
    -> L.BlockNo
    -> Header StandardCrypto
babbageHeader v slotNumber blockNumber =
    Header <*> mkSignedKES $ babbageBody v slotNumber blockNumber

babbageBody
    :: Version -> L.SlotNo -> L.BlockNo -> HeaderBody StandardCrypto
babbageBody v slotNumber blockNumber =
    HeaderBody
        { hbBlockNo = blockNumber
        , hbSlotNo = slotNumber
        , hbPrev = BlockHash hashHeader
        , hbVk = VKey $ deriveVerKeyDSIGN mkKeyDSIGN'
        , hbVrfVk = deriveVerKeyVRF mkKeyVRF'
        , hbVrfRes =
            uncurry CertifiedVRF
                $ evalVRF () ("" :: ByteString) mkKeyVRF'
        , hbBodySize = 42
        , hbBodyHash = bodyHash
        , hbOCert = oCertamente
        , hbProtVer = ProtVer v 0
        }
