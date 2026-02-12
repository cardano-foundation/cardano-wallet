{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.Wallet.Read.Block.Gen.Shelley
    ( block
    , bodyHash
    , mkKeyDSIGN'
    , mkKeyKES'
    , mkKeyVRF'
    , hashHeader
    , HeaderEra
    , mkAnyAfterShelleyBlock
    , mkShelleyBlock
    , mkSignedKES
    , oCertamente
    )
where

import Cardano.Crypto.DSIGN
    ( DSIGNAlgorithm (..)
    , Ed25519DSIGN
    , signedDSIGN
    )
import Cardano.Crypto.Hash
    ( ByteString
    )
import Cardano.Crypto.KES
    ( UnsoundPureKESAlgorithm (..)
    , UnsoundPureSignKeyKES
    , sizeSignKeyKES
    , unsoundPureSignedKES
    )
import Cardano.Crypto.Seed
    ( mkSeedFromBytes
    )
import Cardano.Crypto.Util
    ( SignableRepresentation
    )
import Cardano.Crypto.VRF
    ( CertifiedVRF (CertifiedVRF)
    , VRFAlgorithm (..)
    )
import Cardano.Crypto.VRF.Praos
    ( PraosVRF
    )
import Cardano.Ledger.BaseTypes
    ( ProtVer (..)
    , Version
    )
import Cardano.Ledger.Binary
    ( EncCBOR
    )
import Cardano.Ledger.Block
    ( Block (..)
    )
import Cardano.Ledger.Keys
    ( VKey (..)
    )
import Cardano.Protocol.Crypto
    ( StandardCrypto
    )
import Cardano.Protocol.TPraos.BHeader
    ( BHBody (..)
    , BHeader (..)
    , HashHeader (..)
    , PrevHash (..)
    )
import Cardano.Protocol.TPraos.OCert
    ( KESPeriod (..)
    , OCert (..)
    , OCertSignable (OCertSignable)
    )
import Cardano.Read.Ledger.Block.BlockNo
    ( BlockNo (..)
    )
import Cardano.Read.Ledger.Block.SlotNo
    ( SlotNo (..)
    )
import Cardano.Wallet.Read
    ( Tx
    , TxT
    , unTx
    )
import Cardano.Wallet.Read.Block.Gen.BlockParameters
    ( BlockParameters (..)
    )
import Data.Proxy
    ( Proxy (..)
    )
import Ouroboros.Consensus.Protocol.Praos.Header
    ( Header (..)
    )
import Ouroboros.Consensus.Protocol.TPraos
    ( TPraos
    )
import Prelude

import Cardano.Crypto.DSIGN qualified as Crypto
import Cardano.Crypto.Hash qualified as Crypto
import Cardano.Crypto.KES qualified as Crypto
import Cardano.Ledger.Api qualified as L
import Cardano.Ledger.Block qualified as L
import Cardano.Ledger.Core qualified as L
import Cardano.Ledger.Slot qualified as L
import Data.ByteString.Char8 qualified as B8
import Data.ByteString.Short qualified as BS
import Data.Sequence.Strict qualified as Seq
import Ouroboros.Consensus.Shelley.Ledger qualified as O
import Ouroboros.Consensus.Shelley.Protocol.Abstract qualified as O

type family HeaderEra era where
    HeaderEra L.ShelleyEra = BHeader StandardCrypto
    HeaderEra L.AllegraEra = BHeader StandardCrypto
    HeaderEra L.MaryEra = BHeader StandardCrypto
    HeaderEra L.AlonzoEra = BHeader StandardCrypto
    HeaderEra L.BabbageEra = Header StandardCrypto
    HeaderEra L.ConwayEra = Header StandardCrypto

--------------------------------------------------------------------------------
-- valid for shelley , allegra, mary, alonzo
--------------------------------------------------------------------------------

hbody :: Version -> L.SlotNo -> L.BlockNo -> BHBody StandardCrypto
hbody v slotNumber blockNumber =
    BHBody
        { bheaderBlockNo = blockNumber
        , bheaderSlotNo = slotNumber
        , bheaderPrev = BlockHash hashHeader
        , bheaderVk = VKey $ deriveVerKeyDSIGN mkKeyDSIGN'
        , bheaderVrfVk = deriveVerKeyVRF mkKeyVRF'
        , bheaderEta =
            uncurry CertifiedVRF
                $ evalVRF () ("" :: ByteString) mkKeyVRF'
        , bheaderL =
            uncurry CertifiedVRF
                $ evalVRF () ("" :: ByteString) mkKeyVRF'
        , bsize = 42
        , bhash = bodyHash
        , bheaderOCert = oCertamente
        , bprotver = ProtVer v 0
        }

headerShelley
    :: Version
    -> L.SlotNo
    -> L.BlockNo
    -> BHeader StandardCrypto
headerShelley v slotNumber blockNumber =
    BHeader <*> mkSignedKES $ hbody v slotNumber blockNumber

mkShelleyBlock
    :: ( L.EraSegWits era
       , EncCBOR (HeaderEra era)
       , HeaderEra era ~ BHeader StandardCrypto
       , TxT cardano_era ~ L.Tx era
       )
    => Version
    -> BlockParameters cardano_era
    -> O.ShelleyBlock (TPraos StandardCrypto) era
mkShelleyBlock v BlockParameters{blockNumber, slotNumber, txs} =
    mkAnyAfterShelleyBlock txs $ headerShelley v slotNumber' blockNumber'
  where
    slotNumber' = L.SlotNo $ fromIntegral $ unSlotNo slotNumber
    blockNumber' = L.BlockNo $ fromIntegral $ unBlockNo blockNumber

--------------------------------------------------------------------------------
-- valid for any era after shelley
--------------------------------------------------------------------------------

mkAnyAfterShelleyBlock
    :: ( L.EraSegWits era
       , EncCBOR (HeaderEra era)
       , HeaderEra era ~ O.ShelleyProtocolHeader proto
       , TxT era2 ~ L.Tx era
       )
    => [Tx era2]
    -> HeaderEra era
    -> O.ShelleyBlock proto era
mkAnyAfterShelleyBlock txs header =
    O.ShelleyBlock (block txs' header) hash
  where
    txs' = unTx <$> txs

hash :: O.ShelleyHash
hash = O.ShelleyHash $ Crypto.UnsafeHash $ BS.pack $ replicate 32 42

block
    :: ( L.EraSegWits era
       , EncCBOR (HeaderEra era)
       )
    => [L.Tx era]
    -> HeaderEra era
    -> L.Block (HeaderEra era) era
block txs header' = Block header' (txseq txs)

txseq
    :: (L.EraSegWits era)
    => [L.Tx era]
    -> L.TxSeq era
txseq = L.toTxSeq . Seq.fromList

type KES = Crypto.Sum6KES Crypto.Ed25519DSIGN Crypto.Blake2b_256

mkSignedKES
    :: SignableRepresentation a
    => a
    -> Crypto.SignedKES KES a
mkSignedKES hbody' = unsoundPureSignedKES () 42 hbody' unsoundPureKeyKES

oCertamente :: OCert StandardCrypto
oCertamente =
    OCert
        { ocertVkHot = unsoundPureDeriveVerKeyKES unsoundPureKeyKES
        , ocertN = 42
        , ocertKESPeriod = KESPeriod 42
        , ocertSigma = signedDSIGN () oCertSignable $ genKeyDSIGN seedKeyDSIGN
        }

oCertSignable :: OCertSignable StandardCrypto
oCertSignable =
    OCertSignable (unsoundPureDeriveVerKeyKES unsoundPureKeyKES) 42
        $ KESPeriod 42

unsoundPureKeyKES :: UnsoundPureSignKeyKES KES
unsoundPureKeyKES = unsoundPureGenKeyKES seedKeyKES

seedKeyKES :: Crypto.Seed
seedKeyKES =
    mkSeedFromBytes
        $ B8.pack
        $ flip replicate 'a'
        $ fromIntegral
        $ sizeSignKeyKES (Proxy @KES)

mkKeyKES' :: UnsoundPureKESAlgorithm a => UnsoundPureSignKeyKES a
mkKeyKES' = unsoundPureGenKeyKES seedKeyKES

bodyHash :: Crypto.Hash Crypto.Blake2b_256 L.EraIndependentBlockBody
bodyHash = Crypto.UnsafeHash $ BS.pack $ replicate 32 42

seedKeyVRF :: Crypto.Seed
seedKeyVRF =
    mkSeedFromBytes
        $ B8.pack
        $ flip replicate 'a'
        $ fromIntegral
        $ sizeSignKeyVRF (Proxy :: Proxy PraosVRF)

mkKeyVRF' :: VRFAlgorithm a => SignKeyVRF a
mkKeyVRF' = genKeyVRF seedKeyVRF

hashHeader :: HashHeader
hashHeader = HashHeader $ Crypto.UnsafeHash $ BS.pack $ replicate 32 42

{-# NOINLINE seedKeyDSIGN #-}
seedKeyDSIGN :: Crypto.Seed
seedKeyDSIGN =
    mkSeedFromBytes
        $ B8.pack
        $ flip replicate 'a'
        $ fromIntegral
        $ Crypto.seedSizeDSIGN (Proxy :: Proxy Ed25519DSIGN)

mkKeyDSIGN' :: DSIGNAlgorithm a => SignKeyDSIGN a
mkKeyDSIGN' = genKeyDSIGN seedKeyDSIGN
