{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Wallet.Read.Block.Gen.Shelley
    ( block
    , bodyHash
    , genKeyDSIGN'
    , genKeyKES'
    , genKeyVRF'
    , hashHeader
    , HeaderEra
    , mkAnyAfterShelleyBlock
    , mkShelleyBlock
    , mkSignedKES
    , oCertamente
    )
where

import Prelude

import Cardano.Crypto.DSIGN
    ( DSIGNAlgorithm (..)
    , Ed25519DSIGN
    , signedDSIGN
    )
import Cardano.Crypto.Hash
    ( ByteString
    )
import Cardano.Crypto.KES
    ( KESAlgorithm (..)
    )
import Cardano.Crypto.Seed
    ( readSeedFromSystemEntropy
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
    , natVersion
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
import Data.Void
    ( Void
    )
import Ouroboros.Consensus.Protocol.Praos.Header
    ( Header (..)
    )
import Ouroboros.Consensus.Protocol.TPraos
    ( StandardCrypto
    , TPraos
    )
import System.IO.Unsafe
    ( unsafePerformIO
    )

import qualified Cardano.Crypto.DSIGN as Crypto
import qualified Cardano.Crypto.Hash as Crypto
import qualified Cardano.Crypto.KES as Crypto
import qualified Cardano.Ledger.Api as L
import qualified Cardano.Ledger.Block as L
import qualified Cardano.Ledger.Core as L
import qualified Cardano.Ledger.Slot as L
import Cardano.Wallet.Read.Block.BlockNo
    ( BlockNo (..)
    )
import Cardano.Wallet.Read.Block.SlotNo
    ( SlotNo (..)
    )
import qualified Data.Sequence.Strict as Seq
import qualified Ouroboros.Consensus.Shelley.Ledger as O
import qualified Ouroboros.Consensus.Shelley.Protocol.Abstract as O

type family HeaderEra era where
    HeaderEra L.ShelleyEra = BHeader StandardCrypto
    HeaderEra L.AllegraEra = BHeader StandardCrypto
    HeaderEra L.MaryEra = BHeader StandardCrypto
    HeaderEra L.AlonzoEra = BHeader StandardCrypto
    HeaderEra L.BabbageEra = Header StandardCrypto
    HeaderEra L.ConwayEra = Header StandardCrypto
    HeaderEra _ = Void

--------------------------------------------------------------------------------
-- valid for shelley , allegra, mary, alonzo
--------------------------------------------------------------------------------

hbody :: L.SlotNo -> L.BlockNo -> BHBody StandardCrypto
hbody slotNumber blockNumber =
    BHBody
        { bheaderBlockNo = blockNumber
        , bheaderSlotNo = slotNumber
        , bheaderPrev = BlockHash hashHeader
        , bheaderVk = VKey $ deriveVerKeyDSIGN genKeyDSIGN'
        , bheaderVrfVk = deriveVerKeyVRF genKeyVRF'
        , bheaderEta =
            uncurry CertifiedVRF
                $ evalVRF () ("" :: ByteString) genKeyVRF'
        , bheaderL =
            uncurry CertifiedVRF
                $ evalVRF () ("" :: ByteString) genKeyVRF'
        , bsize = 42
        , bhash = bodyHash
        , bheaderOCert = oCertamente
        , bprotver = ProtVer (natVersion @4) 1
        }

headerShelley
    :: L.SlotNo
    -> L.BlockNo
    -> BHeader StandardCrypto
headerShelley slotNumber blockNumber =
    BHeader <*> mkSignedKES $ hbody slotNumber blockNumber

mkShelleyBlock
    :: ( L.EraSegWits (era StandardCrypto)
       , EncCBOR (HeaderEra era)
       , HeaderEra era ~ BHeader StandardCrypto
       , TxT cardano_era ~ L.Tx (era StandardCrypto)
       )
    => BlockParameters cardano_era
    -> O.ShelleyBlock (TPraos StandardCrypto) (era StandardCrypto)
mkShelleyBlock BlockParameters{blockNumber, slotNumber, txs} =
    mkAnyAfterShelleyBlock txs $ headerShelley slotNumber' blockNumber'
    where
        slotNumber' = L.SlotNo $ fromIntegral $ unSlotNo slotNumber
        blockNumber' = L.BlockNo $ fromIntegral $ unBlockNo blockNumber

--------------------------------------------------------------------------------
-- valid for any era after shelley
--------------------------------------------------------------------------------

mkAnyAfterShelleyBlock
    :: ( L.EraSegWits (era StandardCrypto)
       , EncCBOR (HeaderEra era)
       , O.ProtoCrypto proto ~ StandardCrypto
       , HeaderEra era ~ O.ShelleyProtocolHeader proto
       , TxT era2 ~ L.Tx (era StandardCrypto)
       )
    => [Tx era2]
    -> HeaderEra era
    -> O.ShelleyBlock proto (era StandardCrypto)
mkAnyAfterShelleyBlock txs header =
    O.ShelleyBlock (block txs' header) hash
  where
    txs' = unTx <$> txs

hash :: O.ShelleyHash StandardCrypto
hash = O.ShelleyHash $ Crypto.UnsafeHash "hash"

block
    :: ( L.EraSegWits (era StandardCrypto)
       , EncCBOR (HeaderEra era)
       )
    => [L.Tx (era StandardCrypto)]
    -> HeaderEra era
    -> L.Block (HeaderEra era) (era StandardCrypto)
block txs header' = Block header' (txseq txs)

txseq
    :: (L.EraSegWits (era StandardCrypto))
    => [L.Tx (era StandardCrypto)]
    -> L.TxSeq (era StandardCrypto)
txseq = L.toTxSeq . Seq.fromList

mkSignedKES
    :: SignableRepresentation a
    => a
    -> Crypto.SignedKES
        (Crypto.Sum6KES Crypto.Ed25519DSIGN Crypto.Blake2b_256)
        a
mkSignedKES hbody' = Crypto.signedKES () 42 hbody' $ genKeyKES seedGenKES

oCertamente :: OCert StandardCrypto
oCertamente =
    OCert
        { ocertVkHot = deriveVerKeyKES $ genKeyKES seedGenKES
        , ocertN = 42
        , ocertKESPeriod = KESPeriod 42
        , ocertSigma = signedDSIGN () oCertSignable $ genKeyDSIGN seedGenDSIGN
        }

oCertSignable :: OCertSignable StandardCrypto
oCertSignable =
    OCertSignable (deriveVerKeyKES $ genKeyKES seedGenKES) 42
        $ KESPeriod 42

{-# NOINLINE seedGenKES #-}
seedGenKES :: Crypto.Seed
seedGenKES =
    unsafePerformIO
        $ readSeedFromSystemEntropy
        $ seedSizeKES
            (Proxy :: Proxy (Crypto.Sum6KES Crypto.Ed25519DSIGN Crypto.Blake2b_256))

genKeyKES' :: KESAlgorithm a => SignKeyKES a
genKeyKES' = genKeyKES seedGenKES

bodyHash :: Crypto.Hash Crypto.Blake2b_256 L.EraIndependentBlockBody
bodyHash = Crypto.UnsafeHash "bodyHash"

{-# NOINLINE seedGenVRF #-}
seedGenVRF :: Crypto.Seed
seedGenVRF =
    unsafePerformIO
        $ readSeedFromSystemEntropy
        $ sizeSignKeyVRF (Proxy :: Proxy PraosVRF)

genKeyVRF' :: VRFAlgorithm a => SignKeyVRF a
genKeyVRF' = genKeyVRF seedGenVRF

hashHeader :: HashHeader StandardCrypto
hashHeader = HashHeader $ Crypto.UnsafeHash "hashHeader"

{-# NOINLINE seedGenDSIGN #-}
seedGenDSIGN :: Crypto.Seed
seedGenDSIGN =
    unsafePerformIO
        $ readSeedFromSystemEntropy
        $ Crypto.seedSizeDSIGN (Proxy :: Proxy Ed25519DSIGN)

genKeyDSIGN' :: DSIGNAlgorithm a => SignKeyDSIGN a
genKeyDSIGN' = genKeyDSIGN seedGenDSIGN
