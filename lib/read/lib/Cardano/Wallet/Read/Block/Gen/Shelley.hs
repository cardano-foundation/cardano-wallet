{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

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
import Cardano.Wallet.Read.Block.BlockNo
    ( BlockNo (..)
    )
import Cardano.Wallet.Read.Block.Gen.BlockParameters
    ( BlockParameters (..)
    )
import Cardano.Wallet.Read.Block.SlotNo
    ( SlotNo (..)
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

import qualified Cardano.Crypto.DSIGN as Crypto
import qualified Cardano.Crypto.Hash as Crypto
import qualified Cardano.Crypto.KES as Crypto
import qualified Cardano.Ledger.Api as L
import qualified Cardano.Ledger.Block as L
import qualified Cardano.Ledger.Core as L
import qualified Cardano.Ledger.Slot as L
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Short as BS
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
    :: ( L.EraSegWits (era StandardCrypto)
       , EncCBOR (HeaderEra era)
       , HeaderEra era ~ BHeader StandardCrypto
       , TxT cardano_era ~ L.Tx (era StandardCrypto)
       )
    => Version
    -> BlockParameters cardano_era
    -> O.ShelleyBlock (TPraos StandardCrypto) (era StandardCrypto)
mkShelleyBlock v BlockParameters{blockNumber, slotNumber, txs} =
    mkAnyAfterShelleyBlock txs $ headerShelley v slotNumber' blockNumber'
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
hash = O.ShelleyHash $ Crypto.UnsafeHash $ BS.pack $ replicate 32 42

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
mkSignedKES hbody' = Crypto.signedKES () 42 hbody' $ genKeyKES seedKeyKES

oCertamente :: OCert StandardCrypto
oCertamente =
    OCert
        { ocertVkHot = deriveVerKeyKES $ genKeyKES seedKeyKES
        , ocertN = 42
        , ocertKESPeriod = KESPeriod 42
        , ocertSigma = signedDSIGN () oCertSignable $ genKeyDSIGN seedKeyDSIGN
        }

oCertSignable :: OCertSignable StandardCrypto
oCertSignable =
    OCertSignable (deriveVerKeyKES $ genKeyKES seedKeyKES) 42 $ KESPeriod 42

seedKeyKES :: Crypto.Seed
seedKeyKES =
    mkSeedFromBytes
        $ B8.pack
        $ flip replicate 'a'
        $ fromIntegral
        $ sizeSignKeyKES
            (Proxy @(Crypto.Sum6KES Crypto.Ed25519DSIGN Crypto.Blake2b_256))

mkKeyKES' :: KESAlgorithm a => SignKeyKES a
mkKeyKES' = genKeyKES seedKeyKES

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

hashHeader :: HashHeader StandardCrypto
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
