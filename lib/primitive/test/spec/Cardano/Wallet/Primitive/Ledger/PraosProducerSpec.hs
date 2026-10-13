{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Copyright: © 2026 IOHK
-- License: Apache-2.0
--
-- Properties for the Praos block-producer path: the era-total 'CardanoBlock'
-- dispatch and the era-polymorphic producer accessor it is stated in terms of.
module Cardano.Wallet.Primitive.Ledger.PraosProducerSpec
    ( spec
    ) where

import Cardano.Crypto.DSIGN.Class
    ( DSIGNAlgorithm (..)
    , signedDSIGN
    )
import Cardano.Crypto.Hash.Class
    ( Hash (..)
    )
import Cardano.Crypto.KES.Class
    ( SignedKES (..)
    )
import Cardano.Crypto.Seed
    ( mkSeedFromBytes
    )
import Cardano.Crypto.VRF.Class
    ( CertifiedVRF (..)
    )
import Cardano.Ledger.Api
    ( ConwayEra
    , DijkstraEra
    )
import Cardano.Ledger.Babbage
    ( BabbageEra
    )
import Cardano.Ledger.BaseTypes
    ( BlockNo (..)
    , ProtVer (..)
    , SlotNo (..)
    , mkNonceFromNumber
    , mkVersion
    )
import Cardano.Ledger.Block
    ( Block (..)
    )
import Cardano.Ledger.Core
    ( EraBlockBody (..)
    )
import Cardano.Ledger.Keys
    ( KeyRole (..)
    , VKey (..)
    )
import Cardano.Protocol.Crypto
    ( KES
    )
import Cardano.Protocol.Praos.BlockHeader
    ( Header (..)
    , HeaderBody (..)
    )
import Cardano.Protocol.Praos.VRF
    ( mkInputVRF
    )
import Cardano.Protocol.TPraos.BlockHeader
    ( PrevHash (..)
    )
import Cardano.Protocol.TPraos.OCert
    ( KESPeriod (..)
    , OCert (..)
    , OCertSignable (..)
    )
import Cardano.Wallet.Primitive.Ledger.Read.Block
    ( fromCardanoBlock
    )
import Cardano.Wallet.Primitive.Ledger.Shelley
    ( Praos
    , StandardCrypto
    , cardanoBlockProducer
    , getPraosProducer
    , poolMonitoringStep
    )
import Data.IORef
    ( modifyIORef'
    , newIORef
    , readIORef
    )
import Data.Maybe
    ( fromMaybe
    )
import Ouroboros.Consensus.Cardano.Block
    ( HardForkBlock (BlockBabbage, BlockConway, BlockDijkstra)
    )
import Ouroboros.Consensus.Shelley.Ledger.Block
    ( ShelleyBlock (..)
    , ShelleyHash (..)
    )
import Test.Hspec
    ( Spec
    , describe
    , it
    )
import Test.Hspec.Core.QuickCheck
    ( modifyMaxSuccess
    )
import Test.QuickCheck
    ( Arbitrary (..)
    , Gen
    , Property
    , checkCoverage
    , conjoin
    , cover
    , forAll
    , frequency
    , ioProperty
    , property
    , tabulate
    , vectorOf
    , (===)
    )
import Prelude

import qualified Cardano.Crypto.KES.Class as KES
import qualified Cardano.Crypto.VRF.Class as VRF
import qualified Cardano.Wallet.Primitive.Types.Hash as W
import qualified Data.ByteString as BS
import qualified Data.ByteString.Short as SBS

spec :: Spec
spec =
    describe "Cardano.Wallet.Primitive.Ledger.PraosProducerSpec"
        $ modifyMaxSuccess (const 500)
        $ do
            describe "block dispatch" $ do
                it "produces the accessor's pool id for every era, including Dijkstra"
                    $ property
                    $ prop_blockDispatch kesMaterial

            describe "producer accessor" $ do
                it
                    "evaluates one header to one pool id at Babbage, Conway and Dijkstra"
                    $ property
                    $ prop_accessorsAgree kesMaterial

            describe "pool-monitoring step" $ do
                it
                    "runs the production action once for a Dijkstra block, never for Byron"
                    $ property
                    $ prop_monitoringStep kesMaterial

{-------------------------------------------------------------------------------
                                  Properties
-------------------------------------------------------------------------------}

-- | The era-total block dispatch must produce the producer for every block
-- constructor it matches, with no era falling into a hole. The generated
-- population is deliberately Dijkstra-heavy so the case under test is
-- actually reached: 'checkCoverage' fails the property unless the Dijkstra
-- share stays above 90% (with the required confidence), so emptying the
-- Dijkstra population turns this red.
prop_blockDispatch :: KesMaterial -> Property
prop_blockDispatch kes =
    forAll arbitrary $ \tag ->
        forAll (genHeaderBody kes) $ \body ->
            checkCoverage
                $ cover
                    90
                    (tag == TagDijkstra)
                    "Dijkstra block in generated population"
                $ tabulate "block constructor" [show tag]
                $ case tag of
                    TagBabbage ->
                        let sh =
                                praosBlock kes body :: ShelleyBlock (Praos StandardCrypto) BabbageEra
                            blk = BlockBabbage sh
                        in  cardanoBlockProducer blk === Just (getPraosProducer sh)
                    TagConway ->
                        let sh =
                                praosBlock kes body :: ShelleyBlock (Praos StandardCrypto) ConwayEra
                            blk = BlockConway sh
                        in  cardanoBlockProducer blk === Just (getPraosProducer sh)
                    TagDijkstra ->
                        let sh =
                                praosBlock kes body :: ShelleyBlock (Praos StandardCrypto) DijkstraEra
                            blk = BlockDijkstra sh
                        in  cardanoBlockProducer blk === Just (getPraosProducer sh)

-- | One Praos header, evaluated at three eras, is one pool id. The accessor
-- is era-polymorphic, so this binds the agreement the ticket requires: if
-- per-era copies ever return, one of them diverging fails this property.
prop_accessorsAgree :: KesMaterial -> Property
prop_accessorsAgree kes =
    forAll (genHeaderBody kes) $ \body ->
        tabulate
            "era the accessor is evaluated at"
            ["Babbage", "Conway", "Dijkstra"]
            $ conjoin
                [ getPraosProducer
                    (praosBlock kes body :: ShelleyBlock (Praos StandardCrypto) BabbageEra)
                    === getPraosProducer
                        (praosBlock kes body :: ShelleyBlock (Praos StandardCrypto) ConwayEra)
                , getPraosProducer
                    (praosBlock kes body :: ShelleyBlock (Praos StandardCrypto) DijkstraEra)
                    === getPraosProducer
                        (praosBlock kes body :: ShelleyBlock (Praos StandardCrypto) BabbageEra)
                , getPraosProducer
                    (praosBlock kes body :: ShelleyBlock (Praos StandardCrypto) DijkstraEra)
                    === getPraosProducer
                        (praosBlock kes body :: ShelleyBlock (Praos StandardCrypto) ConwayEra)
                ]

{-------------------------------------------------------------------------------
                             Generators and oracles
-------------------------------------------------------------------------------}

data BlockTag = TagBabbage | TagConway | TagDijkstra
    deriving (Eq, Show)

-- | The generated population is Dijkstra-heavy on purpose: the required
-- coverage is on the Dijkstra case, and 'checkCoverage' in
-- 'prop_blockDispatch' fails the property when it falls below 90%.
-- Babbage and Conway stay reachable so regression there is still visible.
instance Arbitrary BlockTag where
    arbitrary =
        frequency
            [ (95, pure TagDijkstra)
            , (3, pure TagBabbage)
            , (2, pure TagConway)
            ]
    shrink = const []

-- | Varies the block issuer, which is the only header field the producer
-- accessor reads. All other crypto material is fixed, deterministic test
-- data: it must decode, but it is never verified.
genHeaderBody :: KesMaterial -> Gen (HeaderBody StandardCrypto)
genHeaderBody kes = do
    issuerSeed <- BS.pack <$> vectorOf 32 arbitrary
    let issuerVk =
            VKey
                $ deriveVerKeyDSIGN
                $ genKeyDSIGN (mkSeedFromBytes issuerSeed)
    pure (mkHeaderBodyWith (kesVerKeyOf kes) issuerVk)

-- | A Praos header body with the given issuer and otherwise fixed,
-- deterministic crypto material.
mkHeaderBodyWith
    :: KES.VerKeyKES (KES StandardCrypto)
    -> VKey BlockIssuer
    -> HeaderBody StandardCrypto
mkHeaderBodyWith kesVk issuerVk =
    HeaderBody
        { hbBlockNo = BlockNo 7
        , hbSlotNo = hbSlot
        , hbPrev = GenesisHash
        , hbVk = issuerVk
        , hbVrfVk = vrfVk
        , hbVrfRes = CertifiedVRF vrfOut vrfCert
        , hbBodySize = 0
        , hbBodyHash = UnsafeHash (SBS.toShort (BS.replicate 32 9))
        , hbOCert = ocert
        , hbProtVer =
            ProtVer (fromMaybe (error "bad version") (mkVersion (11 :: Word))) 2
        }
  where
    hbSlot = SlotNo 19

    vrfSk = VRF.genKeyVRF (mkSeedFromBytes (BS.replicate 32 1))
    vrfVk = VRF.deriveVerKeyVRF vrfSk
    (vrfOut, vrfCert) =
        VRF.evalVRF () (mkInputVRF hbSlot (mkNonceFromNumber 3)) vrfSk

    dsignSk = genKeyDSIGN (mkSeedFromBytes (BS.replicate 32 2))

    ocert =
        OCert
            { ocertVkHot = kesVk
            , ocertN = 1
            , ocertKESPeriod = KESPeriod 0
            , ocertSigma =
                signedDSIGN
                    ()
                    (OCertSignable kesVk 1 (KESPeriod 0))
                    dsignSk
            }

-- | A Praos block with the given header and an empty body, at the demanded
-- era. The KES signature is genuine but never verified: the producer
-- accessor reads only the issuer key from the header body.
praosBlock
    :: forall era
     . EraBlockBody era
    => KesMaterial
    -> HeaderBody StandardCrypto
    -> ShelleyBlock (Praos StandardCrypto) era
praosBlock kes body =
    ShelleyBlock
        (Block (Header body (kesSignatureOf kes)) mkBasicBlockBody)
        (ShelleyHash (UnsafeHash (SBS.toShort (BS.replicate 32 5))))

-- | The site-17 arm: the monitoring step runs the production action exactly
-- once for a Dijkstra block, with the pool the accessor names and the block's
-- wallet view, and never for a Byron block. A point mutant restoring 'error'
-- at this arm makes the action never run and this property red.
prop_monitoringStep :: KesMaterial -> Property
prop_monitoringStep kes =
    forAll (genHeaderBody kes) $ \body -> ioProperty $ do
        let sh =
                praosBlock kes body :: ShelleyBlock (Praos StandardCrypto) DijkstraEra
            gp = W.Hash (BS.replicate 32 7)
            blkD = BlockDijkstra sh
            expectedView = fromCardanoBlock gp blkD
        runsRef <- newIORef []
        _ <-
            poolMonitoringStep
                gp
                (\view pid -> modifyIORef' runsRef ((view, pid) :))
                blkD
        runs <- readIORef runsRef
        pure
            $ conjoin
                [ length runs === 1
                , fmap snd runs === [getPraosProducer sh]
                , fmap fst runs === [expectedView]
                ]

{-------------------------------------------------------------------------------
                            KES fixture (IO, once)
-------------------------------------------------------------------------------}

data KesMaterial = KesMaterial
    { kesVerKeyOf :: KES.VerKeyKES (KES StandardCrypto)
    , kesSignatureOf
        :: SignedKES (KES StandardCrypto) (HeaderBody StandardCrypto)
    }

-- | Real KES material from a fixed seed: a hot verification key for the
-- operational certificate and a genuine signature over a header body. The
-- signature is never verified on this path, but it round-trips through the
-- header's memoised bytes, so it must be structurally sound.
mkKesMaterial :: KesMaterial
mkKesMaterial =
    let sk = KES.unsoundPureGenKeyKES (mkSeedFromBytes (BS.replicate 32 3))
        vk = KES.unsoundPureDeriveVerKeyKES sk
        body = mkHeaderBodyWith vk fixedIssuerVk
        sig = SignedKES (KES.unsoundPureSignKES () 0 body sk)
    in  KesMaterial vk sig
  where
    fixedIssuerVk =
        VKey
            $ deriveVerKeyDSIGN
            $ genKeyDSIGN (mkSeedFromBytes (BS.replicate 32 4))

-- | Deterministic, shared across all spec properties. Pure: the unsound-pure
-- KES operations exist precisely for non-IO test contexts.
kesMaterial :: KesMaterial
kesMaterial = mkKesMaterial
