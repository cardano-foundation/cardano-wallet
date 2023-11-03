{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Cardano.Wallet.Read.Block.Gen.Byron
    ( mkByronBlock
    )
where

import Prelude

import Cardano.Api
    ( ByronEra
    )
import Cardano.Chain.Block
    ( mkHeaderExplicit
    )
import Cardano.Chain.Slotting
    ( EpochSlots
    )
import Cardano.Crypto.Hashing
    ( serializeCborHash
    )
import Cardano.Crypto.ProtocolMagic
    ( ProtocolMagicId (..)
    )
import Cardano.Crypto.Signing
    ( SigningKey (..)
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
import Cardano.Wallet.Read.Tx
    ( Tx (..)
    )
import Data.Coerce
    ( coerce
    )
import Data.Text
    ( Text
    )
import Test.Cardano.Crypto.CBOR
    ( getBytes
    )
import Test.Cardano.Ledger.Alonzo.AlonzoEraGen
    ()

import qualified Cardano.Chain.Block as Byron
import qualified Cardano.Chain.Common as Byron
import qualified Cardano.Chain.Delegation as Byron
import qualified Cardano.Chain.Delegation as Byron.Delegation
import qualified Cardano.Chain.Slotting as L
import qualified Cardano.Chain.Ssc as Byron
import qualified Cardano.Chain.Update as Byron
import qualified Cardano.Chain.Update as Byron.Update
import qualified Cardano.Chain.UTxO as Byron
import qualified Cardano.Crypto.Signing as S
import qualified Cardano.Crypto.Wallet as CC
import qualified Ouroboros.Consensus.Byron.Ledger as O

mkByronBlock :: BlockParameters ByronEra -> O.ByronBlock
mkByronBlock
    BlockParameters
        { slotNumber = SlotNo slotNo
        , blockNumber = BlockNo blockNo
        , txs
        } =
        O.annotateByronBlock eslots
            $ Byron.ABlock (header slotNo' blockNo' body) body mempty
      where
        body :: Byron.ABody ()
        body = Byron.ABody txs' Byron.SscPayload delegation update

        txs' :: Byron.ATxPayload ()
        txs' = Byron.ATxPayload $ fmap unTx txs

        slotNo' :: L.SlotNumber
        slotNo' = L.SlotNumber $ fromIntegral slotNo

        blockNo' :: Byron.ChainDifficulty
        blockNo' = Byron.ChainDifficulty $ fromIntegral blockNo

header :: L.SlotNumber -> Byron.ChainDifficulty -> Byron.Body -> Byron.Header
header slotNumber blockNumber body =
    mkHeaderExplicit
        protocolMagicId
        headerHash
        blockNumber
        eslots
        slotNumber
        issuerSk
        delegationCertificate
        body
        protocolVersion
        softwareVersion

delegation :: Byron.Delegation.APayload ()
delegation = Byron.Delegation.UnsafeAPayload mempty mempty

update :: Byron.Update.APayload ()
update = Byron.Update.APayload Nothing mempty mempty

eslots :: EpochSlots
eslots = L.EpochSlots 20

protocolMagicId :: ProtocolMagicId
protocolMagicId = ProtocolMagicId 7

headerHash :: Byron.HeaderHash
headerHash = coerce $ serializeCborHash ("HeaderHash" :: Text)

protocolVersion :: Byron.ProtocolVersion
protocolVersion = Byron.ProtocolVersion 3 0 0

softwareVersion :: Byron.SoftwareVersion
softwareVersion = Byron.SoftwareVersion (Byron.ApplicationName "Golden") 99

issuerSk :: SigningKey
delegateSk :: SigningKey
[delegateSk, issuerSk] = someSigningKeys 5 2

someSigningKeys :: Int -> Int -> [SigningKey]
someSigningKeys offset count = map (toKey . (* offset)) [0 .. count - 1]
  where
    toKey start = case CC.xprv (getBytes start 128) of
        Left err -> error $ "exampleSigningKeys: " <> show err
        Right sk -> SigningKey sk

delegationCertificate :: Byron.Certificate
delegationCertificate =
    Byron.signCertificate
        (ProtocolMagicId 7)
        (S.toVerification delegateSk)
        (L.EpochNumber 5)
        (S.noPassSafeSigner issuerSk)
