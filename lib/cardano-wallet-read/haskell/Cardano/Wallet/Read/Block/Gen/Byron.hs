{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Cardano.Wallet.Read.Block.Gen.Byron
    ( mkByronBlock
    )
where

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
import Cardano.Read.Ledger.Block.BlockNo
    ( BlockNo (..)
    )
import Cardano.Read.Ledger.Block.SlotNo
    ( SlotNo (..)
    )
import Cardano.Wallet.Read.Block.Gen.BlockParameters
    ( BlockParameters (..)
    )
import Cardano.Wallet.Read.Eras
    ( Byron
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
import Prelude

import Cardano.Chain.Block qualified as Byron
import Cardano.Chain.Common qualified as Byron
import Cardano.Chain.Delegation qualified as Byron
import Cardano.Chain.Delegation qualified as Byron.Delegation
import Cardano.Chain.Slotting qualified as L
import Cardano.Chain.Ssc qualified as Byron
import Cardano.Chain.UTxO qualified as Byron
import Cardano.Chain.Update qualified as Byron
import Cardano.Chain.Update qualified as Byron.Update
import Cardano.Crypto.Signing qualified as S
import Cardano.Crypto.Wallet qualified as CC
import Data.ByteString qualified as BS
import Ouroboros.Consensus.Byron.Ledger qualified as O

mkByronBlock :: BlockParameters Byron -> O.ByronBlock
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

header
    :: L.SlotNumber -> Byron.ChainDifficulty -> Byron.Body -> Byron.Header
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

getBytes :: Int -> Int -> BS.ByteString
getBytes offset len = BS.take len $ BS.drop offset constantByteString

constantByteString :: BS.ByteString
constantByteString =
    "Kmyw4lDSE5S4fSH6etNouiXezCyEjKc3tG4ja0kFjO8qzai26ZMPUEJfEy15ox5kJ0uKD\
    \bi7i6dLXkuesVZ9JfHgjrctsLFt2NvovXnchsOvX05Y6LohlTNt5mkPFhUoXu1EZSJTIy\
    \3fTU53b412r4AEusD7tcdRgH47yTr5hMO63bJnYBbmNperLHfiT1lP0MLQLh1J1DfoYBs\
    \auoJOzvtAgvjHo6UFttnK6vZ3Cknpuob6uMS2MkJKmuoQsqsAYcRDWbJ2Rgw4bm2ndTM4\
    \zFfuRDKvdrL6sDkuPNPYqxMWlqnXjSbU0eLtceZuKgXLHR8cdvsEvywt4JaZUQhnbq3Vl\
    \7nZqcXdoi4XGTCgSGcGp8N0SDVhvkVh0QF1RVpWPnOMyYISJvuaHfo1zXMdq9tEdtJfID"

delegationCertificate :: Byron.Certificate
delegationCertificate =
    Byron.signCertificate
        (ProtocolMagicId 7)
        (S.toVerification delegateSk)
        (L.EpochNumber 5)
        (S.noPassSafeSigner issuerSk)
