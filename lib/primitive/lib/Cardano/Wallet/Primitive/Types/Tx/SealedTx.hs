{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- This module provides the `SealedTx` data type.
module Cardano.Wallet.Primitive.Types.Tx.SealedTx
    ( -- * Types
      SealedTx (serialisedTx, unsafeReadTx)
    , cardanoTxIdeallyNoLaterThan
    , sealedTxFromBytes
    , sealedTxFromBytes'
    , sealedTxFromCardano
    , sealedTxFromCardano'
    , sealedTxFromCardanoBody
    , sealedTxFromLedgerTx
    , unsafeSealedTxFromBytes
    , SerialisedTx (..)
    , getSealedTxBody
    , getSealedTxWitnesses
    , sealedTxWitnessCount
    , persistSealedTx
    , unPersistSealedTx

      -- * Unit testing helpers
    , mockSealedTx
    , withinEra
    )
where

import Cardano.Api
    ( AnyCardanoEra (..)
    , CardanoEra (..)
    , InAnyCardanoEra (..)
    , anyCardanoEra
    , deserialiseFromCBOR
    )
import Cardano.Api.Tx
    ( Tx (ShelleyTx)
    )
import Cardano.Ledger.Api
    ( addrTxWitsL
    , bootAddrTxWitsL
    , witsTxL
    )
import Cardano.Ledger.Binary
    ( DecoderError
    )
import Cardano.Read.Ledger.Tx.CBOR
    ( deserializeTx
    , serializeTx
    )
import Cardano.Wallet.Read
    ( EraValue (..)
    )
import Cardano.Wallet.Read.Eras
    ( Allegra
    , Alonzo
    , Babbage
    , Conway
    , Dijkstra
    , Era (..)
    , IsEra
    , Mary
    , Shelley
    , theEra
    )
import Cardano.Wallet.Util
    ( HasCallStack
    , internalError
    )
import Control.DeepSeq
    ( NFData (..)
    , deepseq
    )
import Control.Lens
    ( (^.)
    )
import Data.Bifunctor
    ( first
    )
import Data.ByteArray
    ( ByteArray
    , ByteArrayAccess
    )
import Data.ByteString
    ( ByteString
    )
import Data.Either
    ( partitionEithers
    )
import Data.Function
    ( on
    )
import Data.Text
    ( Text
    )
import Fmt
    ( Buildable (..)
    , hexF
    , (+||)
    , (||+)
    )
import GHC.Generics
    ( Generic
    )
import Prelude

import qualified Cardano.Api as Cardano
import qualified Cardano.Wallet.Read as Read
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import qualified Data.Set as Set
import qualified Data.Text as T

-- | 'SealedTx' is a transaction for any hard fork era,
-- possibly incomplete, possibly unsigned, with dual
-- representations to make it convenient to use.
--
-- Serialisation/deserialisation is usually done at the
-- application boundaries (e.g. in the API server), and
-- then the wallet core can use it either as a
-- 'ByteString', or as a 'Read.Tx'.
--
-- Construct it with either 'sealedTxFromCardano' or
-- 'sealedTxFromBytes'.
data SealedTx = SealedTx
    { valid :: Bool
    -- ^ Internal flag - indicates that the
    -- 'serialisedTx' bytes encode a valid Cardano
    -- transaction. If the "proper" constructors are
    -- used, this will always be True, but it will be
    -- False if 'mockSealedTx' is used to construct a
    -- 'SealedTx' for unit tests.
    , unsafeReadTx :: EraValue Read.Tx
    -- ^ Decoded transaction.
    -- Potentially in the wrong era.
    , serialisedTx :: ByteString
    -- ^ CBOR-serialised bytes of the transaction.
    }
    deriving stock (Generic)

instance Show SealedTx where
    showsPrec d (SealedTx v tx' bs) =
        showParen (d > 10)
            $ showString "SealedTx "
                . ( if v
                        then showsPrec 11 tx'
                        else showString "undefined"
                  )
                . showChar ' '
                . showsPrec 11 bs
                . showChar ' '
                . showsPrec 11 v

instance Buildable SealedTx where
    build (SealedTx v tx' bs)
        | v = build (show tx')
        | otherwise = hexF bs

instance Eq SealedTx where
    SealedTx v1 _ bs1 == SealedTx v2 _ bs2
        | v1 == v2 = bs1 == bs2
        | otherwise = False

instance NFData SealedTx where
    rnf (SealedTx v _ bs) =
        v `deepseq` bs `deepseq` ()

-- | Temporary: reconstructs 'InAnyCardanoEra Cardano.Tx'
-- from bytes on demand. Will be removed when callers
-- migrate to 'Read.Tx'.
cardanoTxIdeallyNoLaterThan
    :: AnyCardanoEra
    -> SealedTx
    -> InAnyCardanoEra Cardano.Tx
cardanoTxIdeallyNoLaterThan maxEra stx =
    case cardanoTxFromBytes
        maxEra
        (serialisedTx stx) of
        Right tx -> tx
        Left _ ->
            -- Fall back: try all eras
            case cardanoTxFromBytes
                maxBound
                (serialisedTx stx) of
                Right tx -> tx
                Left e ->
                    internalError
                        $ "cardanoTxIdeallyNoLaterThan: "
                        +|| e
                        ||+ ""

-- | Temporary: reconstructs cardano-api TxBody from
-- bytes. Will be removed when callers migrate.
getSealedTxBody
    :: SealedTx -> InAnyCardanoEra Cardano.TxBody
getSealedTxBody stx =
    case cardanoTxFromBytes
        maxBound
        (serialisedTx stx) of
        Right (InAnyCardanoEra era tx) ->
            InAnyCardanoEra era (Cardano.getTxBody tx)
        Left e ->
            internalError
                $ "getSealedTxBody: "
                +|| e
                ||+ ""

-- | Temporary: reconstructs cardano-api witnesses from
-- bytes. Will be removed when callers migrate.
getSealedTxWitnesses
    :: SealedTx
    -> [InAnyCardanoEra Cardano.KeyWitness]
getSealedTxWitnesses stx =
    case cardanoTxFromBytes
        maxBound
        (serialisedTx stx) of
        Right (InAnyCardanoEra era tx) ->
            [ InAnyCardanoEra era w
            | w <- Cardano.getTxWitnesses tx
            ]
        Left e ->
            internalError
                $ "getSealedTxWitnesses: "
                +|| e
                ||+ ""

-- | Total number of key witnesses (VKey + bootstrap) in
-- a 'SealedTx'. Ledger-native: does not round-trip
-- through cardano-api.
--
-- Equivalent of @length (getSealedTxWitnesses stx)@ for
-- the wallet's fee / witness-count accounting. Byron txs
-- are counted as 0 — the wallet does not construct Byron
-- txs through 'SealedTx'.
sealedTxWitnessCount :: SealedTx -> Int
sealedTxWitnessCount stx = case unsafeReadTx stx of
    EraValue (Read.Tx tx :: Read.Tx era) -> case theEra @era of
        Byron -> 0
        Shelley -> countLedgerTxWits tx
        Allegra -> countLedgerTxWits tx
        Mary -> countLedgerTxWits tx
        Alonzo -> countLedgerTxWits tx
        Babbage -> countLedgerTxWits tx
        Conway -> countLedgerTxWits tx
        Dijkstra -> countLedgerTxWits tx
  where
    countLedgerTxWits tx =
        Set.size (tx ^. witsTxL . addrTxWitsL)
            + Set.size (tx ^. witsTxL . bootAddrTxWitsL)

-- | Convert a cardano-api 'Tx' to 'EraValue Read.Tx'.
cardanoApiTxToReadTx
    :: Cardano.Tx era -> EraValue Read.Tx
cardanoApiTxToReadTx (ShelleyTx sbe ledgerTx) =
    case sbe of
        Cardano.ShelleyBasedEraShelley ->
            EraValue (Read.Tx ledgerTx :: Read.Tx Shelley)
        Cardano.ShelleyBasedEraAllegra ->
            EraValue (Read.Tx ledgerTx :: Read.Tx Allegra)
        Cardano.ShelleyBasedEraMary ->
            EraValue (Read.Tx ledgerTx :: Read.Tx Mary)
        Cardano.ShelleyBasedEraAlonzo ->
            EraValue (Read.Tx ledgerTx :: Read.Tx Alonzo)
        Cardano.ShelleyBasedEraBabbage ->
            EraValue (Read.Tx ledgerTx :: Read.Tx Babbage)
        Cardano.ShelleyBasedEraConway ->
            EraValue (Read.Tx ledgerTx :: Read.Tx Conway)
        Cardano.ShelleyBasedEraDijkstra ->
            EraValue (Read.Tx ledgerTx :: Read.Tx Dijkstra)

-- | Construct a 'SealedTx' from a "Cardano.Api"
-- transaction.
sealedTxFromCardano
    :: InAnyCardanoEra Cardano.Tx -> SealedTx
sealedTxFromCardano (InAnyCardanoEra _era tx) =
    let readTx = cardanoApiTxToReadTx tx
        bs = cardanoApiTxToBytes tx
    in  SealedTx True readTx bs
  where
    cardanoApiTxToBytes
        :: Cardano.Tx e -> ByteString
    cardanoApiTxToBytes tx'@(ShelleyTx sbe _) =
        Cardano.shelleyBasedEraConstraints
            sbe
            (Cardano.serialiseToCBOR tx')

-- | Construct a 'SealedTx' from a "Cardano.Api"
-- transaction.
sealedTxFromCardano'
    :: Cardano.IsCardanoEra era
    => Cardano.Tx era
    -> SealedTx
sealedTxFromCardano' =
    sealedTxFromCardano
        . InAnyCardanoEra Cardano.cardanoEra

-- | Construct a 'SealedTx' from a 'Cardano.Api.TxBody'.
sealedTxFromCardanoBody
    :: Cardano.IsCardanoEra era
    => Cardano.TxBody era
    -> SealedTx
sealedTxFromCardanoBody =
    sealedTxFromCardano
        . InAnyCardanoEra Cardano.cardanoEra
        . mk
  where
    mk body = Cardano.Tx body []

-- | Try to deserialize bytes into 'EraValue Read.Tx',
-- trying the newest ledger era first, then older eras.
readTxFromBytes
    :: AnyCardanoEra
    -> ByteString
    -> Either DecoderError (EraValue Read.Tx)
readTxFromBytes maxEra bs =
    asum
        $ map snd
        $ filter
            (withinEra maxEra . fst)
            [ tryEra @Dijkstra
                (AnyCardanoEra DijkstraEra)
            , tryEra @Conway
                (AnyCardanoEra ConwayEra)
            , tryEra @Babbage
                (AnyCardanoEra BabbageEra)
            , tryEra @Alonzo
                (AnyCardanoEra AlonzoEra)
            , tryEra @Mary
                (AnyCardanoEra MaryEra)
            , tryEra @Allegra
                (AnyCardanoEra AllegraEra)
            , tryEra @Shelley
                (AnyCardanoEra ShelleyEra)
            ]
  where
    lbs = BL.fromStrict bs

    tryEra
        :: forall era
         . IsEra era
        => AnyCardanoEra
        -> ( AnyCardanoEra
           , Either DecoderError (EraValue Read.Tx)
           )
    tryEra eraTag =
        ( eraTag
        , EraValue
            <$> (deserializeTx lbs :: Either DecoderError (Read.Tx era))
        )

    asum :: [Either e a] -> Either e a
    asum xs = case partitionEithers xs of
        (_, (a : _)) -> Right a
        ((e : _), []) -> Left e
        ([], []) ->
            internalError "readTxFromBytes: impossible"

-- | Deserialise a Cardano transaction. The transaction
-- can be in the format of any era. This function will
-- try the most recent era first, then previous eras.
--
-- Temporary: kept for functions that still need
-- cardano-api types. Will be removed.
--
-- Dijkstra is intentionally absent here because the wallet's
-- temporary 'CardanoApiEra' bridge does not support it. The
-- ledger-native 'readTxFromBytes' path above already tries Dijkstra.
cardanoTxFromBytes
    :: AnyCardanoEra
    -> ByteString
    -> Either DecoderError (InAnyCardanoEra Cardano.Tx)
cardanoTxFromBytes maxEra bs =
    asum
        $ map snd
        $ filter
            (withinEra maxEra . fst)
            [ deserialise
                ConwayEra
                Cardano.AsConwayEra
            , deserialise
                BabbageEra
                Cardano.AsBabbageEra
            , deserialise
                AlonzoEra
                Cardano.AsAlonzoEra
            , deserialise
                MaryEra
                Cardano.AsMaryEra
            , deserialise
                AllegraEra
                Cardano.AsAllegraEra
            , deserialise
                ShelleyEra
                Cardano.AsShelleyEra
            ]
  where
    deserialise
        :: forall era
         . Cardano.IsShelleyBasedEra era
        => CardanoEra era
        -> Cardano.AsType era
        -> ( AnyCardanoEra
           , Either
                DecoderError
                (InAnyCardanoEra Cardano.Tx)
           )
    deserialise cera asEra =
        ( anyCardanoEra cera
        , InAnyCardanoEra cera
            <$> deserialiseFromCBOR
                (Cardano.AsTx asEra)
                bs
        )

    asum :: [Either e a] -> Either e a
    asum xs = case partitionEithers xs of
        (_, (a : _)) -> Right a
        ((e : _), []) -> Left e
        ([], []) ->
            internalError
                "cardanoTxFromBytes: impossible"

-- | @a `withinEra` b@ is 'True' iff @b@ is the same
-- era as @a@, or an earlier one.
withinEra :: AnyCardanoEra -> AnyCardanoEra -> Bool
withinEra = (>=) `on` numberEra
  where
    numberEra :: AnyCardanoEra -> Int
    numberEra (AnyCardanoEra e) = case e of
        ByronEra -> 1
        ShelleyEra -> 2
        AllegraEra -> 3
        MaryEra -> 4
        AlonzoEra -> 5
        BabbageEra -> 6
        ConwayEra -> 7
        _ -> 8

-- | Construct a 'SealedTx' from a ledger-native
-- 'Read.Tx'. Serialises to CBOR via ledger-read;
-- bypasses cardano-api entirely.
sealedTxFromLedgerTx
    :: forall era
     . Read.IsEra era
    => Read.Tx era -> SealedTx
sealedTxFromLedgerTx tx =
    SealedTx True (EraValue tx) (BL.toStrict (serializeTx tx))

-- | Deserialise a transaction to construct a
-- 'SealedTx'.
sealedTxFromBytes
    :: ByteString -> Either DecoderError SealedTx
sealedTxFromBytes = sealedTxFromBytes' maxBound

-- | Deserialise a transaction to construct a
-- 'SealedTx'.
sealedTxFromBytes'
    :: AnyCardanoEra
    -- ^ Most recent era
    -> ByteString
    -- ^ Serialised transaction
    -> Either DecoderError SealedTx
sealedTxFromBytes' maxEra bs =
    SealedTx True
        <$> readTxFromBytes maxEra bs
        <*> pure bs

-- | Serialise a 'SealedTx' for storage in a database
-- field. The difference between 'persistSealedTx' and
-- 'serialisedTx' is that this function has a special
-- check for values created by 'mockSealedTx'.
persistSealedTx :: SealedTx -> ByteString
persistSealedTx tx = header <> serialisedTx tx
  where
    header =
        if valid tx then mempty else mockSealedTxMagic

-- | Deserialise a 'SealedTx' which has been stored in
-- a database field. This function includes a special
-- check for 'mockSealedTx' values.
unPersistSealedTx
    :: ByteString -> Either Text SealedTx
unPersistSealedTx bs = case unPersistMock bs of
    Nothing ->
        first (T.pack . show)
            $ sealedTxFromBytes bs
    Just bs' -> Right $ mockSealedTx bs'

-- | A header for use by 'persistSealedTx' and
-- 'unPersistSealedTx'. A valid serialised Cardano
-- transaction could not have this header, because they
-- always start with a CBOR map.
mockSealedTxMagic :: ByteString
mockSealedTxMagic = "MOCK"

unPersistMock :: ByteString -> Maybe ByteString
unPersistMock bs
    | header == mockSealedTxMagic = Just body
    | otherwise = Nothing
  where
    (header, body) =
        B8.splitAt (B8.length mockSealedTxMagic) bs

-- | A serialised transaction that may be only partially
-- signed, or even invalid.
newtype SerialisedTx = SerialisedTx
    {payload :: ByteString}
    deriving stock (Show, Eq, Generic, Ord)
    deriving newtype
        ( Semigroup
        , Monoid
        , ByteArray
        , ByteArrayAccess
        , NFData
        )

{----------------------------------------------------
    Internal functions for unit testing
----------------------------------------------------}

-- | Only use this for tests.
unsafeSealedTxFromBytes
    :: HasCallStack => ByteString -> SealedTx
unsafeSealedTxFromBytes =
    either (internalError . errMsg) id
        . sealedTxFromBytes
  where
    errMsg reason =
        "unsafeSealedTxFromBytes: "
            +|| reason
            ||+ ""

-- | Construct a 'SealedTx' from a string which need
-- not be a well-formed serialised Cardano transaction.
--
-- Be careful using the 'SealedTx', because any attempt
-- to evaluate its 'unsafeReadTx' field will crash.
mockSealedTx
    :: HasCallStack => ByteString -> SealedTx
mockSealedTx =
    SealedTx
        False
        ( internalError
            "mockSealedTx: \
            \attempted to decode gibberish"
        )
