{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- This module provides the `SealedTx` data type.
--
module Cardano.Wallet.Primitive.Types.Tx.SealedTx (
     -- * Types
    SealedTx (serialisedTx, unsafeCardanoTx)
    , cardanoTxIdeallyNoLaterThan
    , sealedTxFromBytes
    , sealedTxFromBytes'
    , sealedTxFromCardano
    , sealedTxFromCardano'
    , sealedTxFromCardanoBody
    , getSerialisedTxParts
    , unsafeSealedTxFromBytes
    , SerialisedTx (..)
    , SerialisedTxParts (..)
    , getSealedTxBody
    , getSealedTxWitnesses
    , persistSealedTx
    , unPersistSealedTx

    -- * Unit testing helpers
    , mockSealedTx
    , withinEra
    )
   where

import Prelude

import Cardano.Api
    ( AnyCardanoEra (..)
    , CardanoEra (..)
    , InAnyCardanoEra (..)
    , anyCardanoEra
    , deserialiseFromCBOR
    , serialiseToCBOR
    )
import Cardano.Binary
    ( DecoderError )
import Cardano.Wallet.Util
    ( HasCallStack, internalError )
import Control.DeepSeq
    ( NFData (..), deepseq )
import Data.Bifunctor
    ( first )
import Data.ByteArray
    ( ByteArray, ByteArrayAccess )
import Data.ByteString
    ( ByteString )
import Data.Either
    ( partitionEithers )
import Data.Function
    ( on )
import Data.Text
    ( Text )
import Data.Type.Equality
    ( (:~:) (..), testEquality )
import Fmt
    ( Buildable (..), Builder, hexF, (+||), (||+) )
import GHC.Generics
    ( Generic )
import Text.Pretty.Simple
    ( pShowNoColor )

import qualified Cardano.Api as Cardano
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text as T

-- | 'SealedTx' is a transaction for any hard fork era, possibly incomplete,
-- possibly unsigned, with dual representations to make it convenient to use.
--
-- Serialisation/deserialisation is usually done at the application boundaries
-- (e.g. in the API server), and then the wallet core can use it either as a
-- 'ByteString', or as a 'Cardano.Api.Tx'.
--
-- Construct it with either 'sealedTxFromCardano' or 'sealedTxFromBytes'.
data SealedTx = SealedTx
    { valid :: Bool
    -- ^ Internal flag - indicates that the 'serialisedTx' bytes encode a valid
    -- Cardano transaction. If the "proper" constructors are used, this will
    -- always be True, but it will be False if 'mockSealedTx' is used to
    -- construct a 'SealedTx' for unit tests.

    , unsafeCardanoTx :: InAnyCardanoEra Cardano.Tx
    -- ^ Decoded transaction. Potentially in the wrong era.

    , serialisedTx :: ByteString
    -- ^ CBOR-serialised bytes of the transaction.

    } deriving stock Generic

instance Show SealedTx where
    -- InAnyCardanoEra is missing a Show instance, so define one inline.
    showsPrec d (SealedTx v tx' bs) = showParen (d > 10) $
        showString "SealedTx " .
        (if v then showParen True (showsTx tx') else showString "undefined") .
        showChar ' ' .
        showsPrec 11 bs .
        showChar ' ' .
        showsPrec 11 v
      where
        showsTx :: InAnyCardanoEra Cardano.Tx -> ShowS
        showsTx (InAnyCardanoEra era tx) =
            showString "InAnyCardanoEra" .
            showChar ' ' .
            showsPrec 11 era .
            showChar ' ' .
            showsPrec 11 tx

instance Buildable SealedTx where
    build (SealedTx v tx' bs) = if v then buildTx tx' else hexF bs
      where
        buildTx :: InAnyCardanoEra Cardano.Tx -> Builder
        buildTx (InAnyCardanoEra _ tx) = build $ pShowNoColor tx

instance Eq SealedTx where
    SealedTx v1 tx1 bs1 == SealedTx v2 tx2 bs2
        | v1 && v2 = sameEra tx1 tx2 && bs1 == bs2
        | v1 == v2 = bs1 == bs2
        | otherwise = False

sameEra :: InAnyCardanoEra a -> InAnyCardanoEra a -> Bool
sameEra (InAnyCardanoEra e1 _) (InAnyCardanoEra e2 _) =
    case testEquality e1 e2 of
        Just Refl -> True
        Nothing -> False

instance NFData SealedTx where
    rnf (SealedTx v (InAnyCardanoEra _ tx) bs) = tx' `deepseq` bs `deepseq` ()
      where
        -- Showing the transaction should be enough to fully evaluate it.
        tx' = if v then show tx else ""

-- Helper function to constrain the era of a 'SealedTx' to at most the provided
-- era. If this is not possible, the original 'SealedTx' is returned.
--
-- In contrast to the "most recent era" argument of @sealedTxFromBytes'@, this
-- function allows constraining the era at a point after the tx has been
-- deserialised. For instance, in a server handler with known current era,
-- instead of in an @Aeson.FromJSON@ instance.
--
-- >>> ideallyNoLaterThan alonzoEra alonzoCompatibleBabbageTx
-- alonzoCompatibleBabbageTx
--
-- >>> ideallyNoLaterThan alonzoEra alonzoIncompatibleBabbageTx
-- babbageTx
--
-- == Is this what we want? (for the new tx-workflow..)
--
-- Probably not. This is a minimally invasive approach to ensure:
-- - tx workflow works in both Alonzo and Babbage
-- - tx workflow tries to create Alonzo txs in Alonzo and Babbage txs in Babbage
--
-- With the added behaviour:
-- - tx workflow may partially work for babbage-only txs when in alonzo
ideallyNoLaterThan
    :: AnyCardanoEra
    -> SealedTx
    -> SealedTx
ideallyNoLaterThan maxEra sealedTx =
    either (const sealedTx) (sealedTxFromCardano)
        (cardanoTxFromBytes maxEra (serialisedTx sealedTx))

cardanoTxIdeallyNoLaterThan
    :: AnyCardanoEra
    -> SealedTx
    -> InAnyCardanoEra Cardano.Tx
cardanoTxIdeallyNoLaterThan era = unsafeCardanoTx . ideallyNoLaterThan era

getSealedTxBody :: SealedTx -> InAnyCardanoEra Cardano.TxBody
getSealedTxBody (SealedTx _ (InAnyCardanoEra era tx) _) =
    InAnyCardanoEra era (Cardano.getTxBody tx)

getSealedTxWitnesses :: SealedTx -> [InAnyCardanoEra Cardano.KeyWitness]
getSealedTxWitnesses (SealedTx _ (InAnyCardanoEra era tx) _) =
    [InAnyCardanoEra era w | w <- Cardano.getTxWitnesses tx]

-- | Construct a 'SealedTx' from a "Cardano.Api" transaction.
sealedTxFromCardano :: InAnyCardanoEra Cardano.Tx -> SealedTx
sealedTxFromCardano tx = SealedTx True tx (cardanoTxToBytes tx)
  where
    cardanoTxToBytes :: InAnyCardanoEra Cardano.Tx -> ByteString
    cardanoTxToBytes (InAnyCardanoEra _era tx') = Cardano.serialiseToCBOR tx'

-- | Construct a 'SealedTx' from a "Cardano.Api" transaction.
sealedTxFromCardano' :: Cardano.IsCardanoEra era => Cardano.Tx era -> SealedTx
sealedTxFromCardano' = sealedTxFromCardano . InAnyCardanoEra Cardano.cardanoEra

-- | Construct a 'SealedTx' from a 'Cardano.Api.TxBody'.
sealedTxFromCardanoBody :: Cardano.IsCardanoEra era => Cardano.TxBody era -> SealedTx
sealedTxFromCardanoBody = sealedTxFromCardano . InAnyCardanoEra Cardano.cardanoEra . mk
  where
    mk body = Cardano.Tx body []

-- | Deserialise a Cardano transaction. The transaction can be in the format of
-- any era. This function will try the most recent era first, then
-- previous eras until 'ByronEra'.
cardanoTxFromBytes
    :: AnyCardanoEra -- ^ Most recent era
    -> ByteString -- ^ Serialised transaction
    -> Either DecoderError (InAnyCardanoEra Cardano.Tx)
cardanoTxFromBytes maxEra bs = asum $ map snd $ filter (withinEra maxEra . fst)
    [ deserialise BabbageEra Cardano.AsBabbageEra
    , deserialise AlonzoEra  Cardano.AsAlonzoEra
    , deserialise MaryEra    Cardano.AsMaryEra
    , deserialise AllegraEra Cardano.AsAllegraEra
    , deserialise ShelleyEra Cardano.AsShelleyEra
    , deserialise ByronEra   Cardano.AsByronEra
    ]
  where
    deserialise
        :: forall era. Cardano.IsCardanoEra era
        => CardanoEra era
        -> Cardano.AsType era
        -> (AnyCardanoEra, Either DecoderError (InAnyCardanoEra Cardano.Tx))
    deserialise era asEra =
        ( anyCardanoEra era
        , InAnyCardanoEra era <$> deserialiseFromCBOR (Cardano.AsTx asEra) bs
        )

    -- | Given a list of deserialise results that may fail, return the first
    -- success. If there was no success, then return the first failure message.
    asum :: [Either e a] -> Either e a
    asum xs = case partitionEithers xs of
        (_, (a:_)) -> Right a
        ((e:_), []) -> Left e
        ([], []) -> internalError "cardanoTxFromBytes: impossible"

-- | @a `withinEra` b@ is 'True' iff @b@ is the same era as @a@, or an earlier
-- one.
withinEra :: AnyCardanoEra -> AnyCardanoEra -> Bool
withinEra = (>=) `on` numberEra
  where
    numberEra :: AnyCardanoEra -> Int
    numberEra (AnyCardanoEra e) = case e of
        ByronEra   -> 1
        ShelleyEra -> 2
        AllegraEra -> 3
        MaryEra    -> 4
        AlonzoEra  -> 5
        BabbageEra -> 6
        ConwayEra  -> 7

-- | Deserialise a transaction to construct a 'SealedTx'.
sealedTxFromBytes :: ByteString -> Either DecoderError SealedTx
sealedTxFromBytes = sealedTxFromBytes' (anyCardanoEra BabbageEra)

-- | Deserialise a transaction to construct a 'SealedTx'.
sealedTxFromBytes'
    :: AnyCardanoEra -- ^ Most recent era
    -> ByteString -- ^ Serialised transaction
    -> Either DecoderError SealedTx
sealedTxFromBytes' era bs = SealedTx True
    <$> cardanoTxFromBytes era bs
    <*> pure bs

-- | Serialise a 'SealedTx' for storage in a database field. The difference
-- between 'persistSealedTx' and 'serialisedTx' is that this function has a
-- special check for values created by 'mockSealedTx'.
persistSealedTx :: SealedTx -> ByteString
persistSealedTx tx = header <> serialisedTx tx
  where
    header = if valid tx then mempty else mockSealedTxMagic

-- | Deserialise a 'SealedTx' which has been stored in a database field. This
-- function includes a special check for 'mockSealedTx' values.
unPersistSealedTx :: ByteString -> Either Text SealedTx
unPersistSealedTx bs = case unPersistMock bs of
    Nothing -> first (T.pack . show) $ sealedTxFromBytes bs
    Just bs' -> Right $ mockSealedTx bs'

-- | A header for use by 'persistSealedTx' and 'unPersistSealedTx'. A valid
-- serialised Cardano transaction could not have this header, because they
-- always start with a CBOR map.
mockSealedTxMagic :: ByteString
mockSealedTxMagic = "MOCK"

unPersistMock :: ByteString -> Maybe ByteString
unPersistMock bs
    | header == mockSealedTxMagic = Just body
    | otherwise = Nothing
  where
    (header, body) = B8.splitAt (B8.length mockSealedTxMagic) bs

-- | Get the serialised transaction body and witnesses from a 'SealedTx'.
getSerialisedTxParts :: SealedTx -> SerialisedTxParts
getSerialisedTxParts (SealedTx _ (InAnyCardanoEra _ tx) _) = SerialisedTxParts
    { serialisedTxBody = serialiseToCBOR $ Cardano.getTxBody tx
    , serialisedTxWitnesses = serialiseToCBOR <$> Cardano.getTxWitnesses tx
    }

-- | A serialised transaction that may be only partially signed, or even
-- invalid.
newtype SerialisedTx = SerialisedTx { payload :: ByteString }
    deriving stock (Show, Eq, Generic, Ord)
    deriving newtype (Semigroup, Monoid, ByteArray, ByteArrayAccess, NFData)

-- | @SerialisedTxParts@ is a serialised transaction body, and a possibly
-- incomplete set of serialised witnesses.
data SerialisedTxParts = SerialisedTxParts
    { serialisedTxBody :: ByteString
    , serialisedTxWitnesses :: [ByteString]
    } deriving stock (Show, Eq, Generic)


{-------------------------------------------------------------------------------
                      Internal functions for unit testing
-------------------------------------------------------------------------------}

-- | Only use this for tests.
unsafeSealedTxFromBytes :: HasCallStack => ByteString -> SealedTx
unsafeSealedTxFromBytes = either (internalError . errMsg) id . sealedTxFromBytes
  where
    errMsg reason = "unsafeSealedTxFromBytes: "+||reason||+""

-- | Construct a 'SealedTx' from a string which need not be a well-formed
-- serialised Cardano transaction.
--
-- Be careful using the 'SealedTx', because any attempt to evaluate its
-- 'cardanoTx' field will crash.
mockSealedTx :: HasCallStack => ByteString -> SealedTx
mockSealedTx = SealedTx False
    (internalError "mockSealedTx: attempted to decode gibberish")
