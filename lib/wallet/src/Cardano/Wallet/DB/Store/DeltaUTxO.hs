{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{- |
 Copyright: © 2021 IOHK
 License: Apache-2.0

 'Store' of changes to the UTxO set in a wallet state
-}
module Cardano.Wallet.DB.Store.DeltaUTxO (
    mkStoreDeltaUTxO,
) where

import Prelude

import Cardano.Wallet.DB.Sqlite.Schema
    ( DeltaUtxo (..), EntityField (..) )
import Cardano.Wallet.Primitive.Types
    ( SlotNo, WalletId )
import Cardano.Wallet.Primitive.Types.Address
    ( Address (..) )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..) )
import Cardano.Wallet.Primitive.Types.Hash
    ( Hash (..) )
import Cardano.Wallet.Primitive.Types.TokenBundle
    ( AssetId (..), TokenBundle (..) )
import Cardano.Wallet.Primitive.Types.TokenPolicy
    ( TokenName (UnsafeTokenName), TokenPolicyId (UnsafeTokenPolicyId) )
import Cardano.Wallet.Primitive.Types.TokenQuantity
    ( TokenQuantity (TokenQuantity) )
import Cardano.Wallet.Primitive.Types.Tx.TxIn
    ( TxIn (..) )
import Cardano.Wallet.Primitive.Types.Tx.TxOut
    ( TxOut (..) )
import Cardano.Wallet.Primitive.Types.UTxO
    ( DeltaUTxO (..), UTxO (..), empty, excludingD, receiveD )
import Control.Exception
    ( Exception, SomeException (..) )
import Data.ByteString
    ( ByteString )
import Data.DBVar
    ( Store (..) )
import Data.Delta
    ( Delta (..) )
import Data.Serialize
    ( Serialize (..)
    , decode
    , encode
    , getListOf
    , getSetOf
    , putListOf
    , putSetOf
    )
import Database.Persist
    ( Entity (..)
    , PersistQueryRead (selectFirst)
    , deleteWhere
    , insert_
    , (==.)
    , (>.)
    )
import Database.Persist.Sql
    ( SqlPersistT )

import qualified Cardano.Wallet.Primitive.Types.TokenMap as TM
import qualified Data.Map.Strict as Map

{- | Verb to handle the storage of the 'DeltaUTxO' for a wallet at a given slot.
 Nothing means that the wallet has no UTxO at the given slot.
-}
data DeltaDeltaUTxO = ReplaceDeltaUTxO (Maybe DeltaUTxO)

instance Semigroup DeltaDeltaUTxO where
    ReplaceDeltaUTxO _a <> ReplaceDeltaUTxO b = ReplaceDeltaUTxO b

-- | Identifier for the 'DeltaUTxO' of a wallet at a given slot.
type DeltaUTxOKey = (WalletId, SlotNo)

instance Delta DeltaDeltaUTxO where
    type Base DeltaDeltaUTxO = Maybe DeltaUTxO
    apply (ReplaceDeltaUTxO delta) = const delta

-- | Signal a failure to decode a 'DeltaUTxO' from the database row.
newtype FailedDecodingDeltaUTxO = FailedDecodingDeltaUTxO String
    deriving (Show, Eq)

instance Exception FailedDecodingDeltaUTxO

-- Internal newtype to implement 'Serialize' for 'DeltaUTxO'.
newtype DeltaUTxOCodec = DeltaUTxOCodec DeltaUTxO

-- We implement 'Serialize' for 'DeltaUTxO' manually because the default
-- instance could silently diverge and cause a decode failure in new commits.

instance Serialize DeltaUTxOCodec where
    put (DeltaUTxOCodec delta) = do
        flip putListOf (Map.assocs $ unUTxO $ received delta) $
            \( TxIn (Hash txid) pos
                , TxOut
                    (Address addr)
                    (TokenBundle (Coin c) m)
                ) -> do
                    put (txid, pos, addr, c)
                    flip putListOf (TM.toFlatList m) $
                        \( AssetId
                                (UnsafeTokenPolicyId (Hash policy))
                                (UnsafeTokenName name)
                            , TokenQuantity quant
                            ) -> do
                                put (policy, name, quant)
        flip putSetOf (excluded delta) $
            \(TxIn (Hash txid) pos) -> do
                put (txid, pos)
    get =
        DeltaUTxOCodec <$> do
            received <-
                UTxO . Map.fromList <$> getListOf do
                    (txid, pos, addr, c) <- get
                    m <-
                        TM.fromFlatList <$> getListOf do
                            (policy, name, quant) <- get
                            pure
                                ( AssetId
                                    (UnsafeTokenPolicyId (Hash policy))
                                    (UnsafeTokenName name)
                                , TokenQuantity quant
                                )
                    pure
                        ( TxIn (Hash txid) pos
                        , TxOut
                            (Address addr)
                            (TokenBundle (Coin c) m)
                        )
            excluded <- getSetOf $ do
                (txid, pos) <- get
                pure (TxIn (Hash txid) pos)
            pure $
                fst (receiveD empty received)
                    <> fst (excludingD empty excluded)

-- | Read a 'DeltaUTxO' from the database field.
deserialiseDeltaUTxO :: ByteString -> Either String DeltaUTxO
deserialiseDeltaUTxO = fmap (\(DeltaUTxOCodec delta) -> delta) . decode

-- | Write a 'DeltaUTxO' to the database field.
serialiseDeltaUTxO :: DeltaUTxO -> ByteString
serialiseDeltaUTxO delta = encode $ DeltaUTxOCodec delta

-- | Create a 'Store' for the 'DeltaUTxO' of a wallet at a given slot.
mkStoreDeltaUTxO :: DeltaUTxOKey -> Store (SqlPersistT IO) DeltaDeltaUTxO
mkStoreDeltaUTxO (wid, slotNo) =
    Store{loadS = load, writeS = write, updateS = update}
  where
    load =
        readDeltaUTxO
            <$> selectFirst
                [DeltaUtxoWalletId ==. wid, DeltaUtxoSlot ==. slotNo]
                []
    write (Just base) = do
        deleteWhere [DeltaUtxoWalletId ==. wid, DeltaUtxoSlot >. slotNo]
        insert_ $ DeltaUtxo wid slotNo $ serialiseDeltaUTxO base
    write Nothing = do
        deleteWhere [DeltaUtxoWalletId ==. wid, DeltaUtxoSlot >. slotNo]
    update _ (ReplaceDeltaUTxO delta) = write delta

    readDeltaUTxO ::
        Maybe (Entity DeltaUtxo) ->
        Either SomeException (Maybe DeltaUTxO)
    readDeltaUTxO = \case
        Nothing -> pure Nothing
        Just (Entity _ DeltaUtxo{deltaUtxoDelta}) ->
            case deserialiseDeltaUTxO deltaUtxoDelta of
                Left e -> Left $ SomeException $ FailedDecodingDeltaUTxO e
                Right delta -> pure $ Just delta
