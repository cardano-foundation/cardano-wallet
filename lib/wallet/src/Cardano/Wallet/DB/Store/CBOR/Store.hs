
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Copyright: 2022 IOHK
License: Apache-2.0

Implementation of a 'Store' for 'TxCBORSet'.

-}

module Cardano.Wallet.DB.Store.CBOR.Store ( mkStoreCBOR ) where

import Prelude

import Cardano.Wallet.DB.Sqlite.Schema
    ( CBOR (..), EntityField (..) )
import Cardano.Wallet.DB.Sqlite.Types
    ( TxId (..) )
import Cardano.Wallet.DB.Store.CBOR.Model
    ( DeltaTxCBOR (..), TxCBORSet (..) )
import Cardano.Wallet.Read.Eras
import Cardano.Wallet.Read.Tx.CBOR
    ( TxCBOR )
import Control.Arrow
    ( (***) )
import Control.Exception
    ( Exception, SomeException (..) )
import Data.Bifunctor
    ( bimap, first )
import Data.ByteString
    ( ByteString )
import Data.ByteString.Lazy.Char8
    ( fromStrict, toStrict )
import Data.DBVar
    ( Store (..) )
import Data.Generics.Internal.VL
    ( Iso', build, fromIso, iso, match, (^.) )
import Data.Maybe
    ( fromJust )
import Data.Typeable
    ( Typeable )
import Data.Word
    ( Word16 )
import Database.Persist
    ( PersistEntity (keyFromRecordM)
    , PersistQueryWrite (deleteWhere)
    , PersistStoreWrite (repsertMany)
    , entityVal
    , selectList
    , (==.)
    )
import Database.Persist.Sql
    ( SqlPersistT )

import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as Map

type TxCBORRaw = (BL.ByteString, Int)

i :: Iso' (BL.ByteString, Int) (ByteString, Word16)
i = iso (toStrict *** fromIntegral) (fromStrict *** fromIntegral)

toTxCBOR :: (TxId, TxCBOR) -> (CBOR, TxCBORRaw)
toTxCBOR (id', tx) =
    let r = build eraValueSerialize tx
    in (uncurry (CBOR id') $ r ^. i, r)

fromTxCBOR :: CBOR -> Either (CBOR, TxCBORRaw ) (TxId, TxCBOR)
fromTxCBOR s@(CBOR {..}) = bimap (s ,) (cborTxId ,) $
    match eraValueSerialize $ (cborTxCBOR, cborTxEra) ^. fromIso i

repsertCBORs :: TxCBORSet -> SqlPersistT IO ()
repsertCBORs (TxCBORSet txs) =
    repsertMany
        [(fromJust keyFromRecordM x, x)
        | x <- fst . toTxCBOR <$> Map.assocs txs
        ]

newtype  CBOROutOfEra = CBOROutOfEra TxCBORRaw
    deriving (Show, Typeable)

instance Exception CBOROutOfEra

mkStoreCBOR :: Store (SqlPersistT IO) DeltaTxCBOR
mkStoreCBOR = Store
    { loadS = do
        cbors <- selectList [] []
        pure $ first (SomeException . CBOROutOfEra . snd) $ do
            ps <- mapM (fromTxCBOR . entityVal) cbors
            pure . TxCBORSet . Map.fromList $ ps
    , writeS = \txs -> do
          repsertCBORs txs
    , updateS = \_ -> \case
          Append addendum -> repsertCBORs addendum
          DeleteTx tid -> deleteWhere [CborTxId ==. tid ]
    }
