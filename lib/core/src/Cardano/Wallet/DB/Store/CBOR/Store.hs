
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

{- |
Copyright: 2022 IOHK
License: Apache-2.0

Implementation of a 'Store' for 'TxCBORHistory'.

-}

module Cardano.Wallet.DB.Store.CBOR.Store ( mkStoreCBOR ) where

import Prelude

import Cardano.Wallet.DB.Sqlite.Types
    ( TxId (..) )
import Cardano.Wallet.DB.Store.CBOR.Model
    ( DeltaTxCBOR (..), TxCBORHistory (..) )
import Cardano.Wallet.Primitive.Types.Tx.CBOR
    ( TxCBOR (..) )
import Data.ByteString.Lazy.Char8
    ( fromStrict, toStrict )
import Data.DBVar
    ( Store (..) )
import Data.Generics.Internal.VL
    ( Iso', fromIso, iso, view )
import Data.Maybe
    ( fromJust )
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

import qualified Cardano.Wallet.DB.Sqlite.Schema as Schema
    ( CBOR (..), EntityField (..) )
import qualified Data.Map.Strict as Map

txCBORiso :: Iso' (TxId, TxCBOR) Schema.CBOR
txCBORiso = iso f g
  where
    f :: (TxId, TxCBOR) -> Schema.CBOR
    f (id',TxCBOR {..}) =
        Schema.CBOR id' (toStrict txCBOR) (fromIntegral $ fromEnum txEra)
    g :: Schema.CBOR -> (TxId, TxCBOR)
    g Schema.CBOR {..} =
        ( cborTxId
        , TxCBOR (fromStrict cborTxCBOR) (toEnum $ fromIntegral cborTxEra))

repsertCBORs :: TxCBORHistory -> SqlPersistT IO ()
repsertCBORs (TxCBORHistory txs) =
    repsertMany
        [(fromJust keyFromRecordM x, x)
        | (view txCBORiso -> x) <- Map.assocs txs
        ]

mkStoreCBOR :: Store (SqlPersistT IO) DeltaTxCBOR
mkStoreCBOR = Store
    { loadS = Right
          . TxCBORHistory
          . Map.fromList
          . fmap (view (fromIso txCBORiso) . entityVal)
          <$> selectList [] []
    , writeS = \txs -> do
          repsertCBORs txs
    , updateS = \_ -> \case
          Append addendum -> repsertCBORs addendum
          DeleteTx tid -> deleteWhere [Schema.CborTxId ==. tid ]
    }


