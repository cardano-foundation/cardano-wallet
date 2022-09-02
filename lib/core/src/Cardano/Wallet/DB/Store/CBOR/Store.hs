
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Copyright: 2022 IOHK
License: Apache-2.0

Implementation of a 'Store' for 'TxCBORHistory'.

-}

module Cardano.Wallet.DB.Store.CBOR.Store ( mkStoreCBOR ) where

import Prelude

import Cardano.Wallet.DB.Sqlite.Schema
    ( CBOR (..), EntityField (..) )
import Cardano.Wallet.DB.Sqlite.Types
    ( TxId (..) )
import Cardano.Wallet.DB.Store.CBOR.Model
    ( DeltaTxCBOR (..), TxCBORHistory (..) )
import Cardano.Wallet.Types.Read.Eras
    ( eraValueSerialize )
import Cardano.Wallet.Types.Read.Tx.CBOR
    ( TxCBOR )
import Control.Arrow
    ( (***) )
import Control.Exception
    ( Exception, SomeException (SomeException) )
import Data.Bifunctor
    ( bimap )
import Data.ByteString
    ( ByteString )
import Data.ByteString.Lazy.Char8
    ( fromStrict, toStrict )
import Data.DBVar
    ( Store (..) )
import Data.Generics.Internal.VL
    ( Iso', Prism, build, fromIso, iso, match, over, prism, (^.) )
import Data.Generics.Sum
    ( AsConstructor (_Ctor) )
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

txCBORPrism :: Prism CBOR (CBOR, TxCBORRaw) (TxId, TxCBOR) (TxId, TxCBOR)
txCBORPrism = prism f g
  where
    i :: Iso' (BL.ByteString, Int) (ByteString, Word16)
    i = iso (toStrict *** fromIntegral) (fromStrict *** fromIntegral)

    f :: (TxId, TxCBOR) -> (CBOR, TxCBORRaw)
    f (id', tx) = let
        r = build eraValueSerialize tx
        in (uncurry (CBOR id') $ r ^. i, r)

    g :: CBOR -> Either (CBOR, TxCBORRaw ) (TxId, TxCBOR)
    g s@(CBOR {..}) = bimap (s ,) (cborTxId ,) $
        match eraValueSerialize $ (cborTxCBOR, cborTxEra) ^. fromIso i

repsertCBORs :: TxCBORHistory -> SqlPersistT IO ()
repsertCBORs (TxCBORHistory txs) =
    repsertMany
        [(fromJust keyFromRecordM x, x)
        | x <- fst . build txCBORPrism <$> Map.assocs txs
        ]

newtype  CBOROutOfEra = CBOROutOfEra TxCBORRaw
    deriving (Show, Typeable)

instance Exception CBOROutOfEra

mkStoreCBOR :: Store (SqlPersistT IO) DeltaTxCBOR
mkStoreCBOR = Store
    { loadS = do
        cbors <- selectList [] []
        pure $ over (_Ctor @"Left")  (SomeException . CBOROutOfEra . snd) $ do
            ps <- mapM (match txCBORPrism  . entityVal) cbors
            pure $ ( TxCBORHistory . Map.fromList) ps
    , writeS = \txs -> do
          repsertCBORs txs
    , updateS = \_ -> \case
          Append addendum -> repsertCBORs addendum
          DeleteTx tid -> deleteWhere [CborTxId ==. tid ]
    }
