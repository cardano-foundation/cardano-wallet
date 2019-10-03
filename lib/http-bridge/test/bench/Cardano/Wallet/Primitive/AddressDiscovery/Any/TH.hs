{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}


-- |
-- Copyright: © 2018-2019 IOHK
-- License: Apache-2.0
--
-- Automatically generated code via Template-Haskell. Contains necessary
-- database rows and columns declaration to work with 'AnyAddressState'.

module Cardano.Wallet.Primitive.AddressDiscovery.Any.TH where

import Prelude

import Cardano.Wallet.DB.Sqlite.Types
    ( sqlSettings' )
import Database.Persist.Class
    ( AtLeastOneUniqueKey (..), OnlyOneUniqueKey (..) )
import Database.Persist.TH
    ( mkDeleteCascade, mkMigrate, mkPersist, persistLowerCase, share )
import GHC.Generics
    ( Generic )

import qualified Cardano.Wallet.Primitive.Types as W

share
    [ mkPersist sqlSettings'
    , mkDeleteCascade sqlSettings'
    , mkMigrate "migrateAll"
    ]
    [persistLowerCase|
AnyAddressState
    anyAddressStateWalletId        W.WalletId  sql=wallet_id
    anyAddressStateCheckpointSlot  W.SlotId    sql=slot
    anyAddressStateProportion      Double      sql=proportion

    UniqueAnyAddressState anyAddressStateWalletId anyAddressStateCheckpointSlot
    deriving Show Generic
|]
