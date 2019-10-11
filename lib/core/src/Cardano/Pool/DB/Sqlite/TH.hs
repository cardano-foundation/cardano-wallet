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
-- Auto-generated Sqlite & Persistent machinery via Template-Haskell. This has
-- been moved into a separate file so that we can treat it slightly differently
-- when computing code-coverage.

module Cardano.Pool.DB.Sqlite.TH where

import Prelude

import Cardano.Pool.DB.Sqlite.Types
    ( sqlSettings' )
import Database.Persist.Class
    ( AtLeastOneUniqueKey (..), OnlyOneUniqueKey (..) )
import Database.Persist.TH
    ( mkDeleteCascade, mkMigrate, mkPersist, persistLowerCase, share )
import GHC.Generics
    ( Generic (..) )

import qualified Cardano.Wallet.Primitive.Types as W

share
    [ mkPersist sqlSettings'
    , mkDeleteCascade sqlSettings'
    , mkMigrate "migrateAll"
    ]
    [persistLowerCase|

-- The set of stake pools that produced a given block
PoolProduction
    poolProductionSlot           W.SlotId     sql=slot
    poolProductionPoolId         W.PoolId     sql=pool_id
    Primary
        poolProductionSlot
    deriving Show Generic
|]
