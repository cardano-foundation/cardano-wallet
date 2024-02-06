{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.DB.Store.Delegations.Types
    ( DelegationStatusEnum (..)
    )
where

import Prelude

import Control.Monad
    ( (>=>)
    )
import Data.Proxy
    ( Proxy (..)
    )
import Data.Text
    ( Text
    )
import Database.Persist.Sql
    ( PersistField (..)
    , PersistFieldSql (..)
    )
import GHC.Generics
    ( Generic
    )

data DelegationStatusEnum
    = InactiveE
    | RegisteredE
    | ActiveE
    | ActiveAndVotedE
    | VotedE
    deriving (Eq, Show, Enum, Generic, Ord)

instance PersistField DelegationStatusEnum where
    toPersistValue =
        toPersistValue . \case
            InactiveE -> "inactive" :: Text
            RegisteredE -> "registered"
            ActiveE -> "active"
            ActiveAndVotedE -> "active_and_voted"
            VotedE -> "voted"
    fromPersistValue = fromPersistValue >=> readDelegationStatus

readDelegationStatus :: Text -> Either Text DelegationStatusEnum
readDelegationStatus "inactive" = Right InactiveE
readDelegationStatus "registered" = Right RegisteredE
readDelegationStatus "active" = Right ActiveE
readDelegationStatus "active_and_voted" = Right ActiveAndVotedE
readDelegationStatus "voted" = Right VotedE
readDelegationStatus other = Left $ "Invalid delegation status: " <> other

instance PersistFieldSql DelegationStatusEnum where
    sqlType _ = sqlType (Proxy @Text)
