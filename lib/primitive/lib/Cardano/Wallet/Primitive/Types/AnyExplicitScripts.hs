{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.Wallet.Primitive.Types.AnyExplicitScripts
    ( AnyExplicitScript (..)
    , changeRoleInAnyExplicitScript
    )
where

import Prelude

import Cardano.Address.Script
    ( KeyHash (KeyHash)
    , KeyRole
    , Script (..)
    )
import Cardano.Wallet.Primitive.Types.TokenMapWithScripts
    ( PlutusScriptInfo
    , ScriptReference
    )
import Control.DeepSeq
    ( NFData
    )
import GHC.Generics
    ( Generic
    )

data AnyExplicitScript
    = NativeExplicitScript !(Script KeyHash) !ScriptReference
    | PlutusExplicitScript !PlutusScriptInfo !ScriptReference
    deriving (Eq, Generic, Show)
    deriving anyclass (NFData)

changeRoleInAnyExplicitScript
    :: KeyRole
    -> AnyExplicitScript
    -> AnyExplicitScript
changeRoleInAnyExplicitScript newrole = \case
    NativeExplicitScript script scriptRole ->
        let changeRole' = \case
                RequireSignatureOf (KeyHash _ p) ->
                    RequireSignatureOf $ KeyHash newrole p
                RequireAllOf xs ->
                    RequireAllOf (map changeRole' xs)
                RequireAnyOf xs ->
                    RequireAnyOf (map changeRole' xs)
                RequireSomeOf m xs ->
                    RequireSomeOf m (map changeRole' xs)
                ActiveFromSlot s ->
                    ActiveFromSlot s
                ActiveUntilSlot s ->
                    ActiveUntilSlot s
        in  NativeExplicitScript (changeRole' script) scriptRole
    PlutusExplicitScript _ _ -> error "wrong usage"
