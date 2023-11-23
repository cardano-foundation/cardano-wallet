{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Cardano.Wallet.Primitive.Types.TokenMapWithScripts
    ( TokenMapWithScripts (..)
    , emptyTokenMapWithScripts
    , AnyScript (..)
    , PlutusScriptInfo (..)
    , PlutusVersion (..)
    , ScriptReference (..)
    , ReferenceInput (..)
    )
where

import Prelude

import Cardano.Address.Script
    ( KeyHash
    , Script
    , ScriptHash
    )
import Cardano.Wallet.Primitive.Types.TokenMap
    ( TokenMap
    )
import Cardano.Wallet.Primitive.Types.TokenPolicy
    ( TokenPolicyId
    )
import Cardano.Wallet.Primitive.Types.Tx.TxIn
    ( TxIn
    )
import Control.DeepSeq
    ( NFData
    )
import Data.Map.Strict
    ( Map
    )
import Data.Text.Class
    ( FromText (..)
    , TextDecodingError (TextDecodingError)
    , ToText (..)
    )
import GHC.Generics
    ( Generic
    )

import qualified Data.Map.Strict as Map

data PlutusVersion = PlutusVersionV1 | PlutusVersionV2 | PlutusVersionV3
    deriving (Eq, Generic, Show)
    deriving anyclass (NFData)

instance ToText PlutusVersion where
    toText PlutusVersionV1 = "v1"
    toText PlutusVersionV2 = "v2"
    toText PlutusVersionV3 = "v3"

instance FromText PlutusVersion where
    fromText txt = case txt of
        "v1" -> Right PlutusVersionV1
        "v2" -> Right PlutusVersionV2
        "v3" -> Right PlutusVersionV3
        _ ->
            Left
                $ TextDecodingError
                $ unwords
                    [ "I couldn't parse the given plutus version."
                    , "I am expecting one of the words 'v1' or"
                    , "'v2'."
                    ]

data PlutusScriptInfo = PlutusScriptInfo
    { languageVersion :: PlutusVersion
    , scriptHash :: ScriptHash
    }
    deriving (Eq, Generic, Show)
    deriving anyclass (NFData)

newtype ReferenceInput = ReferenceInput TxIn
    deriving (Eq, Generic, Show)
    deriving anyclass (NFData)

-- | ScriptReference depicts whether the script is referenced via spending
-- and is bound to be used in the same transaction or is referenced via
-- reference inputs and is to be used in other transactions. The the latter
-- case the script is referenced in other trasactions
data ScriptReference
    = ViaSpending
    | ViaReferenceInput ReferenceInput
    deriving (Eq, Generic, Show)
    deriving anyclass (NFData)

data AnyScript
    = NativeScript !(Script KeyHash) !ScriptReference
    | PlutusScript !PlutusScriptInfo !ScriptReference
    | AnyScriptReference !ScriptHash ![ReferenceInput]
    deriving (Eq, Generic, Show)
    deriving anyclass (NFData)

data TokenMapWithScripts = TokenMapWithScripts
    { txTokenMap :: !TokenMap
    , txScripts :: !(Map TokenPolicyId AnyScript)
    }
    deriving (Show, Generic, Eq)

emptyTokenMapWithScripts :: TokenMapWithScripts
emptyTokenMapWithScripts =
    TokenMapWithScripts
        { txTokenMap = mempty
        , txScripts = Map.empty
        }
