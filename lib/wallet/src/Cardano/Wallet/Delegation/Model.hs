{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Copyright: © 2023 IOHK
-- License: Apache-2.0
--
-- Data types that represents a history of delegations and its changes.
module Cardano.Wallet.Delegation.Model
    ( Operation (..)
    , slotOf
    , Status (..)
    , History
    , status
    , DRep (..)
    , DRepKeyHash (..)
    , DRepScriptHash (..)
    , VoteAction (..)
    , encodeDRepKeyHashBech32
    , decodeDRepKeyHashBech32
    , encodeDRepScriptHashBech32
    , decodeDRepScriptHashBech32
    ) where

import Prelude

import Control.DeepSeq
    ( NFData (..)
    )
import Data.ByteString
    ( ByteString
    )
import Data.Delta
    ( Delta (..)
    )
import Data.Function
    ( (&)
    )
import Data.Map.Strict
    ( Map
    )
import Data.Text
    ( Text
    )
import Data.Text.Class
    ( FromText (..)
    , TextDecodingError (..)
    , ToText (..)
    )
import Fmt
    ( Buildable (..)
    )
import GHC.Generics
    ( Generic
    )

import qualified Codec.Binary.Bech32 as Bech32
import qualified Codec.Binary.Bech32.TH as Bech32
import qualified Data.Map.Strict as Map

-- | Delta type for the delegation 'History'.
data Operation slot pool
    = Register slot
    | Deregister slot
    | Delegate pool slot
    | DelegateAndVote pool VoteAction slot
    | Vote VoteAction slot
    | Rollback slot
    deriving (Show)

-- | Target slot of each 'Operation'.
slotOf :: Operation slot pool -> slot
slotOf (Register x) = x
slotOf (Deregister x) = x
slotOf (Delegate _ x) = x
slotOf (DelegateAndVote _ _ x) = x
slotOf (Vote _ x) = x
slotOf (Rollback x) = x

-- | Valid state for the delegations, independent of time.
data Status pool
    = Inactive
    | Registered
    | Active pool
    | ActiveAndVoted pool VoteAction
    | Voted VoteAction
    deriving (Show, Eq)

-- | Delegation history implementation.
type History slot pool = Map slot (Status pool)

instance (Ord slot, Eq pool) => Delta (Operation slot pool) where
    type Base (Operation slot pool) = History slot pool
    apply r hist = hist' & if miss == wanted then id else Map.insert slot wanted
      where
        slot = slotOf r
        hist' = cut (< slot) hist
        miss = status slot hist'
        wanted = transition r $ status slot hist

--                                              DelegateAndVote
--                                                     |
--                                           Delegate  |   Vote
--                                               |     |    | -------
--                                               v     v    v |     |
--                     ------------  Register   ---------------     | Register
--                     | Inactive |------------>|  Registered |------
--                     ------------             ---------------
--                                 ----------    |     |    |    ---------
--                                 |        v    v     |    v    |       |
--                                 |       ----------  |  ---------      | Vote
--                        Delegate --------| Active |  |  | Voted |<------
--                                         ----------  |  ---------
--                                           Vote |    |      | Delegate
--                                DelegateAndVote v    v      v DelegateAndVote
--                                               ------------------
--                                               | ActiveAndVoted |<---
--                                               ------------------   | Delegate
--                                                         |          | DelegateAndVote
--                                                         ------------ Vote
--
--
--                     ------------ Deregister  ---------------
--                     | Inactive |<----------- |  Registered |
--                     ------------             ---------------
--                    /\ /\ /\ /\
--                     |  |  |  | Deregister
--                     |  |  |  |       ----------     ---------
--                     |  |  |  --------| Active |     | Voted |
--                     |  |             ----------     ---------
--                     |  |        Deregister             |
--                     |  ---------------------------------
--                     |                         ------------------
--                     --------------------------| ActiveAndVoted |
--                             Deregister        ------------------
--
transition :: Operation slot pool -> Status pool -> Status pool
transition (Register _) Inactive = Registered
transition (Delegate p _) Registered = Active p
transition (Vote v _) Registered = Voted v
transition (DelegateAndVote p v _) Registered = ActiveAndVoted p v
transition (Vote v _) (Voted _) = Voted v
transition (Delegate p _) (Voted v) = ActiveAndVoted p v
transition (DelegateAndVote p v _) (Voted _) = ActiveAndVoted p v
transition (Vote v _) (Active p) = ActiveAndVoted p v
transition (Delegate p _) (Active _) = Active p
transition (DelegateAndVote p v _) (Active _) = ActiveAndVoted p v
transition (DelegateAndVote p v _) (ActiveAndVoted _ _) = ActiveAndVoted p v
transition (Delegate p _) (ActiveAndVoted _ v) = ActiveAndVoted p v
transition (Vote v _) (ActiveAndVoted p _) = ActiveAndVoted p v
transition (Deregister _) _ = Inactive
transition _ s = s

type Change slot pool = History slot pool -> History slot pool

cut :: (slot -> Bool) -> Change slot pool
cut op = fst . Map.spanAntitone op

-- | Status of the delegation at a given slot.
status :: Ord slot => slot -> History slot pool -> Status pool
status x = maybe Inactive snd . Map.lookupMax . cut (<= x)

newtype DRepKeyHash = DRepKeyHash { getDRepKeyHash :: ByteString }
    deriving (Generic, Eq, Ord, Show)

instance NFData DRepKeyHash

newtype DRepScriptHash = DRepScriptHash { getDRepScriptHash :: ByteString }
    deriving (Generic, Eq, Ord, Show)

instance NFData DRepScriptHash

data DRep =
    DRepFromKeyHash DRepKeyHash | DRepFromScriptHash DRepScriptHash
    deriving (Eq, Generic, Show)
    deriving anyclass NFData

-- | Encode 'DRepKeyHash' as Bech32 with "drep" hrp.
encodeDRepKeyHashBech32 :: DRepKeyHash -> Text
encodeDRepKeyHashBech32 =
    Bech32.encodeLenient hrp
        . Bech32.dataPartFromBytes
        . getDRepKeyHash
  where
    hrp = [Bech32.humanReadablePart|drep|]

-- | Decode a Bech32 encoded 'DRepKeyHash'.
decodeDRepKeyHashBech32 :: Text -> Either TextDecodingError DRepKeyHash
decodeDRepKeyHashBech32 t =
    case fmap Bech32.dataPartToBytes <$> Bech32.decodeLenient t of
        Left _ -> Left textDecodingError
        Right (hrp', Just bytes)
            | hrp' == hrp -> Right $ DRepKeyHash bytes
        Right _ -> Left textDecodingError
      where
        textDecodingError = TextDecodingError $ unwords
            [ "Invalid DRep key hash: expecting a Bech32 encoded value"
            , "with human readable part of 'drep'."
            ]
        hrp = [Bech32.humanReadablePart|drep|]

-- | Encode 'DRepScriptHash' as Bech32 with "drep_script" hrp.
encodeDRepScriptHashBech32 :: DRepScriptHash -> Text
encodeDRepScriptHashBech32 =
    Bech32.encodeLenient hrp
        . Bech32.dataPartFromBytes
        . getDRepScriptHash
  where
    hrp = [Bech32.humanReadablePart|drep_script|]

-- | Decode a Bech32 encoded 'DRepScriptHash'.
decodeDRepScriptHashBech32 :: Text -> Either TextDecodingError DRepScriptHash
decodeDRepScriptHashBech32 t =
    case fmap Bech32.dataPartToBytes <$> Bech32.decodeLenient t of
        Left _ -> Left textDecodingError
        Right (hrp', Just bytes)
            | hrp' == hrp -> Right $ DRepScriptHash bytes
        Right _ -> Left textDecodingError
      where
        textDecodingError = TextDecodingError $ unwords
            [ "Invalid DRep Script hash: expecting a Bech32 encoded value"
            , "with human readable part of 'drep_script'."
            ]
        hrp = [Bech32.humanReadablePart|drep_script|]

instance Buildable DRep where
    build = \case
        DRepFromKeyHash keyhash -> build $ encodeDRepKeyHashBech32 keyhash
        DRepFromScriptHash scripthash -> build $ encodeDRepScriptHashBech32 scripthash

-- | Vote action.
data VoteAction
    = Abstain
    | NoConfidence
    | VoteTo !DRep
    deriving (Eq, Generic, Show)
    deriving anyclass NFData

instance ToText VoteAction where
    toText Abstain = "abstain"
    toText NoConfidence = "no confidence"
    toText (VoteTo (DRepFromKeyHash keyhash)) =
        encodeDRepKeyHashBech32 keyhash
    toText (VoteTo (DRepFromScriptHash scripthash)) =
        encodeDRepScriptHashBech32 scripthash

instance FromText VoteAction where
    fromText txt = case txt of
        "abstain" -> Right Abstain
        "no confidence" -> Right NoConfidence
        _ -> case decodeDRepKeyHashBech32 txt of
                Right keyhash ->
                     Right $ VoteTo $ DRepFromKeyHash keyhash
                Left _ -> case decodeDRepScriptHashBech32 txt of
                    Right scripthash ->
                        Right $ VoteTo $ DRepFromScriptHash scripthash
                    Left _ -> Left $ TextDecodingError $ unwords
                        [ "I couldn't parse the given vote action."
                        , "I am expecting either 'abstain', 'no confidence'"
                        , "or bech32 encoded drep having prefixes: 'drep_script'"
                        , "or 'drep_script'."]

instance Buildable VoteAction where
    build = \case
        Abstain -> "abstaining"
        NoConfidence -> "casting no confidence"
        VoteTo drep -> "voting to " <> build drep
