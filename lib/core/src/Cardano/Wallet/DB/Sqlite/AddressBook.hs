{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{-# LANGUAGE TypeFamilies #-}
-- |
-- Copyright: © 2021 IOHK
-- License: Apache-2.0
--
-- Address books consist of a 'Prologue'
-- and discovered addresses ('Discoveries').
module Cardano.Wallet.DB.Sqlite.AddressBook
    ( AddressBookIso (..)
    , Prologue (..)
    , Discoveries (..)
    , SeqAddressMap (..)
    )
  where

import Prelude

import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (..)
    , DerivationType (..)
    , Index (..)
    , KeyFingerprint (..)
    , Role (..)
    )
import Cardano.Wallet.Primitive.AddressDerivation.SharedKey
    ( SharedKey (..) )
import Cardano.Wallet.Primitive.Types.Address
    ( Address (..), AddressState (..) )
import Data.Generics.Internal.VL
    ( Iso', iso, withIso )
import Data.Kind
    ( Type )
import Data.Map.Strict
    ( Map )
import Data.Type.Equality
    ( type (==) )

import qualified Cardano.Wallet.Address.Pool as AddressPool
import qualified Cardano.Wallet.Primitive.AddressDiscovery.Random as Rnd
import qualified Cardano.Wallet.Primitive.AddressDiscovery.Sequential as Seq
import qualified Cardano.Wallet.Primitive.AddressDiscovery.Shared as Shared
import qualified Data.Map.Strict as Map

{-------------------------------------------------------------------------------
    AddressBook isomorphism
-------------------------------------------------------------------------------}
-- | FIXME LATER during ADP-1043:
-- Move 'Prologue' and 'Discoveries' closer into address types.
class AddressBookIso s where
    -- | Address information contained in the prologue of the address book,
    -- such as public keys or the address gap.
    data Prologue s :: Type
    -- | Addresses that were collected during discovery on the blockchain.
    data Discoveries s :: Type

    -- | Isomorphism between the address book type 's'
    -- and its two components.
    addressIso :: Iso' s (Prologue s, Discoveries s)

{-------------------------------------------------------------------------------
    Sequential address book
-------------------------------------------------------------------------------}
-- piggy-back on SeqState existing instance, to simulate the same behavior.
instance AddressBookIso (Seq.SeqState n k)
    => AddressBookIso (Seq.SeqAnyState n k p)
  where
    data Prologue (Seq.SeqAnyState n k p) = PS (Prologue (Seq.SeqState n k))
    data Discoveries (Seq.SeqAnyState n k p) = DS (Discoveries (Seq.SeqState n k))

    addressIso = withIso addressIso $ \from to ->
        let from2 st = let (a,b) = from $ Seq.innerState st in (PS a, DS b)
            to2 (PS a, DS b) = Seq.SeqAnyState $ to (a,b)
        in  iso from2 to2

-- | Isomorphism for sequential address book.
instance ( (key == SharedKey) ~ 'False ) => AddressBookIso (Seq.SeqState n key)
  where
    data Prologue (Seq.SeqState n key)
        = SeqPrologue (Seq.SeqState n key)
        -- Trick: We keep the type, but we empty the discovered addresses
    data Discoveries (Seq.SeqState n key)
        = SeqDiscoveries
            (SeqAddressMap 'UtxoInternal key)
            (SeqAddressMap 'UtxoExternal key)

    addressIso = iso from to
      where
        from (Seq.SeqState int ext a b c d) =
            let int0 = clear int
                ext0 = clear ext
            in  ( SeqPrologue $ Seq.SeqState int0 ext0 a b c d
                , SeqDiscoveries (addresses int) (addresses ext)
                )
        to (SeqPrologue (Seq.SeqState int0 ext0 a b c d), SeqDiscoveries ints exts)
          = Seq.SeqState (loadUnsafe int0 ints) (loadUnsafe ext0 exts) a b c d

-- | Address data from sequential address pool.
-- The phantom type parameter @c@ prevents mixing up
-- the internal with the external pool.
newtype SeqAddressMap (c :: Role) (key :: Depth -> Type -> Type) = SeqAddressMap
        ( Map
            (KeyFingerprint "payment" key)
            (Index 'Soft 'AddressK, AddressState)
        )

clear :: Seq.SeqAddressPool c k -> Seq.SeqAddressPool c k
clear = Seq.SeqAddressPool . AddressPool.clear . Seq.getPool

addresses :: Seq.SeqAddressPool c k -> SeqAddressMap c k
addresses = SeqAddressMap . AddressPool.addresses . Seq.getPool

loadUnsafe
    :: Seq.SeqAddressPool c k
    -> SeqAddressMap c k -> Seq.SeqAddressPool c k
loadUnsafe (Seq.SeqAddressPool pool0) (SeqAddressMap addrs) =
    Seq.SeqAddressPool $ AddressPool.loadUnsafe pool0 addrs

{-------------------------------------------------------------------------------
    Shared key address book
-------------------------------------------------------------------------------}
-- | Isomorphism for multi-sig address book.
instance ( key ~ SharedKey ) => AddressBookIso (Shared.SharedState n key)
  where
    data Prologue (Shared.SharedState n key)
        = SharedPrologue (Shared.SharedState n key)
        -- Trick: We keep the type, but we empty the discovered addresses
    data Discoveries (Shared.SharedState n key)
        = SharedDiscoveries
            ( Map
                (KeyFingerprint "payment" SharedKey)
                (Index 'Soft 'ScriptK, AddressState)
            )

    addressIso = iso from to
      where
        from st = case Shared.ready st of
            Shared.Pending -> (SharedPrologue st, SharedDiscoveries Map.empty)
            Shared.Active pool ->
                let pool0 = AddressPool.clear pool
                in  ( SharedPrologue st{ Shared.ready = Shared.Active pool0 }
                    , SharedDiscoveries $ AddressPool.addresses pool
                    )
        to (SharedPrologue st, SharedDiscoveries addrs)
          = case Shared.ready st of
            Shared.Pending -> st
            Shared.Active pool0 ->
                let pool = AddressPool.loadUnsafe pool0 addrs
                in  st{ Shared.ready = Shared.Active pool }

{-------------------------------------------------------------------------------
    HD Random address book
-------------------------------------------------------------------------------}
-- piggy-back on SeqState existing instance, to simulate the same behavior.
instance AddressBookIso (Rnd.RndAnyState n p)
  where
    data Prologue (Rnd.RndAnyState n p) = PR (Prologue (Rnd.RndState n))
    data Discoveries (Rnd.RndAnyState n p) = DR (Discoveries (Rnd.RndState n))

    addressIso = withIso addressIso $ \from to ->
        let from2 st = let (a,b) = from $ Rnd.innerState st in (PR a, DR b)
            to2 (PR a, DR b) = Rnd.RndAnyState $ to (a,b)
        in  iso from2 to2

-- | Isomorphism for HD random address book.
instance AddressBookIso (Rnd.RndState n) where
    data Prologue (Rnd.RndState n)
        = RndPrologue (Rnd.RndState n)
        -- Trick: We keep the type, but we empty the discovered addresses
    data Discoveries (Rnd.RndState n)
        = RndDiscoveries (Map Rnd.DerivationPath (Address, AddressState))

    addressIso = iso from to
      where
        from (Rnd.RndState a b addrs c d)
            = (RndPrologue (Rnd.RndState a b Map.empty c d), RndDiscoveries addrs)
        to (RndPrologue (Rnd.RndState a b _ c d), RndDiscoveries addrs)
            = Rnd.RndState a b addrs c d
