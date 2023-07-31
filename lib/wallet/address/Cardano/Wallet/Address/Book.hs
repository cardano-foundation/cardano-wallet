{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
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
module Cardano.Wallet.Address.Book
    ( AddressBookIso (..)
    , Prologue (..)
    , Discoveries (..)
    , getPrologue
    , getDiscoveries
    , SeqAddressMap (..)
    , SharedAddressMap (..)
    )
  where

import Prelude

import Cardano.Wallet.Address.Derivation
    ( Depth (..)
    , DerivationType (..)
    , Index (..)
    , KeyFingerprint (..)
    , Role (..)
    )
import Cardano.Wallet.Address.Derivation.Shared
    ()
import Cardano.Wallet.Address.Derivation.SharedKey
    ( SharedKey (..) )
import Cardano.Wallet.Primitive.Types.Address
    ( Address, AddressState (..) )
import Control.Lens
    ( Iso', iso, withIso )
import Data.Kind
    ( Type )
import Data.Map.Strict
    ( Map )
import Data.Type.Equality
    ( type (==) )
import Fmt
    ( Buildable (..) )

import qualified Cardano.Wallet.Address.Discovery.Random as Rnd
import qualified Cardano.Wallet.Address.Discovery.Sequential as Seq
import qualified Cardano.Wallet.Address.Discovery.Shared as Shared
import qualified Cardano.Wallet.Address.Pool as AddressPool
import qualified Data.Map.Strict as Map

{-------------------------------------------------------------------------------
    AddressBook isomorphism
-------------------------------------------------------------------------------}
-- | FIXME LATER during ADP-1043:
-- Move 'Prologue' and 'Discoveries' closer into address types.
class (Eq (Prologue s), Eq (Discoveries s)) => AddressBookIso s where
    -- | Address information contained in the prologue of the address book,
    -- such as public keys or the address gap.
    data Prologue s :: Type
    -- | Addresses that were collected during discovery on the blockchain.
    data Discoveries s :: Type

    -- | Isomorphism between the address book type 's'
    -- and its two components.
    addressIso :: Iso' s (Prologue s, Discoveries s)

getPrologue :: AddressBookIso s => s -> Prologue s
getPrologue = withIso addressIso $ \from _ -> fst . from

getDiscoveries :: AddressBookIso s => s -> Discoveries s
getDiscoveries = withIso addressIso $ \from _ -> snd . from

-- | Isomorphism for sequential address book.
instance ( (key == SharedKey) ~ 'False, Eq (Seq.SeqState n key) )
    => AddressBookIso (Seq.SeqState n key)
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
        from (Seq.SeqState int ext a b c d e) =
            let int0 = clear int
                ext0 = clear ext
            in  ( SeqPrologue $ Seq.SeqState int0 ext0 a b c d e
                , SeqDiscoveries (addresses int) (addresses ext)
                )
        to  ( SeqPrologue (Seq.SeqState int0 ext0 a b c d e)
            , SeqDiscoveries ints exts
            )
            = Seq.SeqState
                (loadUnsafe int0 ints) (loadUnsafe ext0 exts) a b c d e

-- | Address data from sequential address pool.
-- The phantom type parameter @c@ prevents mixing up
-- the internal with the external pool.
newtype SeqAddressMap (c :: Role) (key :: Depth -> Type -> Type) =
    SeqAddressMap
        ( Map
            (KeyFingerprint "payment" key)
            (Index 'Soft 'CredFromKeyK, AddressState)
        )
    deriving Eq

clear :: Seq.SeqAddressPool c k -> Seq.SeqAddressPool c k
clear = Seq.SeqAddressPool . AddressPool.clear . Seq.getPool

addresses :: Seq.SeqAddressPool c k -> SeqAddressMap c k
addresses = SeqAddressMap . AddressPool.addresses . Seq.getPool

loadUnsafe
    :: Seq.SeqAddressPool c k
    -> SeqAddressMap c k -> Seq.SeqAddressPool c k
loadUnsafe (Seq.SeqAddressPool pool0) (SeqAddressMap addrs) =
    Seq.SeqAddressPool $ AddressPool.loadUnsafe pool0 addrs

instance Buildable (Prologue (Seq.SeqState n k)) where
    build (SeqPrologue st) = "Prologue of " <> build st

instance Eq (Seq.SeqState n k) => Eq (Prologue (Seq.SeqState n k)) where
    SeqPrologue a == SeqPrologue b = a == b

instance Eq (Seq.SeqState n k) => Eq (Discoveries (Seq.SeqState n k)) where
    (SeqDiscoveries a x) == (SeqDiscoveries b y) = a == b && x == y

{-------------------------------------------------------------------------------
    Shared key address book
-------------------------------------------------------------------------------}

newtype SharedAddressMap (c :: Role) (key :: Depth -> Type -> Type) =
    SharedAddressMap
        ( Map
            (KeyFingerprint "payment" key)
            (Index 'Soft 'CredFromScriptK, AddressState)
        )
    deriving Eq


-- | Isomorphism for multi-sig address book.
instance ( key ~ SharedKey ) => AddressBookIso (Shared.SharedState n key)
  where
    data Prologue (Shared.SharedState n key)
        = SharedPrologue (Shared.SharedState n key)
        -- Trick: We keep the type, but we empty the discovered addresses
    data Discoveries (Shared.SharedState n key)
        = SharedDiscoveries
            (SharedAddressMap 'UtxoExternal key)
            (SharedAddressMap 'UtxoInternal key)

    addressIso = iso from to
      where
        from st = case Shared.ready st of
            Shared.Pending ->
                ( SharedPrologue st
                , SharedDiscoveries (SharedAddressMap Map.empty) (SharedAddressMap Map.empty))
            Shared.Active (Shared.SharedAddressPools extPool intPool pending) ->
                let extPool0 = clearShared extPool
                    intPool0 = clearShared intPool
                    pools0 = Shared.SharedAddressPools extPool0 intPool0 pending
                in  ( SharedPrologue st{ Shared.ready = Shared.Active pools0 }
                    , SharedDiscoveries
                        (addressesShared extPool)
                        (addressesShared intPool)
                    )
        to (SharedPrologue st, SharedDiscoveries exts ints)
          = case Shared.ready st of
            Shared.Pending -> st
            Shared.Active (Shared.SharedAddressPools extPool0 intPool0 pending0) ->
                let extPool = loadUnsafeShared extPool0 exts
                    intPool = loadUnsafeShared intPool0 ints
                    pools = Shared.SharedAddressPools extPool intPool pending0
                in  st{ Shared.ready = Shared.Active pools }

clearShared :: Shared.SharedAddressPool c k -> Shared.SharedAddressPool c k
clearShared = Shared.SharedAddressPool . AddressPool.clear . Shared.getPool

addressesShared :: Shared.SharedAddressPool c k -> SharedAddressMap c k
addressesShared = SharedAddressMap . AddressPool.addresses . Shared.getPool

loadUnsafeShared
    :: Shared.SharedAddressPool c k
    -> SharedAddressMap c k
    -> Shared.SharedAddressPool c k
loadUnsafeShared (Shared.SharedAddressPool pool0) (SharedAddressMap addrs) =
    Shared.SharedAddressPool $ AddressPool.loadUnsafe pool0 addrs

instance ( key ~ SharedKey )
    => Buildable (Prologue (Shared.SharedState n key))
  where
    build (SharedPrologue st) = "Prologue of " <> build st

instance ( key ~ SharedKey ) => Eq (Prologue (Shared.SharedState n key)) where
    SharedPrologue a == SharedPrologue b = a == b

instance ( key ~ SharedKey ) => Eq (Discoveries (Shared.SharedState n key)) where
    SharedDiscoveries ext1 int1 == SharedDiscoveries ext2 int2 =
        ext1 == ext2 && int1 == int2

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

instance Buildable (Prologue (Rnd.RndState n)) where
    build (RndPrologue st) = "Prologue of " <> build st

instance Eq (Prologue (Rnd.RndState n)) where
    RndPrologue a == RndPrologue b = a == b

instance Eq (Discoveries (Rnd.RndState n)) where
    RndDiscoveries a == RndDiscoveries b = a == b
