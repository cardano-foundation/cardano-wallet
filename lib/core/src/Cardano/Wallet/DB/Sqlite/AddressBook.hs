{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
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
    , SeqAddressList (..)
    )
  where

import Prelude

import Cardano.Address.Derivation
    ( XPub )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (..)
    , MkKeyFingerprint (..)
    , NetworkDiscriminant (..)
    , PaymentAddress (..)
    , Role (..)
    , SoftDerivation (..)
    )
import Cardano.Wallet.Primitive.AddressDerivation.SharedKey
    ( SharedKey (..) )
import Cardano.Wallet.Primitive.AddressDiscovery
    ( GetPurpose )
import Cardano.Wallet.Primitive.Types.Address
    ( Address (..), AddressState (..) )
import Data.Generics.Internal.VL
    ( Iso', iso, withIso )
import Data.Kind
    ( Type )
import Data.Map.Strict
    ( Map )
import Data.Proxy
    ( Proxy (..) )
import Data.Type.Equality
    ( type (==) )
import Data.Typeable
    ( Typeable )

import qualified Cardano.Wallet.Primitive.AddressDiscovery.Random as Rnd
import qualified Cardano.Wallet.Primitive.AddressDiscovery.Sequential as Seq
import qualified Cardano.Wallet.Primitive.AddressDiscovery.Shared as Shared
import qualified Cardano.Wallet.Primitive.Types.Address as W
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
-- | Sequential list of addresses
newtype SeqAddressList (c :: Role) = SeqAddressList [(W.Address, W.AddressState)]

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
instance 
    ( MkKeyFingerprint key (Proxy n, key 'AddressK XPub)
    , GetPurpose key
    , SoftDerivation key
    , PaymentAddress n key
    , Typeable n
    , (key == SharedKey) ~ 'False
    ) => AddressBookIso (Seq.SeqState n key)
  where
    data Prologue (Seq.SeqState n key)
        = SeqPrologue (Seq.SeqState n key)
        -- Trick: We keep the type, but we empty the discovered addresses
    data Discoveries (Seq.SeqState n key)
        = SeqDiscoveries
            (SeqAddressList 'UtxoInternal)
            (SeqAddressList 'UtxoExternal)

    addressIso = iso from to
      where
        from (Seq.SeqState int ext a b c) =
            ( SeqPrologue $ Seq.SeqState (emptyPool int) (emptyPool ext) a b c
            , SeqDiscoveries (toDiscoveries @n int) (toDiscoveries @n ext)
            )
        to (SeqPrologue (Seq.SeqState int ext a b c), SeqDiscoveries ints exts)
          = Seq.SeqState (fromDiscoveries @n int ints) (fromDiscoveries @n ext exts) a b c

-- | Extract the discovered addresses from an address pool.
toDiscoveries
    :: forall (n :: NetworkDiscriminant) (c :: Role) key.
    ( GetPurpose key
    , PaymentAddress n key
    , Typeable c
    ) => Seq.AddressPool c key -> SeqAddressList c
toDiscoveries pool = SeqAddressList
    [ (a,st) | (a,st,_) <- Seq.addresses (liftPaymentAddress @n) pool ]

-- | Variant of 'toDiscoveries' that can be used with 'SharedKey'
toDiscoveriesShared
    :: forall (n :: NetworkDiscriminant) (c :: Role) key.
    ( GetPurpose key
    , Typeable c
    , Typeable n
    , key ~ SharedKey
    ) => Seq.AddressPool c key -> SeqAddressList c
toDiscoveriesShared pool = SeqAddressList
    [ (a,st) | (a,st,_) <- Seq.addresses (Shared.liftPaymentAddress @n) pool ]

-- | Fill an empty address pool with addresses.
fromDiscoveries
    :: forall (n :: NetworkDiscriminant) (c :: Role) key.
    ( MkKeyFingerprint key (Proxy n, key 'AddressK XPub)
    , MkKeyFingerprint key Address
    , SoftDerivation key
    , Typeable n
    , Typeable c
    ) => Seq.AddressPool c key -> SeqAddressList c -> Seq.AddressPool c key
fromDiscoveries ctx (SeqAddressList addrs) =
    Seq.mkAddressPool @n (Seq.context ctx) (Seq.gap ctx) addrs

-- | Remove all discovered addresses from an address pool,
-- but keep context.
emptyPool :: Seq.AddressPool c key -> Seq.AddressPool c key
emptyPool pool = pool{ Seq.indexedKeys = Map.empty }

{-------------------------------------------------------------------------------
    Shared key address book
-------------------------------------------------------------------------------}
-- | Isomorphism for multi-sig address book.
instance
    ( MkKeyFingerprint key (Proxy n, key 'AddressK XPub)
    , MkKeyFingerprint key Address
    , GetPurpose key
    , SoftDerivation key
    , Typeable n
    , key ~ SharedKey
    ) => AddressBookIso (Shared.SharedState n key)
  where
    data Prologue (Shared.SharedState n key)
        = SharedPrologue (Shared.SharedState n key)
        -- Trick: We keep the type, but we empty the discovered addresses
    data Discoveries (Shared.SharedState n key)
        = SharedDiscoveries (SeqAddressList 'UtxoExternal)

    addressIso = iso from to
      where
        from st@(Shared.SharedState a b) = case b of
            Shared.PendingFields{} ->
                ( SharedPrologue st
                , SharedDiscoveries (SeqAddressList [])
                )
            Shared.ReadyFields pool ->
                let b0 = Shared.ReadyFields $ emptyPool pool
                in  ( SharedPrologue (Shared.SharedState a b0)
                    , SharedDiscoveries (toDiscoveriesShared @n pool)
                    )
        to  ( SharedPrologue st@(Shared.SharedState a b0)
            , SharedDiscoveries addrs
            )
          = case b0 of
            Shared.PendingFields{} -> st
            Shared.ReadyFields pool0
                -> Shared.SharedState a $ Shared.ReadyFields
                $ fromDiscoveries @n pool0 addrs

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
