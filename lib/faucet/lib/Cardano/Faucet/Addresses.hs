{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

-- `cardano-addresses` library marks byron functionality as deprecated
-- but in a meaning that new projects shouldn't use it. However, we still
-- need to support byron addresses in the wallet and make sure they work forever
-- so for this purpose it isn't deprecated.
-- This option allows to avoid a compiler warning;
{-# OPTIONS_GHC -Wno-deprecations #-}

{- | Module contains functionality to generate addresses
     of various styles for the faucet.
-}
module Cardano.Faucet.Addresses
    ( byron
    , icarus
    , shelley
    , shelleyRewardAccount
    ) where

import Prelude

import Cardano.Address
    ( Address
    )
import Cardano.Address.Derivation
    ( AccountIndexDerivationType
    , AddressIndexDerivationType
    , Depth (AccountK, DelegationK, PaymentK)
    , DerivationType (Hardened)
    , GenMasterKey (genMasterKeyFromMnemonic)
    , Index
    , XPrv
    , XPub
    , coerceWholeDomainIndex
    , deriveAccountPrivateKey
    , deriveAddressPrivateKey
    , nextIndex
    , toXPub
    )
import Cardano.Address.Style.Byron
    ( Byron
    )
import Cardano.Address.Style.Icarus
    ( Icarus
    )
import Cardano.Address.Style.Shelley
    ( Shelley
    )
import Cardano.Mnemonic
    ( Mnemonic
    , SomeMnemonic (..)
    )
import Data.List
    ( unfoldr
    )
import Data.Tuple.Extra
    ( dupe
    )
import GHC.TypeLits
    ( KnownNat
    )

import qualified Cardano.Address as CA
import qualified Cardano.Address.Style.Byron as Byron
import qualified Cardano.Address.Style.Icarus as Icarus
import qualified Cardano.Address.Style.Shelley as Shelley

byron :: KnownNat mw => CA.NetworkTag -> Mnemonic mw -> [Address]
byron netTag mw = mkPaymentAddrForIx <$> paymentKeyIxs
  where
    paymentKeyIxs :: [Index (AddressIndexDerivationType Byron) PaymentK] =
        let firstIx = minBound
        in firstIx : unfoldr (fmap dupe . nextIndex) firstIx
    mkPaymentAddrForIx paymentAddrIx =
        Byron.paymentAddress
            (CA.RequiresNetworkTag, netTag)
            (toXPub <$> paymentKey)
      where
        secondFactor = ()
        paymentKey =
            deriveAddressPrivateKey accountKey secondFactor paymentAddrIx
          where
            accountKey = deriveAccountPrivateKey masterKey accountIx
              where
                masterKey =
                    genMasterKeyFromMnemonic (SomeMnemonic mw) secondFactor
                accountIx :: Index (AddressIndexDerivationType Byron) AccountK =
                    coerceWholeDomainIndex (minBound :: Index Hardened AccountK)

icarus :: KnownNat mw => CA.NetworkTag -> Mnemonic mw -> [Address]
icarus netTag mw = mkPaymentAddrForIx <$> paymentKeyIxs
  where
    paymentKeyIxs :: [Index (AddressIndexDerivationType Icarus) PaymentK] =
        let firstIx = minBound
        in firstIx : unfoldr (fmap dupe . nextIndex) firstIx
    mkPaymentAddrForIx paymentAddrIx =
        Icarus.paymentAddress
            (CA.RequiresNetworkTag, netTag)
            (toXPub <$> paymentKey)
      where
        paymentKey =
            deriveAddressPrivateKey accountKey Icarus.UTxOExternal paymentAddrIx
          where
            accountKey = deriveAccountPrivateKey masterKey accountIx
              where
                accountIx :: Index (AccountIndexDerivationType Icarus) AccountK =
                    minBound
                masterKey = genMasterKeyFromMnemonic (SomeMnemonic mw) mempty

shelley :: KnownNat n => CA.NetworkTag -> Mnemonic n -> [Address]
shelley netTag mnemonic = mkPaymentAddrForIx <$> paymentKeyIxs
  where
    paymentKeyIxs :: [Index (AddressIndexDerivationType Shelley) PaymentK] =
        let firstIx = minBound
            in firstIx : unfoldr (fmap dupe . nextIndex) firstIx
    mkPaymentAddrForIx paymentAddrIx =
        Shelley.paymentAddress netTag credential
      where
        credential :: Shelley.Credential PaymentK =
            Shelley.PaymentFromExtendedKey (toXPub <$> paymentKey)
        paymentKey :: Shelley PaymentK XPrv =
            deriveAddressPrivateKey
                (deriveShelleyAccountKey mnemonic)
                Shelley.UTxOExternal
                paymentAddrIx

shelleyRewardAccount
    :: KnownNat n
    => Mnemonic n
    -> (Shelley DelegationK XPub, Shelley DelegationK XPrv)
shelleyRewardAccount mnemonic = (toXPub <$> xPrv, xPrv)
  where
    xPrv = Shelley.deriveDelegationPrivateKey (deriveShelleyAccountKey mnemonic)

deriveShelleyAccountKey :: KnownNat n => Mnemonic n -> Shelley AccountK XPrv
deriveShelleyAccountKey mnemonic = deriveAccountPrivateKey masterKey accountIx
  where
    accountIx :: Index 'Hardened 'AccountK = minBound
    masterKey = genMasterKeyFromMnemonic (SomeMnemonic mnemonic) mempty
