{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- Implementation of address derivation for 'Shelley' Keys.

module Cardano.Wallet.Primitive.AddressDerivation.Shelley
    ( -- * Types
      ShelleyKey(..)

    -- * Constants
    , minSeedLengthBytes

    -- * Generation and derivation
    , generateKeyFromSeed
    , unsafeGenerateKeyFromSeed

    -- * Reward Account
    , toChimericAccountRaw

    -- * Address
    , decodeShelleyAddress
    ) where

import Prelude

import Cardano.Crypto.Wallet
    ( DerivationScheme (..)
    , XPrv
    , XPub (..)
    , deriveXPrv
    , deriveXPub
    , generateNew
    , toXPub
    , unXPrv
    , unXPub
    , xPrvChangePass
    , xprv
    , xpub
    )
import Cardano.Mnemonic
    ( SomeMnemonic (..), entropyToBytes, mnemonicToEntropy )
import Cardano.Wallet.Primitive.AddressDerivation
    ( ChimericAccount (..)
    , DelegationAddress (..)
    , Depth (..)
    , DerivationIndex (..)
    , DerivationType (..)
    , HardDerivation (..)
    , Index (..)
    , KeyFingerprint (..)
    , MkKeyFingerprint (..)
    , NetworkDiscriminant (..)
    , Passphrase (..)
    , PaymentAddress (..)
    , PersistPrivateKey (..)
    , PersistPublicKey (..)
    , SoftDerivation (..)
    , ToChimericAccount (..)
    , WalletKey (..)
    , deriveRewardAccount
    , fromHex
    , hex
    , mutableAccount
    )
import Cardano.Wallet.Primitive.AddressDiscovery
    ( IsOurs (..) )
import Cardano.Wallet.Primitive.AddressDiscovery.Sequential
    ( DerivationPrefix (..)
    , SeqState (..)
    , coinTypeAda
    , purposeCIP1852
    , rewardAccountKey
    )
import Cardano.Wallet.Primitive.Types
    ( Address (..), invariant )
import Cardano.Wallet.Primitive.Types.Hash
    ( Hash (..) )
import Control.DeepSeq
    ( NFData (..) )
import Control.Monad
    ( guard, (<=<) )
import Crypto.Hash
    ( hash )
import Crypto.Hash.Algorithms
    ( Blake2b_224 (..) )
import Crypto.Hash.IO
    ( HashAlgorithm (hashDigestSize) )
import Crypto.Hash.Utils
    ( blake2b224 )
import Data.Binary.Put
    ( putByteString, putWord8, runPut )
import Data.ByteString
    ( ByteString )
import Data.Maybe
    ( fromMaybe )
import Data.Proxy
    ( Proxy (..) )
import Data.Text.Class
    ( TextDecodingError (..) )
import GHC.Generics
    ( Generic )

import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.List.NonEmpty as NE

{-------------------------------------------------------------------------------
                            Sequential Derivation
-------------------------------------------------------------------------------}

-- | A cryptographic key for Shelley address derivation, with phantom-types to
-- disambiguate derivation paths
--
-- @
-- let rootPrivateKey = ShelleyKey 'RootK XPrv
-- let accountPubKey = ShelleyKey 'AccountK XPub
-- let addressPubKey = ShelleyKey 'AddressK XPub
-- @
newtype ShelleyKey (depth :: Depth) key =
    ShelleyKey { getKey :: key }
    deriving stock (Generic, Show, Eq)

instance (NFData key) => NFData (ShelleyKey depth key)

-- | The minimum seed length for 'generateKeyFromSeed' and
-- 'unsafeGenerateKeyFromSeed'.
minSeedLengthBytes :: Int
minSeedLengthBytes = 16

-- | Generate a root key from a corresponding seed.
-- The seed should be at least 16 bytes.
generateKeyFromSeed
    :: (SomeMnemonic, Maybe SomeMnemonic)
       -- ^ The actual seed and its recovery / generation passphrase
    -> Passphrase "encryption"
    -> ShelleyKey 'RootK XPrv
generateKeyFromSeed = unsafeGenerateKeyFromSeed

-- | Generate a new key from seed. Note that the @depth@ is left open so that
-- the caller gets to decide what type of key this is. This is mostly for
-- testing, in practice, seeds are used to represent root keys, and one should
-- use 'generateKeyFromSeed'.
unsafeGenerateKeyFromSeed
    :: (SomeMnemonic, Maybe SomeMnemonic)
        -- ^ The actual seed and its recovery / generation passphrase
    -> Passphrase "encryption"
    -> ShelleyKey depth XPrv
unsafeGenerateKeyFromSeed (root, m2nd) (Passphrase pwd) =
    ShelleyKey $ generateNew seed' (maybe mempty mnemonicToBytes m2nd) pwd
  where
    mnemonicToBytes (SomeMnemonic mw) = entropyToBytes $ mnemonicToEntropy mw
    seed  = mnemonicToBytes root
    seed' = invariant
        ("seed length : " <> show (BA.length seed) <> " in (Passphrase \"seed\") is not valid")
        seed
        (\s -> BA.length s >= minSeedLengthBytes && BA.length s <= 255)

instance HardDerivation ShelleyKey where
    type AddressIndexDerivationType ShelleyKey = 'Soft

    deriveAccountPrivateKey
            (Passphrase pwd) (ShelleyKey rootXPrv) (Index accIx) =
        let
            purposeXPrv = -- lvl1 derivation; hardened derivation of purpose'
                deriveXPrv DerivationScheme2 pwd rootXPrv (getIndex purposeCIP1852)
            coinTypeXPrv = -- lvl2 derivation; hardened derivation of coin_type'
                deriveXPrv DerivationScheme2 pwd purposeXPrv (getIndex coinTypeAda)
            acctXPrv = -- lvl3 derivation; hardened derivation of account' index
                deriveXPrv DerivationScheme2 pwd coinTypeXPrv accIx
        in
            ShelleyKey acctXPrv

    deriveAddressPrivateKey
            (Passphrase pwd) (ShelleyKey accXPrv) accountingStyle (Index addrIx) =
        let
            changeCode =
                fromIntegral $ fromEnum accountingStyle
            changeXPrv = -- lvl4 derivation; soft derivation of change chain
                deriveXPrv DerivationScheme2 pwd accXPrv changeCode
            addrXPrv = -- lvl5 derivation; soft derivation of address index
                deriveXPrv DerivationScheme2 pwd changeXPrv addrIx
        in
            ShelleyKey addrXPrv

instance SoftDerivation ShelleyKey where
    deriveAddressPublicKey (ShelleyKey accXPub) accountingStyle (Index addrIx) =
        fromMaybe errWrongIndex $ do
            let changeCode = fromIntegral $ fromEnum accountingStyle
            changeXPub <- -- lvl4 derivation in bip44 is derivation of change chain
                deriveXPub DerivationScheme2 accXPub changeCode
            addrXPub <- -- lvl5 derivation in bip44 is derivation of address chain
                deriveXPub DerivationScheme2 changeXPub addrIx
            return $ ShelleyKey addrXPub
      where
        errWrongIndex = error $
            "deriveAddressPublicKey failed: was given an hardened (or too big) \
            \index for soft path derivation ( " ++ show addrIx ++ "). This is \
            \either a programmer error, or, we may have reached the maximum \
            \number of addresses for a given wallet."

{-------------------------------------------------------------------------------
                            WalletKey implementation
-------------------------------------------------------------------------------}

instance WalletKey ShelleyKey where
    changePassphrase (Passphrase oldPwd) (Passphrase newPwd) (ShelleyKey prv) =
        ShelleyKey $ xPrvChangePass oldPwd newPwd prv

    publicKey (ShelleyKey prv) =
        ShelleyKey (toXPub prv)

    digest (ShelleyKey pub) =
        hash (unXPub pub)

    getRawKey =
        getKey

    keyTypeDescriptor _ =
        "she"

{-------------------------------------------------------------------------------
                         Relationship Key / Address
-------------------------------------------------------------------------------}

instance PaymentAddress 'Mainnet ShelleyKey where
    paymentAddress paymentK = do
        Address $ BL.toStrict $ runPut $ do
            putWord8 (enterprise + networkId)
            putByteString . blake2b224 . xpubPublicKey . getKey $ paymentK
      where
        enterprise = 96
        networkId = 1

    liftPaymentAddress (KeyFingerprint fingerprint) =
        Address $ BL.toStrict $ runPut $ do
            putWord8 (enterprise + networkId)
            putByteString fingerprint
      where
        enterprise = 96
        networkId = 1

instance PaymentAddress ('Testnet pm) ShelleyKey where
    paymentAddress paymentK =
        Address $ BL.toStrict $ runPut $ do
            putWord8 (enterprise + networkId)
            putByteString . blake2b224 . xpubPublicKey . getKey $ paymentK
      where
        enterprise = 96
        networkId = 0

    liftPaymentAddress (KeyFingerprint fingerprint) =
        Address $ BL.toStrict $ runPut $ do
            putWord8 (enterprise + networkId)
            putByteString fingerprint
      where
        enterprise = 96
        networkId = 0

instance DelegationAddress 'Mainnet ShelleyKey where
    delegationAddress paymentK stakingK =
        Address $ BL.toStrict $ runPut $ do
            putWord8 (base + networkId)
            putByteString . blake2b224 . xpubPublicKey . getKey $ paymentK
            putByteString . blake2b224 . xpubPublicKey . getKey $ stakingK
      where
        base = 0
        networkId = 1

    liftDelegationAddress (KeyFingerprint fingerprint) stakingK =
        Address $ BL.toStrict $ runPut $ do
            putWord8 (base + networkId)
            putByteString fingerprint
            putByteString . blake2b224. xpubPublicKey . getKey $ stakingK
      where
        base = 0
        networkId = 1

instance DelegationAddress ('Testnet pm) ShelleyKey where
    delegationAddress paymentK stakingK =
        Address $ BL.toStrict $ runPut $ do
            putWord8 (base + networkId)
            putByteString . blake2b224 . xpubPublicKey . getKey $ paymentK
            putByteString . blake2b224 . xpubPublicKey . getKey $ stakingK
      where
        base = 0
        networkId = 0

    liftDelegationAddress (KeyFingerprint fingerprint) stakingK =
        Address $ BL.toStrict $ runPut $ do
            putWord8 (base + networkId)
            putByteString fingerprint
            putByteString . blake2b224 . xpubPublicKey . getKey $ stakingK
      where
        base = 0
        networkId = 0

-- | Verify the structure of a payload decoded from a Bech32 text string
decodeShelleyAddress
    :: ByteString
    -> Either TextDecodingError Address
decodeShelleyAddress _bytes = do
    error "TODO: decodeShelleyAddress"

instance MkKeyFingerprint ShelleyKey Address where
    paymentKeyFingerprint (Address bytes) =
        Right $ KeyFingerprint $ BS.take hashSize $ BS.drop 1 bytes

instance MkKeyFingerprint ShelleyKey (Proxy (n :: NetworkDiscriminant), ShelleyKey 'AddressK XPub) where
    paymentKeyFingerprint (_, paymentK) =
        Right $ KeyFingerprint $ blake2b224 $ xpubPublicKey $ getKey paymentK

{-------------------------------------------------------------------------------
                          Dealing with Rewards
-------------------------------------------------------------------------------}

instance IsOurs (SeqState n ShelleyKey) ChimericAccount
  where
    isOurs account state@SeqState{derivationPrefix} =
        let
            DerivationPrefix (purpose, coinType, accountIx) = derivationPrefix
            path = NE.fromList
                [ DerivationIndex $ getIndex purpose
                , DerivationIndex $ getIndex coinType
                , DerivationIndex $ getIndex accountIx
                , DerivationIndex $ getIndex mutableAccount
                , DerivationIndex $ getIndex @'Soft minBound
                ]
        in
            (guard (account == ourAccount) *> Just path, state)
      where
        ourAccount = toChimericAccount $ rewardAccountKey state

instance ToChimericAccount ShelleyKey where
    toChimericAccount = toChimericAccountRaw . getKey
    someChimericAccount mw =
        (getRawKey acctK, toChimericAccount (publicKey acctK))
      where
        rootK = generateKeyFromSeed (mw, Nothing) mempty
        acctK = deriveRewardAccount mempty rootK

toChimericAccountRaw :: XPub -> ChimericAccount
toChimericAccountRaw = ChimericAccount . blake2b224 . xpubPublicKey

{-------------------------------------------------------------------------------
                          Storing and retrieving keys
-------------------------------------------------------------------------------}

instance PersistPrivateKey (ShelleyKey 'RootK) where
    serializeXPrv (k, h) =
        ( hex . unXPrv . getKey $ k
        , hex . getHash $ h
        )

    unsafeDeserializeXPrv (k, h) = either err id $ (,)
        <$> fmap ShelleyKey (xprvFromText k)
        <*> fmap Hash (fromHex h)
      where
        xprvFromText = xprv <=< fromHex @ByteString
        err _ = error "unsafeDeserializeXPrv: unable to deserialize ShelleyKey"

instance PersistPublicKey (ShelleyKey depth) where
    serializeXPub =
        hex . unXPub . getKey

    unsafeDeserializeXPub =
        either err ShelleyKey . (xpub <=< fromHex @ByteString)
      where
        err _ = error "unsafeDeserializeXPub: unable to deserialize ShelleyKey"

{-------------------------------------------------------------------------------
                                 Internals
-------------------------------------------------------------------------------}

hashSize :: Int
hashSize =
    hashDigestSize Blake2b_224
