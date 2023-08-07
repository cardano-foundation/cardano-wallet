{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
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

module Cardano.Wallet.Address.Derivation.Shelley
    ( -- * Types
      ShelleyKey(..)
    , shelleyKey

    -- * Constants
    , minSeedLengthBytes

    -- * Generation and derivation
    , generateKeyFromSeed
    , unsafeGenerateKeyFromSeed
    , unsafeGenerateKeyFromSeedShelley
    , deriveAccountPrivateKeyShelley
    , deriveAddressPrivateKeyShelley
    , deriveAddressPublicKeyShelley

    -- * Reward Account
    , toRewardAccountRaw
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
    , unXPub
    , xpub
    )
import Cardano.Mnemonic
    ( SomeMnemonic (..), entropyToBytes, mnemonicToEntropy )
import Cardano.Wallet.Address.Derivation
    ( AccountIxForStaking (..)
    , AddressParts (..)
    , DelegationAddress (..)
    , Depth (..)
    , DerivationIndex (..)
    , DerivationType (..)
    , ErrMkKeyFingerprint (..)
    , HardDerivation (..)
    , Index (..)
    , KeyFingerprint (..)
    , MkKeyFingerprint (..)
    , PaymentAddress (..)
    , PersistPublicKey (..)
    , RewardAccount (..)
    , Role (..)
    , SoftDerivation (..)
    , ToRewardAccount (..)
    , fromHex
    , hex
    , mutableAccount
    , toAddressParts
    )
import Cardano.Wallet.Address.Discovery
    ( GetPurpose (..), IsOurs (..) )
import Cardano.Wallet.Address.Discovery.Sequential
    ( DerivationPrefix (..)
    , SeqState (..)
    , coinTypeAda
    , purposeCIP1852
    , rewardAccountKey
    )
import Cardano.Wallet.Primitive.NetworkId
    ( NetworkDiscriminant (..)
    , NetworkDiscriminantCheck (..)
    , SNetworkId (..)
    , networkDiscriminantBits
    )
import Cardano.Wallet.Primitive.Passphrase
    ( Passphrase (..) )
import Cardano.Wallet.Primitive.Types.Address
    ( Address (..) )
import Control.DeepSeq
    ( NFData (..) )
import Control.Lens
    ( Iso, iso, over, (^.) )
import Control.Monad
    ( guard, (<=<) )
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
-- let addressPubKey = ShelleyKey 'CredFromKeyK XPub
-- @
newtype ShelleyKey (depth :: Depth) key =
    ShelleyKey { getKey :: key }
    deriving stock (Generic, Show, Eq)

shelleyKey :: Iso (ShelleyKey depth key) (ShelleyKey depth key') key key'
shelleyKey = iso getKey ShelleyKey

instance NFData key => NFData (ShelleyKey depth key)

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
unsafeGenerateKeyFromSeed mnemonics pwd =
    ShelleyKey $ unsafeGenerateKeyFromSeedShelley mnemonics pwd

unsafeGenerateKeyFromSeedShelley
    :: (SomeMnemonic, Maybe SomeMnemonic)
        -- ^ The actual seed and its recovery / generation passphrase
    -> Passphrase "encryption"
    -> XPrv
unsafeGenerateKeyFromSeedShelley (root, m2nd) pwd =
    generateNew validSeed (maybe mempty mnemonicToBytes m2nd) (unPassphrase pwd)
  where
    mnemonicToBytes (SomeMnemonic mw) = entropyToBytes $ mnemonicToEntropy mw
    seed  = mnemonicToBytes root
    validSeed =
        if BA.length seed >= minSeedLengthBytes && BA.length seed <= 255
            then seed
            else error . unwords $
                [ "seed length:"
                , show (BA.length seed)
                , "in (Passphrase \"seed\") is not valid"
                ]

deriveAccountPrivateKeyShelley
    :: Index 'Hardened 'PurposeK
    -> Passphrase "encryption"
    -> XPrv
    -> Index 'Hardened 'AccountK
    -> XPrv
deriveAccountPrivateKeyShelley purpose (Passphrase pwd) rootXPrv (Index accIx) =
    let
        purposeXPrv = -- lvl1 derivation; hardened derivation of purpose'
            deriveXPrv DerivationScheme2 pwd rootXPrv (getIndex purpose)
        coinTypeXPrv = -- lvl2 derivation; hardened derivation of coin_type'
            deriveXPrv DerivationScheme2 pwd purposeXPrv (getIndex coinTypeAda)
     -- lvl3 derivation; hardened derivation of account' index
    in deriveXPrv DerivationScheme2 pwd coinTypeXPrv accIx

deriveAddressPrivateKeyShelley
    :: Enum a
    => Passphrase "encryption"
    -> XPrv
    -> a
    -> Index derivationType level
    -> XPrv
deriveAddressPrivateKeyShelley (Passphrase pwd) accXPrv role (Index addrIx) =
    let
        changeCode =
            fromIntegral $ fromEnum role
        changeXPrv = -- lvl4 derivation; soft derivation of change chain
            deriveXPrv DerivationScheme2 pwd accXPrv changeCode
       -- lvl5 derivation; soft derivation of address index
    in deriveXPrv DerivationScheme2 pwd changeXPrv addrIx

deriveAddressPublicKeyShelley
    :: Enum a
    => XPub
    -> a
    -> Index derivationType level
    -> XPub
deriveAddressPublicKeyShelley accXPub role (Index addrIx) =
    fromMaybe errWrongIndex $ do
        let changeCode = fromIntegral $ fromEnum role
        changeXPub <- -- lvl4 derivation in bip44 is derivation of change chain
            deriveXPub DerivationScheme2 accXPub changeCode
        -- lvl5 derivation in bip44 is derivation of address chain
        deriveXPub DerivationScheme2 changeXPub addrIx
  where
      errWrongIndex = error $
          "deriveAddressPublicKey failed: was given an hardened (or too big) \
          \index for soft path derivation ( " ++ show addrIx ++ "). This is \
          \either a programmer error, or, we may have reached the maximum \
          \number of addresses for a given wallet."

instance HardDerivation ShelleyKey where
    type AddressIndexDerivationType ShelleyKey = 'Soft
    type AddressCredential ShelleyKey = 'CredFromKeyK

    deriveAccountPrivateKey pwd (ShelleyKey rootXPrv) ix =
        ShelleyKey $ deriveAccountPrivateKeyShelley purposeCIP1852 pwd rootXPrv ix

    deriveAddressPrivateKey pwd (ShelleyKey accXPrv) role ix =
        ShelleyKey $ deriveAddressPrivateKeyShelley pwd accXPrv role ix

instance SoftDerivation ShelleyKey where
    deriveAddressPublicKey (ShelleyKey accXPub) role ix =
        ShelleyKey $ deriveAddressPublicKeyShelley accXPub role ix

{-------------------------------------------------------------------------------
                         Relationship Key / Address
-------------------------------------------------------------------------------}

instance GetPurpose ShelleyKey where
    getPurpose = purposeCIP1852

instance PaymentAddress ShelleyKey 'CredFromKeyK where
    paymentAddress SMainnet paymentK = do
        Address $ BL.toStrict $ runPut $ do
            putWord8 (enterprise + networkId)
            putByteString . blake2b224 . xpubPublicKey . getKey $ paymentK
      where
        enterprise = 96
        networkId = 1
    paymentAddress _ paymentK =
        Address $ BL.toStrict $ runPut $ do
            putWord8 (enterprise + networkId)
            putByteString . blake2b224 . xpubPublicKey . getKey $ paymentK
      where
        enterprise = 96
        networkId = 0
    liftPaymentAddress SMainnet (KeyFingerprint fingerprint) =
        Address $ BL.toStrict $ runPut $ do
            putWord8 (enterprise + networkId)
            putByteString fingerprint
      where
        enterprise = 96
        networkId = 1
    liftPaymentAddress _ (KeyFingerprint fingerprint) =
        Address $ BL.toStrict $ runPut $ do
            putWord8 (enterprise + networkId)
            putByteString fingerprint
      where
        enterprise = 96
        networkId = 0

instance DelegationAddress ShelleyKey 'CredFromKeyK where
    delegationAddress SMainnet paymentK stakingK =
        Address $ BL.toStrict $ runPut $ do
            putWord8 (base + networkId)
            putByteString . blake2b224 . xpubPublicKey . getKey $ paymentK
            putByteString . blake2b224 . xpubPublicKey . getKey $ stakingK
      where
        base = 0
        networkId = 1
    delegationAddress _ paymentK stakingK =
        Address $ BL.toStrict $ runPut $ do
            putWord8 (base + networkId)
            putByteString . blake2b224 . xpubPublicKey . getKey $ paymentK
            putByteString . blake2b224 . xpubPublicKey . getKey $ stakingK
      where
        base = 0
        networkId = 0

    liftDelegationAddress SMainnet (KeyFingerprint fingerprint) stakingK =
        Address $ BL.toStrict $ runPut $ do
            putWord8 (base + networkId)
            putByteString fingerprint
            putByteString . blake2b224. xpubPublicKey . getKey $ stakingK
      where
        base = 0
        networkId = 1
    liftDelegationAddress _ (KeyFingerprint fingerprint) stakingK =
        Address $ BL.toStrict $ runPut $ do
            putWord8 (base + networkId)
            putByteString fingerprint
            putByteString . blake2b224 . xpubPublicKey . getKey $ stakingK
      where
        base = 0
        networkId = 0

instance MkKeyFingerprint ShelleyKey Address where
    paymentKeyFingerprint addr =
        let AddressParts{..} = toAddressParts addr
            baseAddr = 0b00000000       -- keyhash; keyhash
            enterpriseAddr = 0b01100000 -- keyhash
            rewardAcct = 0b11100000     -- keyhash
        in if addrType `elem` [baseAddr, enterpriseAddr, rewardAcct] then
            Right $ KeyFingerprint $ BS.take hashSize rest
           else
            Left $ ErrInvalidAddress addr (Proxy @ShelleyKey)

instance
    MkKeyFingerprint
        ShelleyKey
        ( Proxy (n :: NetworkDiscriminant)
        , ShelleyKey 'CredFromKeyK XPub
        )
    where
    paymentKeyFingerprint (_, paymentK) =
        Right $ KeyFingerprint $ blake2b224 $ xpubPublicKey $ getKey paymentK

instance NetworkDiscriminantCheck ShelleyKey where
    networkDiscriminantCheck network networkTag =
        networkTag == networkDiscriminantBits network

{-------------------------------------------------------------------------------
                          Dealing with Rewards
-------------------------------------------------------------------------------}

instance IsOurs (SeqState n ShelleyKey) RewardAccount
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
        ourAccount = toRewardAccount $ rewardAccountKey state

instance ToRewardAccount ShelleyKey where
    toRewardAccount = toRewardAccountRaw . getKey
    someRewardAccount mw =
        let
            path = NE.fromList
                [ DerivationIndex $ getIndex purposeCIP1852
                , DerivationIndex $ getIndex coinTypeAda
                , DerivationIndex $ getIndex @'Hardened minBound
                , DerivationIndex $ getIndex mutableAccount
                , DerivationIndex $ getIndex @'Soft minBound
                ]
        in
            (stakK ^. shelleyKey, toRewardAccount
                (over shelleyKey toXPub stakK)
            , path)
      where
        rootK = generateKeyFromSeed (mw, Nothing) mempty
        acctK = deriveAccountPrivateKey mempty rootK minBound
        stakK = deriveAddressPrivateKey mempty acctK MutableAccount minBound

toRewardAccountRaw :: XPub -> RewardAccount
toRewardAccountRaw = FromKeyHash . blake2b224 . xpubPublicKey

instance AccountIxForStaking (SeqState n ShelleyKey) where
    getAccountIx _ = Nothing

{-------------------------------------------------------------------------------
                          Storing and retrieving keys
-------------------------------------------------------------------------------}

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
