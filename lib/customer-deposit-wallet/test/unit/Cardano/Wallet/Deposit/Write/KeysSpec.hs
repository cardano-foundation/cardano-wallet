{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- Copyright: © 2024 Cardano Foundation
-- License: Apache-2.0
--
-- Property tests for the deposit wallet.
module Cardano.Wallet.Deposit.Write.KeysSpec
    ( spec
    ) where

import Prelude

import Cardano.Crypto.Wallet
    ( generate
    )
import Cardano.Wallet.Address.BIP32_Ed25519
    ( XPrv
    , XPub
    , sign
    , toXPub
    )
import "customer-deposit-wallet-pure" Cardano.Wallet.Address.Encoding
    ( EnterpriseAddr (..)
    , NetworkTag (..)
    , compactAddrFromEnterpriseAddr
    , credentialFromXPub
    )
import Cardano.Wallet.Deposit.Write.Keys
    ( enterpriseAddressFromVKey
    , signedDSIGNfromXSignature
    , vkeyFromXPub
    )
import Test.Hspec
    ( Spec
    , describe
    , it
    )
import Test.QuickCheck
    ( Arbitrary (..)
    , Blind (..)
    , Property
    , elements
    , property
    , vectorOf
    , withMaxSuccess
    , (===)
    )

import qualified Cardano.Crypto.Hash.Blake2b as Hash
import qualified Cardano.Crypto.Hash.Class as Hash
import qualified Cardano.Ledger.BaseTypes as L
import qualified Cardano.Ledger.Hashes as L
import qualified Cardano.Ledger.Keys as L
import qualified Cardano.Wallet.Read as Read
import qualified Data.ByteString as BS

{-----------------------------------------------------------------------------
    Spec
------------------------------------------------------------------------------}
spec :: Spec
spec = do
    describe "commutes with ledger" $ do
        it "address" $ lessCryptography $ property $
            \xpub networkTag ->
                let network = toLedgerNetwork networkTag
                in  enterpriseAddressFromVKey network (vkeyFromXPub xpub)
                        === enterpriseAddressFromXPub networkTag xpub

        it "verify" $ lessCryptography $ property $
            \(Blind xprv) hash ->
                let xpub = toXPub xprv
                    xsig = sign xprv (Hash.hashToBytes hash)
                in
                    True ===
                        L.verifySignedDSIGN
                            (vkeyFromXPub xpub)
                            hash
                            (signedDSIGNfromXSignature xsig)

lessCryptography :: Property -> Property
lessCryptography = withMaxSuccess 20

{-----------------------------------------------------------------------------
    Helper functions
------------------------------------------------------------------------------}
enterpriseAddressFromXPub :: NetworkTag -> XPub -> Read.CompactAddr
enterpriseAddressFromXPub networkTag =
    compactAddrFromEnterpriseAddr
    . EnterpriseAddrC networkTag
    . credentialFromXPub

toLedgerNetwork :: NetworkTag -> L.Network
toLedgerNetwork MainnetTag = L.Mainnet
toLedgerNetwork TestnetTag = L.Testnet

instance Arbitrary NetworkTag where
    arbitrary = elements [MainnetTag, TestnetTag]

instance Arbitrary XPrv where
    arbitrary = generate . BS.pack <$> vectorOf 100 arbitrary <*> pure BS.empty

instance Arbitrary XPub where
    arbitrary = toXPub <$> arbitrary

instance Arbitrary (Hash.Hash Hash.Blake2b_256 L.EraIndependentTxBody) where
    arbitrary = do
        bytes <- BS.pack <$> vectorOf (32) arbitrary
        let Just hash = Hash.hashFromBytes bytes
        pure hash
