{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.Wallet.Shelley.CompatibilitySpec
    ( spec
    ) where

import Prelude

import Cardano.Address.Derivation
    ( XPrv
    , XPub
    )
import Cardano.Mnemonic
    ( ConsistentEntropy
    , EntropySize
    , Mnemonic
    , SomeMnemonic (..)
    , entropyToMnemonic
    )
import Cardano.Wallet.Address.Derivation
    ( Depth (..)
    , Index (getIndex)
    , PaymentAddress (..)
    , delegationAddress
    )
import Cardano.Wallet.Address.Derivation.Byron
    ( ByronKey (..)
    )
import Cardano.Wallet.Address.Derivation.Shelley
    ( ShelleyKey (..)
    )
import Cardano.Wallet.Address.Encoding
    ( decodeAddress
    , decodeStakeAddress
    , encodeStakeAddress
    , inspectAddress
    )
import Cardano.Wallet.Address.Keys.WalletKey
    ( publicKey
    )
import Cardano.Wallet.Flavor
    ( KeyFlavor
    , keyFlavor
    )
import Cardano.Wallet.Primitive.Ledger.Shelley
    ( StandardCrypto
    )
import Cardano.Wallet.Primitive.NetworkId
    ( NetworkId (..)
    , SNetworkId (..)
    , withSNetworkId
    )
import Cardano.Wallet.Primitive.Types.Address
    ( Address (..)
    )
import Cardano.Wallet.Primitive.Types.RewardAccount
    ( RewardAccount (..)
    )
import Cardano.Wallet.Unsafe
    ( unsafeMkEntropy
    )
import Codec.Binary.Bech32.TH
    ( humanReadablePart
    )
import Control.Monad
    ( forM_
    )
import Data.ByteString
    ( ByteString
    )
import Data.ByteString.Base58
    ( bitcoinAlphabet
    , encodeBase58
    )
import Data.Either
    ( isLeft
    , isRight
    )
import Data.Function
    ( (&)
    )
import Data.Proxy
    ( Proxy (..)
    )
import Data.Text
    ( Text
    )
import GHC.TypeLits
    ( natVal
    )
import Test.Hspec
    ( Spec
    , describe
    , it
    , shouldSatisfy
    )
import Test.Hspec.QuickCheck
    ( prop
    )
import Test.QuickCheck
    ( Arbitrary (..)
    , Gen
    , chooseInt
    , counterexample
    , property
    , vector
    , (===)
    )

import qualified Cardano.Ledger.Address as SL
import qualified Cardano.Wallet.Address.Derivation as Address.Derivation
import qualified Cardano.Wallet.Address.Derivation.Byron as Byron
import qualified Cardano.Wallet.Address.Derivation.Shelley as Shelley
import qualified Codec.Binary.Bech32 as Bech32
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as T

spec :: Spec
spec = do

    describe "Shelley StakeAddress" $ do
        prop "roundtrip / Mainnet" $ \x ->
            (decodeStakeAddress SMainnet . encodeStakeAddress SMainnet) x
            ===
            Right x

        prop "roundtrip / Testnet" $ \x -> withSNetworkId (NTestnet 0) $ \n ->
            (decodeStakeAddress n . encodeStakeAddress n) x === Right x

    describe "Shelley Addresses" $ do
        prop "(Mainnet) can be deserialised by shelley ledger spec" $ \k -> do
            let Address addr
                    = paymentAddress @ShelleyKey @'CredFromKeyK SMainnet k
            case SL.deserialiseAddr @StandardCrypto addr of
                Just _ -> property True
                Nothing -> property False

        prop "Shelley addresses from bech32" $ \k ->
            let addr@(Address bytes)
                    = paymentAddress @ShelleyKey @'CredFromKeyK SMainnet k
            in  decodeAddress SMainnet (bech32 bytes) === Right addr
                    & counterexample (show $ bech32 bytes)

        prop "Shelley addresses with delegation from bech32" $ \k1 k2 ->
            let addr@(Address bytes)
                    = delegationAddress
                        @ShelleyKey @'CredFromKeyK SMainnet k1 k2
            in  decodeAddress SMainnet (bech32 bytes) === Right addr
                    & counterexample (show $ bech32 bytes)

        prop "Shelley addresses from bech32 - testnet"
            $ \k -> withSNetworkId (NTestnet 0)
                $ \n ->
                    let addr@(Address raw) =
                            paymentAddress @ShelleyKey @'CredFromKeyK n k
                    in  decodeAddress n (bech32testnet raw)
                            === Right addr
                            & counterexample (show $ bech32testnet raw)

        prop "Byron addresses from base58" $ \k ->
            let addr@(Address bytes)
                    = paymentAddress @ByronKey @'CredFromKeyK SMainnet k
            in  decodeAddress SMainnet (base58 bytes) === Right addr
                    & counterexample (show $ base58 bytes)

    describe "InspectAddr" $ do
        -- Cases below are taken straight from cardano-addresses. We don't go in
        -- depth with testing here because this is already tested on
        -- cardano-addresses.
        let matrix =
                [ ( "Byron (1)"
                  , "37btjrVyb4KEgoGCHJ7XFaJRLBRiVuvcrQWPpp4HeaxdTxhKwQjXHNKL43\
                    \NhXaQNa862BmxSFXZFKqPqbxRc3kCUeTRMwjJevFeCKokBG7A7num5Wh"
                  , isRight
                  )
                , ( "Byron (2)"
                  , "DdzFFzCqrht5csm2GKhnVrjzKpVHHQFNXUDhAFDyLWVY5w8ZsJRP2uhwZ\
                    \q2CEAVzDZXYXa4GvggqYEegQsdKAKikFfrrCoHheLH2Jskr"
                  , isRight
                  )
                , ( "Icarus"
                  , "Ae2tdPwUPEYz6ExfbWubiXPB6daUuhJxikMEb4eXRp5oKZBKZwrbJ2k7EZe"
                  , isRight
                  )
                , ( "Shelley (base)"
                  , "addr1vpu5vlrf4xkxv2qpwngf6cjhtw542ayty80v8dyr49rf5eg0yu80w"
                  , isRight
                  )
                , ( "Shelley (stake by value)"
                  , "addr1qdu5vlrf4xkxv2qpwngf6cjhtw542ayty80v8dyr49rf5ew\
                    \vxwdrt70qlcpeeagscasafhffqsxy36t90ldv06wqrk2q5ggg4z"
                  , isRight
                  )
                , ( "Shelley (stake by pointer)"
                  , "addr1gw2fxv2umyhttkxyxp8x0dlpdt3k6cwng5pxj3jhsydzer5ph3wczvf2x4v58t"
                  , isRight
                  )
                , ( "Shelley (reward by key)"
                  , "stake1upshvetj09hxjcm9v9jxgunjv4ehxmr0d3hkcmmvdakx7mqcjv83c"
                  , isRight
                  )
                , ( "Shelley (reward by script)"
                  , "stake17pshvetj09hxjcm9v9jxgunjv4ehxmr0d3hkcmmvdakx7mq36s8xc"
                  , isRight
                  )
                , ( "Shelley (testnet 1)"
                  , "addr_test1qpwr8l57ceql23ylyprl6qgct239lxph8clwxy5w8r4qdz8ct9uut5a\
                    \hmxqkgwy9ecn5carsv39frsgsq09u70wmqwhqjqcjqs"
                  , isRight
                  )
                , ( "Shelley (testnet 2)"
                  , "stake_test1uru9j7w96wmanqty8zzuuf6vw3cxgj53cygq8j708hds8tsntl0j7"
                  , isRight
                  )
                , ( "Malformed (1)"
                  , "💩"
                  , isLeft
                  )
                , ( "Malformed (2)"
                  , "79467c69a9ac66280174d09d62575ba955748b21dec3b483a9469a65"
                  , isLeft
                  )
                ]

        forM_ matrix $ \(title, addr, predicate) ->
            it title $ inspectAddress addr `shouldSatisfy` predicate

--------------------------------------------------------------------------------
-- Conversions
--------------------------------------------------------------------------------

instance Arbitrary RewardAccount where
    arbitrary = FromKeyHash . BS.pack <$> vector 28

instance Arbitrary (ShelleyKey 'CredFromKeyK XPrv) where
    shrink _ = []
    arbitrary = do
        mnemonic <- arbitrary
        return $ Shelley.unsafeGenerateKeyFromSeed mnemonic mempty

instance Arbitrary (ByronKey 'CredFromKeyK XPrv) where
    shrink _ = []
    arbitrary = do
        mnemonic <- arbitrary
        acctIx <- arbitrary
        addrIx <- arbitrary
        pure $ Byron.unsafeGenerateKeyFromSeed (acctIx, addrIx) mnemonic mempty

instance
    ( Enum (Address.Derivation.Index derivationType depth)
    , Bounded (Address.Derivation.Index derivationType depth)
    ) =>
    Arbitrary (Address.Derivation.Index derivationType depth) where
    arbitrary = toEnum <$> chooseInt (0, maxIndex)
      where
        maxIndex = fromIntegral . getIndex $
            maxBound @(Address.Derivation.Index derivationType depth)

instance (KeyFlavor k, Arbitrary (k 'CredFromKeyK XPrv)) =>
    Arbitrary (k 'CredFromKeyK XPub)
  where
    shrink _ = []
    arbitrary = publicKey (keyFlavor @k) <$> arbitrary

instance Arbitrary SomeMnemonic where
    arbitrary = SomeMnemonic <$> genMnemonic @12

genMnemonic
    :: forall mw ent csz.
     ( ConsistentEntropy ent mw csz
     , EntropySize mw ~ ent
     )
    => Gen (Mnemonic mw)
genMnemonic = do
        let n = fromIntegral (natVal $ Proxy @(EntropySize mw)) `div` 8
        bytes <- BS.pack <$> vector n
        let ent = unsafeMkEntropy @(EntropySize mw) bytes
        return $ entropyToMnemonic ent

instance Show XPrv where
    show _ = "<xprv>"

--
-- Helpers
--
--

bech32 :: ByteString -> Text
bech32 = Bech32.encodeLenient hrp . Bech32.dataPartFromBytes
  where hrp = [humanReadablePart|addr|]

-- Expected bech32 encoding for testnets
-- https://github.com/cardano-foundation/CIPs/tree/master/CIP5
bech32testnet :: ByteString -> Text
bech32testnet = Bech32.encodeLenient hrp . Bech32.dataPartFromBytes
  where hrp = [humanReadablePart|addr_test|]

base58 :: ByteString -> Text
base58 = T.decodeUtf8 . encodeBase58 bitcoinAlphabet
