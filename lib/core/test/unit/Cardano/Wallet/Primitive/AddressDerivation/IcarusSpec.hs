{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.Primitive.AddressDerivation.IcarusSpec
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Api.Types
    ( encodeAddress )
import Cardano.Wallet.Primitive.AddressDerivation
    ( AccountingStyle (..)
    , Depth (..)
    , DerivationType (..)
    , DerivationType (..)
    , FromMnemonic (..)
    , HardDerivation (..)
    , Index
    , MkKeyFingerprint (..)
    , NetworkDiscriminant (..)
    , Passphrase (..)
    , PaymentAddress (..)
    , SoftDerivation (..)
    , WalletKey (..)
    , XPrv
    )
import Cardano.Wallet.Primitive.AddressDerivation.Icarus
    ( IcarusKey (..)
    , generateKeyFromHardwareLedger
    , generateKeyFromSeed
    , minSeedLengthBytes
    , unsafeGenerateKeyFromSeed
    )
import Cardano.Wallet.Primitive.AddressDerivationSpec
    ( genLegacyAddress )
import Cardano.Wallet.Primitive.Mnemonic
    ( ConsistentEntropy, EntropySize, mkMnemonic )
import Cardano.Wallet.Primitive.Types
    ( Address )
import Control.Monad
    ( forM_ )
import Data.Text
    ( Text )
import Test.Hspec
    ( Spec, describe, it, shouldBe )
import Test.QuickCheck
    ( Arbitrary (..)
    , Property
    , arbitraryBoundedEnum
    , choose
    , property
    , vectorOf
    , (===)
    )

import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.Text as T

spec :: Spec
spec = do
    describe "Golden Tests - Icarus' style addresses" $ do
        let seed0 = Passphrase "4\175\242L\184\243\191 \169]\171 \207\r\v\233\NUL~&\ETB"

        goldenAddressGeneration $ GoldenAddressGeneration
            seed0 (toEnum 0x80000000) UTxOExternal (toEnum 0x00000000)
            "Ae2tdPwUPEZGQVrA6qKreDzdtYxcWMMrpTFYCpFcuJfhJBEfoeiuW4MtaXZ"

        goldenAddressGeneration $ GoldenAddressGeneration
            seed0 (toEnum 0x80000000) UTxOExternal (toEnum 0x0000000E)
            "Ae2tdPwUPEZDLWQQEBR1UW7HeXJVaqUnuw8DUFu52TDWCJbxbkCyQYyxckP"

        goldenAddressGeneration $ GoldenAddressGeneration
            seed0 (toEnum 0x8000000E) UTxOInternal (toEnum 0x0000002A)
            "Ae2tdPwUPEZFRbyhz3cpfC2CumGzNkFBN2L42rcUc2yjQpEkxDbkPodpMAi"

        let (Right seed1) = fromMnemonic @'[12]
                [ "ghost", "buddy", "neutral", "broccoli", "face", "rack"
                , "relief", "odor", "swallow", "real", "once", "ecology"
                ]

        goldenAddressGeneration $ GoldenAddressGeneration
            seed1 (toEnum 0x80000000) UTxOExternal (toEnum 0x00000000)
            "Ae2tdPwUPEYz6ExfbWubiXPB6daUuhJxikMEb4eXRp5oKZBKZwrbJ2k7EZe"

        goldenAddressGeneration $ GoldenAddressGeneration
            seed1 (toEnum 0x80000000) UTxOExternal (toEnum 0x00000001)
            "Ae2tdPwUPEZCJUCuVgnysar8ZJeyKuhjXU35VNgKMMTcXWmS9zzYycmwKa4"

        goldenAddressGeneration $ GoldenAddressGeneration
            seed1 (toEnum 0x80000000) UTxOExternal (toEnum 0x00000002)
            "Ae2tdPwUPEZFJtMH1m5HvsaQZrmgLcVcyuk5TxYtdRHZFo8yV7yEnnJyqTs"

    describe "BIP-0044 Derivation Properties" $ do
        it "deriveAccountPrivateKey works for various indexes" $
            property prop_accountKeyDerivation
        it "N(CKDpriv((kpar, cpar), i)) === CKDpub(N(kpar, cpar), i)" $
            property prop_publicChildKeyDerivation

    describe "MkKeyFingerprint Properties" $ do
        it "paymentKeyFingerprint . liftPaymentAddress == pure" $
            property prop_roundtripFingerprintLift

    describe "Hardware Ledger" $ do
        goldenHardwareLedger @12 (Passphrase mempty)
            [ "struggle", "section", "scissors", "siren", "garbage", "yellow"
            , "maximum", "finger", "duty", "require", "mule", "earn"
            ]
            [ "Ae2tdPwUPEZ4Gs4s2recjNjQHBKfuBTkeuqbHJJrC6CuyjGyUD44cCTq4sJ"
            , "Ae2tdPwUPEZ8ozZuJWsLVb7aEb5p9ntcja47B9i68GV3y9by1eY5C2y6WUT"
            , "Ae2tdPwUPEZJoUCoyoCxUAKAbn2vFo6nu6B7aTWL1Pv9MRKm8unG9ixLurg"
            , "Ae2tdPwUPEYwFNKLxqF8s31nbaNt5MZisVqsQ5qsiY763HY5wsBN3mSzPRa"
            , "Ae2tdPwUPEZ4ZXzzehKoWWC9QYVqJfEL9x63zjH6wyEJbNRsZ9eccR6nSpv"
            , "Ae2tdPwUPEYyX7ug8zm6K7nLWhgEEBo7Ewf1qALxkvqyHHSC5jMFzH418Q1"
            , "Ae2tdPwUPEZ95eCwDjNQjReRkeLZFv6kBs3vwaKPHJsw2cxXc3HaCD2jzqw"
            , "Ae2tdPwUPEZDHGbQ9sbLZuw3cfhcSzqqdK8Xj3dhAzmWZGeVgJhncu5LR9N"
            , "Ae2tdPwUPEYyDca1eVbeEea6CjihoMAgt6mPiNuC1hEpy5U2qQ1Tzt6E8q8"
            , "Ae2tdPwUPEZHRMjjXMT2icJXp5h2k2j3Ph6dB5iGRashA2QxHLgFZbHzdms"
            ]

        goldenHardwareLedger @18 (Passphrase mempty)
            [ "vague" , "wrist" , "poet" , "crazy" , "danger" , "dinner"
            , "grace" , "home" , "naive" , "unfold" , "april" , "exile"
            , "relief" , "rifle" , "ranch" , "tone" , "betray" , "wrong"
            ]
            [ "Ae2tdPwUPEZMCGyPAK85FrcserPvzVZZUcbFk5TvDmL9LrUyq2KPYubPcru"
            , "Ae2tdPwUPEZ6drrnNd1KW3UoiU3U1ZK3mxSpQpFAdXzJHuwvDcYB7Wzxkp1"
            , "Ae2tdPwUPEZ7Jaw9qt1q2CjCcds6zpHMyzmPGDh9tBeyQG28AdRGHcaWYx7"
            , "Ae2tdPwUPEZ9SW4qxWkFoozTux5i7F9jVpHQFQUycQuNanSUScyMTYrnQXK"
            , "Ae2tdPwUPEZ6YegpN8XurGfWyKqkNHLgdbHpdohumKt5QpkNVJhw4FCSRdo"
            , "Ae2tdPwUPEZLgrXt3zJeHgFWM2stxRjdm6wWATSoUzJ1CmUxKqgbYQXR8cC"
            , "Ae2tdPwUPEZ6axGCfo5nCLn5hEoRo4yNmQKBzn12B2quPncgQRFP6JBZ2ex"
            , "Ae2tdPwUPEYzdHGmJDL9tEWXfzyshohvzyS3K9wmLc5qMrwRNFPQA611uzB"
            , "Ae2tdPwUPEYxLNQJXcT3XUh54BXn5w53pPe5EHMXo6qo47gpNM9QyJsaXz4"
            , "Ae2tdPwUPEYvq2fnzqs9EWxFF2j87nZzBAZZ7y3qoj5oTce1ZGvsc4potp3"
            ]

        goldenHardwareLedger @24 (Passphrase mempty)
            [ "recall" , "grace" , "sport" , "punch" , "exhibit" , "mad"
            , "harbor" , "stand" , "obey" , "short" , "width" , "stem"
            , "awkward" , "used" , "stairs" , "wool" , "ugly" , "trap"
            , "season" , "stove" , "worth" , "toward" , "congress" , "jaguar"
            ]
            [ "Ae2tdPwUPEZFvG914wGXtCsb9hCr9aKjJC2ZciLKSNRqAKtjnduH7XtPn78"
            , "Ae2tdPwUPEZ8rVsdBE6EMZpac32MLzciY75MrwrPs8ikjf6MWYFJUHkGaw5"
            , "Ae2tdPwUPEZADQdQy2cbHDwwFRYUcrfreiu82Ngm9Bxdw1pJqJFUnFoQmNL"
            , "Ae2tdPwUPEZ3NULtb3fK6qtJYwJbVnmhDeWzoMbjzPbCsEC9MyB4foBABhz"
            , "Ae2tdPwUPEZ3rGvPCdzCPrVRvzEfpUp8XnZ861nss3XfLun5wA3c3YMA41v"
            , "Ae2tdPwUPEZ575pMY9TBJyPdrwGkq2kr49V9fuqRWpF6wM9JbuZLmxHDo2N"
            , "Ae2tdPwUPEZFaVKwy9bcN81ZPVL8uHRfsrCj7ZZhbm2uqiwLrzsy9Bs1rBN"
            , "Ae2tdPwUPEZ4K16qFm6qVRWTEGpq5TJiyt8ZojmRANTSpPDAWZuH2Ge85uB"
            , "Ae2tdPwUPEZMMYd8JP9F16HJgCsDsPjUoERWoFzZugN4mNjhR9ZnFwPonCs"
            , "Ae2tdPwUPEZ3anXo172NFuumSGjrvbk1pHK9LiF82nGmPKC52NMYR77V2dM"
            ]

        goldenHardwareLedger @24 (Passphrase "very secure passphrase")
            [ "burden", "destroy", "client", "air", "agent", "episode"
            , "horror", "orient", "scrap", "car", "point", "easy"
            , "local", "primary", "grunt", "seminar", "goose", "spin"
            , "charge", "olive", "angry", "hour", "start", "shop"
            ]
            [ "Ae2tdPwUPEZHiTeWAxzLFm5qYAGqLLwZ35huQJ7Dg5fJ4SN97d1QwhsuDrG"
            , "Ae2tdPwUPEZBD4rL6Msf2DdthRYLuYFxeG1hawjqtKYzMw2USoFQ9VmuU9C"
            , "Ae2tdPwUPEZH5dhpwZHFVsemEpvgMMpSboyrM1PEThh6MwQTCXG2FoCCPHR"
            , "Ae2tdPwUPEYz1AhmV59DNL92P8rrU8Wa9x9ttPTUoBSCvnEoJacmZbTahRu"
            , "Ae2tdPwUPEZKqr3xfBLtQuhwNqtnFLq7ttptUxwyatoFF1ofgSaUTFmqiBb"
            , "Ae2tdPwUPEZL1exTaCW3yfMj8eosgg7zcG35qfTQFyetgcJ3969agkrXXU5"
            , "Ae2tdPwUPEZL2X1g23MKScFTvaLACCgpdWxozSjb5aXmg3YCESeiSftHyGJ"
            , "Ae2tdPwUPEZKHqZcXY3AqLhWHzKVieBhedr7ixRmMsxsVKA1aVTEHPVK5aG"
            , "Ae2tdPwUPEZLU4TEkPMmkT2dfQ23YyKLFWXBwuxLi4rxF4kT6najwAi6APQ"
            , "Ae2tdPwUPEZBFKNnz2F1Bn5pLkhp2rm9byDAyW1JzN7ZUYSgRPqrH3Jgs88"
            ]

{-------------------------------------------------------------------------------
                               Golden Tests
-------------------------------------------------------------------------------}

data GoldenAddressGeneration = GoldenAddressGeneration
    { goldSeed :: Passphrase "seed"
    , goldAcctIx :: Index 'Hardened 'AccountK
    , goldAcctStyle :: AccountingStyle
    , goldAddrIx :: Index 'Soft 'AddressK
    , goldAddr :: String
    }

-- | Compare addresses obtained from a given derivation path and a root seed to
-- their known equivalent in base58.
goldenAddressGeneration
    :: GoldenAddressGeneration
    -> Spec
goldenAddressGeneration test = it title $ do
    let encPwd = mempty
    let rootXPrv = generateKeyFromSeed goldSeed encPwd
    let acctXPrv = deriveAccountPrivateKey encPwd rootXPrv goldAcctIx
    let addrXPrv = deriveAddressPrivateKey encPwd acctXPrv goldAcctStyle goldAddrIx
    base58 (paymentAddress @'Mainnet $ publicKey addrXPrv) `shouldBe` goldAddr
  where
    GoldenAddressGeneration
        { goldSeed
        , goldAddr
        , goldAcctIx
        , goldAddrIx
        , goldAcctStyle
        } = test

    title = unwords
        [ fmtPath goldAcctIx goldAcctStyle goldAddrIx
        , "-->"
        , goldAddr
        ]

    base58 = T.unpack . encodeAddress @'Mainnet

    -- e.g. m/.../0'/0/0
    fmtPath p3 p4 p5 = mconcat
        [ "m/.../"
        , show (fromEnum p3 - fromEnum (minBound @(Index 'Hardened _)))
        , "'/"
        , show (fromEnum p4)
        , "/"
        , show (fromEnum p5)
        ]

goldenHardwareLedger
    :: forall mw ent csz.
        ( ConsistentEntropy ent mw csz
        , EntropySize mw ~ ent
        )
    => Passphrase "encryption"
        -- ^ An encryption passphrase
    -> [Text]
        -- ^ 24-word mnemonic
    -> [Text]
        -- ^ Some addresses, starting at index 0
    -> Spec
goldenHardwareLedger encPwd sentence addrs =
    it title $ do
        let Right mnemonic = mkMnemonic @mw sentence
        let rootXPrv = generateKeyFromHardwareLedger mnemonic encPwd
        let acctXPrv = deriveAccountPrivateKey encPwd rootXPrv minBound
        let deriveAddr = deriveAddressPrivateKey encPwd acctXPrv UTxOExternal

        forM_ (zip [0..] addrs) $ \(ix, addr) -> do
            let addrXPrv = deriveAddr (toEnum ix)
            base58 (paymentAddress @'Mainnet $ publicKey addrXPrv) `shouldBe` addr
  where
    title = T.unpack
        $ T.unwords
        $ take 3 sentence ++ [ "..." ] ++ drop (length sentence - 3) sentence
    base58 = encodeAddress @'Mainnet

{-------------------------------------------------------------------------------
                                 Properties
-------------------------------------------------------------------------------}

-- | Deriving address public key should be equal to deriving address
-- private key and extracting public key from it (works only for non-hardened
-- child keys).
--
-- To compute the public child key of a parent private key:
--  * N(CKDpriv((kpar, cpar), i)) (works always).
--  * CKDpub(N(kpar, cpar), i) (works only for non-hardened child keys).
--
-- Thus:
--
-- N(CKDpriv((kpar, cpar), i)) === CKDpub(N(kpar, cpar), i)
--
-- if (kpar, cpar) is a non-hardened key.
--
-- For details see <https://github.com/bitcoin/bips/blob/master/bip-0032.mediawiki#private-parent-key--public-child-key bip-0039>
prop_publicChildKeyDerivation
    :: Passphrase "seed"
    -> Passphrase "encryption"
    -> AccountingStyle
    -> Index 'Soft 'AddressK
    -> Property
prop_publicChildKeyDerivation seed encPwd cc ix =
    addrXPub1 === addrXPub2
  where
    accXPrv = unsafeGenerateKeyFromSeed seed encPwd :: IcarusKey 'AccountK XPrv
    -- N(CKDpriv((kpar, cpar), i))
    addrXPub1 = publicKey $ deriveAddressPrivateKey encPwd accXPrv cc ix
    -- CKDpub(N(kpar, cpar), i)
    addrXPub2 = deriveAddressPublicKey (publicKey accXPrv) cc ix

prop_accountKeyDerivation
    :: Passphrase "seed"
    -> Passphrase "encryption"
    -> Index 'Hardened 'AccountK
    -> Property
prop_accountKeyDerivation seed encPwd ix =
    accXPrv `seq` property () -- NOTE Making sure this doesn't throw
  where
    rootXPrv = generateKeyFromSeed seed encPwd :: IcarusKey 'RootK XPrv
    accXPrv = deriveAccountPrivateKey encPwd rootXPrv ix

prop_roundtripFingerprintLift
    :: Address
    -> Property
prop_roundtripFingerprintLift addr =
    let
        fingerprint = paymentKeyFingerprint @IcarusKey addr
        eAddr = liftPaymentAddress @'Mainnet <$> fingerprint
    in
        eAddr === Right addr

{-------------------------------------------------------------------------------
                             Arbitrary Instances
-------------------------------------------------------------------------------}

instance Arbitrary AccountingStyle where
    shrink _ = []
    arbitrary = arbitraryBoundedEnum

instance {-# OVERLAPS #-} Arbitrary (Passphrase "seed") where
    arbitrary = do
        n <- choose (minSeedLengthBytes, 64)
        bytes <- BS.pack <$> vectorOf n arbitrary
        return $ Passphrase $ BA.convert bytes

instance Arbitrary Address where
    arbitrary = genLegacyAddress (30, 50)
