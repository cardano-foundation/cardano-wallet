{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Cardano.Wallet.Faucet
    ( Faucet (..)
    , NextWallet
    , nextWallet
    , nextTxBuilder

      -- * Sea horses
    , seaHorseTokenName
    , seaHorsePolicyId

      -- * Integration test funds
    , shelleyIntegrationTestFunds
    , byronIntegrationTestFunds
    , maryIntegrationTestFunds
    , hwLedgerTestFunds
    , seaHorseTestAssets

      -- * Internals
    ) where

import Prelude hiding
    ( appendFile
    )

import Cardano.Address
    ( Address
    )
import Cardano.Address.Derivation
    ( AddressIndexDerivationType
    , Depth (PaymentK)
    , Index
    , deriveAccountPrivateKey
    , deriveAddressPrivateKey
    , nextIndex
    , toXPub
    )
import Cardano.Address.Style.Icarus
    ( Icarus
    )
import Cardano.Mnemonic
    ( Mnemonic
    , SomeMnemonic (..)
    )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..)
    )
import Cardano.Wallet.Primitive.Types.TokenBundle
    ( TokenBundle
    )
import Cardano.Wallet.Primitive.Types.TokenPolicy
    ( TokenName (..)
    , TokenPolicyId
    , nullTokenName
    )
import Cardano.Wallet.Primitive.Types.TokenQuantity
    ( TokenQuantity (..)
    )
import Cardano.Wallet.Unsafe
    ( unsafeFromText
    , unsafeMkMnemonic
    )
import Control.Arrow
    ( (>>>)
    )
import Data.Bifunctor
    ( first
    )
import Data.ByteString
    ( ByteString
    )
import Data.Function
    ( (&)
    )
import Data.List
    ( unfoldr
    )
import Data.Tuple.Extra
    ( dupe
    )
import GHC.TypeLits
    ( Nat
    , Symbol
    )
import Numeric.Natural
    ( Natural
    )
import UnliftIO.MVar
    ( MVar
    , modifyMVar
    )

import qualified Cardano.Address as CA
import qualified Cardano.Address.Style.Icarus as Icarus
import qualified Cardano.Wallet.Faucet.Addresses as Addresses
import qualified Cardano.Wallet.Faucet.Mnemonics as Mnemonics
import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle
import qualified Data.ByteString.Char8 as B8
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T

-- | An opaque 'Faucet' type from which one can get a wallet with funds
data Faucet = Faucet
    { shelley :: MVar [Mnemonic 15]
    , icarus :: MVar [Mnemonic 15]
    , random :: MVar [Mnemonic 12]
    , reward :: MVar [Mnemonic 24]
    , ma :: MVar [Mnemonic 24]
    , txBuilder :: MVar [(Address, Coin) -> IO ByteString]
    }

-- | Get the next faucet wallet. Requires the 'initFaucet' to be called in order
-- to get a hand on a 'Faucet'.
class NextWallet (scheme :: Symbol) where
    type MnemonicSize scheme :: Nat
    nextWallet :: Faucet -> IO (Mnemonic (MnemonicSize scheme))

takeNext :: String -> MVar [a] -> IO a
takeNext description mvar = do
    result <- modifyMVar mvar $ \case
        [] -> pure ([], Nothing)
        (h : q) -> pure (q, Just h)
    case result of
        Nothing ->
            fail
                $ "No more faucet wallets of type "
                    <> description
                    <> "! "
                    <> "You may need to manually add entries to the relevant list in "
                    <> "lib/wallet/src/Test/Integration/Faucet.hs"
        Just a -> pure a

instance NextWallet "shelley" where
    type MnemonicSize "shelley" = 15
    nextWallet (Faucet mvar _ _ _ _ _) = takeNext "shelley" mvar

instance NextWallet "icarus" where
    type MnemonicSize "icarus" = 15
    nextWallet (Faucet _ mvar _ _ _ _) = takeNext "icarus" mvar

instance NextWallet "random" where
    type MnemonicSize "random" = 12
    nextWallet (Faucet _ _ mvar _ _ _) = takeNext "random" mvar

instance NextWallet "reward" where
    type MnemonicSize "reward" = 24
    nextWallet (Faucet _ _ _ mvar _ _) = takeNext "reward" mvar

instance NextWallet "ma" where
    type MnemonicSize "ma" = 24
    nextWallet = takeNext "ma" . ma

-- | Get a raw transaction builder. It constructs and sign a transaction via an
-- private key that is owned "externally". Returns a bytes string ready to be
-- sent to a node.
nextTxBuilder :: Faucet -> IO ((Address, Coin) -> IO ByteString)
nextTxBuilder (Faucet _ _ _ _ _ mvar) = takeNext "txBuilder" mvar

shelleyIntegrationTestFunds :: CA.NetworkTag -> [(Address, Coin)]
shelleyIntegrationTestFunds networkTag =
    -- NOTE: Generating e.g. 100 addresses for inclusion in the shelley genesis
    -- sgInitialFunds could theoretically cause some funds not to be
    -- discoverable by the wallet with its default paymentKey gap of 20, as the
    -- ordering in sgInitialFunds is not guaranteed.
    -- This seems to work out in practice, however.
    mconcat
        [ Mnemonics.sequential >>=
            ( Addresses.shelley
                >>> (`zip` defaultAmounts)
                >>> take 10
            )
        , Mnemonics.icarus >>=
            ( Addresses.icarus networkTag
                >>> (`zip` defaultAmounts)
                >>> take 10
            )
        , Mnemonics.onlyDustWallet &
            ( Addresses.shelley
                >>> (`zip` dustAmounts)
            )
        , Mnemonics.bigDustWallet &
            ( Addresses.shelley
                >>> (`zip` defaultAmounts)
                >>> take 100
            )
        , Mnemonics.bigDustWallet &
            ( Addresses.shelley
                >>> (`zip` (repeat (adaToCoin 1)))
                >>> drop 100
                >>> take 100
            )
        , Mnemonics.preregKeyWallet &
            ( Addresses.shelley
                >>> (`zip` defaultAmounts)
                >>> take 100
            )
        , Mnemonics.mir >>=
            ( Addresses.shelley
                >>> (`zip` defaultAmounts)
                >>> take 1
            )
        ]
  where
    defaultAmounts :: [Coin] = repeat (adaToCoin 100_000)
    dustAmounts :: [Coin] = map adaToCoin [1, 1, 5, 12, 1, 5, 3, 10, 2, 3]
    adaToCoin :: Natural -> Coin = Coin . (* 1_000_000)

-- | A list of pre-generated policy IDs, paired with
-- @(signing key, verification key hash)@ string tuples.
--
-- Use @Cluster.genMonetaryPolicyScript mempty "/tmp"@ to make these.
maryAssetScripts :: [(TokenPolicyId, (String, String))]
maryAssetScripts =
    map
        (first (unsafeFromText . T.pack))
        [
            ( "4bfe7acae1bd2599649962b146a1e47d2e14933809b367e804c61f86"
            ,
                ( "5820c5b0fff479beae303743c8ca2ac1b94a79309ac5a19bd968a5a7117447a71e3a"
                , "41ba83cad5cef09350b0bea49eca8cbfc0179d1e4b151b614fd1673b"
                )
            )
        ,
            ( "f4137b0691b01c7ca46c2fc05576f4f0ab8eebb8f8e4946cb9107e0f"
            ,
                ( "582014d4e21a4128e6df919179be768b27a872e48d6192fd1afe609e02c7203affb1"
                , "3e4b7054a74ea2168522ce5bf59aff8ff3bed46096d15cdb3fe3bbc1"
                )
            )
        ,
            ( "b3579e6306a5b3f49ba91ed4c5fd79dbe92d54867433ff6f92d47b40"
            ,
                ( "58209e1caa45500051163e03176099f53dd85aff98331d6fc2c857226d6c406fe2dc"
                , "31fe7edd49aaca7982a28cfb917f8af01b9c1088bff300b1bc784f03"
                )
            )
        ,
            ( "e9f14eb5a8c5c4b70d7e41ba16b833396191bee9fb3966ccd0d012f8"
            ,
                ( "5820e58c10bac5b4cbc984524a92576fad307fa8d53da4f408abd8ee8c1d3d0e9daf"
                , "84f25deb23ec4ebaa20998fdb9db5aa91d46938c1a5a5efa35766e30"
                )
            )
        ,
            ( "7c76a63436f2b94997b7602fc9d962c1272d95dcb4eadf72fbb34200"
            ,
                ( "582087a20b27a48feca4fc73f101fd067eb195f6bb0a1ea06d9d5ba8fb4e623d11ae"
                , "119748fed505b1a809a5fb9c991810bf07f34cabcc24b0a3d5f1d61f"
                )
            )
        ,
            ( "5b0b70ddaa8aca1af1c0e3d7a20fd269a359f070c1d42c2707fb15ba"
            ,
                ( "58209a9c4ad309c31eac53c70630981dd085bd4964940a29a07035d2bc9c1963b2e3"
                , "d4ca2ab165a2fb1bb75a0540febd5ddaf9e450d899185b7e4301464a"
                )
            )
        ,
            ( "a1a17b6cab3afaf2305aad6c30ce3596f193dd7276f8ace32a5ed50e"
            ,
                ( "5820a4809edc4db46c15d0e22d0d412ae4bcd0a6fc8be683a6582bf941e904481fce"
                , "3f80be7f1cf0c9698e32e792457f15a1ac4e5b06ca9f4bc05f38579c"
                )
            )
        ,
            ( "2715f36ea83fe74b87ad5a36d15820b1a8bd6d4d02c4c30a3a2950e0"
            ,
                ( "58203784e75acdec4c1c7e0552515be8364298d713645f847cd549e1106811be2d20"
                , "0549d39e9356db51fd2a4c72a5477a56a178a32fae1fe835cae23be1"
                )
            )
        ,
            ( "0f589d48a3ab60064cfeb60d3c0f7f02c0e2243af8e96f4c3d843be2"
            ,
                ( "5820045d5b2491c992768dcc1b8346d57eabf6237b69b6d5d00a5a797491b487387b"
                , "41d71703500df1cefd3fab37d39c27693a7b156f3fb5d9b25252d7c8"
                )
            )
        ]

-- | A list of addresses, and assets to be provisioned there.
--
-- Beside the assets, there is a list of @(signing key, verification key hash)@,
-- so that they can be minted by the faucet.
maryIntegrationTestFunds
    :: Coin
    -- ^ Amount of ada in each bundle
    -> [(Address, (TokenBundle, [(String, String)]))]
maryIntegrationTestFunds tips =
    Mnemonics.shelleyMA
        >>= Addresses.shelley
            & (`zip` cycle maryTokenBundles)
            & take 3
  where
    maryTokenBundles = zipWith mint [simple, fruit, combined] maryAssetScripts

    mint :: (t -> a) -> (t, b) -> (a, [b])
    mint mk (pid, info) = (mk pid, [info])

    bundle p assets = TokenBundle.fromNestedList tips [(p, NE.fromList assets)]

    simple p = bundle p [(nullTokenName, TokenQuantity 1_000_000_000)]
    fruit p =
        bundle
            p
            [ (UnsafeTokenName "apple", TokenQuantity 65_000_000)
            , (UnsafeTokenName "banana", TokenQuantity 66_000_000)
            , (UnsafeTokenName "cherry", TokenQuantity 67_000_000)
            ]
    combined p = simple p `TokenBundle.add` fruit p

-- | Create @n@ unique SeaHorse tokens for each provided `Address`.
--
-- The result can be used for minting using the cli-based faucet.
seaHorseTestAssets
    :: Int
    -- ^ Number of sea horses per `Address`
    -> Coin
    -- ^ The Coin value for each `TokenBundle`
    -> [Address]
    -> [(Address, (TokenBundle, [(String, String)]))]
seaHorseTestAssets nPerAddr c addrs =
    zip addrs
        $ map
            (\is -> mint (seaHorse is) seaHorseAssetScript)
            (chunks nPerAddr [1 ..])
  where
    mint :: (t -> a) -> (t, b) -> (a, [b])
    mint mk (pid, info) = (mk pid, [info])
    seaHorse is p = bundle p $ flip map is $ \i ->
        (seaHorseTokenName i, TokenQuantity 1)
    bundle p assets = TokenBundle.fromNestedList c [(p, NE.fromList assets)]

seaHorsePolicyId :: TokenPolicyId
seaHorsePolicyId = fst seaHorseAssetScript

-- | A pre-generated policy ID, paired with
-- @(signing key, verification key hash)@ .
seaHorseAssetScript :: (TokenPolicyId, (String, String))
seaHorseAssetScript =
    first
        (unsafeFromText . T.pack)
        ( "4ff049585c4b3070563966370f5427d4a2f3588bce4146d57a93c7d3"
        ,
            ( "582082a0d2af81ca0528387c37823706507478cead44f0250661542cdc5619ecaead"
            , "452bbda4110154506faaddbbdf366e4db088e963a3f56e98832b3332"
            )
        )

seaHorseTokenName :: Int -> TokenName
seaHorseTokenName i =
    UnsafeTokenName
        $ B8.pack
        $ "00000000000000000SeaHorse" <> show i

-- https://stackoverflow.com/questions/12876384/grouping-a-list-into-lists-of-n-elements-in-haskell
chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n xs =
    let (ys, zs) = splitAt n xs
    in  ys : chunks n zs

byronIntegrationTestFunds :: CA.NetworkTag -> [(Address, Coin)]
byronIntegrationTestFunds networkTag =
    mconcat
        [ Mnemonics.random >>=
            take 10 . map (,defaultAmt) . (Addresses.byron networkTag)
        , dustWallet1Funds
        , dustWallet2Funds
        ]
  where
    defaultAmt = Coin 100_000_000_000

    -- A special Byron Wallet coming from
    -- with with only dust
    dustWallet1 :: Mnemonic 12
    dustWallet1 =
        unsafeMkMnemonic @12
            [ "suffer"
            , "decorate"
            , "head"
            , "opera"
            , "yellow"
            , "debate"
            , "visa"
            , "fire"
            , "salute"
            , "hybrid"
            , "stone"
            , "smart"
            ]

    dustWallet1Funds =
        zip
            (Addresses.byron networkTag dustWallet1)
            [Coin 1, Coin 2, Coin 3, Coin 4, Coin 5]

    dustWallet2 :: Mnemonic 12
    dustWallet2 =
        unsafeMkMnemonic @12
            [ "collect"
            , "fold"
            , "file"
            , "clown"
            , "injury"
            , "sun"
            , "brass"
            , "diet"
            , "exist"
            , "spike"
            , "behave"
            , "clip"
            ]

    dustWallet2Funds =
        zip
            (Addresses.byron networkTag dustWallet2)
            (replicate 100 (Coin 10_000_000_000) <> replicate 100 (Coin 1))

hwLedgerTestFunds :: CA.NetworkTag -> [(Address, Coin)]
hwLedgerTestFunds networkTag = do
    mnemonic <- Mnemonics.hardwareLedger
    address <- take 10 $ deriveLedgerAddresses mnemonic
    pure (address, Coin 100_000_000_000)
  where
    deriveLedgerAddresses :: SomeMnemonic -> [Address]
    deriveLedgerAddresses mnemonic =
        Icarus.paymentAddress (CA.RequiresNetworkTag, networkTag) . fmap toXPub . addrXPrv
            <$> paymentKeyIxs
      where
        rootXPrv = Icarus.unsafeGenerateKeyFromHardwareLedger mnemonic
        accXPrv = deriveAccountPrivateKey rootXPrv minBound
        addrXPrv = deriveAddressPrivateKey accXPrv Icarus.UTxOExternal
        paymentKeyIxs :: [Index (AddressIndexDerivationType Icarus) PaymentK] =
            let firstIx = minBound
            in firstIx : unfoldr (fmap dupe . nextIndex) firstIx
