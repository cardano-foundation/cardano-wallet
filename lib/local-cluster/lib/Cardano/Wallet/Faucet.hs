{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Cardano.Wallet.Faucet
    ( Faucet (..)
    , FaucetM
    , initFaucet

      -- * Sea horses
    , seaHorseAssetName
    , seaHorsePolicyId

      -- * Integration test funds
    , shelleyFunds
    , byronFunds
    , icarusFunds
    , onlyDustWallet
    , bigDustWallet
    , preregKeyWallet
    , mirFunds
    , mirMnemonics
    , byronIntegrationTestFunds
    , maryAllegraFunds
    , hwLedgerFunds
    , seaHorseTestAssets
    , runFaucetM

    ) where

import Prelude hiding
    ( appendFile
    )

import qualified Cardano.Address as CA
import qualified Cardano.Address.Style.Icarus as Icarus
import qualified Cardano.Faucet.Addresses as Addresses
import qualified Cardano.Faucet.Mnemonics as Mnemonics
import qualified Cardano.Faucet.Types as Faucet
import qualified Cardano.Wallet.Primitive.Types.AssetName as AssetName
import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle
import qualified Data.ByteString.Char8 as B8
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T

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
import Cardano.Faucet.Http.Client
    ( fetchMnemonicAddresses
    , fetchMnemonicByIndex
    , fetchMnemonicRange
    )
import Cardano.Faucet.Mnemonics
    ( MnemonicLength (..)
    )
import Cardano.Faucet.Types
    ( AddressStyle (..)
    , MnemonicIndex
    , unFaucetAddress
    , unIndexedAddress
    , unIndexedMnemonic
    )
import Cardano.Mnemonic
    ( Mnemonic
    , SomeMnemonic (..)
    )
import Cardano.Wallet.Primitive.Types.AssetName
    ( AssetName (..)
    )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..)
    )
import Cardano.Wallet.Primitive.Types.TokenBundle
    ( TokenBundle
    )
import Cardano.Wallet.Primitive.Types.TokenPolicyId
    ( TokenPolicyId
    )
import Cardano.Wallet.Primitive.Types.TokenQuantity
    ( TokenQuantity (..)
    )
import Cardano.Wallet.Unsafe
    ( unsafeFromText
    , unsafeMkMnemonic
    )
import Control.Concurrent
    ( newMVar
    )
import Control.Exception
    ( throwIO
    )
import Control.Monad.Extra
    ( concatMapM
    )
import Control.Monad.IO.Class
    ( liftIO
    )
import Data.Bifunctor
    ( first
    )
import Data.Functor
    ( (<&>)
    )
import Data.List
    ( unfoldr
    )
import Data.Tuple.Extra
    ( dupe
    )
import Numeric.Natural
    ( Natural
    )
import Servant.Client
    ( ClientEnv
    , ClientM
    , runClientM
    )
import UnliftIO.MVar
    ( MVar
    , modifyMVar
    )

--------------------------------------------------------------------------------
data MnemonicRange = MnemonicRange
    { from :: MnemonicIndex
    , to :: MnemonicIndex
    }
    deriving stock (Show, Eq, Ord)

enumRange :: MnemonicRange -> [MnemonicIndex]
enumRange = enumFromTo <$> from <*> to

nextRange :: MnemonicIndex -> MnemonicRange -> MnemonicRange
nextRange step MnemonicRange{..} =
    MnemonicRange (to + 1) (to + step)

consumeRange :: MnemonicRange -> Maybe (MnemonicIndex, MnemonicRange)
consumeRange (MnemonicRange from to) =
    let
        next = succ from
    in
        if next <= to
            then Just (from, MnemonicRange next to)
            else Nothing

data Faucet = Faucet
    { nextShelleyMnemonic :: IO SomeMnemonic
    , nextIcarusMnemonic :: IO SomeMnemonic
    , nextByronMnemonic :: IO SomeMnemonic
    , nextRewardMnemonic :: IO SomeMnemonic
    , nextMaryAllegraMnemonic :: IO SomeMnemonic
    , bigDustWalletMnemonic :: IO SomeMnemonic
    , onlyDustWalletMnemonic :: IO SomeMnemonic
    , preregKeyWalletMnemonic :: IO SomeMnemonic
    }

newtype FaucetM a = FaucetM
   { unFaucetM :: ClientM a
   }
    deriving newtype (Functor, Applicative, Monad)

runFaucetM :: ClientEnv -> FaucetM a -> IO a
runFaucetM env action =
    runClientM (unFaucetM action) env
        >>= either throwIO pure

executeClientM :: ClientEnv -> ClientM a -> IO a
executeClientM env action = runClientM action env >>= either throwIO pure

initFaucet :: ClientEnv -> IO Faucet
initFaucet clientEnv = do
    shelley <- newMVar shelleyMnemonicRange
    icarus <- newMVar icarusMnemonicRange
    byron <- newMVar byronMnemonicRange
    reward <- newMVar mirMnemonicRange
    maryAllegra <- newMVar maryAllegraMnemonicRange

    let nextMnemonic
            :: MVar MnemonicRange
            -> MnemonicLength
            -> IO SomeMnemonic
        nextMnemonic var len = liftIO $ modifyMVar var $ \range -> do
            case consumeRange range of
                Just (index, range') -> do
                    Faucet.Mnemonic mnemonic <-
                        executeClientM clientEnv
                            $ fetchMnemonicByIndex len index
                    pure (range', mnemonic)
                Nothing -> throwIO
                    $ userError "Faucet: no more mnemonics available"

    let fixedMnemonic :: MnemonicRange -> MnemonicLength -> IO SomeMnemonic
        fixedMnemonic index len = do
            Faucet.Mnemonic mnemonic <-
                executeClientM clientEnv $ fetchMnemonicByIndex len $ from index
            pure mnemonic

    pure
        Faucet
            { nextShelleyMnemonic = nextMnemonic shelley M15
            , nextIcarusMnemonic = nextMnemonic icarus M15
            , nextByronMnemonic = nextMnemonic byron M12
            , nextRewardMnemonic = nextMnemonic reward M24
            , nextMaryAllegraMnemonic = nextMnemonic maryAllegra M24
            , bigDustWalletMnemonic = fixedMnemonic bigDustWalletRange M15
            , onlyDustWalletMnemonic = fixedMnemonic onlyDustWalletRange M15
            , preregKeyWalletMnemonic = fixedMnemonic preregKeyWalletRange M15
            }

anyFunds
    :: MnemonicRange
    -- ^ Mnemonic range
    -> MnemonicLength
    -- ^ Mnemonic length
    -> AddressStyle
    -- ^ Address style
    -> Faucet.AddressIndex
    -- ^ Address index start
    -> Faucet.AddressIndex
    -- ^ Address index end
    -- ^ Network tag
    -> CA.NetworkTag
    -> FaucetM [Address]
    -- ^ Resulting list of addresses and coins
anyFunds mnemonicRange mlength style start end networkTag= FaucetM $ do
    let run' index = do
            ias <-
                fetchMnemonicAddresses
                    mlength
                    index
                    style
                    networkTag
                    start
                    end
            pure $ ias <&> \ia ->
                (unFaucetAddress . unIndexedAddress $ ia)
    concatMapM run' $ enumRange mnemonicRange

shelleyMnemonicRange :: MnemonicRange
shelleyMnemonicRange = MnemonicRange 0 399

-- | Takes 10 addresses for each of the first hundred mnemonics
shelleyFunds :: CA.NetworkTag -> FaucetM [(Address, Coin)]
shelleyFunds tag = do
    as <- anyFunds shelleyMnemonicRange M15 AddressStyleShelley 0 9 tag
    pure $ as <&> (,defaultAmount)

byronMnemonicRange :: MnemonicRange
byronMnemonicRange = MnemonicRange 0 199

-- | Takes 10 addresses for each mnemonic within the byron mnemonic range
byronFunds :: CA.NetworkTag -> FaucetM [(Address, Coin)]
byronFunds tag = do
    as <- anyFunds byronMnemonicRange M12 AddressStyleByron 0 9 tag
    pure $ as <&> (,defaultAmount)

icarusMnemonicRange :: MnemonicRange
icarusMnemonicRange = nextRange 200 shelleyMnemonicRange

-- | Takes 10 addresses for each mnemonic within the icarus mnemonic range
icarusFunds :: CA.NetworkTag -> FaucetM [(Address, Coin)]
icarusFunds tag = do
    as <- anyFunds icarusMnemonicRange M15 AddressStyleIcarus 0 9 tag
    pure $ as <&> (,defaultAmount)

onlyDustWalletRange :: MnemonicRange
onlyDustWalletRange = nextRange 1 icarusMnemonicRange

onlyDustWallet :: CA.NetworkTag -> FaucetM [(Address, Coin)]
onlyDustWallet tag = do
    as <- anyFunds onlyDustWalletRange M15 AddressStyleShelley 1 10 tag
    pure $ zip as dustAmounts
  where
    dustAmounts :: [Coin] = map adaToCoin [1, 1, 5, 12, 1, 5, 3, 10, 2, 3]

bigDustWalletRange :: MnemonicRange
bigDustWalletRange = nextRange 1 onlyDustWalletRange

bigDustWallet :: CA.NetworkTag -> FaucetM [(Address, Coin)]
bigDustWallet tag = do
    as <- anyFunds bigDustWalletRange M15 AddressStyleShelley 0 199 tag
    pure
        $ zip as
        $ replicate 100 defaultAmount ++ replicate 100 (adaToCoin 1)

preregKeyWalletRange :: MnemonicRange
preregKeyWalletRange = nextRange 1 bigDustWalletRange

preregKeyWallet :: CA.NetworkTag -> FaucetM [(Address, Coin)]
preregKeyWallet tag = do
    as <- anyFunds preregKeyWalletRange M15 AddressStyleShelley 0 99 tag
    pure $ as <&> (,defaultAmount)

mirMnemonicRange :: MnemonicRange
mirMnemonicRange = MnemonicRange 0 199

mirMnemonics :: FaucetM [SomeMnemonic]
mirMnemonics = FaucetM $
    fmap (Faucet.toSomeMnemonic . unIndexedMnemonic)
        <$> fetchMnemonicRange M24 (from mirMnemonicRange) (to mirMnemonicRange)

mirFunds :: CA.NetworkTag -> FaucetM [(Address, Coin)]
mirFunds tag = do
    as <- anyFunds mirMnemonicRange M24 AddressStyleShelley 0 0 tag
    pure $ as <&> (,defaultAmount)

adaToCoin :: Natural -> Coin
adaToCoin = Coin . (* 1_000_000)

defaultAmount :: Coin
defaultAmount = adaToCoin 100_000

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

maryAllegraMnemonicRange :: MnemonicRange
maryAllegraMnemonicRange = nextRange 200 mirMnemonicRange

-- | A list of addresses, and assets to be provisioned there.
--
-- Beside the assets, there is a list of @(signing key, verification key hash)@,
-- so that they can be minted by the faucet.

maryAllegraFunds :: Coin -> CA.NetworkTag -> FaucetM [(Address, (TokenBundle, [(String, String)]))]
maryAllegraFunds tips tag  = do
    as <- anyFunds maryAllegraMnemonicRange M24 AddressStyleShelley 0 2 tag
    pure $ zip as $ cycle maryTokenBundles
  where
    maryTokenBundles :: [(TokenBundle, [(String, String)])]
    maryTokenBundles = zipWith mint [simple, fruit, combined] maryAssetScripts

    mint :: (t -> a) -> (t, b) -> (a, [b])
    mint mk (pid, info) = (mk pid, [info])

    bundle p assets = TokenBundle.fromNestedList tips [(p, NE.fromList assets)]

    simple p = bundle p [(AssetName.empty, TokenQuantity 1_000_000_000)]
    fruit p =
        bundle
            p
            [ (UnsafeAssetName "apple", TokenQuantity 65_000_000)
            , (UnsafeAssetName "banana", TokenQuantity 66_000_000)
            , (UnsafeAssetName "cherry", TokenQuantity 67_000_000)
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
        (seaHorseAssetName i, TokenQuantity 1)
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

seaHorseAssetName :: Int -> AssetName
seaHorseAssetName i =
    UnsafeAssetName
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
        [ Mnemonics.random
            >>= take 10 . map (,defaultAmount) . (Addresses.byron networkTag)
        , dustWallet1Funds
        , dustWallet2Funds
        ]
  where
    -- A special Byron Wallet coming from with with only dust
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

hwLedgerFunds :: CA.NetworkTag -> [(Address, Coin)]
hwLedgerFunds networkTag = do
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
            in  firstIx : unfoldr (fmap dupe . nextIndex) firstIx
