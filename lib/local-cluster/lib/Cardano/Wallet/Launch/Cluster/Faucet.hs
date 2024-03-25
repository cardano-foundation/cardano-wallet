{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.Launch.Cluster.Faucet
    ( takeFaucet
    , readFaucetAddresses
    , faucetAmt
    , depositAmt
    , preRegisteredStakeKeyPair
    , resetGlobals
    , sendFaucetFundsTo
    , sendFaucetAssetsTo
    , withFaucet
    )
where

import Prelude

import Cardano.Address
    ( Address
    )
import Cardano.Launcher.Node
    ( CardanoNodeConn
    )
import Cardano.Wallet.Launch.Cluster.CardanoCLI
    ( cli
    )
import Cardano.Wallet.Launch.Cluster.ClusterEra
    ( clusterEraToString
    )
import Cardano.Wallet.Launch.Cluster.ClusterM
    ( ClusterM
    )
import Cardano.Wallet.Launch.Cluster.Config
    ( Config (..)
    )
import Cardano.Wallet.Launch.Cluster.FileOf
    ( FileOf (..)
    , changeFileOf
    )
import Cardano.Wallet.Launch.Cluster.MonetaryPolicyScript
    ( writeMonetaryPolicyScriptFile
    )
import Cardano.Wallet.Launch.Cluster.Tx
    ( signAndSubmitTx
    )
import Cardano.Wallet.Primitive.Types.AssetId
    ( AssetId (..)
    )
import Cardano.Wallet.Primitive.Types.AssetName
    ( AssetName (..)
    )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..)
    )
import Cardano.Wallet.Primitive.Types.TokenBundle
    ( TokenBundle (TokenBundle)
    )
import Cardano.Wallet.Primitive.Types.TokenQuantity
    ( TokenQuantity (..)
    )
import Control.Concurrent
    ( MVar
    , modifyMVar
    , newMVar
    , swapMVar
    )
import Control.Monad
    ( forM_
    , void
    , when
    )
import Control.Monad.Reader
    ( MonadIO (..)
    , MonadReader (..)
    , asks
    )
import Cryptography.Hash.Blake
    ( blake2b256
    )
import Data.Aeson
    ( object
    , (.=)
    )
import Data.ByteArray.Encoding
    ( Base (Base16)
    , convertToBase
    )
import Data.ByteString.Base58
    ( bitcoinAlphabet
    , decodeBase58
    )
import Data.List
    ( intercalate
    , isSuffixOf
    , nub
    )
import Data.Maybe
    ( fromMaybe
    )
import Data.Tagged
    ( Tagged (Tagged)
    , untag
    )
import Data.Text.Class
    ( ToText (..)
    )
import Data.Traversable
    ( forM
    )
import GHC.Stack
    ( HasCallStack
    )
import Servant.Client
    ( BaseUrl (..)
    , ClientEnv
    , Scheme (..)
    , mkClientEnv
    )
import System.Directory
    ( listDirectory
    )
import System.FilePath
    ( (<.>)
    , (</>)
    )
import System.IO.Unsafe
    ( unsafePerformIO
    )

import qualified Cardano.Address as Address
import qualified Cardano.Address as CA
import qualified Cardano.Faucet.Http.Server as Faucet
import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle
import qualified Cardano.Wallet.Primitive.Types.TokenMap as TokenMap
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as TIO
import qualified Network.HTTP.Client as Http
import qualified Network.Wai.Handler.Warp as Warp

-- | Hard-wired faucets referenced in the genesis file. Purpose is simply to
-- fund some initial transaction for the cluster. Faucet have plenty of money to
-- pay for certificates and are intended for a one-time usage in a single
-- transaction.
takeFaucet
    :: HasCallStack
    => ClusterM (Tagged "tx-in" String, FileOf "faucet-prv")
takeFaucet = do
    Config{..} <- ask
    i <- liftIO $ modifyMVar faucetIndex (\i -> pure (i + 1, i))
    let basename =
            pathOf cfgClusterConfigs
                </> "faucet-addrs"
                </> "faucet"
                <> show i
    base58Addr <- liftIO $ BS.readFile $ basename <> ".addr"
    let addr =
            fromMaybe (error $ "decodeBase58 failed for " ++ show base58Addr)
                . decodeBase58 bitcoinAlphabet
                . T.encodeUtf8
                . T.strip
                $ T.decodeUtf8 base58Addr

    let txin = B8.unpack (convertToBase Base16 (blake2b256 addr)) <> "#0"
    let signingKey = basename <> ".shelley.key"
    pure (Tagged @"tx-in" txin, FileOf @"faucet-prv" signingKey)

readFaucetAddresses
    :: HasCallStack
    => ClusterM [Address]
readFaucetAddresses = do
    Config{..} <- ask
    let faucetDataPath = pathOf cfgClusterConfigs </> "faucet-addrs"
    allFileNames <- liftIO $ listDirectory faucetDataPath
    let addrFileNames = filter (".addr" `isSuffixOf`) allFileNames
    liftIO $ forM addrFileNames $ readAddress . (faucetDataPath </>)
  where
    readAddress :: HasCallStack => FilePath -> IO Address
    readAddress addrFile = do
        rawFileContents <- TIO.readFile addrFile
        let base58EncodedAddress = T.strip rawFileContents
        case Address.fromBase58 base58EncodedAddress of
            Just address -> pure address
            Nothing ->
                error
                    $ "Failed to base58-decode address file: " <> addrFile

-- | List of faucets also referenced in the shelley 'genesis.yaml'
faucetIndex :: MVar Int
faucetIndex = unsafePerformIO $ newMVar 1
{-# NOINLINE faucetIndex #-}

-- | Allow running the test cluster a second time in the same process.
resetGlobals :: IO ()
resetGlobals = do
    void $ swapMVar faucetIndex 1

-- | A public stake key pair
-- associated with a mnemonic that we pre-registered for STAKE_POOLS_JOIN_05.
--
-- ["over", "decorate", "flock", "badge", "beauty"
-- , "stamp", "chest", "owner", "excess", "omit"
-- , "bid", "raccoon", "spin", "reduce", "rival"
-- ]
preRegisteredStakeKeyPair
    :: (Aeson.Value, Aeson.Value)
preRegisteredStakeKeyPair =
    ( Aeson.object
        [ "type" .= Aeson.String "StakeVerificationKeyShelley_ed25519"
        , "description" .= Aeson.String "Free form text"
        , "cborHex"
            .= Aeson.String
                "5820949fc9e6b7e1e12e933ac35de5a565c9264b0ac5b631b4f5a21548bc6d65616f"
        ]
    , Aeson.object
        [ "type" .= Aeson.String "StakeExtendedSigningKeyShelley_ed25519_bip32"
        , "description" .= Aeson.String "Free form text"
        , "cborHex"
            .= Aeson.String
                "5880784cda4d590b72c795792ec5d05b2a4216d153e36dad0b5376b6ea6308008d4e3cb0088852dce3b89577c7a4fb262ebf6a71f44f2c8ca45794ccfaa76bd95bcb949fc9e6b7e1e12e933ac35de5a565c9264b0ac5b631b4f5a21548bc6d65616f3042af27ce48e0fce0f88696b6ed3476f8c3412cce2f984931fb7658dee1872e"
        ]
    )

-- | Deposit amount required for registering certificates.
depositAmt :: Integer
depositAmt = 1_000_000

-- | Initial amount in each of these special cluster faucet
faucetAmt :: Integer
faucetAmt = 1_000 * oneMillionAda
  where
    -- | Just one million Ada, in Lovelace.
    oneMillionAda = 1_000_000_000_000

batch :: (HasCallStack, Monad m) => Int -> [a] -> ([a] -> m b) -> m ()
batch s xs = forM_ (group s xs)
  where
    -- TODO: Use split package?
    -- https://stackoverflow.com/questions/12876384/grouping-a-list-into-lists-of-n-elements-in-haskell
    group :: Int -> [a] -> [[a]]
    group _ [] = []
    group n l
        | n > 0 = (take n l) : (group n (drop n l))
        | otherwise = error "Negative or zero n"

sendFaucetFundsTo
    :: CardanoNodeConn
    -> [(Address, Coin)]
    -> ClusterM ()
sendFaucetFundsTo conn targets =
    batch 80 targets $ sendFaucet conn "ada" . map coinBundle
  where
    coinBundle :: (any, Coin) -> (any, (TokenBundle, [a]))
    coinBundle = fmap (\c -> (TokenBundle.fromCoin c, []))

-- | Create transactions to fund the given faucet addresses with Ada and assets.
--
-- Beside the 'TokenBundle' of Ada and assets, there is a list of
-- @(signing key, verification key hash)@ pairs needed to sign the
-- minting transaction.
sendFaucetAssetsTo
    :: CardanoNodeConn
    -> Int
    -- ^ batch size
    -> [(Address, (TokenBundle, [(String, String)]))]
    -- ^ (address, assets)
    -> ClusterM ()
sendFaucetAssetsTo conn batchSize targets = batch batchSize targets
        $ sendFaucet conn "assets"

-- | Build, sign, and send a batch of faucet funding transactions using
-- @cardano-cli@. This function is used by 'sendFaucetFundsTo' and
-- 'sendFaucetAssetsTo'.
sendFaucet
    :: HasCallStack
    => CardanoNodeConn
    -> String
    -- ^ label for logging
    -> [(Address, (TokenBundle, [(String, String)]))]
    -> ClusterM ()
sendFaucet conn what targets = do
    Config{..} <- ask
    let clusterDir = cfgClusterDir
    (faucetInput, faucetPrv) <- takeFaucet
    let file = pathOf clusterDir </> "faucet-tx.raw"

    let mkOutput addr (TokenBundle (Coin c) tokens) =
            [ "--tx-out"
            , unwords
                $ [T.unpack (CA.bech32 addr), show c, "lovelace"]
                    ++ map (("+ " ++) . cliAsset) (TokenMap.toFlatList tokens)
            ]
        cliAsset (aid, (TokenQuantity q)) = unwords [show q, cliAssetId aid]
        cliAssetId (AssetId pid (UnsafeAssetName name)) =
            mconcat
                [ T.unpack (toText pid)
                , if B8.null name then "" else "."
                , B8.unpack (convertToBase Base16 name)
                ]
        mkMint [] = []
        mkMint assets = ["--mint", intercalate " + " (map cliAsset assets)]

    let total =
            fromIntegral
                $ sum
                $ map (unCoin . TokenBundle.getCoin . fst . snd) targets
    when (total > faucetAmt) $ error "sendFaucetFundsTo: too much to pay"

    let targetAssets = concatMap (snd . TokenBundle.toFlatList . fst . snd) targets

    scripts <- forM
        (nub $ concatMap (map snd . snd . snd) targets)
        writeMonetaryPolicyScriptFile

    cli
        $ [ clusterEraToString cfgLastHardFork
          , "transaction"
          , "build-raw"
          , "--tx-in"
          , untag faucetInput
          , "--ttl"
          , "6000000"
          , -- Big enough to allow minting in the actual integration tests,
            -- before the wallet API supports it.
            "--fee"
          , show (faucetAmt - total)
          , "--out-file"
          , file
          ]
            ++ concatMap (uncurry mkOutput . fmap fst) targets
            ++ mkMint targetAssets
            ++ (concatMap (\f -> ["--minting-script-file", pathOf f]) scripts)

    policyKeys <-
        forM (nub $ concatMap (snd . snd) targets)
            $ \(skey, keyHash) -> do
                f <- writePolicySigningKey keyHash skey
                pure $ FileOf @"signing-key" $ pathOf f

    signAndSubmitTx
        conn
        (FileOf @"tx-body" file)
        (changeFileOf faucetPrv : policyKeys)
        (Tagged @"name" $ what ++ " faucet tx")

writePolicySigningKey
    :: String
    -- ^ Name of file, keyhash perhaps.
    -> String
    -- ^ The cbor-encoded key material, encoded in hex
    -> ClusterM (FileOf "policy-signing-key")
    -- ^ Returns the filename written
writePolicySigningKey keyHash cborHex = do
    outputDir <- asks cfgClusterDir
    let keyFile = pathOf outputDir </> keyHash <.> "skey"
    liftIO $ Aeson.encodeFile keyFile
        $ object
            [ "type" .= Aeson.String "PaymentSigningKeyShelley_ed25519"
            , "description" .= Aeson.String "Payment Signing Key"
            , "cborHex" .= cborHex
            ]
    pure $ FileOf keyFile

withFaucet :: (ClientEnv -> IO a) -> IO a
withFaucet useBaseUrl = Warp.withApplication Faucet.initApp $ \port -> do
    let baseUrl = BaseUrl Http "localhost" port ""
    let tenSeconds = 10 * 1_000_000 -- 10s in microseconds
    manager <-
        Http.newManager
            Http.defaultManagerSettings
                { Http.managerResponseTimeout = Http.responseTimeoutMicro tenSeconds
                }
    useBaseUrl $ mkClientEnv manager baseUrl
