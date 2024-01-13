{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.Launch.Cluster.Faucet
    ( takeFaucet
    , readFaucetAddresses
    , faucetAmt
    , depositAmt
    , preRegisteredStakeKey
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
    ( ClusterEra (..)
    , clusterEraToString
    )
import Cardano.Wallet.Launch.Cluster.Config
    ( Config (..)
    )
import Cardano.Wallet.Launch.Cluster.MonetaryPolicyScript
    ( writeMonetaryPolicyScriptFile
    )
import Cardano.Wallet.Launch.Cluster.Tx
    ( signTx
    , submitTx
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
import Crypto.Hash.Extra
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
    , retag
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
    , showBaseUrl
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
    => Tagged "cluster-configs" FilePath
    -> IO (Tagged "tx-in" String, Tagged "faucet-prv" FilePath)
takeFaucet setupDir = do
    i <- modifyMVar faucetIndex (\i -> pure (i + 1, i))
    let basename = untag setupDir </> "faucet-addrs" </> "faucet" <> show i
    base58Addr <- BS.readFile $ basename <> ".addr"
    let addr =
            fromMaybe (error $ "decodeBase58 failed for " ++ show base58Addr)
                . decodeBase58 bitcoinAlphabet
                . T.encodeUtf8
                . T.strip
                $ T.decodeUtf8 base58Addr

    let txin = B8.unpack (convertToBase Base16 (blake2b256 addr)) <> "#0"
    let signingKey = basename <> ".shelley.key"
    pure (Tagged @"tx-in" txin, Tagged @"faucet-prv" signingKey)

readFaucetAddresses
    :: HasCallStack
    => Tagged "cluster-configs" FilePath
    -> IO [Address]
readFaucetAddresses setupDir = do
    let faucetDataPath = untag setupDir </> "faucet-addrs"
    allFileNames <- listDirectory faucetDataPath
    let addrFileNames = filter (".addr" `isSuffixOf`) allFileNames
    forM addrFileNames $ readAddress . (faucetDataPath </>)
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

-- | A public stake key associated with a mnemonic that we pre-registered for
-- STAKE_POOLS_JOIN_05.
--
-- ["over", "decorate", "flock", "badge", "beauty"
-- , "stamp", "chest", "owner", "excess", "omit"
-- , "bid", "raccoon", "spin", "reduce", "rival"
-- ]
preRegisteredStakeKey
    :: Aeson.Value
preRegisteredStakeKey =
    Aeson.object
        [ "type" .= Aeson.String "StakeVerificationKeyShelley_ed25519"
        , "description" .= Aeson.String "Free form text"
        , "cborHex"
            .= Aeson.String
                "5820949fc9e6b7e1e12e933ac35de5a565c9264b0ac5b631b4f5a21548bc6d65616f"
        ]

-- | Deposit amount required for registering certificates.
depositAmt :: Integer
depositAmt = 1_000_000

-- | Initial amount in each of these special cluster faucet
faucetAmt :: Integer
faucetAmt = 1_000 * oneMillionAda
  where
    -- | Just one million Ada, in Lovelace.
    oneMillionAda = 1_000_000_000_000

batch :: HasCallStack => Int -> [a] -> ([a] -> IO b) -> IO ()
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
    :: Config
    -> CardanoNodeConn
    -> [(Address, Coin)]
    -> IO ()
sendFaucetFundsTo config conn targets =
    batch 80 targets $ sendFaucet config conn "ada" . map coinBundle
  where
    coinBundle :: (any, Coin) -> (any, (TokenBundle, [a]))
    coinBundle = fmap (\c -> (TokenBundle.fromCoin c, []))

-- | Create transactions to fund the given faucet addresses with Ada and assets.
--
-- Beside the 'TokenBundle' of Ada and assets, there is a list of
-- @(signing key, verification key hash)@ pairs needed to sign the
-- minting transaction.
sendFaucetAssetsTo
    :: Config
    -> CardanoNodeConn
    -> Int
    -- ^ batch size
    -> [(Address, (TokenBundle, [(String, String)]))]
    -- ^ (address, assets)
    -> IO ()
sendFaucetAssetsTo config conn batchSize targets =
    when (cfgLastHardFork config >= MaryHardFork)
        $ batch batchSize targets
        $ sendFaucet config conn "assets"

-- | Build, sign, and send a batch of faucet funding transactions using
-- @cardano-cli@. This function is used by 'sendFaucetFundsTo' and
-- 'sendFaucetAssetsTo'.
sendFaucet
    :: HasCallStack
    => Config
    -> CardanoNodeConn
    -> String
    -- ^ label for logging
    -> [(Address, (TokenBundle, [(String, String)]))]
    -> IO ()
sendFaucet config conn what targets = do
    let clusterDir = cfgClusterDir config
    (faucetInput, faucetPrv) <- takeFaucet (cfgClusterConfigs config)
    let file = untag clusterDir </> "faucet-tx.raw"

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
    let outputDir = retag @"cluster" @_ @"output" clusterDir

    scripts <-
        forM (nub $ concatMap (map snd . snd . snd) targets)
            $ writeMonetaryPolicyScriptFile outputDir

    cli (cfgTracer config)
        $ [ clusterEraToString (cfgLastHardFork config)
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
            ++ (concatMap (\f -> ["--minting-script-file", untag f]) scripts)

    policyKeys <-
        forM (nub $ concatMap (snd . snd) targets) $ \(skey, keyHash) ->
            writePolicySigningKey outputDir keyHash skey

    signTx
        config
        outputDir
        (Tagged @"tx-body" file)
        (retag @"faucet-prv" @_ @"signing-key" faucetPrv : map retag policyKeys)
        >>= submitTx
            config
            conn
            (Tagged @"name" $ what ++ " faucet tx")

writePolicySigningKey
    :: Tagged "output" FilePath
    -- ^ destination directory for key file
    -> String
    -- ^ Name of file, keyhash perhaps.
    -> String
    -- ^ The cbor-encoded key material, encoded in hex
    -> IO (Tagged "policy-signing-key" FilePath)
    -- ^ Returns the filename written
writePolicySigningKey outputDir keyHash cborHex = do
    let keyFile = untag outputDir </> keyHash <.> "skey"
    Aeson.encodeFile keyFile
        $ object
            [ "type" .= Aeson.String "PaymentSigningKeyShelley_ed25519"
            , "description" .= Aeson.String "Payment Signing Key"
            , "cborHex" .= cborHex
            ]
    pure $ Tagged keyFile

withFaucet :: (ClientEnv -> IO a) -> IO a
withFaucet useBaseUrl = Warp.withApplication Faucet.initApp $ \port -> do
    let baseUrl = BaseUrl Http "localhost" port ""
    putStrLn $ "Faucet started at " <> showBaseUrl baseUrl
    let tenSeconds = 10 * 1_000_000 -- 10s in microseconds
    manager <- Http.newManager Http.defaultManagerSettings {
            Http.managerResponseTimeout = Http.responseTimeoutMicro tenSeconds
        }
    useBaseUrl $ mkClientEnv manager baseUrl
