{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.Launch.Cluster.ConfiguredPool
    ( ConfiguredPool (..)
    , configurePools
    )
where

import Prelude

import Cardano.Api
    ( AsType (..)
    , File (..)
    , HasTextEnvelope
    , Key (..)
    , SerialiseAsBech32
    , SerialiseAsCBOR (..)
    )
import Cardano.Binary
    ( FromCBOR (..)
    )
import Cardano.CLI.Types.Key
    ( VerificationKeyOrFile (..)
    , readVerificationKeyOrFile
    )
import Cardano.Launcher.Node
    ( CardanoNodeConfig (..)
    , NodePort (..)
    )
import Cardano.Ledger.Api
    ( StandardCrypto
    )
import Cardano.Ledger.BaseTypes
    ( Network (Testnet)
    , StrictMaybe (..)
    , textToUrl
    )
import Cardano.Ledger.Shelley.API
    ( ShelleyGenesis (..)
    , ShelleyGenesisStaking (sgsPools)
    )
import Cardano.Wallet.Launch.Cluster.CardanoCLI
    ( cli
    )
import Cardano.Wallet.Launch.Cluster.ClusterEra
    ( clusterEraToString
    )
import Cardano.Wallet.Launch.Cluster.ClusterM
    ( ClusterM
    , UnliftClusterM (..)
    , askNodeDir
    , askUnliftClusterM
    , traceClusterLog
    )
import Cardano.Wallet.Launch.Cluster.Config
    ( Config (..)
    , NodePathSegment (..)
    )
import Cardano.Wallet.Launch.Cluster.Faucet
    ( faucetAmt
    , takeFaucet
    )
import Cardano.Wallet.Launch.Cluster.FileOf
    ( FileOf (..)
    , changeFileOf
    )
import Cardano.Wallet.Launch.Cluster.Logging
    ( ClusterLog (..)
    , setLoggingName
    )
import Cardano.Wallet.Launch.Cluster.Node.GenNodeConfig
    ( genNodeConfig
    )
import Cardano.Wallet.Launch.Cluster.Node.GenTopology
    ( genTopology
    )
import Cardano.Wallet.Launch.Cluster.Node.NodeParams
    ( NodeParams (NodeParams)
    )
import Cardano.Wallet.Launch.Cluster.Node.Process
    ( withCardanoNodeProcess
    )
import Cardano.Wallet.Launch.Cluster.Node.RunningNode
    ( RunningNode (RunningNode)
    )
import Cardano.Wallet.Launch.Cluster.PoolMetadataServer
    ( PoolMetadataServer (registerMetadataForPoolIndex, urlFromPoolIndex)
    )
import Cardano.Wallet.Launch.Cluster.PoolRecipe
    ( PoolRecipe (PoolRecipe, operatorKeys)
    )
import Cardano.Wallet.Launch.Cluster.Tx
    ( signAndSubmitTx
    )
import Cardano.Wallet.Launch.Cluster.UnsafeInterval
    ( unsafeUnitInterval
    )
import Cardano.Wallet.Util
    ( HasCallStack
    )
import Control.Lens
    ( over
    )
import Control.Monad.IO.Class
    ( MonadIO (..)
    )
import Control.Monad.Reader
    ( MonadReader (..)
    )
import Control.Tracer
    ( traceWith
    )
import Crypto.Hash.Extra
    ( blake2b256
    )
import Data.Foldable
    ( traverse_
    )
import Data.Generics.Labels
    ()
import Data.IntCast
    ( intCast
    )
import Data.List.NonEmpty
    ( NonEmpty
    )
import Data.Maybe
    ( fromMaybe
    )
import Data.Tagged
    ( Tagged (..)
    , untag
    )
import Data.Text
    ( Text
    )
import Data.Word.Odd
    ( Word31
    )
import GHC.TypeLits
    ( Symbol
    )
import System.Directory
    ( createDirectoryIfMissing
    )
import System.FilePath
    ( (</>)
    )
import Test.Utils.StaticServer
    ( withStaticServer
    )

import qualified Cardano.Ledger.Address as Ledger
import qualified Cardano.Ledger.Shelley.API as Ledger
import qualified Codec.CBOR.Read as CBOR
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Data.ListMap as ListMap
import qualified Data.Set as Set
import qualified Data.Text as T

-- | Represents the notion of a fully configured pool. All keys are known, but
-- not necessarily exposed using this interface.
data ConfiguredPool = ConfiguredPool
    { operatePool
        :: forall a
         . NodeParams
        -> (RunningNode -> IO a)
        -> IO a
    -- ^ Precondition: the pool must first be registered.
    , metadataUrl
        :: Text
    , recipe
        :: PoolRecipe
    -- ^ The 'PoolRecipe' used to create this 'ConfiguredPool'.
    , registerViaShelleyGenesis
        :: IO (ShelleyGenesis StandardCrypto -> ShelleyGenesis StandardCrypto)
    , finalizeShelleyGenesisSetup :: RunningNode -> IO ()
    -- ^ Submit any pool retirement certificate according to the 'recipe'
    -- on-chain.
    }

configurePools
    :: PoolMetadataServer
    -> NonEmpty PoolRecipe
    -> ClusterM (NonEmpty ConfiguredPool)
configurePools metadataServer =
    traverse (configurePool metadataServer)

-- | Create a key pair for a node KES operational key
genKesKeyPair
    :: NodePathSegment
    -> ClusterM (FileOf "kes-prv" , FileOf "kes-pub")
genKesKeyPair nodeSegment = do
    poolDir <- askNodeDir nodeSegment
    let kesPrv = FileOf @"kes-prv" $ poolDir </> "kes.prv"
    let kesPub = FileOf @"kes-pub" $ poolDir </> "kes.pub"
    cli
        [ "node"
        , "key-gen-KES"
        , "--verification-key-file"
        , pathOf kesPub
        , "--signing-key-file"
        , pathOf kesPrv
        ]
    pure (kesPrv, kesPub)

-- | Create a key pair for a node VRF operational key
genVrfKeyPair
    :: NodePathSegment
    -> ClusterM (FileOf "vrf-prv" , FileOf "vrf-pub")
genVrfKeyPair nodeSegment = do
    poolDir <- askNodeDir nodeSegment
    let vrfPrv = FileOf @"vrf-prv" $ poolDir </> "vrf.prv"
    let vrfPub = FileOf @"vrf-pub" $ poolDir </> "vrf.pub"
    cli
        [ "node"
        , "key-gen-VRF"
        , "--verification-key-file"
        , pathOf vrfPub
        , "--signing-key-file"
        , pathOf vrfPrv
        ]
    pure (vrfPrv, vrfPub)

-- | Write a key pair for a node operator's offline key and a new certificate
-- issue counter
writeOperatorKeyPair
    :: NodePathSegment
    -> PoolRecipe
    -> ClusterM
        ( FileOf "op-prv"
        , FileOf "op-pub"
        , FileOf "op-cnt"
        )
writeOperatorKeyPair nodeSegment recipe = do
    poolDir <- askNodeDir nodeSegment
    let (_pId, pub, prv, count) = operatorKeys recipe
    traceClusterLog $ MsgGenOperatorKeyPair poolDir

    let opPub = poolDir </> "op.pub"
    let opPrv = poolDir </> "op.prv"
    let opCount = poolDir </> "op.count"

    liftIO $ do
        Aeson.encodeFile opPub pub
        Aeson.encodeFile opPrv prv
        Aeson.encodeFile opCount count

    pure
        ( FileOf @"op-prv" opPrv
        , FileOf @"op-pub" opPub
        , FileOf @"op-cnt" opCount
        )

-- | Issue a node operational certificate
issueOpCert
    :: NodePathSegment
    -> FileOf "kes-pub"
    -> FileOf "op-prv"
    -> FileOf "op-cnt"
    -> ClusterM FilePath
issueOpCert nodeSegment kesPub opPrv opCount = do
    poolDir <- askNodeDir nodeSegment
    let file = poolDir </> "op.cert"
    cli
        [ "node"
        , "issue-op-cert"
        , "--kes-verification-key-file"
        , pathOf kesPub
        , "--cold-signing-key-file"
        , pathOf opPrv
        , "--operational-certificate-issue-counter-file"
        , pathOf opCount
        , "--kes-period"
        , "0"
        , "--out-file"
        , file
        ]
    pure file

-- | Create a stake address key pair
genStakeAddrKeyPair
    :: (FileOf "stake-prv", FileOf "stake-pub")
    -> ClusterM ()
genStakeAddrKeyPair (stakePrv, stakePub) = do
    cli
        [ "stake-address"
        , "key-gen"
        , "--verification-key-file"
        , pathOf stakePub
        , "--signing-key-file"
        , pathOf stakePrv
        ]

readFailVerificationKeyOrFile
    :: forall keyrole (s :: Symbol)
     . ( HasTextEnvelope (VerificationKey keyrole)
       , SerialiseAsBech32 (VerificationKey keyrole)
       )
    => AsType keyrole
    -> FileOf s
    -> ClusterM (VerificationKey keyrole)
readFailVerificationKeyOrFile role op =
    liftIO
        $ either (error . show) id
            <$> readVerificationKeyOrFile
                role
                (VerificationKeyFilePath $ File $ pathOf op)

stakePoolIdFromOperatorVerKey
    :: HasCallStack
    => FileOf "op-pub"
    -> ClusterM (Ledger.KeyHash 'Ledger.StakePool (StandardCrypto))
stakePoolIdFromOperatorVerKey opPub = do
    stakePoolVerKey <- readFailVerificationKeyOrFile AsStakePoolKey opPub
    let bytes = serialiseToCBOR $ verificationKeyHash stakePoolVerKey
    pure
        $ either (error . show) snd
        $ CBOR.deserialiseFromBytes fromCBOR (BL.fromStrict bytes)

poolVrfFromFile
    :: HasCallStack
    => FileOf "vrf-pub"
    -> ClusterM (Ledger.Hash StandardCrypto (Ledger.VerKeyVRF StandardCrypto))
poolVrfFromFile vrfPub = do
    stakePoolVerKey <- readFailVerificationKeyOrFile AsVrfKey vrfPub
    let bytes = serialiseToCBOR $ verificationKeyHash stakePoolVerKey
    pure
        $ either (error . show) snd
        $ CBOR.deserialiseFromBytes fromCBOR (BL.fromStrict bytes)

stakingKeyHashFromFile
    :: HasCallStack
    => FileOf "stake-pub"
    -> ClusterM (Ledger.KeyHash 'Ledger.Staking StandardCrypto)
stakingKeyHashFromFile stakePub = do
    stakePoolVerKey <- readFailVerificationKeyOrFile AsStakeKey stakePub
    let bytes = serialiseToCBOR $ verificationKeyHash stakePoolVerKey
    pure
        $ either (error . show) snd
        $ CBOR.deserialiseFromBytes fromCBOR (BL.fromStrict bytes)

stakingAddrFromVkFile
    :: HasCallStack
    => FileOf "stake-pub"
    -> ClusterM (Ledger.Addr StandardCrypto)
stakingAddrFromVkFile stakePub = do
    stakePoolVerKey <- readFailVerificationKeyOrFile AsStakeKey stakePub
    let bytes = serialiseToCBOR $ verificationKeyHash stakePoolVerKey
    let payKH =
            either (error . show) snd
                $ CBOR.deserialiseFromBytes fromCBOR (BL.fromStrict bytes)
    let delegKH =
            either (error . show) snd
                $ CBOR.deserialiseFromBytes fromCBOR (BL.fromStrict bytes)
    pure
        $ Ledger.Addr
            Testnet
            (Ledger.KeyHashObj payKH)
            (Ledger.StakeRefBase (Ledger.KeyHashObj delegKH))

preparePoolRetirement
    :: NodePathSegment
    -> [FileOf "retirement-cert"]
    -> ClusterM (FileOf "retirement-tx", FileOf "faucet-prv")
preparePoolRetirement nodeSegment certs = do
    Config{..} <- ask
    poolDir <- askNodeDir nodeSegment
    let file = poolDir </> "tx.raw"
    (faucetInput, faucetPrv) <- takeFaucet
    cli
        $ [ clusterEraToString cfgLastHardFork
          , "transaction"
          , "build-raw"
          , "--tx-in"
          , untag faucetInput
          , "--ttl"
          , "400"
          , "--fee"
          , show faucetAmt
          , "--out-file"
          , file
          ]
            ++ mconcat ((\cert -> ["--certificate-file", pathOf cert]) <$> certs)

    pure (FileOf file, faucetPrv)

issuePoolRetirementCert
    :: NodePathSegment
    -> FileOf "op-pub"
    -> Word31
    -> ClusterM (FileOf "retirement-cert")
issuePoolRetirementCert nodeSegment opPub retirementEpoch = do
    poolDir <- askNodeDir nodeSegment
    let file = poolDir </> "pool-retirement.cert"
    cli
        [ "stake-pool"
        , "deregistration-certificate"
        , "--cold-verification-key-file"
        , pathOf opPub
        , "--epoch"
        , show retirementEpoch
        , "--out-file"
        , file
        ]
    pure $ FileOf @"retirement-cert" file

configurePool
    :: HasCallStack
    => PoolMetadataServer
    -> PoolRecipe
    -> ClusterM ConfiguredPool
configurePool metadataServer recipe = do
    let PoolRecipe pledgeAmt i mretirementEpoch metadata _ _ = recipe

    UnliftClusterM withConfig Config{..} <- askUnliftClusterM
    -- Use pool-specific dir
    let name = "pool-" <> show i
        nodeSegment = NodePathSegment name
    poolDir <- askNodeDir nodeSegment
    liftIO $ createDirectoryIfMissing False poolDir

    -- Generate/assign keys
    (vrfPrv, vrfPub) <- genVrfKeyPair nodeSegment
    (kesPrv, kesPub) <- genKesKeyPair nodeSegment
    (opPrv, opPub, opCount) <- writeOperatorKeyPair nodeSegment recipe
    opCert <- issueOpCert nodeSegment kesPub opPrv opCount
    let ownerPub = FileOf @"stake-pub" $ poolDir </> "stake.pub"
    let ownerPrv = FileOf @"stake-prv" $ poolDir </> "stake.prv"
    genStakeAddrKeyPair (ownerPrv, ownerPub)

    let metadataURL = urlFromPoolIndex metadataServer i
    liftIO $ registerMetadataForPoolIndex metadataServer i metadata
    let metadataBytes = Aeson.encode metadata
    pure
        ConfiguredPool
            { operatePool = \nodeParams action -> do
                let NodeParams genesisFiles hardForks (port, peers) logCfg =
                        nodeParams
                let logCfg' = setLoggingName name logCfg

                topology <- withConfig $ genTopology nodeSegment peers
                withStaticServer poolDir $ \url -> do
                    traceWith cfgTracer $ MsgStartedStaticServer poolDir url

                    (nodeConfig, genesisData, vd) <-
                        withConfig
                            $ genNodeConfig
                                nodeSegment
                                (Tagged @"node-name" mempty)
                                genesisFiles
                                hardForks
                                logCfg'

                    let cfg =
                            CardanoNodeConfig
                                { nodeDir = poolDir
                                , nodeConfigFile = pathOf @"node-config" nodeConfig
                                , nodeTopologyFile = pathOf @"topology" topology
                                , nodeDatabaseDir = "db"
                                , nodeDlgCertFile = Nothing
                                , nodeSignKeyFile = Nothing
                                , nodeOpCertFile = Just opCert
                                , nodeKesKeyFile = Just $ pathOf @"kes-prv" kesPrv
                                , nodeVrfKeyFile = Just $ pathOf @"vrf-prv" vrfPrv
                                , nodePort = Just (NodePort port)
                                , nodeLoggingHostname = Just name
                                , nodeExecutable = Nothing
                                }

                    withConfig
                        $ withCardanoNodeProcess name cfg
                        $ \socket -> action $ RunningNode socket genesisData vd
            , registerViaShelleyGenesis = withConfig $ do
                poolId <- stakePoolIdFromOperatorVerKey opPub
                vrf <- poolVrfFromFile vrfPub
                stakePubHash <- stakingKeyHashFromFile ownerPub
                pledgeAddr <- stakingAddrFromVkFile ownerPub

                let params =
                        Ledger.PoolParams
                            { ppId = poolId
                            , ppVrf = vrf
                            , ppPledge = Ledger.Coin $ intCast pledgeAmt
                            , ppCost = Ledger.Coin 0
                            , ppMargin = unsafeUnitInterval 0.1
                            , ppRewardAcnt =
                                Ledger.RewardAcnt Testnet
                                    $ Ledger.KeyHashObj stakePubHash
                            , ppOwners = Set.fromList [stakePubHash]
                            , ppRelays = mempty
                            , ppMetadata =
                                SJust
                                    $ Ledger.PoolMetadata
                                        ( fromMaybe (error "invalid url (too long)")
                                            $ textToUrl
                                            $ T.pack metadataURL
                                        )
                                        (blake2b256 (BL.toStrict metadataBytes))
                            }

                let updateStaking sgs =
                        sgs
                            { Ledger.sgsPools =
                                ListMap.ListMap [(poolId, params)] <> sgsPools sgs
                            , Ledger.sgsStake =
                                ListMap.fromList [(stakePubHash, poolId)]
                                    <> Ledger.sgsStake sgs
                            }
                let poolSpecificFunds =
                        ListMap.fromList
                            [(pledgeAddr, Ledger.Coin $ intCast pledgeAmt)]

                pure
                    $ over #sgInitialFunds (poolSpecificFunds <>)
                        . over #sgStaking updateStaking
            , finalizeShelleyGenesisSetup = \(RunningNode socket _ _) -> do
                -- Here is our chance to respect the 'retirementEpoch' of
                -- the 'PoolRecipe'.
                --
                -- NOTE: We also submit the retirement cert in
                -- @registerViaTx@, but this seems to work regardless. (We
                -- do want to submit it here for the sake of babbage)
                let retire e = do
                        retCert <- issuePoolRetirementCert nodeSegment opPub e
                        (rawTx, faucetPrv) <-
                            preparePoolRetirement
                                nodeSegment
                                [retCert]
                        signAndSubmitTx
                            socket
                            (changeFileOf @"retirement-tx"  @"tx-body" rawTx)
                            [ changeFileOf @"faucet-prv"  @"signing-key" faucetPrv
                            , changeFileOf @"stake-prv"  @"signing-key" ownerPrv
                            , changeFileOf @"op-prv"  @"signing-key" opPrv
                            ]
                            "retirement cert"

                withConfig $ traverse_ retire mretirementEpoch
            , metadataUrl = T.pack metadataURL
            , recipe = recipe
            }
