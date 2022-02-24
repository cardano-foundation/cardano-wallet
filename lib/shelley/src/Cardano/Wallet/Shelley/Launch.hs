{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- Command-line option passing for cardano-wallet shelley.
--

module Cardano.Wallet.Shelley.Launch
    ( -- * Network
      NetworkConfiguration (..)
    , CardanoNodeConn
    , nodeSocketOption
    , networkConfigurationOption
    , parseGenesisData

      -- * Utils
    , withSystemTempDir
    , withTempDir
    , isEnvSet
    , envFromText
    , lookupEnvNonEmpty

    -- * Logging
    , TempDirLog (..)

    -- * Light Mode
    , Mode (..)
    , modeFlag
    ) where

import Prelude

import Cardano.BM.Data.Severity
    ( Severity (..) )
import Cardano.BM.Data.Tracer
    ( HasPrivacyAnnotation (..), HasSeverityAnnotation (..) )
import Cardano.Chain.Genesis
    ( GenesisData (..), readGenesisData )
import Cardano.CLI
    ( optionT )
import Cardano.Launcher
    ( LauncherLog )
import Cardano.Launcher.Node
    ( CardanoNodeConn, cardanoNodeConn, isWindows )
import Cardano.Wallet.Logging
    ( BracketLog, BracketLog' (..), bracketTracer )
import Cardano.Wallet.Primitive.AddressDerivation
    ( NetworkDiscriminant (..) )
import Cardano.Wallet.Primitive.Types
    ( Block (..), NetworkParameters (..), ProtocolMagic (..) )
import Cardano.Wallet.Shelley
    ( SomeNetworkDiscriminant (..) )
import Control.Monad.IO.Unlift
    ( MonadUnliftIO, liftIO )
import Control.Monad.Trans.Except
    ( ExceptT (..), withExceptT )
import Control.Tracer
    ( Tracer (..), contramap )
import Data.Bifunctor
    ( first )
import Data.Maybe
    ( isJust )
import Data.Proxy
    ( Proxy (..) )
import Data.Text
    ( Text )
import Data.Text.Class
    ( FromText (..), TextDecodingError, ToText (..) )
import GHC.TypeLits
    ( KnownNat, Nat, SomeNat (..), someNatVal )
import Options.Applicative
    ( Parser
    , eitherReader
    , flag
    , flag'
    , help
    , long
    , metavar
    , option
    , optional
    , (<|>)
    )
import Ouroboros.Network.Magic
    ( NetworkMagic (..) )
import Ouroboros.Network.NodeToClient
    ( NodeToClientVersionData (..) )
import System.Environment
    ( lookupEnv )
import System.Exit
    ( ExitCode (..) )
import System.IO.Temp
    ( createTempDirectory, getCanonicalTemporaryDirectory )
import UnliftIO.Temporary
    ( withTempDirectory )

import qualified Cardano.Wallet.Byron.Compatibility as Byron
import qualified Cardano.Wallet.Primitive.Types as W
import qualified Cardano.Wallet.Shelley.Launch.Blockfrost as Blockfrost
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T

-- | Shelley hard fork network configuration has two genesis data.
-- As a special case for mainnet, we hardcode the byron genesis data.
data NetworkConfiguration where
    -- | Mainnet does not have network discrimination.
    MainnetConfig
        :: NetworkConfiguration

    -- | Testnet has network magic.
    TestnetConfig
        :: FilePath
        -- ^ Genesis data in JSON format, for byron era.
        -> NetworkConfiguration

    -- | Staging does not have network discrimination.
    StagingConfig
        :: FilePath
        -- ^ Genesis data in JSON format, for byron era.
        -> NetworkConfiguration
  deriving (Show, Eq)

-- | --node-socket=FILE
nodeSocketOption :: Parser CardanoNodeConn
nodeSocketOption = option (eitherReader (addHelp . cardanoNodeConn)) $ mempty
    <> long "node-socket"
    <> metavar (if isWindows then "PIPENAME" else "FILE")
    <> help helpText
  where
    helpText = mconcat
        [ "Path to the node's domain socket file (POSIX) "
        , "or pipe name (Windows). "
        , "Note: Maximum length for POSIX socket files is approx. 100 bytes. "
        , "Note:", pipeHelp ]
    pipeHelp = " Windows named pipes are of the form \\\\.\\pipe\\cardano-node"
    addHelp = first (if isWindows then (++ pipeHelp) else id)

-- | --mainnet --shelley-genesis=FILE
-- --testnet --byron-genesis=FILE --shelley-genesis=FILE
-- --staging --byron-genesis=FILE --shelley-genesis=FILE
networkConfigurationOption :: Parser NetworkConfiguration
networkConfigurationOption = mainnet <|> testnet <|> staging
  where
    mainnet = mainnetFlag
    testnet = TestnetConfig <$> genesisFileOption "byron" "testnet"
    staging = StagingConfig <$> genesisFileOption "byron" "staging"

    mainnetFlag = flag' MainnetConfig $ mempty
        <> long "mainnet"
        <> help "Use Cardano mainnet protocol"

    genesisFileOption :: String -> String -> Parser FilePath
    genesisFileOption era net = optionT $ mempty
        <> long net
        <> metavar "FILE"
        <> help ("Path to the " <> era <> " genesis data in JSON format.")

someCustomDiscriminant
    :: (forall (pm :: Nat). KnownNat pm => Proxy pm -> SomeNetworkDiscriminant)
    -> ProtocolMagic
    -> (SomeNetworkDiscriminant, NodeToClientVersionData)
someCustomDiscriminant mkSomeNetwork pm@(ProtocolMagic n) =
    case someNatVal (fromIntegral n) of
        Just (SomeNat proxy) ->
            ( mkSomeNetwork proxy
            , NodeToClientVersionData $
                NetworkMagic $ fromIntegral $ W.getProtocolMagic pm
            )
        _ -> error "networkDiscriminantFlag: failed to convert \
            \ProtocolMagic to SomeNat."

parseGenesisData
    :: NetworkConfiguration
    -> ExceptT String IO
        (SomeNetworkDiscriminant, NetworkParameters, NodeToClientVersionData, Block)
parseGenesisData = \case
    MainnetConfig -> do
        let nm = NetworkMagic $ fromIntegral $ W.getProtocolMagic W.mainnetMagic
        let mainnetVersionData = NodeToClientVersionData nm
        pure
            ( SomeNetworkDiscriminant $ Proxy @'Mainnet
            , Byron.mainnetNetworkParameters
            , mainnetVersionData
            , Byron.emptyGenesis (genesisParameters Byron.mainnetNetworkParameters)
            )

    TestnetConfig byronGenesisFile -> do
        (genesisData, genesisHash) <-
            withExceptT show $ readGenesisData byronGenesisFile

        let mkSomeNetwork
                :: forall (pm :: Nat). KnownNat pm
                => Proxy pm
                -> SomeNetworkDiscriminant
            mkSomeNetwork _ = SomeNetworkDiscriminant $ Proxy @('Testnet pm)


        let pm = Byron.fromProtocolMagicId $ gdProtocolMagicId genesisData
        let (discriminant, vData) = someCustomDiscriminant mkSomeNetwork pm
        let (np, outs) = Byron.fromGenesisData (genesisData, genesisHash)
        let block0 = Byron.genesisBlockFromTxOuts (genesisParameters np) outs

        pure
            ( discriminant
            , np
            , vData
            , block0
            )

    StagingConfig byronGenesisFile -> do
        (genesisData, genesisHash) <-
            withExceptT show $ readGenesisData byronGenesisFile

        let mkSomeNetwork
                :: forall (pm :: Nat). KnownNat pm
                => Proxy pm
                -> SomeNetworkDiscriminant
            mkSomeNetwork _ = SomeNetworkDiscriminant $ Proxy @('Staging pm)

        let pm = Byron.fromProtocolMagicId $ gdProtocolMagicId genesisData
        let (discriminant, vData) = someCustomDiscriminant mkSomeNetwork pm
        let (np, outs) = Byron.fromGenesisData (genesisData, genesisHash)
        let block0 = Byron.genesisBlockFromTxOuts (genesisParameters np) outs

        pure
            ( discriminant
            , np
            , vData
            , block0
            )

{-------------------------------------------------------------------------------
                           Environment Variable Utils
-------------------------------------------------------------------------------}

-- | Looks up an environment variable, treating variables which are defined but
-- empty the same as variables which are undefined.
lookupEnvNonEmpty :: MonadUnliftIO m => String -> m (Maybe String)
lookupEnvNonEmpty = liftIO . fmap nonEmpty . lookupEnv
  where
    nonEmpty (Just "") = Nothing
    nonEmpty m = m

-- | Returns true iff an environment variable is defined and non-empty.
isEnvSet :: MonadUnliftIO m => String -> m Bool
isEnvSet = fmap isJust . lookupEnvNonEmpty

-- | Parses an environment variable using text-class.
envFromText
    :: (MonadUnliftIO m, FromText a)
    => String
    -> m (Maybe (Either TextDecodingError a))
envFromText = liftIO . fmap (fmap (fromText . T.pack)) . lookupEnv

{-------------------------------------------------------------------------------
                                     Utils
-------------------------------------------------------------------------------}

-- | Create a temporary directory and remove it after the given IO action has
-- finished -- unless the @NO_CLEANUP@ environment variable has been set.
withTempDir
    :: MonadUnliftIO m
    => Tracer m TempDirLog
    -> FilePath -- ^ Parent directory
    -> String -- ^ Directory name template
    -> (FilePath -> m a) -- ^ Callback that can use the directory
    -> m a
withTempDir tr parent name action = isEnvSet "NO_CLEANUP" >>= \case
    True -> do
        dir <- liftIO $ createTempDirectory parent name
        let tr' = contramap (MsgNoCleanup dir) tr
        bracketTracer tr' $ action dir
    False -> withTempDirectory parent name action

withSystemTempDir
    :: MonadUnliftIO m
    => Tracer m TempDirLog
    -> String   -- ^ Directory name template
    -> (FilePath -> m a) -- ^ Callback that can use the directory
    -> m a
withSystemTempDir tr name action = do
    parent <- liftIO getCanonicalTemporaryDirectory
    withTempDir tr parent name action


{-------------------------------------------------------------------------------
                                    Logging
-------------------------------------------------------------------------------}

data ClusterLog
    = MsgRegisteringStakePools Int -- ^ How many pools
    | MsgStartingCluster FilePath
    | MsgLauncher String LauncherLog
    | MsgStartedStaticServer String FilePath
    | MsgTempDir TempDirLog
    | MsgBracket Text BracketLog
    | MsgCLIStatus Text ExitCode BL8.ByteString BL8.ByteString
    | MsgCLIRetry Text
    | MsgCLIRetryResult Text Int BL8.ByteString
    | MsgSocketIsReady CardanoNodeConn
    | MsgStakeDistribution String ExitCode BL8.ByteString BL8.ByteString
    | MsgDebug Text
    | MsgGenOperatorKeyPair FilePath
    | MsgCLI [String]
    deriving (Show)

instance ToText ClusterLog where
    toText = \case
        MsgStartingCluster dir ->
            "Configuring cluster in " <> T.pack dir
        MsgRegisteringStakePools 0 -> mconcat
                [ "Not registering any stake pools due to "
                , "NO_POOLS=1. Some tests may fail."
                ]
        MsgRegisteringStakePools n -> mconcat
                [ T.pack (show n)
                , " stake pools are being registered on chain... "
                , "Can be skipped using NO_POOLS=1."
                ]
        MsgLauncher name msg ->
            T.pack name <> " " <> toText msg
        MsgStartedStaticServer baseUrl fp ->
            "Started a static server for " <> T.pack fp
                <> " at " <> T.pack baseUrl
        MsgTempDir msg -> toText msg
        MsgBracket name b -> name <> ": " <> toText b
        MsgCLIStatus msg st out err -> case st of
            ExitSuccess -> "Successfully finished " <> msg
            ExitFailure code -> "Failed " <> msg <> " with exit code " <>
                T.pack (show code)  <> ":\n" <> indent out <> "\n" <> indent err
        MsgCLIRetry msg -> msg
        MsgCLIRetryResult msg code err ->
            "Failed " <> msg <> " with exit code " <>
                T.pack (show code) <> ":\n" <> indent err
        MsgSocketIsReady conn ->
            toText conn <> " is ready."
        MsgStakeDistribution name st out err -> case st of
            ExitSuccess ->
                "Stake distribution query for " <> T.pack name <>
                ":\n" <> indent out
            ExitFailure code ->
                "Query of stake-distribution failed with status " <>
                T.pack (show code) <> ":\n" <> indent err
        MsgDebug msg -> msg
        MsgGenOperatorKeyPair dir ->
            "Generating stake pool operator key pair in " <> T.pack dir
        MsgCLI args -> T.pack $ unwords ("cardano-cli":args)
      where
        indent = T.unlines . map ("  " <>) . T.lines . T.decodeUtf8With T.lenientDecode . BL8.toStrict

instance HasPrivacyAnnotation ClusterLog
instance HasSeverityAnnotation ClusterLog where
    getSeverityAnnotation = \case
        MsgStartingCluster _ -> Notice
        MsgRegisteringStakePools _ -> Notice
        MsgLauncher _ _ -> Info
        MsgStartedStaticServer _ _ -> Info
        MsgTempDir msg -> getSeverityAnnotation msg
        MsgBracket _ _ -> Debug
        MsgCLIStatus _ ExitSuccess _ _-> Debug
        MsgCLIStatus _ (ExitFailure _) _ _-> Error
        MsgCLIRetry _ -> Info
        MsgCLIRetryResult{} -> Info
        -- NOTE: ^ Some failures are expected, so for cleaner logs we use Info,
        -- instead of Warning.
        MsgSocketIsReady _ -> Info
        MsgStakeDistribution _ ExitSuccess _ _-> Info
        MsgStakeDistribution _ (ExitFailure _) _ _-> Info
        -- NOTE: ^ Some failures are expected, so for cleaner logs we use Info,
        -- instead of Warning.
        MsgDebug _ -> Debug
        MsgGenOperatorKeyPair _ -> Debug
        MsgCLI _ -> Debug

data TempDirLog = MsgNoCleanup FilePath BracketLog deriving (Show)

instance ToText TempDirLog where
    toText = \case
        MsgNoCleanup _ BracketStart -> ""
        MsgNoCleanup dir _ -> "NO_CLEANUP of temporary directory " <> T.pack dir

instance HasPrivacyAnnotation TempDirLog
instance HasSeverityAnnotation TempDirLog where
    getSeverityAnnotation = \case
        MsgNoCleanup _ BracketStart -> Debug
        MsgNoCleanup _ _ -> Notice

{-------------------------------------------------------------------------------
                                    Mode
-------------------------------------------------------------------------------}

data Mode = Normal | Light (Maybe Blockfrost.TokenFile)
  deriving (Show)

modeFlag :: Parser Mode
modeFlag = do
    light <- flag False True $
        mconcat [ long "light", help "Enable light mode" ]
    creds <- optional Blockfrost.tokenFileOption
    pure $ if light then Light creds else Normal
