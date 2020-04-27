{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Pool.Jormungandr.MetadataSpec (spec) where

import Prelude

import Cardano.Pool.Jormungandr.Metadata
    ( FetchError (..)
    , MetadataConfig (..)
    , RegistryLog (..)
    , RegistryLogMsg (..)
    , StakePoolMetadata (..)
    , StakePoolTicker
    , cacheArchive
    , getMetadataConfig
    , getStakePoolMetadata
    )
import Cardano.Wallet.Primitive.Types
    ( PoolOwner (..) )
import Cardano.Wallet.Unsafe
    ( unsafeFromText )
import Codec.Archive.Zip
    ( CompressionMethod (..), addEntry, createArchive, mkEntrySelector )
import Control.Monad
    ( forM_, replicateM, void )
import Control.Tracer
    ( Tracer )
import Data.Aeson
    ( encode )
import Data.ByteString
    ( ByteString )
import Data.Text
    ( Text )
import Data.Text.Class
    ( ToText (..) )
import Data.Time.Clock
    ( addUTCTime, getCurrentTime )
import System.Directory
    ( removeDirectoryRecursive, setModificationTime )
import System.FilePath
    ( takeDirectory, (<.>), (</>) )
import System.IO
    ( IOMode (..), withFile )
import System.IO.Temp
    ( withSystemTempDirectory )
import Test.Hspec
    ( Spec, around, describe, it, shouldBe )
import Test.QuickCheck
    ( Arbitrary (..)
    , Property
    , arbitraryPrintableChar
    , choose
    , counterexample
    , elements
    , frequency
    , listOf
    , listOf1
    , property
    , shuffle
    , sublistOf
    , vectorOf
    )
import Test.QuickCheck.Monadic
    ( assert, monadicIO, monitor, run )
import Test.Utils.Paths
    ( getTestData )
import Test.Utils.StaticServer
    ( withStaticServer )
import Test.Utils.Trace
    ( captureLogging, withLogging )

import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T

spec :: Spec
spec = do
    describe "Cardano.Pool.Registry.getStakePoolMetadata specs" $
        around withFixtures $ do

        it "Loads the example zip" $ \(cfg, tr, _) -> do
            res <- getStakePoolMetadata tr cfg presentOwners
            res `shouldBe` Right (map Just presentMetas)

        it "Handles a missing pool" $ \(cfg, tr, _) -> do
            res <- getStakePoolMetadata tr cfg (absentOwner:presentOwners)
            res `shouldBe` Right (Nothing:map Just presentMetas)

        let expectDownloadError name res = case res of
                Left (FetchErrorDownload msg) ->
                    head (words msg) `shouldBe` name
                _ -> error "expected fetch error but got none"

        it "Fails with an unavailable HTTP server" $ \(cfg, tr, _) -> do
            let badCfg = cfg { registryURL = "http://localhost:99/master.zip" }
            res <- getStakePoolMetadata tr badCfg presentOwners
            expectDownloadError "HttpExceptionRequest" res

        it "Handles name resolution failures" $ \(cfg, tr, _) -> do
            let badCfg = cfg { registryURL = "http://xyzzy/master.zip" }
            res <- getStakePoolMetadata tr badCfg presentOwners
            expectDownloadError "HttpExceptionRequest" res

        it "Caches the zip" $ \(cfg, tr, getMsgs) -> do
            void $ getStakePoolMetadata tr cfg presentOwners

            res <- getStakePoolMetadata tr cfg presentOwners
            res `shouldBe` Right (map Just presentMetas)

            msgs <- map registryLogMsg <$> getMsgs
            length (filter isMsgDownloadStarted msgs) `shouldBe` 1
            length (filter isMsgUsingCached msgs) `shouldBe` 1

        it "Refreshes expired cache" $ \(cfg, tr, getMsgs) -> do
            void $ getStakePoolMetadata tr cfg presentOwners

            makeCacheExpired cfg

            res <- getStakePoolMetadata tr cfg presentOwners
            res `shouldBe` Right (map Just presentMetas)

            msgs <- map registryLogMsg <$> getMsgs
            length (filter isMsgUsingCached msgs) `shouldBe` 0
            length (filter isMsgDownloadStarted msgs) `shouldBe` 2

        it "Cache it, removes it, re-download it" $ \(cfg, tr, getMsgs) -> do
            void $ getStakePoolMetadata tr cfg presentOwners

            removeDirectoryRecursive (cacheDirectory cfg)

            res <- getStakePoolMetadata tr cfg presentOwners
            res `shouldBe` Right (map Just presentMetas)

            msgs <- map registryLogMsg <$> getMsgs
            length (filter isMsgUsingCached msgs) `shouldBe` 0
            length (filter isMsgDownloadStarted msgs) `shouldBe` 2

        it "Handles broken cache file" $ \(cfg, tr, _) -> do
            void $ getStakePoolMetadata tr cfg presentOwners

            -- truncate cache file
            withFile (cacheArchive cfg) WriteMode (const $ pure ())

            res <- getStakePoolMetadata tr cfg presentOwners
            case res of
                Left (FetchErrorZipParsingFailed _ _) -> pure ()
                _ -> error "expected FetchErrorZipParsingFailed"

        it "Does not leave poisoned cache" $ \(cfg, tr, getMsgs) -> do
            let badCfg = cfg { registryURL = "http://localhost:99/master.zip" }
            _ <- getStakePoolMetadata tr badCfg presentOwners
            res <- getStakePoolMetadata tr badCfg presentOwners
            expectDownloadError "HttpExceptionRequest" res

            msgs <- map registryLogMsg <$> getMsgs
            length (filter (== MsgCleanupDownload) msgs) `shouldBe` 2

    describe "Cardano.Pool.Registry.getStakePoolMetadata" $
        it "Arbitrary zip files" $
            property prop_getStakePoolMetadata

{-------------------------------------------------------------------------------
                                 Test fixtures
-------------------------------------------------------------------------------}

withFixtures :: ((MetadataConfig, Tracer IO RegistryLog, IO [RegistryLog]) -> IO a) -> IO a
withFixtures action =
    withStaticServer dataDir $ \baseUrl ->
    withSystemTempDirectory "stake-pool-metadata" $ \metadataDir ->
    withLogging $ \(tr, getMsgs) -> do
        cfg <- getMetadataConfig metadataDir
        let cfg' = cfg { registryURL = testUrl baseUrl }
        action (cfg', tr, getMsgs)

dataDir :: FilePath
dataDir = $(getTestData) </> "stake-pool-registry"

-- | Make a file server URL for the test data file.
-- This file was downloaded from <https://github.com/input-output-hk/testnet-stake-pool-registry/archive/master.zip>
testUrl :: String -> String
testUrl baseUrl = baseUrl ++ "testnet-stake-pool-registry-master.zip"

presentOwners :: [PoolOwner]
presentOwners = map unsafeFromText
    [ "ed25519_pk1a6mv6x2r0f3y62ddavxvf5lkdsj8ttc8mk3yngdy895j8tn6jqyqesvuk2"
    , "ed25519_pk1fh2ajuu7rqquuxqrkh6ldfvdktlthkl4wlfhymrath85wzhy7slsr37f87"
    , "ed25519_pk1wc84pj9h3qw5kknawj2d0mqwr43gxhtsnq5pmuzxg7ssrcpt84ds9sce69"
    ]

presentMetas :: [StakePoolMetadata]
presentMetas =
    [ StakePoolMetadata
        { ticker = unsafeFromText "FST"
        , homepage = "https://12345"
        , owner = unsafeFromText "ed25519_pk1a6mv6x2r0f3y62ddavxvf5lkdsj8ttc8mk3yngdy895j8tn6jqyqesvuk2"
        , name = "Ada Lovelace"
        , description = Nothing
        , pledgeAddress = "addr1svklmf8yl78x9cw30ystvprhxtm790k4380xlsjrjqn2p8nekup8uvzfezl"
        }
    , StakePoolMetadata
        { ticker = unsafeFromText "IOHK1"
        , homepage = "https://iohk.io"
        , owner = unsafeFromText "ed25519_pk1fh2ajuu7rqquuxqrkh6ldfvdktlthkl4wlfhymrath85wzhy7slsr37f87"
        , name = "IOHK Pool - 1"
        , description = Nothing
        , pledgeAddress = "addr1s4xatktnncvqrnscqw6lta493ke0aw7m74maxunv04wu73c2un6r7vm0k7z"
        }
    , StakePoolMetadata
        { ticker = unsafeFromText "TICK"
        , homepage = "https://12345"
        , owner = unsafeFromText "ed25519_pk1wc84pj9h3qw5kknawj2d0mqwr43gxhtsnq5pmuzxg7ssrcpt84ds9sce69"
        , name = "Ada Lovelace"
        , description = Nothing
        , pledgeAddress = "addr1svklmf8yl78x9cw30ystvprhxtm790k4380xlsjrjqn2p8nekup8uvzfezl"
        }
    ]

absentOwner :: PoolOwner
absentOwner = PoolOwner "pk192m4ytl5k357e2l666yleuwjlurmf0vxjyh4atxzu5m22q6mexlsp88k7x"

{-------------------------------------------------------------------------------
                                    Property
-------------------------------------------------------------------------------}

data TestCase = TestCase
    { stakePools :: [(PoolOwner, StakePoolEntry)]
    -- ^ Stake pool metadata in the zip file.
    , poolOwners :: [PoolOwner]
    -- ^ Stake pools to look up for the test.
    , topDir :: FilePath
    -- ^ The name of the top-level directory in the zip file.
    } deriving (Show, Eq)

data StakePoolEntry
    = Junk
    | Meta StakePoolMetadata
    deriving (Show, Eq)

-- | Tests looking up stake pools ids in a randomish zip file. Some metadata is
-- missing, some fails to parse. The property asserts that all metadata which
-- exists is looked up correctly, and that any parse errors are logged.
prop_getStakePoolMetadata :: TestCase -> Property
prop_getStakePoolMetadata tc = monadicIO $ do
    -- Generate a zip file for the test case and serve on a local file server.
    -- Run getStakePoolMeta and take the actual result, and the log messages.
    (msgs, res) <- run $ withTestCaseZip tc $ \zipFile ->
        withStaticServer (takeDirectory zipFile) $ \baseUrl -> do
            withSystemTempDirectory "stake-pool-metadata" $ \metadataDir ->
                captureLogging $ \tr -> do
                    baseCfg <- getMetadataConfig metadataDir
                    let cfg = baseCfg { registryURL = testCaseUrl tc baseUrl }
                    getStakePoolMetadata tr cfg (poolOwners tc)
    let numDecodeErrors = count isDecodeErrorMsg msgs

    -- Expected results
    let expected =
            [ stakePoolEntryMeta =<< lookup p (stakePools tc) | p <- poolOwners tc ]
    let numJunkEntries =
            count (\(p, e) -> isJunk e && p `elem` poolOwners tc) (stakePools tc)

    monitor $ counterexample $ unlines $
        [ "expected        = " ++ show expected
        , "actual          = " ++ show res
        , "# junk entries  = " ++ show numJunkEntries
        , "# decode errors = " ++ show numDecodeErrors
        , "logs ="
        ] ++ map (T.unpack . toText) msgs

    assert $ res == Right expected && numJunkEntries == numDecodeErrors

withTestCaseZip :: TestCase -> (FilePath -> IO a) -> IO a
withTestCaseZip tc action = withSystemTempDirectory "registry-spec" $ \dir -> do
    let zipFile = dir </> topDir tc <.> "zip"
    createArchive zipFile $ forM_ (stakePools tc) $ \(owner, contents) -> do
        sel <- mkEntrySelector (registryFile (topDir tc) owner)
        addEntry Deflate (encodeStakePoolEntry contents) sel
    action zipFile

registryFile :: FilePath -> PoolOwner -> FilePath
registryFile top owner =
    top </> "registry" </> T.unpack (toText owner) <.> "json"

testCaseUrl :: TestCase -> String -> String
testCaseUrl tc baseUrl = baseUrl ++ topDir tc ++ ".zip"

{-------------------------------------------------------------------------------
                               Test case helpers
-------------------------------------------------------------------------------}

isDecodeErrorMsg :: RegistryLog -> Bool
isDecodeErrorMsg rl = case registryLogMsg rl of
    MsgExtractFileResult (Just (Left _)) -> True
    _ -> False

stakePoolEntryMeta :: StakePoolEntry -> Maybe StakePoolMetadata
stakePoolEntryMeta Junk = Nothing
stakePoolEntryMeta (Meta m) = Just m

encodeStakePoolEntry :: StakePoolEntry -> ByteString
encodeStakePoolEntry Junk = "junk"
encodeStakePoolEntry (Meta m) = BL.toStrict $ encode m

isJunk :: StakePoolEntry -> Bool
isJunk Junk = True
isJunk _ = False

count :: (a -> Bool) -> [a] -> Int
count p = length . filter p

{-------------------------------------------------------------------------------
                              Test case generation
-------------------------------------------------------------------------------}

instance Arbitrary TestCase where
    arbitrary = do
        stakePools <- arbitrary
        presentPoolIds <- sublistOf (map fst stakePools)
        absentPoolIds <- arbitrary
        poolOwners <- shuffle (presentPoolIds ++ absentPoolIds)
        PathElement topDir <- arbitrary
        pure $ TestCase {stakePools, poolOwners, topDir}
    shrink (TestCase sps pids td)
        = [ TestCase sps' pids' td'
          | (sps', pids', td') <- shrink (sps, pids, td)
          , not (null td')
          ]

instance Arbitrary StakePoolEntry where
    arbitrary = frequency
        [ (1, pure Junk)
        , (4, Meta <$> arbitrary)
        ]

instance Arbitrary StakePoolMetadata where
    arbitrary = StakePoolMetadata
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary

instance Arbitrary Text where
    arbitrary = T.pack <$> listOf c
      where
        c = frequency
            [ (6, arbitraryPrintableChar)
            , (1, arbitrary)
            , (1, elements ['☃', 'ż', '好'])
            ]

instance Arbitrary StakePoolTicker where
    arbitrary = unsafeFromText . T.pack <$> do
        len <- choose (3, 5)
        replicateM len arbitrary

instance Arbitrary PoolOwner where
    arbitrary = PoolOwner . B8.pack <$> vectorOf 32 c
        where c = elements ['a'..'z']

newtype PathElement = PathElement FilePath deriving (Show, Eq)

instance Arbitrary PathElement where
    arbitrary = PathElement <$> listOf1 (elements ['a'..'z'])

{-------------------------------------------------------------------------------
                                     Utils
-------------------------------------------------------------------------------}

makeCacheExpired :: MetadataConfig -> IO ()
makeCacheExpired cfg = do
    -- Subtract the cache TTL plus one second for good measure.
    old <- addUTCTime ((-1) - cacheTTL cfg) <$> getCurrentTime
    setModificationTime (cacheArchive cfg) old

{-------------------------------------------------------------------------------
                                    Logging
-------------------------------------------------------------------------------}

isMsgDownloadStarted :: RegistryLogMsg -> Bool
isMsgDownloadStarted MsgDownloadStarted{} = True
isMsgDownloadStarted _ = False

isMsgUsingCached :: RegistryLogMsg -> Bool
isMsgUsingCached MsgUsingCached{} = True
isMsgUsingCached _ = False
