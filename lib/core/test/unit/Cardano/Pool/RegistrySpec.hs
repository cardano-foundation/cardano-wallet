{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Pool.RegistrySpec (spec) where

import Prelude

import Cardano.BM.Configuration.Static
    ( defaultConfigStdout )
import Cardano.BM.Data.LogItem
    ( LOContent (..), LogObject (..) )
import Cardano.BM.Setup
    ( setupTrace_ )
import Cardano.BM.Trace
    ( Trace, traceInTVarIO )
import Cardano.Pool.Metrics
    ( StakePoolMetadata (..), StakePoolTicker )
import Cardano.Pool.Registry
    ( FetchError (..)
    , RegistryLog (..)
    , RegistryLogMsg (..)
    , getStakePoolMetadata
    , transformTrace
    )
import Cardano.Wallet.Primitive.Types
    ( PoolId (..) )
import Cardano.Wallet.Unsafe
    ( unsafeFromText )
import Codec.Archive.Zip
    ( CompressionMethod (..), addEntry, createArchive, mkEntrySelector )
import Control.Concurrent.STM.TVar
    ( newTVarIO, readTVarIO )
import Control.Monad
    ( forM_, replicateM )
import Data.Aeson
    ( encode )
import Data.ByteString
    ( ByteString )
import Data.FileEmbed
    ( makeRelativeToProject )
import Data.Maybe
    ( mapMaybe )
import Network.Wai.Application.Static
    ( defaultWebAppSettings, staticApp )
import Network.Wai.Handler.Warp
    ( Port, withApplication )
import System.FilePath
    ( takeDirectory, (<.>), (</>) )
import System.IO.Temp
    ( withSystemTempDirectory )
import Test.Hspec
    ( Spec, around, describe, it, shouldBe )
import Test.QuickCheck
    ( Arbitrary (..)
    , Property
    , choose
    , counterexample
    , elements
    , frequency
    , infiniteListOf
    , listOf1
    , property
    , shuffle
    , sublistOf
    )
import Test.QuickCheck.Monadic
    ( assert, monadicIO, monitor, run )

import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Language.Haskell.TH.Syntax as TH

spec :: Spec
spec = do
    describe "Cardano.Pool.Registry.getStakePoolMetadata specs" $
        around (testServer dataDir) $ do
        it "Loads the example zip" $ \port -> do
            tr <- setupLogging
            res <- getStakePoolMetadata tr (testUrl port) presentPids
            res `shouldBe` Right (map Just presentMetas)

        it "Handles a missing pool" $ \port -> do
            tr <- setupLogging
            res <- getStakePoolMetadata tr (testUrl port) (absentPid:presentPids)
            res `shouldBe` Right (Nothing:map Just presentMetas)
            
        it "Fails with an unavailable HTTP server" $ \_port -> do
            tr <- setupLogging
            let badUrl = "http://localhost:99/master.zip"
            res <- getStakePoolMetadata tr badUrl presentPids
            case res of
                Left (FetchErrorDownload msg) ->
                    head (words msg) `shouldBe` "HttpExceptionRequest"
                _ -> error "expected fetch error but got none"
                
    describe "Cardano.Pool.Registry.getStakePoolMetadata" $
        it "Arbitrary zip files" $
            property prop_getStakePoolMetadata
   
{-------------------------------------------------------------------------------
                                 Test fixtures
-------------------------------------------------------------------------------}

-- | Run a HTTP file server on any free port
testServer :: FilePath -> (Port -> IO a) -> IO a
testServer root = withApplication (pure app)
    where app = staticApp $ defaultWebAppSettings root

dataDir :: FilePath
dataDir = $( TH.LitE . TH.StringL <$>
    makeRelativeToProject "test/data/stake-pool-registry" )

-- | Make a file server URL for the test data file.
-- This file was downloaded from <https://github.com/input-output-hk/testnet-stake-pool-registry/archive/master.zip>
testUrl :: Port -> String
testUrl p = "http://localhost:" <> show p <> "/testnet-stake-pool-registry-master.zip"

presentPids :: [PoolId]
presentPids = map PoolId
    [ "pk1afhcpw2tg7nr2m3wr4x8jaa4dv7d09gnv27kwfxpjyvukwxs8qdqwg85xp"
    , "pk1z4vh8gva25w07x8574uujuveu8gz43fu6qfln3t4prcavrvcphjsk0pdqs"
    ]

presentMetas :: [StakePoolMetadata]
presentMetas =
    [ StakePoolMetadata (unsafeFromText "FST") "https://12345"
        "ed25519_pk15vz9yc5c3upgze8tg5kd7kkzxqgqfxk5a3kudp22hdg0l2za00sq2ufkk7"
    , StakePoolMetadata (unsafeFromText "TICK") "https://12345"
        "ed25519_pk15vz9yc5c3upgze8tg5kd7kkzxqgqfxk5a3kudp22hdg0l2za00sq2ufkk7"
    ]
    
absentPid :: PoolId
absentPid = PoolId "pk192m4ytl5k357e2l666yleuwjlurmf0vxjyh4atxzu5m22q6mexlsp88k7x"

{-------------------------------------------------------------------------------
                                    Property
-------------------------------------------------------------------------------}

data TestCase = TestCase
    { stakePools :: [(PoolId, StakePoolEntry)]
    , poolIds :: [PoolId]
    , topDir :: FilePath
    } deriving (Show, Eq)

data StakePoolEntry
    = Junk
    | Meta StakePoolMetadata
    deriving (Show, Eq)

prop_getStakePoolMetadata :: TestCase -> Property
prop_getStakePoolMetadata tc = monadicIO $ do
  (res, msgs) <- run $ withTestCaseZip tc $ \zipFile ->
      testServer (takeDirectory zipFile) $ \port -> do
          let url = "http://localhost:" <> show port <> "/" <> topDir tc <> ".zip"
          captureLogging $ \tr ->
              getStakePoolMetadata tr url (poolIds tc)
  let expected =
          [ stakePoolEntryMeta =<< lookup p (stakePools tc)
          | p <- poolIds tc ]
  let numDecodeErrors = length (filter isDecodeError msgs)
  let junkEntry (pid, entry) = isJunk entry && pid `elem` poolIds tc
  let numJunkEntries = length (filter junkEntry (stakePools tc))
  monitor $ counterexample $ unlines
      [ "expected = " ++ show expected
      , "actual =   " ++ show res
      , "logs =\n" ++ unlines (map show msgs)
      , "#junk entries =  " ++ show numJunkEntries
      , "#decode errors = " ++ show numDecodeErrors
      ]
  assert $ res == Right expected 
  assert $ numJunkEntries == numDecodeErrors

withTestCaseZip :: TestCase -> (FilePath -> IO a) -> IO a
withTestCaseZip tc action = withSystemTempDirectory "registry-spec" $ \dir -> do
    let zipFile = dir </> topDir tc <.> "zip"
    createArchive zipFile $ forM_ (stakePools tc) $ \(poolId, contents) -> do
        sel <- mkEntrySelector (registryFile (topDir tc) poolId)
        addEntry Deflate (encodeStakePoolEntry contents) sel
    action zipFile
    
registryFile :: FilePath -> PoolId -> FilePath
registryFile top (PoolId poolId) = top </> "registry" </> B8.unpack poolId <.> "json"

{-------------------------------------------------------------------------------
                               Test case helpers
-------------------------------------------------------------------------------}

isDecodeError :: RegistryLog -> Bool
isDecodeError rl = case registryLogMsg rl of
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

{-------------------------------------------------------------------------------
                              Test case generation
-------------------------------------------------------------------------------}

instance Arbitrary TestCase where
    arbitrary = do
        stakePools <- arbitrary
        presentPoolIds <- sublistOf (map fst stakePools)
        absentPoolIds <- arbitrary
        poolIds <- shuffle (presentPoolIds ++ absentPoolIds)
        PathElement topDir <- arbitrary
        pure $ TestCase {stakePools, poolIds, topDir}
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
        <$> arbitrary <*> arbitraryText <*> arbitraryText
      where
        arbitraryText = T.pack <$> arbitrary

instance Arbitrary StakePoolTicker where
    arbitrary = unsafeFromText . T.pack <$> do
        len <- choose (3, 4)
        replicateM len arbitrary

instance Arbitrary PoolId where
    arbitrary = do
        bytes <- infiniteListOf $ elements ['a'..'z']
        return $ PoolId $ B8.pack $ take 32 bytes

newtype PathElement = PathElement FilePath deriving (Show, Eq)

instance Arbitrary PathElement where
    arbitrary = PathElement <$> listOf1 (elements ['a'..'z'])

{-------------------------------------------------------------------------------
                                    Logging
-------------------------------------------------------------------------------}

setupLogging :: IO (Trace IO RegistryLog)
setupLogging = do
    cfg <- defaultConfigStdout
    transformTrace . fst <$> setupTrace_ cfg "RegistrySpec"

withLogging :: ((Trace IO RegistryLog, IO [RegistryLog]) -> IO a) -> IO a
withLogging action = do
    tvar <- newTVarIO []
    let tr = traceInTVarIO tvar
    let unMsg lo = case lo of
            LogMessage msg -> Just msg
            _ -> Nothing
    let getMsgs = mapMaybe (unMsg . loContent) <$> readTVarIO tvar    
    action (tr, getMsgs)

captureLogging :: (Trace IO RegistryLog -> IO a) -> IO (a, [RegistryLog])
captureLogging action = withLogging $ \(tr, getMsgs) -> do
    res <- action tr
    msgs <- getMsgs
    pure (res, msgs)
    
