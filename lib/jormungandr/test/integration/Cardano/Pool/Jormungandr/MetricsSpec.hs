{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Cardano.Pool.Jormungandr.MetricsSpec where

import Prelude

import Cardano.BM.Trace
    ( nullTracer )
import Cardano.Pool.DB
    ( DBLayer (..) )
import Cardano.Pool.Jormungandr.Metrics
    ( Block, monitorStakePools )
import Cardano.Wallet.Jormungandr
    ( toSPBlock )
import Cardano.Wallet.Jormungandr.Compatibility
    ( Jormungandr )
import Cardano.Wallet.Jormungandr.Launch
    ( withConfig )
import Cardano.Wallet.Jormungandr.Network
    ( JormungandrBackend (..), withJormungandr, withNetworkLayer )
import Cardano.Wallet.Network
    ( NetworkLayer, timeInterpreter )
import Cardano.Wallet.Primitive.Types
    ( BlockHeader (..)
    , GenesisParameters (..)
    , NetworkParameters (..)
    , SlotNo
    , SlottingParameters (..)
    )
import Control.Concurrent
    ( forkIO, killThread )
import Control.Exception
    ( bracket, throwIO )
import Control.Monad
    ( void )
import Data.List
    ( isPrefixOf, (\\) )
import Fmt
    ( pretty )
import System.FilePath
    ( (</>) )
import System.IO.Temp
    ( withSystemTempDirectory )
import Test.Hspec
    ( Spec, around, expectationFailure, it )
import Test.Integration.Framework.DSL
    ( eventually )

import qualified Cardano.Pool.DB.Sqlite as Pool

spec :: Spec
spec = around setup $ do
    it "a monitorStakePools thread with db, if killed and restarted, catches up\
        \ with one that stays running" $ \(nl, (block0, k), referenceDB) ->
        withDB "db.sqlite" nl $ \db ->
            withMonitorStakePoolsThread (block0, k) nl db $ \worker -> do
                eventually "db `shouldContainSameSlotsAs` referenceDB and db shouldNotBeEmpty" $ do
                    db `shouldContainSameSlotsAs` referenceDB
                    shouldNotBeEmpty db

                killThread worker

                eventually "shouldBeBehindBy 3 db referenceDB" $
                    shouldBeBehindBy 3 db referenceDB

                withMonitorStakePoolsThread (block0, k) nl db $ \_ ->
                    eventually "db `shouldContainSameSlotsAs` referenceDB" $
                        db `shouldContainSameSlotsAs` referenceDB
  where
    shouldBeBehindBy n db ref = do
        refSlots <- readSlots ref
        dbSlots <- readSlots db
        if length (refSlots \\ dbSlots) >= n
            && dbSlots `isPrefixOf` refSlots
            -- NOTE: The isPrefixOf-check is arguably not vital, but also
            -- expected since the self-node we're testing against shouldn't
            -- rollback.
        then return ()
        else expectationFailure $ mconcat
            [ "The db with slots:\n    "
            , pretty dbSlots
            , "\nshould be behind the reference db containing:\n    "
            , pretty refSlots
            , "\nbut isn't."
            ]

    shouldContainSameSlotsAs db ref = do
        refSlots <- readSlots ref
        dbSlots <- readSlots db
        if dbSlots == refSlots
        then return ()
        else expectationFailure $ mconcat
            [ "The db containing slots:\n    "
            , pretty dbSlots
            , "\nshould contain the same slots as the reference db:\n    "
            , pretty refSlots
            , "\nbut they are different."
            ]

    shouldNotBeEmpty db = do
        dbSlots <- readSlots db
        if not $ null dbSlots
        then return ()
        else expectationFailure "The db shouldn't be empty"

    -- | Intended as "read all pool productions from the db". Oldest first, the
    -- tip last. In practice only reads the 10000 latest, but in this module we
    -- expect the dbs to contain ~10 slots, so it doesn't matter.
    readSlots :: DBLayer IO -> IO [SlotNo]
    readSlots DBLayer{atomically, readPoolProductionCursor} =
        map slotNo <$> atomically (readPoolProductionCursor 10000)

    withMonitorStakePoolsThread (block0, k) nl db action = do
        bracket
            (forkIO $ void $ monitorStakePools nullTracer (block0, k)
                (nl :: NetworkLayer IO Jormungandr Block) db)
            killThread
            action

    setup cb = withConfig $ \cfg -> do
        let tr = nullTracer
        e <- withJormungandr tr cfg $ \cp ->
            withNetworkLayer tr (UseRunning cp) $ \case
                Right (_, (b0, np), nl) -> withDB "reference.sqlite" nl $ \db -> do
                    let gp = genesisParameters np
                    let el = getEpochLength $ slottingParameters np
                    let nl' = toSPBlock el <$> nl
                    let k = getEpochStability gp
                    let block0 = toSPBlock el b0
                    withMonitorStakePoolsThread (block0, k) nl' db $ \_workerThread ->
                        cb (nl', (block0, k), db)
                Left e -> throwIO e
        either throwIO (\_ -> return ()) e

    withDB name nl action =
        withSystemTempDirectory "stake-pool" $ \dir -> do
            let dbFile = dir </> name
            Pool.withDBLayer nullTracer (Just dbFile) (timeInterpreter nl) action
