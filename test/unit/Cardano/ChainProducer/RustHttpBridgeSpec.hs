module Cardano.ChainProducer.RustHttpBridgeSpec (spec) where

import Prelude

import Cardano.ChainProducer
    ( nextBlocks )
import Cardano.ChainProducer.RustHttpBridge
    ( RustBackend, runRustBackend )
import Cardano.ChainProducer.RustHttpBridge.MockNetworkLayer
    ( mockNetworkLayer )
import Cardano.ChainProducer.RustHttpBridge.NetworkLayer
    ( NetworkLayer )
import Cardano.Wallet.Primitive
    ( BlockHeader (..), header )
import Cardano.Wallet.Slotting
    ( SlotId (SlotId) )
import Control.Exception
    ( Exception, throwIO )
import Control.Monad
    ( (<=<) )
import Control.Monad.Except
    ( ExceptT, runExceptT )
import Control.Monad.IO.Class
    ( MonadIO, liftIO )
import Test.Hspec
    ( Spec, SpecWith, beforeAll, describe, it, shouldBe, shouldSatisfy )

spec :: Spec
spec = do
    describe "Getting next blocks with a mock backend" $ do
        beforeAll (pure $ mockNetworkLayer 105 (SlotId 106 1492)) $ do
             getNextBlocksSpec

getNextBlocksSpec :: SpecWith (NetworkLayer IO)
getNextBlocksSpec = do
    it "should get something from the latest epoch" $ \network -> do
        blocks <- runBackend network $ nextBlocks 1000 (SlotId 106 1000)
        -- the number of blocks between slots 1000 and 1492 inclusive
        length blocks `shouldBe` 493
        let hdrs = map header blocks
        map slotNumber hdrs `shouldBe` [1000 .. 1492]
        map epochIndex hdrs `shouldSatisfy` all (== 106)

    it "should get something from an unstable epoch" $ \network -> do
        blocks <- runBackend network $ nextBlocks 1000 (SlotId 105 17000)
        length blocks `shouldBe` 1000

    it "should get from old epochs" $ \network -> do
        blocks <- runBackend network $ nextBlocks 1000 (SlotId 104 10000)
        length blocks `shouldBe` 1000
        map (epochIndex . header) blocks `shouldSatisfy` all (== 104)

    it "should produce no blocks if start slot is after tip" $ \network -> do
        blocks <- runBackend network $ nextBlocks 1000 (SlotId 107 0)
        blocks `shouldBe` []

    it "should work for zero blocks" $ \network -> do
        blocks <- runBackend network $ nextBlocks 0 (SlotId 106 1000)
        blocks `shouldBe` []

unsafeRunExceptT :: (Exception e, MonadIO m) => ExceptT e m a -> m a
unsafeRunExceptT = either (liftIO . throwIO) pure <=< runExceptT

runBackend :: Exception e => NetworkLayer IO -> ExceptT e RustBackend a -> IO a
runBackend network = runRustBackend network . unsafeRunExceptT
