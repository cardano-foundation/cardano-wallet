{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Wallet.Api.ServerSpec (spec) where

import Prelude

import Cardano.Slotting.Slot
    ( EpochNo (..) )
import Cardano.Wallet.Api.Server
    ( Listen (..)
    , ListenError (..)
    , getNetworkInformation
    , withListeningSocket
    )
import Cardano.Wallet.Api.Types
    ( ApiNetworkInformation (..) )
import Cardano.Wallet.Network
    ( NetworkLayer (..) )
import Cardano.Wallet.Primitive.Slotting
    ( TimeInterpreter, mkTimeInterpreter )
import Cardano.Wallet.Primitive.SyncProgress
    ( mkSyncTolerance )
import Cardano.Wallet.Primitive.Types
    ( Block (..), BlockHeader (..), SlotNo (..), StartTime (..) )
import Cardano.Wallet.Primitive.Types.Hash
    ( Hash (..) )
import Control.Concurrent
    ( threadDelay )
import Control.Concurrent.Async
    ( concurrently_, race_ )
import Control.Exception
    ( throwIO )
import Control.Monad
    ( void )
import Data.Maybe
    ( isJust, isNothing )
import Data.Quantity
    ( Quantity (..) )
import Data.Time.Clock
    ( addUTCTime, getCurrentTime )
import Network.Socket
    ( Family (..)
    , SockAddr (..)
    , SocketType (..)
    , accept
    , connect
    , defaultProtocol
    , getSocketName
    , socket
    , tupleToHostAddress
    )
import Ouroboros.Consensus.BlockchainTime.WallClock.Types
    ( RelativeTime (..), mkSlotLength )
import Ouroboros.Consensus.Config.SecurityParam
    ( SecurityParam (..) )
import Ouroboros.Consensus.Util.Counting
    ( exactlyOne )
import Servant.Server.Internal.Handler
    ( runHandler )
import Test.Hspec
    ( Spec, describe, it, shouldBe, shouldReturn )
import Test.QuickCheck.Modifiers
    ( NonNegative (..) )
import Test.QuickCheck.Monadic
    ( PropertyM, assert, monadicIO, monitor, run )
import Test.QuickCheck.Property
    ( counterexample, property )
import Test.Utils.Windows
    ( skipOnWindows )

import qualified Ouroboros.Consensus.HardFork.History.EraParams as HF
import qualified Ouroboros.Consensus.HardFork.History.Qry as HF
import qualified Ouroboros.Consensus.HardFork.History.Summary as HF

spec :: Spec
spec = describe "API Server" $ do
    let lo = tupleToHostAddress (0x7f, 0, 0, 1)

    it "binds to the local interface" $ do
        withListeningSocket "127.0.0.1" ListenOnRandomPort $ \case
            Right (port, sock) -> do
                getSocketName sock `shouldReturn`
                    SockAddrInet (fromIntegral port) lo
            Left e -> fail (show e)

    it "can bind on any interface" $ do
        withListeningSocket "0.0.0.0" ListenOnRandomPort $ \case
            Right (port, sock) -> do
                getSocketName sock `shouldReturn`
                    SockAddrInet (fromIntegral port) 0
            Left e -> fail (show e)

    it "listens on the local interface" $ do
        let
            client port = do
                sock <- socket AF_INET Stream defaultProtocol
                connect sock $ SockAddrInet (fromIntegral port) lo

            server sock = do
                threadDelay 1_000_000
                void $ accept sock

            timeout = do
                threadDelay 2_000_000
                fail "test case timed out"

        withListeningSocket "127.0.0.1" ListenOnRandomPort $ \case
            Right (port, sock) -> race_ timeout $
                concurrently_ (client port) (server sock)
            Left e -> fail (show e)

    -- assuming there is no host "patate"
    it "handles bad host name" $ do
        withListeningSocket "patate" ListenOnRandomPort $ \res ->
            res `shouldBe` Left (ListenErrorHostDoesNotExist "patate")

    -- can't bind to link-local IPv6 address
    it "handles invalid address" $ do
        withListeningSocket "fe80::90c2:786f:431:b721" ListenOnRandomPort $ \res ->
            res `shouldBe` Left (ListenErrorInvalidAddress "fe80::90c2:786f:431:b721")

    -- assuming we are not running the tests as root
    it "handles privileged ports" $ do
        skipOnWindows "Impossible to uniquely detect this error case"
        withListeningSocket "127.0.0.1" (ListenOnPort 23) $ \res ->
            res `shouldBe` Left ListenErrorOperationNotPermitted

    it "handles port in use" $ do
        skipOnWindows "Windows permits listening on same port multiple times"
        withListeningSocket "127.0.0.1" ListenOnRandomPort $ \case
            Right (port, _) ->
                withListeningSocket "127.0.0.1" (ListenOnPort port) $ \res ->
                    res `shouldBe` Left (ListenErrorAddressAlreadyInUse (Just port))
            Left e -> fail (show e)

    describe "getNetworkInformation" $ do
        it "doesn't return 500 when the time interpreter horizon is behind\
           \ the current time" $ property $ \(gap' ::(NonNegative Int)) ->
            monadicIO $ do
            let gap = fromRational $ toRational $ getNonNegative gap'
            st <- run $ StartTime . ((negate gap) `addUTCTime`)
                    <$> getCurrentTime
            let ti = either throwIO pure . forkInterpreter st
            let nodeTip' = SlotNo 0
            let nl = dummyNetworkLayer nodeTip' ti
            let tolerance = mkSyncTolerance 5
            Right info <- run $ runHandler $ getNetworkInformation tolerance nl

            -- 0              20
            -- *               |        *
            -- Node tip     Horizon   Network Tip
            -- <------------------------>
            --           gap
            --
            -- 20 = epoch length = 10*k
            if gap >= 20
            then do
                assertWith "networkTip is Nothing" $ isNothing $ networkTip info
                assertWith "nextEpoch is Nothing" $ isNothing $ nextEpoch info
            else do
                assertWith "networkTip is Just " $ isJust $ networkTip info
                assertWith "nextEpoch is Just" $ isJust $ nextEpoch info

  where
    assertWith :: String -> Bool -> PropertyM IO ()
    assertWith lbl condition = do
        let flag = if condition then "✓" else "✗"
        monitor (counterexample $ lbl <> " " <> flag)
        assert condition

    dummyNetworkLayer :: SlotNo -> TimeInterpreter IO -> NetworkLayer IO () Block
    dummyNetworkLayer sl ti = NetworkLayer
        { nextBlocks = error "nextBlocks: not implemented"
        , initCursor = error "initCursor: not implemented"
        , destroyCursor = error "destroyCursor: not implemented"
        , cursorSlotNo = error "cursorSlotNo: not implemented"
        , currentNodeTip = return $
                BlockHeader
                    sl
                    (Quantity $ fromIntegral $ unSlotNo sl)
                    (Hash "header hash")
                    (Hash "prevHeaderHash")
        , watchNodeTip = error "todo"
        , getProtocolParameters = error "getProtocolParameters: not implemented"
        , postTx = error "postTx: not implemented"
        , stakeDistribution = error "stakeDistribution: not implemented"
        , getAccountBalance = error "getAccountBalance: not implemented"
        , timeInterpreter = ti
        }

    forkInterpreter startTime =
        let
            start = HF.initBound
            end = HF.Bound
                    (RelativeTime 20)
                    (SlotNo 20)
                    (EpochNo 1)

            era1Params = HF.defaultEraParams (SecurityParam 2) (mkSlotLength 1)
            summary = HF.summaryWithExactly $ exactlyOne $
                HF.EraSummary start (HF.EraEnd end) era1Params
            int = HF.mkInterpreter summary
        in mkTimeInterpreter startTime int
