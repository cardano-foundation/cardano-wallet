{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Cardano.Wallet.Api.ServerSpec (spec) where

import Prelude

import Cardano.Api
    ( AnyCardanoEra (..)
    , CardanoEra (..)
    , NetworkId (..)
    , NetworkMagic (..)
    )
import Cardano.BM.Trace
    ( nullTracer
    )
import Cardano.Slotting.Slot
    ( EpochNo (..)
    )
import Cardano.Wallet.Api.Http.Shelley.Server
    ( IsServerError (..)
    , Listen (..)
    , ListenError (..)
    , getNetworkClock
    , getNetworkInformation
    , liftHandler
    , withListeningSocket
    )
import Cardano.Wallet.Api.Types
    ( ApiNetworkInformation (..)
    , ApiWalletMode (..)
    )
import Cardano.Wallet.DB.Errors
    ( ErrNoSuchWallet (..)
    )
import Cardano.Wallet.DummyTarget.Primitive.Types
    ( dummyNetworkLayer
    )
import Cardano.Wallet.Network
    ( NetworkLayer (..)
    )
import Cardano.Wallet.Primitive.Slotting
    ( PastHorizonException
    , TimeInterpreter
    , currentRelativeTime
    , mkTimeInterpreter
    , neverFails
    )
import Cardano.Wallet.Primitive.SyncProgress
    ( SyncTolerance (..)
    )
import Cardano.Wallet.Primitive.Types
    ( Block (..)
    , SlotNo (..)
    , StartTime (..)
    )
import Cardano.Wallet.Unsafe
    ( unsafeFromText
    )
import Control.Monad
    ( void
    )
import Control.Monad.Trans.Except
    ( ExceptT (..)
    , throwE
    )
import Data.Either
    ( isLeft
    )
import Data.Maybe
    ( isJust
    , isNothing
    )
import Data.SOP.Counting
    ( exactlyOne
    )
import Data.Time.Clock
    ( addUTCTime
    , getCurrentTime
    )
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
    ( RelativeTime (..)
    , mkSlotLength
    )
import Ouroboros.Consensus.Config.SecurityParam
    ( SecurityParam (..)
    )
import Servant.Server
    ( ServerError (..)
    , runHandler
    )
import Test.Hspec
    ( Spec
    , describe
    , it
    , pendingWith
    , shouldBe
    , shouldReturn
    , shouldSatisfy
    )
import Test.QuickCheck.Modifiers
    ( NonNegative (..)
    )
import Test.QuickCheck.Monadic
    ( PropertyM
    , assert
    , monadicIO
    , monitor
    , run
    )
import Test.QuickCheck.Property
    ( counterexample
    , property
    )
import Test.Utils.Platform
    ( skipOnWindows
    )
import UnliftIO.Async
    ( concurrently_
    , race_
    )
import UnliftIO.Concurrent
    ( threadDelay
    )

import qualified Cardano.Wallet.Primitive.SyncProgress as S
import qualified Cardano.Wallet.Read as Read
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import qualified Ouroboros.Consensus.HardFork.History.EraParams as HF
import qualified Ouroboros.Consensus.HardFork.History.Qry as HF
import qualified Ouroboros.Consensus.HardFork.History.Summary as HF

spec :: Spec
spec = do
    serverSpec
    networkInfoSpec
    errorHandlingSpec

serverSpec :: Spec
serverSpec = describe "API Server" $ do
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

networkInfoSpec :: Spec
networkInfoSpec = describe "getNetworkInformation" $ do
    it "doesn't return 500 when the time interpreter horizon is behind\
       \ the current time" $ property $ \(gap' ::(NonNegative Int)) ->
        monadicIO $ do
        let gap = fromRational $ toRational $ getNonNegative gap'
        st <- run $ StartTime . ((negate gap) `addUTCTime`) <$> getCurrentTime
        let ti = forkInterpreter st
        now <- currentRelativeTime ti
        let nodeTip' = SlotNo 0
        let nl = mockNetworkLayer nodeTip' ti now
        Right info <- run
            $ runHandler
            $ getNetworkInformation (Testnet $ NetworkMagic 1) nl Node

        --  0              20
        --  *               |        *
        --  Node tip     Horizon   Network Tip
        --  <------------------------>
        --            gap
        --
        --  20 = epoch length = 10*k
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

    mockNetworkLayer
        :: SlotNo
        -> TimeInterpreter (ExceptT PastHorizonException IO)
        -> RelativeTime
        -> NetworkLayer IO Block
    mockNetworkLayer sl ti relativeTime =
        dummyNetworkLayer
            { currentNodeEra = pure $ AnyCardanoEra MaryEra
            , currentNodeTip = pure $
                Read.BlockTip
                    { slotNo = Read.SlotNo $ fromIntegral $ unSlotNo sl
                    , headerHash = mockHash
                    , blockNo = Read.BlockNo $ fromIntegral $ unSlotNo sl
                    }
            , timeInterpreter = ti
            , syncProgress = \slot ->
                S.syncProgress
                    (SyncTolerance 10)
                    (neverFails "syncProgress" ti)
                    slot
                    relativeTime
            }
      where
        mockHash :: Read.RawHeaderHash
        mockHash = Read.mockRawHeaderHash 0

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
        in mkTimeInterpreter nullTracer startTime (pure int)

errorHandlingSpec :: Spec
errorHandlingSpec = describe "liftHandler and toServerError" $ do
    let testWalletHandler
            :: IsServerError e
            => ExceptT e IO a
            -> IO (Either ServerError a)
        testWalletHandler = runHandler . liftHandler

    -- Check that an example error handled by the wallet API server produces a
    -- structured JSON error message response.
    it "ErrNoSuchWallet" $ do
        let handler :: ExceptT ErrNoSuchWallet IO ()
            handler = throwE $ ErrNoSuchWallet wid
            wid = unsafeFromText "0000000000000000000000000000000000000000"
        res <- testWalletHandler handler
        res `shouldSatisfy` isLeft
        let Left actualErr = res
        errHTTPCode actualErr `shouldBe` 404
        errReasonPhrase actualErr `shouldBe`
            "Not Found"
        BL.toStrict (errBody actualErr) `shouldSatisfy`
            (B8.isInfixOf "no_such_wallet")
        errHeaders actualErr `shouldBe`
            [("Content-Type","application/json;charset=utf-8")]

    it "Unhandled exception" $ do
        pendingWith "TODO: ADP-641 catch all exceptions in application"
        let expectedErr = ServerError
                { errHTTPCode = 500
                , errReasonPhrase = "Internal Server Error"
                , errBody = mconcat
                    [ "{\"code\":\"internal_server_error\","
                    , "\"message\":\"Something went wrong\"}" ]
                , errHeaders =
                    [("Content-Type","application/json;charset=utf-8")]
                }

        runHandler (getNetworkClock (error "bomb") True)
            `shouldReturn` Left expectedErr
