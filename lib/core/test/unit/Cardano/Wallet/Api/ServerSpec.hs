{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.Api.ServerSpec (spec) where

import Prelude

import Cardano.Api
    ( AnyCardanoEra (..), CardanoEra (..) )
import Cardano.BM.Trace
    ( nullTracer )
import Cardano.Slotting.Slot
    ( EpochNo (..) )
import Cardano.Wallet
    ( ErrNoSuchWallet (..) )
import Cardano.Wallet.Api.Server
    ( IsServerError (..)
    , Listen (..)
    , ListenError (..)
    , getNetworkClock
    , getNetworkInformation
    , liftHandler
    , listStakeKeys'
    , withListeningSocket
    )
import Cardano.Wallet.Api.Types
    ( ApiNetworkInformation (..)
    , ApiStakeKeys (..)
    , ApiWalletDelegation (..)
    , ApiWalletDelegationNext (..)
    , ApiWalletDelegationStatus (..)
    , coinToQuantity
    )
import Cardano.Wallet.DummyTarget.Primitive.Types
    ( dummyNetworkLayer )
import Cardano.Wallet.Network
    ( NetworkLayer (..) )
import Cardano.Wallet.Primitive.Slotting
    ( PastHorizonException, TimeInterpreter, mkTimeInterpreter )
import Cardano.Wallet.Primitive.SyncProgress
    ( mkSyncTolerance )
import Cardano.Wallet.Primitive.Types
    ( Block (..), BlockHeader (..), SlotNo (..), StartTime (..) )
import Cardano.Wallet.Primitive.Types.Address
    ( Address (..) )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..) )
import Cardano.Wallet.Primitive.Types.Coin.Gen
    ( genCoinAny, shrinkCoinAny )
import Cardano.Wallet.Primitive.Types.Hash
    ( Hash (..) )
import Cardano.Wallet.Primitive.Types.RewardAccount
    ( RewardAccount (..) )
import Cardano.Wallet.Primitive.Types.UTxO
    ( UTxO, balance )
import Cardano.Wallet.Primitive.Types.UTxOIndex
    ( fromUTxO, toUTxO )
import Cardano.Wallet.Primitive.Types.UTxOIndex.Gen
    ( genUTxOIndexSmall, shrinkUTxOIndexSmall )
import Cardano.Wallet.Unsafe
    ( unsafeFromText )
import Control.Monad
    ( void )
import Control.Monad.Trans.Except
    ( ExceptT (..), throwE )
import Data.Either
    ( isLeft )
import Data.Functor.Identity
    ( Identity (..) )
import Data.Generics.Internal.VL.Lens
    ( view )
import Data.Generics.Labels
    ()
import Data.Map
    ( Map )
import Data.Maybe
    ( isJust, isNothing )
import Data.Quantity
    ( Quantity (..) )
import Data.Set
    ( Set )
import Data.Time.Clock
    ( NominalDiffTime, addUTCTime, getCurrentTime )
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
import Numeric.Natural
    ( Natural )
import Ouroboros.Consensus.BlockchainTime.WallClock.Types
    ( RelativeTime (..), mkSlotLength )
import Ouroboros.Consensus.Config.SecurityParam
    ( SecurityParam (..) )
import Ouroboros.Consensus.Util.Counting
    ( exactlyOne )
import Servant.Server
    ( ServerError (..), runHandler )
import Test.Hspec
    ( Spec, describe, it, pendingWith, shouldBe, shouldReturn, shouldSatisfy )
import Test.QuickCheck
    ( Arbitrary (..)
    , NonNegative (..)
    , Property
    , checkCoverage
    , classify
    , cover
    , elements
    , (===)
    )
import Test.QuickCheck.Monadic
    ( PropertyM, assert, monadicIO, monitor, run )
import Test.QuickCheck.Property
    ( counterexample, property )
import Test.Utils.Platform
    ( skipOnWindows )
import UnliftIO.Async
    ( concurrently_, race_ )
import UnliftIO.Concurrent
    ( threadDelay )

import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import qualified Data.Set as Set
import qualified Ouroboros.Consensus.HardFork.History.EraParams as HF
import qualified Ouroboros.Consensus.HardFork.History.Qry as HF
import qualified Ouroboros.Consensus.HardFork.History.Summary as HF

spec :: Spec
spec = do
    serverSpec
    networkInfoSpec
    errorHandlingSpec
    listStakeKeysSpec

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
        let (gap :: NominalDiffTime) = fromRational $ toRational $ getNonNegative gap'
        st <- run $ StartTime . ((negate gap) `addUTCTime`)
                <$> getCurrentTime
        let ti = forkInterpreter st
        let nodeTip' = SlotNo 0
        let nl = mockNetworkLayer nodeTip' ti
        let tolerance = mkSyncTolerance 5
        Right info <- run $ runHandler $ getNetworkInformation tolerance nl

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
        -> NetworkLayer IO Block
    mockNetworkLayer sl ti = dummyNetworkLayer
        { currentNodeEra = return $ AnyCardanoEra MaryEra
        , currentNodeTip = return $
                BlockHeader
                    sl
                    (Quantity $ fromIntegral $ unSlotNo sl)
                    (Hash "header hash")
                    (Hash "prevHeaderHash")
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

listStakeKeysSpec :: Spec
listStakeKeysSpec = do
    describe "Listing stake keys" $ do
        it "no rewards => sum of all keys' stake == utxo balance" $ do
            property prop_listStakeKeysBalance
        it "ours and foreign keys are disjoint" $ do
            property prop_listStakeKeysDisjoint

prop_listStakeKeysDisjoint
    :: UTxO
    -> Set RewardAccount
    -> (Map RewardAccount (Maybe Coin))
    -> Property
prop_listStakeKeysDisjoint utxo ours' m =
    let
        -- Build a list to pass listStakeKeys'
        ours = zipWith (\ix acc -> (acc, ix, noDelegation))
            [(0::Natural) ..]
            (Set.toList ours')

        ApiStakeKeys{_ours, _foreign, _none } = runIdentity $
            listStakeKeys' utxo accountOfAddress (const $ pure m) ours

        ourKeys = keys _ours
        foreignKeys = keys _foreign
    in
        classify (length ourKeys > 1 && length foreignKeys > 1) "non-trivial" $
            keys _ours `disjoint` keys _foreign
  where
    labeledCheck notF f a b = counterexample
        (show a <> " " <> notF <> " " <> show b)
        (property $ f a b)

    disjoint = labeledCheck "is not disjoint from" Set.disjoint

    keys = Set.fromList . map (view #_key)

prop_listStakeKeysBalance
    :: UTxO
    -> Set RewardAccount
    -> Property
prop_listStakeKeysBalance utxo ours' = checkCoverage $
    let
        -- Build a list to pass listStakeKeys'
        --
        -- Passing the same account twice in the list would break the
        -- property, but protecting against this in the implementation of
        -- listStakeKeys' might not be worth it.
        ours = zipWith (\ix acc -> (acc, ix, noDelegation))
            [(0::Natural) ..]
            (Set.toList ours')
        ApiStakeKeys{_ours, _foreign, _none } = runIdentity $
            listStakeKeys' utxo accountOfAddress (const $ pure mempty) ours

        ourKeys = keys _ours
        foreignKeys = keys _foreign
        isNonTrivial =
            length ourKeys > 1
            && length foreignKeys > 1
            && totalStake [_none] > 0

    in
       cover 35 isNonTrivial "non-trivial" $
            ((totalStake _ours) +
                (totalStake _foreign) +
                (totalStake [_none]))
                ===
            getQuantity (coinToQuantity (TokenBundle.getCoin (balance utxo)))
  where
    keys = Set.fromList . map (view #_key)
    totalStake = sum . map (getQuantity . view (#_stake))

-- | Just something to pass listStakeKeys
noDelegation :: ApiWalletDelegation
noDelegation = ApiWalletDelegation
    (ApiWalletDelegationNext NotDelegating Nothing Nothing)
    []

accountOfAddress :: Address -> Maybe RewardAccount
accountOfAddress (Address bytes) =
    let
        str = B8.unpack bytes
        char = last str
    in
        -- Leveraging the fact that addresses from genUTxOIndexSmall are
        -- "ADDR{0 .. 7}"
        if char == '7'
        then Nothing
        else Just $ RewardAccount $ B8.pack [char]

instance Arbitrary UTxO where
    arbitrary = toUTxO <$> genUTxOIndexSmall
    shrink = map toUTxO . shrinkUTxOIndexSmall . fromUTxO

instance Arbitrary Natural where
    arbitrary = fromIntegral . getNonNegative @Int <$> arbitrary
    shrink = map (fromIntegral @Int . getNonNegative)
        . shrink
        . NonNegative . fromIntegral

instance Arbitrary Coin where
    arbitrary = genCoinAny
    shrink = shrinkCoinAny

-- In listStakeKey properties, this instance will be used to generate the set of
-- our keys.
--
-- The UTxO will contain stake keys ['0' .. '6'], so we want this instance to
-- generate both elements from and outside this set.
instance Arbitrary RewardAccount where
    arbitrary = RewardAccount . B8.singleton
        <$> elements (['0' .. '7'] ++ ['a' .. 'z'])
