{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.Wallet.BlockSyncerSpec
    ( spec
    ) where


import Prelude

import Cardano.Wallet.BlockSyncer
    ( BlockHeadersConsumed (..), tickingFunction )
import Cardano.Wallet.Primitive
    ( Block (..), BlockHeader (..), EpochId (..), Hash (..), SlotId (..) )
import Control.Concurrent
    ( forkIO, killThread )
import Control.Concurrent.MVar
    ( MVar, modifyMVar_, newEmptyMVar, newMVar, putMVar, takeMVar )
import Control.Monad
    ( foldM )
import Control.Monad.IO.Class
    ( liftIO )
import Data.Functor
    ( ($>) )
import Data.Time.Units
    ( Millisecond, fromMicroseconds )
import Test.Hspec
    ( Spec, describe, it, shouldReturn )
import Test.QuickCheck
    ( Arbitrary (..), Property, property, withMaxSuccess )
import Test.QuickCheck.Gen
    ( Gen, choose )
import Test.QuickCheck.Monadic
    ( monadicIO )

import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Write as CBOR
import qualified Data.List as L


spec :: Spec
spec = do
    describe "Block syncer downloads blocks properly" $ do
        it "Check ticking function when blocks are sent"
            (withMaxSuccess 10 $ property tickingFunctionTest)


{-------------------------------------------------------------------------------
                                    Test Logic
-------------------------------------------------------------------------------}

tickingFunctionTest
    :: (TickingTime, Blocks)
    -> Property
tickingFunctionTest (TickingTime tickTime, Blocks blocks) =
    monadicIO $ liftIO $ do
        (readerChan, reader) <- mkReader
        (writerChan, writer) <- mkWriter blocks
        waitFor writerChan $
            tickingFunction writer reader tickTime (BlockHeadersConsumed [])
        takeMVar readerChan `shouldReturn` L.nub (reverse $ mconcat blocks)

waitFor
    :: MVar ()
    -> IO ()
    -> IO ()
waitFor done action = do
    threadId <- forkIO action
    _ <- takeMVar done
    killThread threadId

mkWriter
    :: [[a]]
    -> IO (MVar (), IO [a])
mkWriter xs0 = do
    ref <- newMVar xs0
    done <- newEmptyMVar
    return
        ( done
        , takeMVar ref >>= \case
            [] -> putMVar done () $> []
            h:q -> putMVar ref q $> h
        )

mkReader
    :: IO (MVar [a], a -> IO ())
mkReader = do
    ref <- newMVar []
    return
        ( ref
        , \x -> modifyMVar_ ref $ return . (x :)
        )

{-------------------------------------------------------------------------------
                            Arbitrary Instances
-------------------------------------------------------------------------------}


newtype TickingTime = TickingTime Millisecond
    deriving (Show)

instance Arbitrary TickingTime where
    -- No shrinking
    arbitrary = do
        tickTime <- fromMicroseconds . (* 1000) <$> choose (50, 100)
        return $ TickingTime tickTime


newtype Blocks = Blocks [[Block]]
    deriving Show

instance Arbitrary Blocks where
    -- No Shrinking
    arbitrary = do
        n <- arbitrary
        let h0 = BlockHeader 1 0 (Hash "initial block")
        let blocks = map snd $ take n $ iterate next
                ( blockHeaderHash h0
                , Block h0 mempty
                )
        mapM duplicateMaybe blocks >>= fmap Blocks . groups . mconcat
      where
        next :: (Hash "BlockHeader", Block) -> (Hash "BlockHeader", Block)
        next (prev, b) =
            let
                epoch = epochIndex (header b)
                slot = slotNumber (header b) + 1
                h = BlockHeader epoch slot prev
            in
                (blockHeaderHash h, Block h mempty)

        blockHeaderHash :: BlockHeader -> Hash "BlockHeader"
        blockHeaderHash =
            Hash . CBOR.toStrictByteString . encodeBlockHeader
          where
            encodeBlockHeader (BlockHeader (EpochId epoch) (SlotId slot) prev) =
                mempty
                    <> CBOR.encodeListLen 3
                    <> CBOR.encodeWord64 epoch
                    <> CBOR.encodeWord16 slot
                    <> CBOR.encodeBytes (getHash prev)


-- | Construct arbitrary groups of elements from a given list.
--
-- >>> generate $ groups [0,1,2,3,4,5,6,7,8,9]
-- [[0,1],[2,3],[4,5,6],[7,8,9]]
--
--
-- >>> generate $ groups [0,1,2,3,4,5,6,7,8,9]
-- [[],[0],[1,2,3,4,5,6,7,8],[9]]
--
groups :: [a] -> Gen [[a]]
groups = fmap reverse . foldM arbitraryGroup [[]]
  where
    arbitraryGroup :: [[a]] -> a -> Gen [[a]]
    arbitraryGroup [] _ = return [] -- Can't happen with the given initial value
    arbitraryGroup (grp:rest) a = do
        choose (1 :: Int, 3) >>= \case
            1 -> return $ [a]:grp:rest
            _ -> return $ (grp ++ [a]):rest

-- | Generate a singleton or a pair from a given element.
--
-- >>> generate $ duplicateMaybe 14
-- [14]
--
-- >>> generate $ duplicateMaybe 14
-- [14, 14]
--
duplicateMaybe :: a -> Gen [a]
duplicateMaybe a = do
    predicate <- arbitrary
    if predicate then return [a, a] else return [a]
