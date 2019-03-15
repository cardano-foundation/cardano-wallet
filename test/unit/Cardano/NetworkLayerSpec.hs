{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.NetworkLayerSpec
    ( spec
    ) where


import Prelude

import Cardano.NetworkLayer
    ( TickResult (..), tick )
import Cardano.Wallet.Primitive
    ( Block (..), BlockHeader (..), Hash (..), SlotId (..) )
import Control.Concurrent
    ( forkIO, killThread )
import Control.Concurrent.MVar
    ( MVar, modifyMVar_, newEmptyMVar, newMVar, putMVar, takeMVar )
import Control.Monad
    ( foldM )
import Control.Monad.IO.Class
    ( liftIO )
import Data.Foldable
    ( toList )
import Data.Functor
    ( ($>) )
import Data.Time.Units
    ( Millisecond, fromMicroseconds )
import Data.Word
    ( Word8 )
import Test.Hspec
    ( Spec, describe, it, shouldReturn )
import Test.QuickCheck
    ( Arbitrary (..), Gen, Property, checkCoverage, choose, cover )
import Test.QuickCheck.Monadic
    ( monadicIO )

import qualified Data.ByteString.Char8 as B8


spec :: Spec
spec = do
    describe "tick terminates an passes blocks from a source to a consumer" $ do
        it "Check ticking function when blocks are sent"
            (checkCoverage tickingFunctionTest)


{-------------------------------------------------------------------------------
                                    Test Logic
-------------------------------------------------------------------------------}

tickingFunctionTest
    :: (TickingTime, Blocks)
    -> Property
tickingFunctionTest (TickingTime tickTime, Blocks blocks) =
    cover 80 (sum (length <$> blocks) /= 0) "Non empty blocks" prop
  where
    prop = monadicIO $ liftIO $ do
        (readerChan, reader) <- mkReader
        (writerChan, writer) <- mkWriter
        waitFor writerChan $ tick writer reader tickTime blocks
        takeMVar readerChan `shouldReturn` reverse (flatten blocks)

waitFor
    :: MVar ()
    -> IO ()
    -> IO ()
waitFor done action = do
    threadId <- forkIO action
    _ <- takeMVar done
    killThread threadId

mkWriter
    :: st ~ [TickResult [a]] => IO (MVar (), st -> IO (TickResult [a], st))
mkWriter = do
    done <- newEmptyMVar
    return
        ( done
        , \case
            st@[] -> putMVar done () $> (Sleep, st)
            h:st -> return (h, st)
        )

mkReader
    :: IO (MVar [a], a -> IO ())
mkReader = do
    ref <- newMVar []
    return
        ( ref
        , \x -> modifyMVar_ ref $ return . (x :)
        )

flatten :: [TickResult [Block]] -> [Block]
flatten = concat . concatMap toList

{-------------------------------------------------------------------------------
                            Arbitrary Instances
-------------------------------------------------------------------------------}


newtype TickingTime = TickingTime Millisecond
    deriving (Show)

instance Arbitrary TickingTime where
    -- No shrinking
    arbitrary = do
        tickTime <- fromMicroseconds . (* 1000) <$> choose (1, 3)
        return $ TickingTime tickTime


newtype Blocks = Blocks [TickResult [Block]]
    deriving Show

instance Arbitrary Blocks where
    -- No Shrinking
    arbitrary = do
        n <- fromIntegral . (`mod` 42) <$> arbitrary @Word8
        let h0 = BlockHeader (SlotId 1 0) (Hash "initial block")
        let b0 = (blockHeaderHash h0, Block h0 mempty)
        gs <- groups (map snd $ take n $ iterate next b0)
        Blocks <$> tickResults gs
      where
        next :: (Hash "BlockHeader", Block) -> (Hash "BlockHeader", Block)
        next (prev, b) =
            let
                slot = slotId (header b)
                h = BlockHeader (succ slot) prev
            in
                (blockHeaderHash h, Block h mempty)

        blockHeaderHash :: BlockHeader -> Hash "BlockHeader"
        blockHeaderHash (BlockHeader (SlotId e s) _) =
            Hash (B8.pack (show e <> show s))

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

-- Converts list of chunks to TickResult and inserts some Sleeps.
tickResults :: [[a]] -> Gen [TickResult [a]]
tickResults [] = pure []
tickResults (c:cs) = do
    choose (1 :: Int, 3) >>= \case
        1 -> do
            rs <- tickResults cs
            return (GotChunk c:rs)
        _ -> do
            rs <- tickResults (c:cs)
            return (Sleep:rs)
