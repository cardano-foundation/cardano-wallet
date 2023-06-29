-- | Demonstration of the light mode
module Demo where

import Control.Monad
    ( forM_
    )
import Control.Monad.IO.Class
    ( MonadIO (..)
    )
import Data.List
    ( nub
    )
import Data.Set
    ( Set
    )
import Data.Text
    ( Text
    )
import Data.Time.Clock
import Light.ReadBlocks
import Light.Types
import Say
    ( say
    , sayString
    )

import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Blockfrost.Client
import qualified Blockfrost.Client as BF

{-----------------------------------------------------------------------------
    Main
------------------------------------------------------------------------------}

-- | Test that we can connect to the Blockfrost API.
main :: IO ()
main = run BF.getLatestBlock >>= print

-- | Run a 'BlockfrostClient' action using the hardcoded API token.
run :: BlockfrostClient a -> IO (Either BlockfrostError a)
run action = do
    net <- projectFromFile "secrets/blockfrost.mainnet.token"
    BF.runBlockfrost net action

-- | Test of the main 'discoverTransactions' function.
testDiscovery :: BlockfrostClient ()
testDiscovery = do
    now <- fromBlock <$> BF.getLatestBlock
    let blocks = mkBlockSummaryBlockfrost Origin now
    pool0 <- (mkPool . take 10 <$> liftIO someAddresses)
    -- let pool0 = mkPool [mainAddr]
    ((res, _), t) <-
        timed
            $ discoverTransactions (query blocks) (pool0 :: Pool Address Int)
    liftIO $ do
        say "discoverTransactions"
        sayString $ "  transactions: " <> show (Set.size res)
        sayString $ "  time:         " <> show t

{-
discover :: Int -> Pool Address Int -> BlockfrostClient ()
discover n addr =
    lightSync follower
  where
    follower = ChainFollower
        { readChainPoints = pure [Origin]
        , rollForward  = \summary _ -> do
            (txs, pool) <- discoverTransactions (cheatNAddresses n) (query summary) (mkPool addr)
            liftIO $ forM_ txs $ \tx ->
                print $ _transactionUtxosHash tx
        , rollBackward = pure
        }
-}

{-----------------------------------------------------------------------------
    Generate random addresses that are found on the chain
------------------------------------------------------------------------------}
someAddresses :: IO [Address]
someAddresses = do
    map BF.Address . T.lines <$> T.readFile "data/addresses.txt"

testAddr :: Address
testAddr = Address "addr_test1vzevguhvhthc8msxxx82z4pa86vk5cq5f63l6wermr4r7ws8tve8v"

mainAddr :: Address
mainAddr = Address "addr1qy3cw2t0r4763fwx0tlm4wxx7sjk0xr8hfczj9xn06s9hn2cjjl6ng8awddvfndxm56z0ww7ug0w4vnal3msdv5jk7jq4zp6hx"

genAddresses :: Int -> Address -> BlockfrostClient [Address]
genAddresses n = repeatN n genAddress

printAddresses :: Int -> Text -> BlockfrostClient ()
printAddresses n addr = do
    xs <- nub <$> genAddresses n (Address addr)
    prints $ map unAddress xs

prints :: (MonadIO m) => [Text] -> m ()
prints = mapM_ say

{-----------------------------------------------------------------------------
    Utilities
------------------------------------------------------------------------------}
repeatN :: (Monad m) => Int -> (a -> m (Maybe a)) -> a -> m [a]
repeatN 0 _ a = pure [a]
repeatN n f a1 = do
    ma2 <- f a1
    case ma2 of
        Nothing -> pure []
        Just a2 -> (a2 :) <$> repeatN (n - 1) f a2

-- | Monadic unfold
unfoldrM :: (Monad m) => (b -> m (Maybe (a, b))) -> b -> m [a]
unfoldrM f = go
  where
    go b = do
        m <- f b
        case m of
            Nothing -> return []
            Just (a, b') -> (a :) <$> (go b')

-- | Execution time of a monadic action
timed :: (MonadIO m) => m a -> m (a, NominalDiffTime)
timed action = do
    start <- liftIO getCurrentTime
    res <- action
    finish <- liftIO getCurrentTime
    let t = finish `diffUTCTime` start
    pure (res, t)
