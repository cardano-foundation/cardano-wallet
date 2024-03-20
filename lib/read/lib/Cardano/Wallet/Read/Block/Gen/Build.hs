{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.Read.Block.Gen.Build
    ( ChainF
    , ChainBuild
    , TxF
    , TxBuild
    , input
    , output
    , tx
    , block
    , exampleChainF
    , mkChainM
    , txP
    , address
    , exampleBlocks
    )
where

import Prelude hiding
    ( (.)
    )

import Cardano.Ledger.Address
    ( serialiseAddr
    )
import Cardano.Ledger.Binary
    ( byronProtVer
    , serialize
    )
import Cardano.Wallet.Read.Block
    ( ConsensusBlock
    , toConsensusBlock
    )
import Cardano.Wallet.Read.Block.BlockNo
    ( BlockNo (..)
    )
import Cardano.Wallet.Read.Block.Gen
    ( mkBlockEra
    )
import Cardano.Wallet.Read.Block.Gen.BlockParameters
    ( BlockParameters (..)
    , txsL
    )
import Cardano.Wallet.Read.Block.SlotNo
    ( SlotNo (..)
    )
import Cardano.Wallet.Read.Eras
    ( Era (..)
    , IsEra
    )
import Cardano.Wallet.Read.Tx
    ( Tx
    )
import Cardano.Wallet.Read.Tx.Gen
    ( mkTxEra
    )
import Cardano.Wallet.Read.Tx.Gen.Byron
    ( mkByronAddrFromXPub
    )
import Cardano.Wallet.Read.Tx.Gen.Shelley
    ( mkShelleyPaymentPart
    )
import Cardano.Wallet.Read.Tx.Gen.TxParameters
    ( Address (..)
    , Index (..)
    , Lovelace (..)
    , TxId (..)
    , TxParameters (..)
    )
import Cardano.Wallet.Read.Tx.Hash
    ( getEraTxHash
    )
import Control.Category
    ( (.)
    )
import Control.Lens
    ( over
    , strict
    , view
    )
import Control.Monad
    ( forever
    , replicateM
    , replicateM_
    )
import Control.Monad.Operational
    ( ProgramT
    , ProgramViewT (..)
    , singleton
    , viewT
    )
import Control.Monad.Trans.Class
    ( lift
    )
import Control.Monad.Trans.Writer
    ( WriterT
    , execWriterT
    , tell
    )
import Data.Either.Extra
    ( partitionEithers
    )
import Data.Functor
    ( ($>)
    )
import Data.Kind
    ( Type
    )
import Data.List.NonEmpty
    ( NonEmpty (..)
    )
import Data.Monoid
    ( Endo (..)
    )
import Numeric.Natural
    ( Natural
    )
import Test.QuickCheck
    ( Gen
    , choose
    , elements
    )
import Test.QuickCheck.Gen
    ( Gen (..)
    )
import Test.QuickCheck.Random
    ( mkQCGen
    )

import qualified Data.ByteString.Char8 as B8

-- | DSL for building a tx
data TxBuild a where
    Input :: Index -> TxId -> TxBuild ()
    Output :: Address -> Lovelace -> TxBuild ()

-- | DSL for building a tx
type TxF m = ProgramT TxBuild m

-- | add an input to the current tx
input :: Index -> TxId -> ProgramT TxBuild m ()
input i txid' = singleton $ Input i txid'

-- | add an output to the current tx
output :: Address -> Lovelace -> ProgramT TxBuild m ()
output addr val = singleton $ Output addr val

-- | DSL for building a chain
data ChainBuild m (address :: Type) a where
    Tx :: TxF m () -> ChainBuild m addr TxId
    TxP :: TxParameters -> ChainBuild m addr TxId
    NewAddress :: addr -> ChainBuild m addr Address
    Block :: IsEra era => Era era -> SlotNo -> ChainBuild m addr ()

-- | DSL for building a chain
type ChainF m addr = ProgramT (ChainBuild m addr) m

-- | add a tx to the current block
tx :: TxF m () -> ChainF m addr TxId
tx txf = singleton $ Tx txf

-- | add a tx to the current block
txP :: TxParameters -> ChainF m addr TxId
txP txp = singleton $ TxP txp

-- | aintroduce a new address
address :: addr -> ChainF m addr Address
address k = singleton $ NewAddress k

-- | add a new block to the chain
block
    :: IsEra era
    => Era era
    -> Natural
    -> ChainF m addr ()
block p slot = singleton $ Block p (SlotNo slot)

-- | add specific era blocks to the chain
byron
    , shelley
    , allegra
    , mary
    , alonzo
    , babbage
    , conway
        :: Natural -> ChainF m addr ()
byron = block Byron
shelley = block Shelley
allegra = block Allegra
mary = block Mary
alonzo = block Alonzo
babbage = block Babbage
conway = block Conway

--------------------------------------------------------------------------------
-- ChainF interpreter
--------------------------------------------------------------------------------

-- | Lazy list output for the interpreter
type ChainM m = WriterT (Endo [ConsensusBlock]) m

-- | Generate blocks from a 'ChainF ()' value
mkChainM
    :: Monad m
    => (addr -> m Address)
    -> ChainF m addr ()
    -> ChainM m ()
mkChainM genAddress c =
    interpretChainF c genAddress (BlockNo 0) Nothing

-- | Current block parameters together with the field lens to the current era
-- from every EraFun
data CurrentBlockParameters = forall era.
      IsEra era =>
    CurrentBlockParameters
    { _currentBlockValue :: BlockParameters era
    }

-- | Interpreter of ChainF into ChainM
interpretChainF
    :: forall m addr a
     . (Monad m)
    => ChainF m addr a
    -- ^ DSL instance to interpret
    -> (addr -> m Address)
    -- ^ new address generator
    -> BlockNo
    -- ^ current block number
    -> Maybe CurrentBlockParameters
    -- ^ lens to the current block in its era
    -> ChainM m a
    -- ^ final state
interpretChainF m genAddress blockNo ml = do
    r <- lift $ viewT m
    case r of
        Return x -> case ml of
            Nothing -> pure x
            Just cur -> produce cur $> x -- produce the last block
        Tx ft :>>= k -> case ml of
            Nothing -> error "chainFToBlockParams: no open block for the tx"
            Just (CurrentBlockParameters bp) -> do
                newTx <- lift $ mkTx ft
                updateCurrentBlock k newTx bp
        TxP txp :>>= k -> case ml of
            Nothing -> error "chainFToBlockParams: no open block for the tx"
            Just (CurrentBlockParameters bp) -> do
                let newTx = mkTxEra txp
                updateCurrentBlock k newTx bp
        NewAddress addr :>>= k -> do
            addr' <- lift $ genAddress addr
            interpretChainF (k addr') genAddress blockNo ml
        Block p sn :>>= k -> newBlock p sn (k ())
  where
    updateCurrentBlock
        :: IsEra era
        => (TxId -> ChainF m addr a)
        -> Tx era
        -> BlockParameters era
        -> ChainM m a
    updateCurrentBlock k newTx bp =
        let txid' = getEraTxHash newTx
        in  interpretChainF (k $ TxId txid') genAddress blockNo
                $ Just
                $ CurrentBlockParameters
                $ over txsL (newTx :) bp
    produce :: CurrentBlockParameters -> WriterT (Endo [ConsensusBlock]) m ()
    produce (CurrentBlockParameters bp) =
        tell $ Endo $ (:) $ toConsensusBlock $ mkBlockEra bp
    newBlock
        :: forall era
         . IsEra era
        => Era era
        -> SlotNo
        -> ChainF m addr a
        -> ChainM m a
    newBlock _ slot next = case ml of
        Nothing ->
            interpretChainF next genAddress blockNo
                $ Just
                $ CurrentBlockParameters
                $ BlockParameters @era slot (BlockNo 0) []
        Just cur@(CurrentBlockParameters _) -> do
            produce cur
            interpretChainF next genAddress (succ blockNo)
                $ Just
                $ CurrentBlockParameters
                $ BlockParameters @era slot blockNo []

-- | Interpreter of 'TxF b' into an era specific 'Tx', given a lens into any
-- erafun field
mkTx :: Monad m => IsEra era => TxF m b -> m (Tx era)
mkTx = fmap (mkTxEra . mkTxParameters) . go --  . go
  where
    go :: Monad m => TxF m b -> m [Either (Index, TxId) (Address, Lovelace)]
    go m = do
        r <- viewT m
        case r of
            Return _ -> pure []
            Input i txid' :>>= next -> (Left (i, txid') :) <$> go (next ())
            Output addr val :>>= next -> (Right (addr, val) :) <$> go (next ())

-- | Partition input and outputs into a 'TxParameters'
mkTxParameters :: [Either (Index, TxId) (Address, Lovelace)] -> TxParameters
mkTxParameters xs =
    let
        (ins, outs) = partitionEithers xs
    in
        case (ins, outs) of
            ([], _) -> error "mkTxParameters: no inputs"
            (_, []) -> error "mkTxParameters: no outputs"
            (i : is, j : js) -> TxParameters (i :| is) (j :| js)

--------------------------------------------------------------------------------
-- Example
--------------------------------------------------------------------------------

-- a simple address indexing schema
data MkAddress where
    WalletByronAddress :: MkAddress
    ExternalByronAddress :: MkAddress
    WalletShelleyAddress :: MkAddress

-- a simple chainF example
exampleChainF :: ChainF Gen MkAddress ()
exampleChainF = do
    byron 0
    byron 5 -- a new byron block at slot 0, with following txs, block 0
    aby1 <- address WalletByronAddress
    tx1 <- tx $ do
        input (Index 0) (txid 'a') -- from outside
        v <- lift $ choose (1, 1000)
        output aby1 (Lovelace v) -- index 0
    tx2 <- tx $ do
        input (Index 0) (txid 'c') -- from outside
        output aby1 (Lovelace 500) -- index 0
        -- some noise
    n <- lift $ choose (6, 12)
    extByrons <- replicateM 6 $ address ExternalByronAddress
    replicateM_ n $ tx $ do
        txid' <- lift genTxId
        idx' <- lift $ fromIntegral <$> choose (0 :: Int, 10)
        input (Index idx') txid'
        -- from outside
        randAddress <- lift $ elements extByrons
        value <- lift $ choose (1, 1000)
        output randAddress (Lovelace value) -- index 0
    byron 10 -- a new byron block at slot 10, with following txs, block 1
    aby2 <- address WalletByronAddress
    tx3 <- tx $ do
        input (Index 0) tx2 -- use tx2 output (500)
        output aby1 (Lovelace 300) -- index 0
        output aby2 (Lovelace 200) -- index 1
    shelley 20 -- a new shelley block at slot 20, with following txs, block 2
    ash1 <- address WalletShelleyAddress
    shtx <- tx $ do
        input (Index 0) tx1 -- use tx1 (500)
        input (Index 1) tx3 -- use tx3 (200)
        output aby1 (Lovelace 700) -- index 0, byron address in shelley era
        output ash1 (Lovelace 10) -- index 1
    allegra 30
    replicateM_ 50 $ tx $ do
        input (Index 0) shtx
        output ash1 (Lovelace 10)
    mary 40
    replicateM_ 50 $ tx $ do
        input (Index 0) shtx
        output ash1 (Lovelace 10)
    alonzo 50
    replicateM_ 50 $ tx $ do
        input (Index 0) shtx
        output ash1 (Lovelace 10)
    babbage 60
    replicateM_ 50 $ tx $ do
        input (Index 0) shtx
        output ash1 (Lovelace 10)
    conway 70
    replicateM_ 50 $ tx $ do
        input (Index 0) shtx
        output ash1 (Lovelace 10)

-- Generate an invalid (not an hash) txid from a char
txid :: Char -> TxId
txid = TxId . B8.pack . replicate 32

-- Generate a random invalid txid
genTxId :: Gen TxId
genTxId = TxId . B8.pack <$> replicateM 32 (choose ('a', 'z'))

-- an infinite list of example blocks computed out of repeating the 'exampleChainF'
exampleBlocks :: [ConsensusBlock]
exampleBlocks =
    unGen
        ( fmap (`appEndo` [])
            $ execWriterT
            $ mkChainM genRandomAddress
            $ forever exampleChainF
        )
        (mkQCGen 43)
        43

-- generate an invalid random address for a MkAddress request
genRandomAddress :: MkAddress -> Gen Address
genRandomAddress = \case
    WalletByronAddress ->
        ByronAddress
            <$> genRandomAddress'
                (view strict . serialize byronProtVer . mkByronAddrFromXPub)
    ExternalByronAddress ->
        ByronAddress
            <$> genRandomAddress'
                (view strict . serialize byronProtVer . mkByronAddrFromXPub)
    WalletShelleyAddress ->
        ShelleyAddress
            <$> genRandomAddress'
                (serialiseAddr . mkShelleyPaymentPart)
  where
    genRandomAddress'
        :: (B8.ByteString -> B8.ByteString) -- (B8.ByteString -> b)
        -> Gen B8.ByteString
    genRandomAddress' f =
        -- . serialize v
        f
            . B8.pack
            <$> replicateM 28 (choose ('a', 'z'))
