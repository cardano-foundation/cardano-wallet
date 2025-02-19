{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Wallet.Deposit.Testing.DSL
    ( Scenario (..)
    , ScenarioP
    , existsTx
    , deposit
    , deposit_
    , withdrawal
    , block
    , rollForward
    , rollBackward
    , historyByTime
    , historyByCustomer
    , newHistoryByTime
    , availableBalance
    , assert
    , interpret
    , InterpreterState (..)
    , spend
    , sign
    , utxo
    , wallet
    , balance
    )
where

import Prelude

import Cardano.Wallet.Deposit.Pure
    ( Customer
    , ResolvedTx (..)
    , WalletState
    , getTxHistoryByTime
    )
import Cardano.Wallet.Deposit.Pure.API.TxHistory
    ( ByCustomer
    , ByTime
    , LookupTimeFromSlot
    )
import Cardano.Wallet.Deposit.Pure.State.Creation
    ( createMnemonicFromWords
    , credentialsFromMnemonics
    )
import Cardano.Wallet.Deposit.Pure.State.Payment
    ( createPaymentTxBody
    )
import Cardano.Wallet.Deposit.Read
    ( Address
    , ChainPoint (..)
    , EraValue (..)
    , UTxO
    , getChainPoint
    , mockNextBlock
    , slotFromChainPoint
    )
import Cardano.Wallet.Deposit.Testing.DSL.ByTime
    ( ByTimeM
    , ByTimeMContext (..)
    )
import Cardano.Wallet.Deposit.Testing.DSL.Types
    ( BlockI (..)
    , TxI (..)
    , UnspentI (..)
    )
import Cardano.Wallet.Deposit.Write
    ( Block
    , Tx
    , TxBody
    , addTxIn
    , addTxOut
    , emptyTxBody
    , mkAda
    , mkTx
    , mkTxOut
    , txOutsL
    )
import qualified Cardano.Wallet.Deposit.Write as Write
import Cardano.Wallet.Read
    ( Coin (..)
    , Slot
    , Value (..)
    , WithOrigin
    , getTxId
    , pattern TxIn
    )
import Control.Lens
    ( At (..)
    , Field1 (_1)
    , Field2 (_2)
    , Ixed (..)
    , Lens'
    , lens
    , use
    , uses
    , zoom
    , (%=)
    , (.=)
    , (^?)
    )
import Control.Monad
    ( void
    , (>=>)
    )
import Control.Monad.Operational
    ( ProgramT
    , ProgramViewT (..)
    , singleton
    , viewT
    )
import Control.Monad.Reader
    ( MonadIO (..)
    , runReader
    )
import Control.Monad.State
    ( MonadState (..)
    , MonadTrans (..)
    , StateT
    , evalStateT
    , execStateT
    , modify
    )
import Data.List
    ( mapAccumL
    )
import Data.Map
    ( Map
    )
import Data.Maybe
    ( fromJust
    )
import Data.Text
    ( Text
    )
import Data.Time
    ( UTCTime
    )

import qualified Cardano.Wallet.Deposit.Pure as Wallet
import qualified Cardano.Wallet.Deposit.Time as Time
import qualified Cardano.Wallet.Read as Read
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import qualified Data.Set as Set

data Scenario p a where
    ResetWallet :: Int -> Text -> Text -> Scenario p ()
    ExistsTx :: Scenario p TxI
    Deposit :: TxI -> Customer -> Int -> Scenario p UnspentI
    Spend :: TxI -> Address -> Int -> Scenario p ()
    Withdrawal :: TxI -> UnspentI -> Scenario p ()
    CreateBlock :: [TxI] -> Scenario p (BlockI)
    RollForward :: [BlockI] -> Scenario p ()
    RollBackward :: Maybe BlockI -> Scenario p ()
    HistoryByTime :: Scenario p ByTime
    HistoryByCustomer :: Scenario p ByCustomer
    NewHistoryByTime :: ByTimeM ByTime -> Scenario p ByTime
    AvailableBalance :: Scenario p Int
    Sign :: Tx -> Text -> Scenario p Tx
    Balance :: TxI -> Scenario p Tx
    UTxO :: UnspentI -> Scenario p UTxO
    Assert :: p -> Scenario p ()

type ScenarioP p m = ProgramT (Scenario p) m

wallet :: Int -> Text -> Text -> ScenarioP p m ()
wallet customers seed passphrase =
    singleton (ResetWallet customers seed passphrase)

existsTx :: ScenarioP p m TxI
existsTx = singleton ExistsTx

deposit :: TxI -> Customer -> Int -> ScenarioP p m UnspentI
deposit tx customer value = singleton (Deposit tx customer value)

deposit_ :: Monad m => TxI -> Customer -> Int -> ScenarioP p m ()
deposit_ tx customer value = void $ deposit tx customer value

spend :: TxI -> Address -> Int -> ScenarioP p m ()
spend tx addr value = singleton (Spend tx addr value)

withdrawal :: TxI -> UnspentI -> ScenarioP p m ()
withdrawal tx unspent = singleton (Withdrawal tx unspent)

block :: [TxI] -> ScenarioP p m BlockI
block txs = singleton (CreateBlock txs)

rollForward :: [BlockI] -> ScenarioP p m ()
rollForward blocks = singleton (RollForward blocks)

rollBackward :: Maybe BlockI -> ScenarioP p m ()
rollBackward slot = singleton (RollBackward slot)

historyByTime :: ScenarioP p m ByTime
historyByTime = singleton HistoryByTime

historyByCustomer :: ScenarioP p m ByCustomer
historyByCustomer = singleton HistoryByCustomer

newHistoryByTime :: ByTimeM ByTime -> ScenarioP p m ByTime
newHistoryByTime = singleton . NewHistoryByTime

availableBalance :: ScenarioP p m Int
availableBalance = singleton AvailableBalance

sign :: Tx -> Text -> ScenarioP p m Write.Tx
sign tx pass = singleton (Sign tx pass)

balance :: TxI -> ScenarioP p m Tx
balance tx = singleton (Balance tx)

utxo :: UnspentI -> ScenarioP p m UTxO
utxo = singleton . UTxO

assert :: p -> ScenarioP p m ()
assert = singleton . Assert

rollForwardBlocks
    :: LookupTimeFromSlot
    -> [BlockI]
    -> (WalletState, InterpreterState)
    -> (WalletState, InterpreterState)
rollForwardBlocks timeOf blocks (w, interpreter@InterpreterState{..}) =
    ( w'
    , interpreter{iBlocks = newIBlocks, iBlockPoints = newIBlockPoints}
    )
  where
    w' = Wallet.rollForwardMany timeOf (NE.fromList blocks') w
    ((newIBlocks, newIBlockPoints), blocks') =
        mapAccumL
            rollForwardBlock
            (iBlocks, iBlockPoints)
            blocks
    rollForwardBlock (iBlocksCurrent, iBlockPointsCurrent) blockI =
        (
            ( Map.insert blockPoint newBlock iBlocksCurrent
            , Map.insert blockI blockPoint iBlockPointsCurrent
            )
        , EraValue newBlock
        )
      where
        txs = iBlockContents Map.! blockI
        newBlock = mockNextBlock startPoint txs
        blockPoint = getChainPoint newBlock
        startPoint =
            maybe GenesisPoint fst
                $ Map.lookupMax iBlocksCurrent

rollBackwardBlock
    :: LookupTimeFromSlot
    -> Maybe BlockI
    -> (WalletState, InterpreterState)
    -> (WalletState, InterpreterState)
rollBackwardBlock timeOf Nothing (w, interpreter) =
    ( fst $ Wallet.rollBackward timeOf GenesisPoint w
    , interpreter{iBlocks = mempty, iBlockPoints = mempty}
    )
rollBackwardBlock timeOf (Just blockI) (w, interpreter@InterpreterState{..}) =
    case Map.lookup blockI iBlockPoints of
        Just keep ->
            ( w'
            , interpreter{iBlocks = newIBlocks, iBlockPoints = newIBlockPoints}
            )
          where
            w' = fst $ Wallet.rollBackward timeOf keep w
            newIBlocks = Map.takeWhileAntitone (<= keep) iBlocks
            newIBlockPoints = Map.filter (<= keep) iBlockPoints
        Nothing -> (w, interpreter)

data InterpreterState = InterpreterState
    { iTxs :: Map TxI TxBody
    , iBlockContents :: Map BlockI [Tx]
    , iBlockPoints :: Map BlockI ChainPoint
    , iBlocks :: Map ChainPoint Block
    }
    deriving (Show)

iTxsL :: Lens' InterpreterState (Map TxI TxBody)
iTxsL = lens iTxs (\s x -> s{iTxs = x})

iBlockContentsL :: Lens' InterpreterState (Map BlockI [Tx])
iBlockContentsL = lens iBlockContents (\s x -> s{iBlockContents = x})

iBlockPointsL :: Lens' InterpreterState (Map BlockI ChainPoint)
iBlockPointsL = lens iBlockPoints (\s x -> s{iBlockPoints = x})

newTxId :: Monad m => StateT InterpreterState m TxI
newTxId = zoom iTxsL $ do
    txs <- get
    let z = maybe 0 fst $ Map.lookupMax txs
        txId = z + 1
    put $ Map.insert txId emptyTxBody txs
    return txId

newBlockId :: Monad m => StateT InterpreterState m BlockI
newBlockId = zoom iBlockContentsL $ do
    blocks <- get
    let z = maybe 0 fst $ Map.lookupMax blocks
        blockId = z + 1
    put $ Map.insert blockId [] blocks
    return blockId

freshInterpreterState :: InterpreterState
freshInterpreterState = InterpreterState mempty mempty mempty mempty

unsafeCustomerAddress
    :: Wallet.WalletState -> Customer -> Write.Address
unsafeCustomerAddress w = fromJust . flip Wallet.customerAddress w

interpret
    :: (MonadIO m, MonadFail m)
    => WalletState
    -> (p -> m ())
    -> (Slot -> WithOrigin UTCTime)
    -> ScenarioP
        p
        (StateT (WalletState, InterpreterState) m)
        ()
    -> m ()
interpret w runP slotTimes p = flip evalStateT w $ do
    walletState <- get
    (walletState', _) <-
        lift
            $ execStateT
                (go p)
                (walletState, freshInterpreterState)
    put walletState'
  where
    go = viewT >=> eval
    eval (ResetWallet customers seed passphrase :>>= k) = do
        Right mnemonics <- pure $ createMnemonicFromWords seed
        let new =
                Wallet.fromCredentialsAndGenesis
                    (credentialsFromMnemonics mnemonics passphrase)
                    (fromIntegral customers)
                    Read.mockGenesisDataMainnet
        id .= (new, freshInterpreterState)
        go $ k ()
    eval (Return x) = return x
    eval (ExistsTx :>>= k) = do
        txId <- zoom _2 newTxId
        go $ k txId
    eval (Deposit tx customer value :>>= k) = do
        customerAddresses <- uses _1 unsafeCustomerAddress
        let v = mkAda $ fromIntegral value
            txOut = mkTxOut (customerAddresses customer) v
        Just txBody <- use (_2 . iTxsL . at tx)
        let (txBody', tix) = addTxOut txOut txBody
        _2 . iTxsL . ix tx .= txBody'
        go $ k $ UnspentI (tx, tix)
    eval (Withdrawal tx (UnspentI (tx', tix)) :>>= k) = do
        Just txId <- uses (_2 . iTxsL . at tx') $ fmap (getTxId . mkTx)
        _2 . iTxsL . ix tx %= \txBody -> addTxIn (TxIn txId tix) txBody
        go $ k ()
    eval (Spend tx address value :>>= k) = do
        Just txBody <- use (_2 . iTxsL . at tx)
        let (txBody', _tix) =
                addTxOut (mkTxOut address (mkAda $ fromIntegral value)) txBody
        _2 . iTxsL . ix tx .= txBody'
        go $ k ()
    eval (CreateBlock txs :>>= k) = do
        blockId <- zoom _2 newBlockId
        send <-
            uses (_2 . iTxsL)
                $ flip Map.restrictKeys
                $ Set.fromList txs
        _2 . iBlockContentsL . ix blockId .= (mkTx <$> Map.elems send)
        go $ k blockId
    eval (RollForward blocks :>>= k) = do
        modify $ rollForwardBlocks (fmap Just slotTimes) blocks
        go $ k ()
    eval (RollBackward blockKeep :>>= k) = do
        modify $ rollBackwardBlock (fmap Just slotTimes) blockKeep
        go $ k ()
    eval (HistoryByTime :>>= k) = do
        v <- uses _1 getTxHistoryByTime
        go $ k v
    eval (HistoryByCustomer :>>= k) = do
        v <- uses _1 Wallet.getTxHistoryByCustomer
        go $ k v
    eval (NewHistoryByTime m :>>= k) = do
        customerAddresses <- uses _1 unsafeCustomerAddress
        txIds' <- uses (_2 . iTxsL) $ (Map.!) . fmap (getTxId . mkTx)
        blockSlots <-
            uses (_2 . iBlockPointsL) $ (Map.!) . fmap slotFromChainPoint
        go
            $ k
            $ runReader m
            $ ByTimeMContext txIds' customerAddresses slotTimes blockSlots
    eval (AvailableBalance :>>= k) = do
        ValueC (CoinC v) _ <- uses _1 Wallet.availableBalance
        go $ k $ fromIntegral v
    eval (Sign tx pass :>>= k) = do
        Just stx <- uses _1 $ Wallet.signTx tx pass
        go $ k stx
    eval (Balance tx :>>= k) = do
        Just txBody <- use (_2 . iTxsL . at tx)
        ws <- use _1
        let etx =
                createPaymentTxBody
                    (Read.EraValue Read.mockPParamsConway)
                    (Time.toTimeTranslationPure Time.mockTimeInterpreter)
                    txBody
                    ws
        ResolvedTx btx _ <- case etx of
            Left e -> fail $ "createPaymentTxBody failed: " <> show e
            Right tx' -> return tx'
        go $ k btx
    eval (UTxO (UnspentI (tx, tix)) :>>= k) = do
        Just txBody <- use (_2 . iTxsL . at tx)
        let txId = getTxId $ mkTx txBody
        Just txOut <- pure $ txBody ^? txOutsL . ix tix
        go $ k $ Map.singleton (TxIn txId tix) txOut
    eval (Assert assertion :>>= k) = do
        lift $ runP assertion
        go $ k ()
