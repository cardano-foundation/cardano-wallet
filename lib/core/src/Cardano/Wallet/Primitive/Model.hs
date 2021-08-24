{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- Here we find the "business logic" to manage a Cardano wallet. This is a
-- direct implementation of the model from the [Formal Specification for a Cardano Wallet](https://github.com/input-output-hk/cardano-wallet/blob/master/specifications/wallet/formal-specification-for-a-cardano-wallet.pdf)
-- Note that, this module is purposedly agnostic to how blocks are retrieved or
-- how various types are serialized.
--
-- This is really about how the wallet keep track of its internal state, and its
-- UTxO (where the relationship is defined via the 'IsOurs' abstraction to allow
-- this core code to be used with any sort of derivation scheme).
--
-- All those functions are pure and there's no reason to shove in any sort of
-- side-effects in here :)

module Cardano.Wallet.Primitive.Model
    (
    -- * Type
      Wallet

    -- * Construction & Modification
    , FilteredBlock (..)
    , initWallet
    , updateState
    , applyBlock
    , applyBlocks
    , unsafeInitWallet

    -- * Accessors
    , currentTip
    , getState
    , availableBalance
    , totalBalance
    , totalUTxO
    , availableUTxO
    , utxo
    ) where

import Prelude

import Cardano.Wallet.Primitive.AddressDiscovery
    ( IsOurs (..) )
import Cardano.Wallet.Primitive.Types
    ( Block (..)
    , BlockHeader (..)
    , DelegationCertificate (..)
    , dlgCertAccount
    )
import Cardano.Wallet.Primitive.Types.Address
    ( Address (..) )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..), distance, sumCoins )
import Cardano.Wallet.Primitive.Types.RewardAccount
    ( RewardAccount (..) )
import Cardano.Wallet.Primitive.Types.TokenBundle
    ( TokenBundle )
import Cardano.Wallet.Primitive.Types.Tx
    ( Direction (..)
    , Tx (..)
    , TxIn (..)
    , TxMeta (..)
    , TxOut (..)
    , TxStatus (..)
    , inputs
    , txIns
    , txOutCoin
    )
import Cardano.Wallet.Primitive.Types.UTxO
    ( Dom (..), UTxO (..), balance, excluding, restrictedBy )
import Control.DeepSeq
    ( NFData (..), deepseq )
import Control.Monad
    ( foldM, forM )
import Control.Monad.Extra
    ( mapMaybeM )
import Control.Monad.Trans.State.Strict
    ( State, evalState, runState, state )
import Data.Functor
    ( (<&>) )
import Data.Generics.Internal.VL.Lens
    ( (^.) )
import Data.Generics.Labels
    ()
import Data.List.NonEmpty
    ( NonEmpty (..) )
import Data.Maybe
    ( catMaybes, isJust )
import Data.Set
    ( Set )
import Fmt
    ( Buildable (..), indentF )
import GHC.Generics
    ( Generic )

import Cardano.Wallet.Primitive.AddressDerivation
    ( DerivationIndex (DerivationIndex) )
import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TB
import qualified Data.Bifunctor as Bifunctor
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import qualified Data.Map.Strict as M
import qualified Data.Set as Set

{-------------------------------------------------------------------------------
                                     Type
-------------------------------------------------------------------------------}

-- | An opaque wallet type, see 'initWallet', 'updateState', 'applyBlock', and
-- 'applyBlocks' to construct and update wallets.
--
-- Internally, this keeps track or a few things including:
--
--  - UTxOs
--  - Transaction history
--  - Known & used addresses, via address discovery state
--  - Blockchain parameters
--
-- The 'Wallet' is parameterized over a single type:
--
-- - @s@ is a /state/ used to keep track of known addresses. The business logic
--   doesn't know how to answer the question \"Is this address ours?\", so we
--   expect this state to be able to answer that for us.
--   Typically, this state will be an instance of the 'IsOurs' class,
--   e.g. @'IsOurs' s 'Address'@.
--
-- A few examples to make it concrete:
--
-- @
-- Wallet (RndState k n)
-- Wallet (SeqState n ShelleyKey)
-- @
data Wallet s = Wallet
    { -- | Unspent tx outputs belonging to this wallet
      utxo :: UTxO

      -- | Header of the latest applied block (current tip)
    , currentTip :: BlockHeader

      -- | Address discovery state
    , getState :: s
    } deriving (Generic, Eq, Show)

instance NFData s => NFData (Wallet s) where
    rnf (Wallet u sl s) =
        deepseq (rnf u) $
        deepseq (rnf sl) $
        deepseq (rnf s)
        ()

instance Buildable s => Buildable (Wallet s) where
    build (Wallet u tip s) = "Wallet s\n"
        <> indentF 4 ("Tip: " <> build tip)
        <> indentF 4 ("UTxO:\n" <> indentF 4 (build u))
        <> indentF 4 (build s)

{-------------------------------------------------------------------------------
                          Construction & Modification
-------------------------------------------------------------------------------}

-- | Create a an empty wallet and apply the given genesis block
--
-- The wallet tip will be set to the header of the applied genesis block.
initWallet
    :: (IsOurs s Address, IsOurs s RewardAccount)
    => Block
        -- ^ The genesis block
    -> s
        -- ^ Initial address discovery state
    -> ([(Tx, TxMeta)], Wallet s)
initWallet block s =
    let
        ((FilteredBlock _ txs, u), s') = prefilterBlock block mempty s
    in
        (txs, Wallet u (header block) s')

-- | Constructs a wallet from the exact given state. Using this function instead
-- of 'initWallet' and 'applyBlock' allows the wallet invariants to be
-- broken. Therefore it should only be used in the special case of loading
-- wallet checkpoints from the database (where it is assumed a valid wallet was
-- stored into the database).
unsafeInitWallet
    :: UTxO
       -- ^ Unspent tx outputs belonging to this wallet
    -> BlockHeader
    -- ^ Header of the latest applied block (current tip)
    -> s
    -- ^ Address discovery state
    -> Wallet s
unsafeInitWallet = Wallet

-- | Update the state of an existing Wallet model
updateState
    :: s
    -> Wallet s
    -> Wallet s
updateState s (Wallet u tip _) = Wallet u tip s

-- | Represents the subset of data from a single block that are relevant to a
--   particular wallet, discovered when applying a block to that wallet.
data FilteredBlock = FilteredBlock
    { delegations :: ![DelegationCertificate]
        -- ^ Stake delegations made on behalf of the wallet, listed in order of
        -- discovery. If the list contains more than element, those that appear
        -- later in the list supercede those that appear earlier on.
    , transactions :: ![(Tx, TxMeta)]
        -- ^ The set of transactions that affect the wallet.
    } deriving (Generic, Show, Eq)

-- | Apply a single block to a wallet.
--
-- This is the primary way of making a wallet evolve.
--
-- Returns an updated wallet, as well as the set of data relevant to the wallet
-- that were discovered while applying the block.
--
applyBlock
    :: (IsOurs s Address, IsOurs s RewardAccount)
    => Block
    -> Wallet s
    -> (FilteredBlock, Wallet s)
applyBlock !b (Wallet !u _ s) =
    (filteredBlock, Wallet u' (b ^. #header) s')
  where
    ((filteredBlock, u'), s') = prefilterBlock b u s

-- | Apply multiple blocks in sequence to an existing wallet, returning a list
--   of intermediate wallet states.
--
-- Each intermediate wallet state is paired with the set of transactions that
-- belong to that state but not the previous state.
--
-- For an original wallet state __@w@__ and a list of blocks __@b@__ such that:
--
-- > b = [b1, b2, ..., bn]
--
-- Returns the following list of updates:
--
-- > [ (t b1, w + b1)
-- > , (t b2, w + b1 + b2)
-- > , ...
-- > , (t bn, w + b1 + b2 + ... + bn) ]
--
-- Where:
--
-- * __@(t bi)@__   is the set of transactions contained within block __@bi@__.
-- * __@(w + bi)@__ is the wallet state after applying block __@bi@__ to wallet
--   __@w@__.
--
applyBlocks
    :: (IsOurs s Address, IsOurs s RewardAccount)
    => NonEmpty (Block)
    -> Wallet s
    -> NonEmpty (FilteredBlock, Wallet s)
applyBlocks (block0 :| blocks) cp =
    NE.scanl (flip applyBlock . snd) (applyBlock block0 cp) blocks

{-------------------------------------------------------------------------------
                                   Accessors
-------------------------------------------------------------------------------}

-- | Available balance = 'balance' . 'availableUTxO'
availableBalance :: Set Tx -> Wallet s -> TokenBundle
availableBalance pending =
    balance . availableUTxO pending

-- | Total balance = 'balance' . 'totalUTxO' +? rewards
totalBalance
    :: (IsOurs s Address, IsOurs s RewardAccount)
    => Set Tx
    -> Coin
    -> Wallet s
    -> TokenBundle
totalBalance pending rewards wallet@(Wallet _ _ s) =
    balance (totalUTxO pending wallet) `TB.add` rewardsBalance
  where
    rewardsBalance
        | hasPendingWithdrawals pending = mempty
        | otherwise = TB.fromCoin rewards

    hasPendingWithdrawals =
        anyS (anyM (\acct _ -> isJust $ fst (isOurs acct s)) . withdrawals)
      where
        anyS predicate = not . Set.null . Set.filter predicate
        anyM predicate = not . Map.null . Map.filterWithKey predicate

-- | Available UTxO = @pending ⋪ utxo@
availableUTxO
    :: Set Tx
    -> Wallet s
    -> UTxO
availableUTxO pending (Wallet u _ _) =
    u  `excluding` txIns pending

-- | Total UTxO = 'availableUTxO' @<>@ 'changeUTxO'
totalUTxO
    :: IsOurs s Address
    => Set Tx
    -> Wallet s
    -> UTxO
totalUTxO pending wallet@(Wallet _ _ s) =
    availableUTxO pending wallet <> changeUTxO pending s

{-------------------------------------------------------------------------------
                               Internals
-------------------------------------------------------------------------------}

-- | An abstract type representing the effect of applying a single transaction
-- to a UTxO.
data X' = X'
    { _utxo :: !UTxO
    , _lastTx :: !Tx
    }

-- | Apply a transaction to a UTxO, requires some function used to determine
-- ownership of an address in a UTxO.
--
-- transactionInputs' (applyTransaction tx u) = transactionInputsUTxO u <> transactionInputs tx
-- collateralInputs' (applyTransaction tx u) = collateralInputsUTxO u <> collateralInputs tx
-- isValidScript tx => applyTransaction tx u = txins u `Set.difference` txins (inputs tx)
-- not (isValidScript tx) => applyTransaction tx u = txins u `Set.difference` txins (collateralInputs tx)
applyTransaction :: Tx -> X' -> X'
applyTransaction tx !x =
    let
        u = _utxo x
        collateralIns = Set.fromList (fst <$> tx ^. #resolvedCollateral)
        transactionIns = Set.fromList (inputs tx)
  
        txUTxO = _utxo $ fromTx tx
  
        u' = case tx ^. #isValidScript of
                Just False ->
                    u `excluding` collateralIns
                _ ->
                    (u <> txUTxO) `excluding` transactionIns
    in
      X' u' tx

-- isOurs :: Address -> s -> (Maybe (NonEmpty DerivationIndex), s)

isOurs' :: IsOurs s Address => Address -> (s -> (s, Bool))
isOurs' addr =
    let
        x1 :: s -> (Maybe (NonEmpty DerivationIndex), s)
        x1 = isOurs addr

        x2 :: s -> (s, Bool)
        x2 = fmap (\(mDeriv, s) -> case mDeriv of
                                       Nothing -> (s, False)
                                       Just _  -> (s, True)
                  ) x1
    in
        x2

getXUTxO :: X' -> UTxO
getXUTxO = _utxo

fromTx :: Tx -> X'
fromTx = undefined
            -- let
            --     indexedOutputs = zip [0..] (outputs tx)
            -- in
            --     UTxO
            --     . Map.fromList
            --     . fmap (Bifunctor.first (TxIn (txId tx)))
            --     $ indexedOutputs

-- Have we applied this Tx?
known :: Tx -> X' -> Bool
known = undefined

limitUTxO :: (Address -> f Bool) -> X' -> f X'
limitUTxO isOurs x@(X' _ tx) = 
    let
        u :: UTxO
        u = _utxoF x

        u' :: f UTxO
        u' =
            UTxO . Map.fromList
            <$> foldM (\utxo (txIn, txOut) -> do
                            ours <- isOurs $ address txOut
                            if ours
                                then pure $ utxo <> (txIn, txOut)
                                else pure utxo
                    )
                    mempty
            $ Map.toList (getUTxO u)
    in
        X' <$> u' <*> pure (txApplied x)

-- ourTransactionInputs (applyTx tx mempty) === inputs tx
ourTransactionInputs :: X' -> (Address -> f Bool) -> Set TxIn
ourTransactionInputs = undefined

-- ourCollateralInputs (applyTx tx mempty) === collateralInputs tx
ourCollateralInputs :: X' -> (Address -> f Bool) -> Set TxIn
ourCollateralInputs = undefined

-- hasKnownInput = ourTransactionInputs /= mempty
hasKnownInput :: X' -> (Address -> f Bool) -> Bool
hasKnownInput = undefined

-- hasKnownOutput ownfunc = or . fmap (ownfunc . address) . outputs . txApplied
hasKnownOutput :: X' -> (Address -> f Bool) -> Bool
hasKnownOutput = undefined

-- txApplied (applyTx tx utxo) = tx
txApplied :: X' -> Tx
txApplied = _lastTx

-- isValidScript tx => spent (applyTx tx u) = balance u `add` withdrawals tx `difference` bal resolvedInputs
-- not (isValidScript tx) => spent (applyTx tx u) = balance u `add` withdrawals tx `difference` bal collateralInputs
spent :: X' -> TokenBundle
spent = undefined

-- u1 `spent'` u2 = balance u1 `difference` balance u2
-- all in u1 that are no longer in u2, clamping to 0
spent' :: X' -> X' -> TokenBundle
spent' = undefined 

-- all in u2 that are not in u1, clamping to 0
received' :: X' -> X' -> TokenBundle
received' = undefined 

-- | Apply the state transition rules for a UTxO when applied to a Tx.
applyTx''
    :: IsOurs s Address
    => Tx
    -> UTxO
    -> State s (UTxO)
applyTx'' tx !u = do
    ourU <- state $ utxoOurs tx
    let ourIns =
            Set.fromList (inputs tx)
                `Set.intersection`
                    dom (u <> ourU)
    let ourCollateralIns =
            Set.fromList (fst <$> tx ^. #resolvedCollateral)
                `Set.intersection`
                    dom (u <> ourU)
    let u' =
            case tx ^. #isValidScript of
                -- If the transaction failed to validate, remove the
                -- collateral inputs (that belonged to us) from our UTxO
                -- set.
                Just False ->
                    u `excluding` ourCollateralIns
                -- Otherwise, if the transaction succeeded validation, or
                -- did not make use of a script that needed to be validated,
                -- remove the regular inputs (that belonged to us) from our
                -- UTxO set.
                _ ->
                    (u <> ourU) `excluding` ourIns
    pure u'

-- | Prefiltering returns all transactions of interest for the wallet. A
-- transaction is a matter of interest for the wallet if:
--
--    - It has known input(s)
--    - and/or It has known output(s)
--
-- In practice, most transactions that are of interest have an output to the
-- wallet but some may actually have no change output whatsoever and be only
-- linked to the wallet by their inputs.
--
-- In order to identify transactions that are ours, we do therefore look for
-- known inputs and known outputs. However, we can't naively look at the domain
-- of the utxo constructed from all outputs that are ours (as the specification
-- would suggest) because some transactions may use outputs of a previous
-- transaction within the same block as an input. Therefore, looking solely at
-- the final 'dom (UTxO ⊳ oursOuts)', we would be missing all intermediate txs
-- that happen from _within_ the block itself.
--
-- As a consequence, we do have to traverse the block, and look at transactions
-- in order, starting from the known inputs that can be spent (from the previous
-- UTxO) and collect resolved tx outputs that are ours as we apply transactions.
prefilterBlock
    :: (IsOurs s Address, IsOurs s RewardAccount)
    => Block
    -> UTxO
    -> s
    -> ((FilteredBlock, UTxO), s)
prefilterBlock b u0 = runState $ do
    delegations <- mapMaybeM ourDelegation (b ^. #delegations)
    (transactions, ourU) <- foldM applyTx (mempty, u0) (b ^. #transactions)
    return (FilteredBlock {delegations, transactions}, ourU)
  where
    ourDelegation
        :: IsOurs s RewardAccount
        => DelegationCertificate
        -> State s (Maybe DelegationCertificate)
    ourDelegation cert =
        state (isOurs $ dlgCertAccount cert) <&> \case
            Nothing -> Nothing
            Just{}  -> Just cert
    ourWithdrawal
        :: IsOurs s RewardAccount
        => (RewardAccount, Coin)
        -> State s (Maybe (RewardAccount, Coin))
    ourWithdrawal (acct, amt) =
        state (isOurs acct) <&> \case
            Nothing -> Nothing
            Just{}  -> Just (acct, amt)
    mkTxMeta :: Coin -> Direction -> TxMeta
    mkTxMeta amount dir = TxMeta
        { status = InLedger
        , direction = dir
        , slotNo = b ^. #header . #slotNo
        , blockHeight = b ^. #header . #blockHeight
        , amount = amount
        , expiry = Nothing
        }
    txUTxO =
        let
            indexedOutputs = zip [0..] (outputs tx)
        in
            UTxO
            . Map.fromList
            . fmap (Bifunctor.first (TxIn (txId tx)))
            $ indexedOutputs
    applyTx
        :: (IsOurs s Address, IsOurs s RewardAccount)
        => ([(Tx, TxMeta)], UTxO)
        -> Tx
        -> State s ([(Tx, TxMeta)], UTxO)
    applyTx (!txs, !u) tx = do
        let x' = New.applyTx tx (_ u)

        ourU <- New.filterUTxO isOurs' u
        ourU' <- New.filterUTxO isOurs' x'

        ourWithdrawals <- Coin . sum . fmap (unCoin . snd) <$>
            mapMaybeM ourWithdrawal (Map.toList $ withdrawals tx)
        
        let received = balance $ ourU' `difference` ourU
        let spent = (balance $ ourU `difference` ourU') `TB.add` TB.fromCoin _withdrawals
        -- We separate known and "owned" concepts. "isOurs" checks ownership and
        -- a Tx is "known" to a UTxO if it's present in the UTxO.
        let hasKnownInput = inputsKnown tx ourU' /= mempty
        let hasKnownOutput = outputsKnown tx ourU' /= mempty
        let hasKnownWithdrawal = ourWithdrawals /= mempty

        -- ourU <- state $ utxoOurs tx
        -- let ourIns =
        --         Set.fromList (inputs tx)
        --             `Set.intersection`
        --                 dom (u <> ourU)
        -- let ourCollateralIns =
        --         Set.fromList (fst <$> tx ^. #resolvedCollateral)
        --             `Set.intersection`
        --                 dom (u <> ourU)
        -- u' <- applyTx'' tx u
        -- let received = balance ourU
        -- let spent =
        --         case tx ^. #isValidScript of
        --             Just False ->
        --                 balance (u `restrictedBy` ourCollateralIns) `TB.add` TB.fromCoin ourWithdrawals
        --             _ ->
        --                 balance (u `restrictedBy` ourIns) `TB.add` TB.fromCoin ourWithdrawals
        -- let hasKnownInput = ourIns /= mempty
        -- let hasKnownOutput = ourU /= mempty
        -- let hasKnownWithdrawal = ourWithdrawals /= mempty
        -- let failedScriptValidation = case tx ^. #isValidScript of
        --         Just False -> True
        --         _ -> False

        -- NOTE 1: The only case where fees can be 'Nothing' is when dealing with
        -- a Byron transaction. In which case fees can actually be calculated as
        -- the delta between inputs and outputs.
        --
        -- NOTE 2: We do not have in practice the actual input amounts, yet we
        -- do make the assumption that if one input is ours, then all inputs are
        -- necessarily ours and therefore, known as part of our current UTxO.
        let actualFee direction = case (tx ^. #fee, direction) of
                (Just x, Outgoing) -> -- Shelley and beyond.
                    Just x

                (Nothing, Outgoing) -> -- Byron
                    let
                        totalOut = sumCoins (txOutCoin <$> outputs tx)

                        totalIn = TB.getCoin spent
                    in
                        Just $ distance totalIn totalOut

                (_, Incoming) ->
                    Nothing

        return $ if hasKnownOutput && not hasKnownInput then
            let dir = Incoming in
            ( (tx { fee = actualFee dir }, mkTxMeta (TB.getCoin received) dir) : txs
            , getXUTxO ourU
            )
        else if hasKnownInput || hasKnownWithdrawal then
            let
                adaSpent = TB.getCoin spent
                -- adaReceived = if failedScriptValidation then mempty else TB.getCoin received
                adaReceived = TB.getCoin received
                dir = if adaSpent > adaReceived then Outgoing else Incoming
                amount = distance adaSpent adaReceived
            in
                ( (tx { fee = actualFee dir }, mkTxMeta amount dir) : txs
                , getXUTxO ourU
                )
        else
            (txs, u)

-- | Get the change UTxO
--
-- NOTE
-- We _safely_ discard the state here because we aren't intending to
-- discover any new addresses through this operation. In practice, we
-- can only discover new addresses when applying blocks. The state is
-- therefore use in a read-only mode here.
changeUTxO
    :: IsOurs s Address
    => Set Tx
    -> s
    -> UTxO
changeUTxO pending = evalState $
    mconcat <$> mapM (state . utxoOurs) (Set.toList pending)

-- | Construct our _next_ UTxO (possible empty) from a transaction by selecting
-- outputs that are ours. It is important for the transaction outputs to be
-- ordered correctly, since they become available inputs for the subsequent
-- blocks.
utxoOurs
    :: IsOurs s Address
    => Tx
    -> s
    -> (UTxO, s)
utxoOurs tx = runState $ do
    fmap toUtxo $ do
        let indexedOutputs = zip [0..] (outputs tx)
        xs <- forM indexedOutputs filterOut
        pure $ filterFailedValidation <$> xs
  where
    toUtxo = UTxO . Map.fromList . catMaybes
    filterOut (ix, out) = do
        state (isOurs $ address out) <&> \case
            Just{}  -> Just (TxIn (txId tx) ix, out)
            Nothing -> Nothing
    filterFailedValidation =
        case tx ^. #isValidScript of
            Just False -> const Nothing
            _ -> id

