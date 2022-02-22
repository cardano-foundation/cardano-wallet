{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fdefer-typed-holes -Wno-typed-holes #-}

-- |
-- Copyright: © 2018-2022 IOHK
-- License: Apache-2.0
-- Stability: experimental
--
-- Mock-up of a multi-asset coin selection interface for UTxO blockchains.
--
-- The goal is to come up with the simplest possible API which works for a
-- Cardano wallet, without committing to concrete types and implementations.

module CoinSelection
    ( -- * Requirements
      --
      -- $req

      -- * Coin selection
      --
      -- $naming
      xyzzy
    , Selector (..)
    , Change (..)
    , SelectionError (..)
    -- * The transaction type
    , TxBody (..)
    , Value
    , TxInput (..)
    , TxOutput (..)
    -- ** Information about transactions
    , Params (..)
    , AddressOwner (..)
    ) where

import Prelude

import GHC.Generics (Generic)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Map (Map)

-- $req
-- This is what we need the coin selection to be capable of.
--
-- __Use cases__
--
--  * Payment
--
--  * Join stake pool
--
--  * Spend 'Value' from reward account
--
--  * Configurable change generation
--
--  * Wallet v'Sweep'
--
--  * Active UTxO management
--
--        * Coalesce UTxO
--        * Create /n/ new UTxO
--
--  * Emergent UTxO management => Add a change output to input tx
--
-- __Constraints__
--
--  * Minimum UTxO 'Value'
--  * Script transactions require selection of a suitable "collateral" "TxOutput".
--  * Maximum 'TxBody' size.
--
--      Usually in bytes, but this module does not prescribe a unit.
--
--  * Maximum 'TxOut' size.
--
--      For Cardano there are two maximums - one for
--      serialized size of the 'TxOut' in bytes, and one for the maximum number of
--      assets in a 'Value'.
--
--      Again, this module does not prescribe any particular unit.
--
--  * what is reward account balance?
--
-- __Optimization__
--
-- The coin selection function must minimize costs to the user. The following things should be part of the coin selection function:
--
--  * Fee minimization.
--  * Use the smallest possible number of transactions to satisfy request.
--  * Over time, in the long run, eventually, utxo distribution should evolve to match payment distribution (emergent utxo management).
--
-- __Algorithms__
--
--  * v'Sweep' - Select /all/ unspent coins, regardless of transaction outputs.
--  * v'RandomImprove'
--  * Random-Improve-MA - Multi-asset variant of Random-Improve.
--

-- | The transaction type is polymorphic and defined by the caller. Coin
-- selection however needs to get certain information about the transaction
-- under construction.
--
-- This class defines the types of transaction components and therefore
-- represents transactions which can have the coin selection algorithms of this
-- module applied to them.
class (Monoid (Amount tx), Ord (Amount tx), Ord (Cost tx)) => TxBody tx where
    -- | A transaction input identifier - the source of funds for transaction.
    type TxIn tx

    -- | Payment destination for transaction.
    type Address tx

    -- | This type uniquely identifies a fungible asset.
    type AssetId tx

    -- | Quantity type of any given 'AssetId'.
    --
    -- The 'xyzzy' function requires this type to be a 'Monoid' under addition.
    type Amount tx

    -- | How fees are measured for this type of transaction. Its instance type
    -- needn't necessarily be a monetary value.
    --
    -- The 'xyzzy' function requires 'Cost' values to be comparable, so that it
    -- can minimize fees.
    type Cost tx

    -- | Get the outputs of a 'TxBody'.
    txOutputs :: Foldable f => tx -> f (TxOutput tx)

    -- | Append an output to the 'TxBody'.
    appendTxOutput :: TxOutput tx -> tx -> tx

    -- | Get the inputs of a 'TxBody'
    txInputs :: Foldable f => tx -> f (TxInput tx)

    -- | Add an input to the 'TxBody' input set.
    addTxInput :: TxInput tx -> tx -> tx

-- | A list of assets and their quantity.
type Value tx = Map (AssetId tx) (Amount tx)

-- | An abstract transaction output is defined as a payment destination and a
-- 'Value' amount.
data TxOutput tx = TxOutput
    { outputaddress :: !(Address tx)
    , outputValue   :: !(Value tx)
    } deriving Generic

-- | An abstract transaction input is defined as 'TxIn' identifier paired with
-- its 'Value' amount.
--
-- Objects of this type are used for funding transactions, and are usually taken
-- from the wallet's set of available UTxO.
data TxInput tx = TxInput
    { souceTxIn   :: !(TxIn tx)
    , sourceValue :: !(Value tx)
    } deriving Generic

-- | Describes an 'Address' as being as part of either a change output or a
-- payment output.
data AddressOwner
    = OurAddress
    -- ^ Using the address in a tx output would increase this wallet's balance.
    | NotOurAddress
    -- ^ Using the address in a tx output would not increase this wallet's
    -- balance.
    deriving (Show, Read, Eq, Enum, Generic)

-- | This record of functions provides implementations of 'TxBody' functions
-- needed by 'xyzzy'.
data Params tx = Params
    { marginalCostOfInput  :: tx -> TxIn tx -> Cost tx
    -- ^ Calculate the relative price of adding a given input to a transaction.

    , marginalCostOfOutput :: tx -> TxOutput tx -> Cost tx
    -- ^ Calculate the relative price of adding a given output to the given
    -- transaction.

    , isTxOutSizeOk        :: tx -> TxOutput tx -> TxIn tx -> Bool
    -- ^ Predicate: If a given 'TxIn' were added into the given 'TxOut', would
    -- the size be less than the maximum output size.

    , isTxOutValueOk       :: tx -> Value tx -> Bool
    -- ^ Predicate: is 'TxOut' sufficiently large (i.e. less than min utxo)?

    , isOurAddress         :: Address tx -> AddressOwner
    -- ^ Classifies addresses as either the recipient's or our own.
    } deriving Generic

-- | How to choose inputs for a transaction.
data Selector
    = Sweep
    -- ^ Use all possible unspent transaction inputs, regardless of the
    -- requested payment amount. This is called a wallet "sweep" -- where all
    -- coins from the wallet are shifted together.
    | RandomImprove
    -- ^ Use the /Random-Improve-MA/ algorithm for selecting inputs. This will
    -- select approximately double the amount necessary for the payment.
    deriving (Show, Read, Eq, Enum, Generic)

-- | How to assign excess value to change outputs.
data Change
    = Balanced
    -- ^ Value is distributed evenly between change outputs.
    | Proportional
    -- ^ Value is distributed between change outputs in proportion to payment
    -- outputs. Use this with v'RandomImprove' for /emergent/ management of the
    -- wallet's UTxO.
    | Unbalanced
    -- ^ Value is added to the first change output until it reaches the maximum
    -- size. Then the next change output will receive value.
    deriving (Show, Read, Eq, Enum, Generic)

-- | Placeholder for the type of possible failure cases in coin selection.
newtype SelectionError = SelectionError String -- ^ Something went wrong.
    deriving (Show, Read, Eq, Generic)

-- $naming
-- We call the coin selection function 'xyzzy' in lieu of a better name.
--
-- Possible names could be @balanceTransaction@, @selectCoins@, or
-- something like that.

-- | The top-level coin selection function.
--
-- Given an initial partially funded transaction body, it will produce a
-- complete, funded, and balanced transaction body from the provided 'UTxO'.
--
-- If successful, it returns one or more transactions, otherwise the reason for
-- failure is provided.
--
-- __How to use__
--
-- The main interface of 'xyzzy' is its parameter @tx@ the partial
-- transaction. The caller specifies the outputs of @tx@, and 'xyzzy' calculates
-- a balanced 'TxBody' from that.
--
-- [Payment]: Add a 'TxOut' to the initial @tx@ with the recipient
--   'Address' and 'Value'.
-- [Change Output]: Add a 'TxOut' to the initial @tx@ such that the 'Address'
--   satisfies 'isOurAddress'. Its 'Value' would usually be zero.
--   If the maximum output size is reached, another 'TxOut' with the same
--   address will be added to the transaction.
-- [No new change addresses]: If there is no transaction output that satisfies
--   v'IsOurAddress', then change will be directed back to the addresses
--   belonging to the wallet which funded the transaction.
-- [Join stake pool]: This transaction will involve a delegation certificate,
--   and possibly also a stake key registration certificate. These cost money.
--   However 'xyzzy', is not necessarily Cardano-specific or even POS-specific.
--   Therefore, the 'Address' type for @tx@ should include a constructor for
--   certificates. The implementation of 'appendTxOutput' should put these
--   "certificate" address outputs in the correct place within the concrete
--   'TxBody' type.
-- [Withdraw from reward account]: Reward account withdrawals add value to the
--   transaction. The 'TxIn' type for @tx@ should include a constructor for
--   withdrawals. Put this 'TxIn' either in the 'UTxO' list or in the initial
--   @tx@.
-- [Coalesce UTxO]: Add a transaction output with an address belonging to the
--   wallet and pass the v'Sweep' option.
-- [Split UTxO]: Add several transaction outputs with addresses belonging to the
--   wallet. Use the v'Balanced' option to distribute funds evenly.
-- [Spending particular UTxO]: To ensure that a particular UTxO(s) is/are spent,
--   add them to the 'TxIn' set of the initial @tx@.
-- [Get fee]: Caller should calculate the difference of inputs and outputs
--   in the returned transaction.
--
xyzzy
    :: forall f tx. (Foldable f, TxBody tx)
    => Params tx
    -- ^ Transaction constraints and cost functions. These are specific
    -- characteristics of the blockchain.
    -> Selector
    -- ^ How to select inputs from the UTxO.
    -> Change
    -- ^ How to assign excess value to change outputs.
    -> f (TxInput tx)
    -- ^ Transaction outputs available to use as inputs of a this transaction.
    -> tx
    -- ^ Partial transaction body which is the starting point for coin selection.
    -> Either SelectionError (NonEmpty tx)
    -- ^ One or more funded and balanced transaction bodies, or an error message.
xyzzy _params sel = case sel of
    Sweep -> _
    RandomImprove -> _
