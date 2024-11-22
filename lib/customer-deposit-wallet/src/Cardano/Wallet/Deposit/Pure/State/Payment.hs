{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Wallet.Deposit.Pure.State.Payment
    ( ErrCreatePayment (..)
    , createPayment
    , CurrentEraResolvedTx
    , resolveCurrentEraTx
    ) where

import Prelude hiding
    ( lookup
    )

import Cardano.Wallet.Deposit.Pure.State.Submissions
    ( availableUTxO
    )
import Cardano.Wallet.Deposit.Pure.State.Type
    ( WalletState (..)
    )
import Cardano.Wallet.Deposit.Pure.UTxO.Tx
    ( ResolvedTx (..)
    , resolveInputs
    )
import Cardano.Wallet.Deposit.Read
    ( Address
    )
import Cardano.Wallet.Deposit.Write
    ( Tx
    , TxBody (..)
    )
import Control.Monad.Trans.Except
    ( runExceptT
    )
import Data.Bifunctor
    ( first
    )
import Data.Digest.CRC32
    ( crc32
    )
import Data.Text.Class.Extended
    ( ToText (..)
    )

import qualified Cardano.Wallet.Deposit.Pure.Address as Address
import qualified Cardano.Wallet.Deposit.Read as Read
import qualified Cardano.Wallet.Deposit.Write as Write
import qualified Cardano.Wallet.Read.Hash as Hash
import qualified Control.Monad.Random.Strict as Random
import qualified Data.Map.Strict as Map
import qualified Data.Text as T

data ErrCreatePayment
    = ErrCreatePaymentNotRecentEra (Read.EraValue Read.Era)
    | ErrCreatePaymentBalanceTx (Write.ErrBalanceTx Write.Conway)
    deriving (Eq, Show)

instance ToText ErrCreatePayment where
    toText = \case
        ErrCreatePaymentNotRecentEra era ->
            "Cannot create a payment in the era: " <> T.pack (show era)
        ErrCreatePaymentBalanceTx err ->
            "Cannot create a payment: " <> T.pack (show err)

type CurrentEraResolvedTx = ResolvedTx Read.Conway

resolveCurrentEraTx :: Tx -> WalletState -> CurrentEraResolvedTx
resolveCurrentEraTx tx w = resolveInputs (availableUTxO w) tx

-- | Create a payment to a list of destinations.
createPayment
    :: Read.EraValue Read.PParams
    -> Write.TimeTranslation
    -> [(Address, Write.Value)]
    -> WalletState
    -> Either ErrCreatePayment Write.Tx
createPayment (Read.EraValue (Read.PParams pparams :: Read.PParams era)) a b w =
    case Read.theEra :: Read.Era era of
        Read.Conway ->
            first ErrCreatePaymentBalanceTx
                $ createPaymentConway pparams a b w
        era' -> Left $ ErrCreatePaymentNotRecentEra (Read.EraValue era')

-- | In the Conway era: Create a payment to a list of destinations.
createPaymentConway
    :: Write.PParams Write.Conway
    -> Write.TimeTranslation
    -> [(Address, Write.Value)]
    -> WalletState
    -> Either (Write.ErrBalanceTx Write.Conway) Write.Tx
createPaymentConway pparams timeTranslation destinations w =
    fmap (Read.Tx . fst)
        . flip Random.evalRand (pilferRandomGen w)
        . runExceptT
        . balance
            (availableUTxO w)
            (addresses w)
        . mkPartialTx
        $ paymentTxBody
  where
    paymentTxBody :: Write.TxBody
    paymentTxBody =
        Write.TxBody
            { spendInputs = mempty
            , collInputs = mempty
            , txouts =
                Map.fromList
                    $ zip [(toEnum 0) ..]
                    $ map (uncurry Write.mkTxOut) destinations
            , collRet = Nothing
            , expirySlot = Just . computeExpirySlot $ walletTip w
            }

    mkPartialTx :: Write.TxBody -> Write.PartialTx Write.Conway
    mkPartialTx txbody =
        Write.PartialTx
            { tx = Read.unTx $ Write.mkTx txbody
            , extraUTxO = mempty :: Write.UTxO Write.Conway
            , redeemers = mempty
            , stakeKeyDeposits = Write.StakeKeyDepositMap mempty
            , timelockKeyWitnessCounts = Write.TimelockKeyWitnessCounts mempty
            }

    balance utxo addressState =
        Write.balanceTx
            pparams
            timeTranslation
            Write.AllKeyPaymentCredentials
            (Write.constructUTxOIndex $ Write.toConwayUTxO utxo)
            (changeAddressGen addressState)
            ()

    changeAddressGen s =
        Write.ChangeAddressGen
            { Write.genChangeAddress =
                first Read.decompactAddr . Address.newChangeAddress s
            , Write.maxLengthChangeAddress =
                Read.decompactAddr $ Address.mockMaxLengthChangeAddress s
            }

-- | Use entropy contained in the current 'WalletState'
-- to construct a pseudorandom seed.
-- (NOT a viable source of cryptographic randomness.)
--
-- Possible downsides of this approach:
--
-- 1. security/privacy
-- 2. concurrency
-- 3. retries for different coin selections
pilferRandomGen :: WalletState -> Random.StdGen
pilferRandomGen =
    Random.mkStdGen . fromEnum . fromChainPoint . walletTip
  where
    fromChainPoint (Read.GenesisPoint) = 0
    fromChainPoint (Read.BlockPoint _ headerHash) =
        crc32 $ Hash.hashToBytes headerHash

-- | Compute an expiry slot from a current 'ChainPoint'.
computeExpirySlot :: Read.ChainPoint -> Read.SlotNo
computeExpirySlot Read.GenesisPoint = 0
computeExpirySlot (Read.BlockPoint slotNo _) =
    slotNo + hour
  where
    hour = 60 * 60
