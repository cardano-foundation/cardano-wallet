{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Wallet.Deposit.Pure.State.Payment
    ( ErrCreatePayment (..)
    , createPayment
    , createPaymentTxBody
    , CurrentEraResolvedTx
    , resolveCurrentEraTx
    , translateBalanceTxError
    ) where

import Prelude hiding
    ( lookup
    )

import Cardano.Ledger.Val
    ( isAdaOnly
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
    ( Coin
    , Tx
    , TxBody (..)
    , Value
    )
import Cardano.Wallet.Primitive.Types.Tx.Constraints
    ( TxSize (..)
    )
import Cardano.Wallet.Read
    ( AssetID (AdaID)
    , Coin (..)
    , fromEraValue
    , injectCoin
    , lookupAssetID
    , toMaryValue
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
import Data.Fixed
    ( E6
    , Fixed
    )
import Data.Text
    ( Text
    )
import Data.Text.Class.Extended
    ( ToText (..)
    )
import Numeric.Natural
    ( Natural
    )

import qualified Cardano.Read.Ledger.Value as Read.L
import qualified Cardano.Wallet.Deposit.Pure.Address as Address
import qualified Cardano.Wallet.Deposit.Read as Read
import qualified Cardano.Wallet.Deposit.Write as Write
import qualified Cardano.Wallet.Read.Hash as Hash
import qualified Control.Monad.Random.Strict as Random
import qualified Data.Map.Strict as Map
import qualified Data.Text as T

data ErrCreatePayment
    = ErrCreatePaymentNotRecentEra (Read.EraValue Read.Era)
    | ErrNotEnoughAda { shortfall :: Value }
    | ErrEmptyUTxO

    | ErrTxOutAdaInsufficient { outputIx :: Int, suggestedMinimum :: Coin }

    -- | Only possible when sending (non-ada) assets.
    | ErrTxOutValueSizeExceedsLimit { outputIx :: Int }

    -- | Only possible when sending (non-ada) assets.
    | ErrTxOutTokenQuantityExceedsLimit
        { outputIx :: Int
        , quantity :: Natural
        , quantityMaxBound :: Natural
        }

    -- | The final balanced tx was too big. Either because the payload was too
    -- big to begin with, or because we failed to select enough inputs without
    -- making it too big, e.g. due to the UTxO containing lots of dust.
    --
    -- We should ideally split out 'TooManyPayments' from this error.
    -- We should ideally also be able to create payments even when dust causes
    -- us to need preparatory txs.
    | ErrTxMaxSizeLimitExceeded{ size :: TxSize, maxSize :: TxSize }
    deriving (Eq, Show)

translateBalanceTxError :: Write.ErrBalanceTx Write.Conway -> ErrCreatePayment
translateBalanceTxError = \case
    Write.ErrBalanceTxAssetsInsufficient
        Write.ErrBalanceTxAssetsInsufficientError{shortfall} ->
            ErrNotEnoughAda
                { shortfall = fromLedgerValue shortfall
                }
    Write.ErrBalanceTxMaxSizeLimitExceeded{size, maxSize} ->
            ErrTxMaxSizeLimitExceeded{size, maxSize}
    Write.ErrBalanceTxExistingKeyWitnesses _ ->
        impossible "ErrBalanceTxExistingKeyWitnesses"
    Write.ErrBalanceTxExistingCollateral ->
        impossible "ErrBalanceTxExistingCollateral"
    Write.ErrBalanceTxExistingTotalCollateral ->
        impossible "ErrBalanceTxExistingTotalCollateral"
    Write.ErrBalanceTxExistingReturnCollateral ->
        impossible "ErrBalanceTxExistingReturnCollateral"
    Write.ErrBalanceTxInsufficientCollateral _ ->
        impossible "ErrBalanceTxInsufficientCollateral"
    Write.ErrBalanceTxAssignRedeemers _ ->
        impossible "ErrBalanceTxAssignRedeemers"
    Write.ErrBalanceTxInternalError e ->
        impossible $ show e
    Write.ErrBalanceTxInputResolutionConflicts _ ->
        -- We are never creating partialTxs with pre-selected inputs, which
        -- means this is impossible.
        impossible "conflicting input resolution"
    Write.ErrBalanceTxUnresolvedInputs _ ->
        -- We are never creating partialTxs with pre-selected inputs, which
        -- means this is impossible.
        impossible "unresolved inputs"
    Write.ErrBalanceTxUnresolvedRefunds _ ->
        impossible "unresolved refunds"
    Write.ErrBalanceTxOutputError (Write.ErrBalanceTxOutputErrorOf ix info) -> case info of
        Write.ErrBalanceTxOutputAdaQuantityInsufficient{minimumExpectedCoin} ->
            ErrTxOutAdaInsufficient
                { outputIx = ix
                , suggestedMinimum = minimumExpectedCoin
                }
        Write.ErrBalanceTxOutputSizeExceedsLimit{} ->
            ErrTxOutValueSizeExceedsLimit
                { outputIx = ix
                }
        Write.ErrBalanceTxOutputTokenQuantityExceedsLimit{quantity, quantityMaxBound} ->
            ErrTxOutTokenQuantityExceedsLimit
                { outputIx = ix
                , quantity
                , quantityMaxBound
                }
    Write.ErrBalanceTxUnableToCreateChange
        Write.ErrBalanceTxUnableToCreateChangeError{shortfall} ->
        ErrNotEnoughAda
            { shortfall = injectCoin shortfall
            }
    Write.ErrBalanceTxUnableToCreateInput ->
        ErrEmptyUTxO

  where
    fromLedgerValue v = fromEraValue (Read.L.Value v :: Read.L.Value Write.Conway)

    impossible :: String -> a
    impossible reason = error $ "impossible: translateBalanceTxError: " <> reason

instance ToText ErrCreatePayment where
    toText = \case
        ErrCreatePaymentNotRecentEra era ->
            "Cannot create a payment in the era: " <> showT era
        ErrNotEnoughAda{shortfall} -> T.unwords
            [ "Insufficient funds. Shortfall: ", prettyValue shortfall
            ]
        ErrEmptyUTxO -> "Wallet has no funds"
        ErrTxOutAdaInsufficient{outputIx, suggestedMinimum} -> T.unwords
            [ "Ada amount in output " <> showT outputIx
            , "is below the required minimum."
            , "Suggested minimum amount:", prettyCoin suggestedMinimum
            ]
        ErrTxMaxSizeLimitExceeded{size, maxSize} -> T.unlines
            [ "Exceeded the maximum size limit when creating the transaction."
                <> " (size: ", prettyTxSize size, " max size: ", prettyTxSize maxSize <> ")"
            , "\nPotential solutions:"
            , "1) Make fewer payments at the same time."
            , "2) Send smaller amounts of ada in total."
            , "3) Fund wallet with more ada."
            , "4) Make preparatory payments to yourself to coalesce dust into"
            , "larger UTxOs."
            ]
        ErrTxOutValueSizeExceedsLimit{outputIx} -> T.unwords
            [ "The size of the value of output", showT outputIx, "is too large."
            , "Try sending fewer assets or splitting them over multiple outputs."
            ]
        ErrTxOutTokenQuantityExceedsLimit{outputIx, quantity, quantityMaxBound} -> T.unwords
            [ "The asset quantity of ", showT quantity, "in output"
            , showT outputIx, ", is larger than the maximum allowed"
            , "limit", showT quantityMaxBound <> "."
            ]
      where
        showT :: Show a => a -> Text
        showT = T.pack . show

        prettyTxSize :: TxSize -> Text
        prettyTxSize (TxSize s) = T.pack (show s)

        prettyValue :: Value -> Text
        prettyValue v
            | isAdaOnly (toMaryValue v) = prettyCoin (CoinC $ lookupAssetID AdaID v)
            | otherwise                 = T.pack (show v)

        prettyCoin :: Coin -> Text
        prettyCoin c = T.pack (show c') <> "₳"
          where
            c' :: Fixed E6
            c' = toEnum $ fromEnum c

type CurrentEraResolvedTx = ResolvedTx Read.Conway

resolveCurrentEraTx :: Tx -> WalletState -> CurrentEraResolvedTx
resolveCurrentEraTx tx w = resolveInputs (availableUTxO w) tx

createPayment
    :: Read.EraValue Read.PParams
    -> Write.TimeTranslation
    -> [(Address, Write.Value)]
    -> WalletState
    -> Either ErrCreatePayment CurrentEraResolvedTx
createPayment pp tt destinations w =
    createPaymentTxBody pp tt (mkPaymentTxBody w destinations) w

-- | Create a payment to a list of destinations.
createPaymentTxBody
    :: Read.EraValue Read.PParams
    -> Write.TimeTranslation
    -> TxBody
    -> WalletState
    -> Either ErrCreatePayment CurrentEraResolvedTx
createPaymentTxBody
    (Read.EraValue (Read.PParams pparams :: Read.PParams era))
    timeTranslation
    txBody
    state =
        case Read.theEra :: Read.Era era of
            Read.Conway ->
                first translateBalanceTxError
                    $ flip resolveCurrentEraTx state
                        <$> createPaymentConway
                            pparams
                            timeTranslation
                            txBody
                            state
            era' -> Left $ ErrCreatePaymentNotRecentEra (Read.EraValue era')

mkPaymentTxBody
    :: WalletState -> [(Address, Write.Value)] -> Write.TxBody
mkPaymentTxBody w destinations =
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

-- | In the Conway era: Create a payment to a list of destinations.
createPaymentConway
    :: Write.PParams Write.Conway
    -> Write.TimeTranslation
    -> TxBody
    -> WalletState
    -> Either (Write.ErrBalanceTx Write.Conway) Write.Tx
createPaymentConway pparams timeTranslation body w =
    fmap (Read.Tx . fst)
        . flip Random.evalRand (pilferRandomGen w)
        . runExceptT
        . balance
            (availableUTxO w)
            (addresses w)
        . mkPartialTx
        $ body
  where
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
