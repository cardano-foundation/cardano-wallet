{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Copyright: © 2018-2022 IOHK
-- License: Apache-2.0
--
-- 'IsServerError' definition along with most instances
--
module Cardano.Wallet.Api.Http.Server.Error
  ( IsServerError (..)
  , liftHandler
  , liftE
  , apiError
  , err425
  , showT
  )
  where

import Prelude

import Cardano.Address.Script
    ( Cosigner (..) )
import Cardano.Ledger.Alonzo.TxInfo
    ( TranslationError (..) )
import Cardano.Tx.Balance.Internal.CoinSelection
    ( SelectionBalanceError (..)
    , SelectionCollateralError
    , SelectionError (..)
    , SelectionOutputError (..)
    , SelectionOutputErrorInfo (..)
    , SelectionOutputSizeExceedsLimitError
    , SelectionOutputTokenQuantityExceedsLimitError (..)
    , UnableToConstructChangeError (..)
    , WalletSelectionContext
    )
import Cardano.Wallet
    ( ErrAddCosignerKey (..)
    , ErrCannotJoin (..)
    , ErrCannotQuit (..)
    , ErrConstructSharedWallet (..)
    , ErrConstructTx (..)
    , ErrCreateMigrationPlan (..)
    , ErrCreateRandomAddress (..)
    , ErrDecodeTx (..)
    , ErrDerivePublicKey (..)
    , ErrFetchRewards (..)
    , ErrGetPolicyId (..)
    , ErrGetTransaction (..)
    , ErrImportAddress (..)
    , ErrImportRandomAddress (..)
    , ErrInvalidDerivationIndex (..)
    , ErrListTransactions (..)
    , ErrListUTxOStatistics (..)
    , ErrMkTransaction (..)
    , ErrNoSuchTransaction (..)
    , ErrNoSuchWallet (..)
    , ErrNotASequentialWallet (..)
    , ErrPostTx (..)
    , ErrReadAccountPublicKey (..)
    , ErrReadPolicyPublicKey (..)
    , ErrReadRewardAccount (..)
    , ErrRemoveTx (..)
    , ErrSelectAssets (..)
    , ErrSignMetadataWith (..)
    , ErrSignPayment (..)
    , ErrStakePoolDelegation (..)
    , ErrStartTimeLaterThanEndTime (..)
    , ErrSubmitTransaction (..)
    , ErrSubmitTx (..)
    , ErrUpdatePassphrase (..)
    , ErrUpdateSealedTx (..)
    , ErrWalletAlreadyExists (..)
    , ErrWalletNotResponding (..)
    , ErrWithRootKey (..)
    , ErrWithdrawalNotBeneficial (..)
    , ErrWitnessTx (..)
    , ErrWritePolicyPublicKey (..)
    , ErrWrongPassphrase (..)
    , WalletException (..)
    )
import Cardano.Wallet.Api.Types
    ( ApiCosignerIndex (..), ApiCredentialType (..), Iso8601Time (..) )
import Cardano.Wallet.Api.Types.Error
    ( ApiError (..)
    , ApiErrorInfo (..)
    , ApiErrorMessage (..)
    , ApiErrorSharedWalletNoSuchCosigner (..)
    , ApiErrorTxOutputLovelaceInsufficient (..)
    )
import Cardano.Wallet.Primitive.AddressDerivation
    ( DerivationType (Hardened, Soft), Index (Index) )
import Cardano.Wallet.Primitive.AddressDiscovery.Shared
    ( ErrAddCosigner (..), ErrScriptTemplate (..) )
import Cardano.Wallet.Primitive.Slotting
    ( PastHorizonException )
import Cardano.Wallet.Primitive.Types.TokenMap
    ( Flat (..) )
import Cardano.Wallet.Transaction
    ( ErrAssignRedeemers (..), ErrSignTx (..) )
import Cardano.Wallet.Write.Tx.Balance
    ( ErrBalanceTx (..), ErrBalanceTxInternalError (..) )
import Control.Monad.Except
    ( ExceptT, withExceptT )
import Control.Monad.Trans.Except
    ( throwE )
import Data.Generics.Internal.VL
    ( view, (^.) )
import Data.IntCast
    ( intCastMaybe )
import Data.List
    ( isInfixOf, isPrefixOf, isSubsequenceOf )
import Data.Maybe
    ( isJust )
import Data.Text
    ( Text )
import Data.Text.Class
    ( ToText (..) )
import Data.Word
    ( Word32 )
import Fmt
    ( blockListF', build, fmt, listF, pretty )
import Network.HTTP.Media
    ( renderHeader )
import Network.HTTP.Types
    ( hContentType )
import Network.Wai
    ( Request (pathInfo) )
import Safe
    ( fromJustNote )
import Servant
    ( Accept (contentType), JSON, Proxy (Proxy) )
import Servant.Server
    ( Handler (Handler)
    , ServerError (..)
    , err400
    , err403
    , err404
    , err409
    , err500
    , err501
    , err503
    )

import qualified Cardano.Api as Cardano
import qualified Cardano.Wallet.Primitive.Types.Coin as Coin
import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle
import qualified Cardano.Wallet.Primitive.Types.TokenMap as TokenMap
import qualified Cardano.Wallet.Write.Tx as WriteTx
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Foldable as F
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

-- | Maps types to servant error responses.
class IsServerError e where
    -- | A structured human-readable error code to return to API clients.
    toServerError :: e -> ServerError

-- | Lift our wallet layer into servant 'Handler', by mapping each error to a
-- corresponding servant error.
liftHandler :: IsServerError e => ExceptT e IO a -> Handler a
liftHandler action = Handler (withExceptT toServerError action)

liftE :: IsServerError e => e -> Handler a
liftE = liftHandler . throwE

apiError :: ServerError -> ApiErrorInfo -> Text -> ServerError
apiError err info messageUnformatted = err
    { errBody = Aeson.encode ApiError {info, message}
    , errHeaders =
        (hContentType, renderHeader $ contentType $ Proxy @JSON)
        : errHeaders err
    }
  where
    message = ApiErrorMessage (T.replace "\n" " " messageUnformatted)

err425 :: ServerError
err425 = ServerError 425 "Too early" "" []

-- | Small helper to easy show things to Text
showT :: Show a => a -> Text
showT = T.pack . show

instance IsServerError WalletException where
    toServerError = \case
        ExceptionNoSuchWallet e -> toServerError e
        ExceptionSignMetadataWith e -> toServerError e
        ExceptionDerivePublicKey e -> toServerError e
        ExceptionAddCosignerKey e -> toServerError e
        ExceptionConstructSharedWallet e -> toServerError e
        ExceptionReadAccountPublicKey e -> toServerError e
        ExceptionListUTxOStatistics e -> toServerError e
        ExceptionSignPayment e -> toServerError e
        ExceptionBalanceTx e -> toServerError e
        ExceptionBalanceTxInternalError e -> toServerError e
        ExceptionSubmitTransaction e -> toServerError e
        ExceptionConstructTx e -> toServerError e
        ExceptionGetPolicyId e -> toServerError e
        ExceptionWitnessTx e -> toServerError e
        ExceptionDecodeTx e -> toServerError e
        ExceptionSubmitTx e -> toServerError e
        ExceptionUpdatePassphrase e -> toServerError e
        ExceptionWithRootKey e -> toServerError e
        ExceptionListTransactions e -> toServerError e
        ExceptionGetTransaction e -> toServerError e
        ExceptionStartTimeLaterThanEndTime e -> toServerError e
        ExceptionCreateMigrationPlan e -> toServerError e
        ExceptionSelectAssets e -> toServerError e
        ExceptionStakePoolDelegation e -> toServerError e
        ExceptionFetchRewards e -> toServerError e
        ExceptionWalletNotResponding e -> toServerError e
        ExceptionCreateRandomAddress e -> toServerError e
        ExceptionImportRandomAddress e -> toServerError e
        ExceptionNotASequentialWallet e -> toServerError e
        ExceptionReadRewardAccount e -> toServerError e
        ExceptionWithdrawalNotBeneficial e -> toServerError e
        ExceptionReadPolicyPublicKey e -> toServerError e
        ExceptionWritePolicyPublicKey e -> toServerError e
        ExceptionSoftDerivationIndex e -> toServerError e
        ExceptionHardenedDerivationIndex e -> toServerError e

instance IsServerError ErrNoSuchWallet where
    toServerError = \case
        ErrNoSuchWallet wid ->
            apiError err404 NoSuchWallet $ mconcat
                [ "I couldn't find a wallet with the given id: "
                , toText wid
                ]

instance IsServerError ErrWalletNotResponding where
    toServerError = \case
        ErrWalletNotResponding wid ->
            apiError err500 WalletNotResponding $ T.unwords
                [ "That's embarrassing. My associated worker for", toText wid
                , "is no longer responding. This is not something that is supposed"
                , "to happen. The worker must have left a trace in the logs of"
                , "severity 'Error' when it died which might explain the cause."
                , "Said differently, this wallet won't be accessible until the"
                , "server is restarted but there are good chances it'll recover"
                , "itself upon restart."
                ]

instance IsServerError ErrWalletAlreadyExists where
    toServerError = \case
        ErrWalletAlreadyExists wid ->
            apiError err409 WalletAlreadyExists $ mconcat
                [ "This operation would yield a wallet with the following id: "
                , toText wid
                , " However, I already know of a wallet with this id."
                ]

instance IsServerError ErrWithRootKey where
    toServerError = \case
        ErrWithRootKeyNoRootKey wid ->
            apiError err403 NoRootKey $ mconcat
                [ "I couldn't find a root private key for the given wallet: "
                , toText wid, ". However, this operation requires that I do "
                , "have such a key. Either there's no such wallet, or I don't "
                , "fully own it."
                ]
        ErrWithRootKeyWrongPassphrase wid ErrWrongPassphrase ->
            apiError err403 WrongEncryptionPassphrase $ mconcat
                [ "The given encryption passphrase doesn't match the one I use "
                , "to encrypt the root private key of the given wallet: "
                , toText wid
                ]
        ErrWithRootKeyWrongMnemonic wid ->
            apiError err403 WrongMnemonic $ mconcat
                [ "The given mnemonic doesn't match the one this wallet was created with "
                , ": "
                , toText wid
                ]
        ErrWithRootKeyWrongPassphrase wid (ErrPassphraseSchemeUnsupported s) ->
            apiError err501 WrongEncryptionPassphrase $ mconcat
                [ "This build is not compiled with support for the "
                , toText s <> " scheme used by the given wallet: "
                , toText wid
                ]

instance IsServerError ErrListUTxOStatistics where
    toServerError = \case
        ErrListUTxOStatisticsNoSuchWallet e -> toServerError e

instance IsServerError ErrSignPayment where
    toServerError = \case
        ErrSignPaymentMkTx e -> toServerError e
        ErrSignPaymentNoSuchWallet e -> (toServerError e)
            { errHTTPCode = 404
            , errReasonPhrase = errReasonPhrase err404
            }
        ErrSignPaymentWithRootKey e@ErrWithRootKeyNoRootKey{} -> (toServerError e)
            { errHTTPCode = 403
            , errReasonPhrase = errReasonPhrase err403
            }
        ErrSignPaymentWithRootKey e@ErrWithRootKeyWrongPassphrase{} -> toServerError e
        ErrSignPaymentWithRootKey e@ErrWithRootKeyWrongMnemonic{} -> toServerError e
        ErrSignPaymentIncorrectTTL e -> toServerError e

instance IsServerError ErrWitnessTx where
    toServerError = \case
        ErrWitnessTxSignTx e -> toServerError e
        ErrWitnessTxNoSuchWallet e -> (toServerError e)
            { errHTTPCode = 404
            , errReasonPhrase = errReasonPhrase err404
            }
        ErrWitnessTxWithRootKey e@ErrWithRootKeyNoRootKey{} -> (toServerError e)
            { errHTTPCode = 403
            , errReasonPhrase = errReasonPhrase err403
            }
        ErrWitnessTxWithRootKey e@ErrWithRootKeyWrongPassphrase{} -> toServerError e
        ErrWitnessTxWithRootKey e@ErrWithRootKeyWrongMnemonic{} -> toServerError e
        ErrWitnessTxIncorrectTTL e -> toServerError e

instance IsServerError ErrSignTx where
    toServerError = \case
        ErrSignTxAddressUnknown txin ->
            apiError err500 KeyNotFoundForAddress $ mconcat
                [ "I couldn't sign the given transaction because I "
                , "could not resolve the address of a transaction input "
                , "that I should be tracking: ", showT txin, "."
                ]
        ErrSignTxUnimplemented ->
            apiError err501 NotImplemented
                "This feature is not yet implemented."

instance IsServerError ErrMkTransaction where
    toServerError = \case
        ErrMkTransactionTxBodyError hint ->
            apiError err500 CreatedInvalidTransaction hint
        ErrMkTransactionInvalidEra _era ->
            apiError err500 CreatedInvalidTransaction $ mconcat
                [ "Whoops, it seems like I just experienced a hard-fork in the "
                , "middle of other tasks. This is a pretty rare situation but "
                , "as a result, I must throw away what I was doing. Please "
                , "retry your request."
                ]
        ErrMkTransactionJoinStakePool e -> toServerError e
        ErrMkTransactionQuitStakePool e -> toServerError e
        ErrMkTransactionNoSuchWallet wid -> toServerError (ErrNoSuchWallet wid)
        ErrMkTransactionIncorrectTTL e -> toServerError e

instance IsServerError ErrConstructTx where
    toServerError = \case
        ErrConstructTxWrongPayload ->
            apiError err403 CreatedInvalidTransaction $ mconcat
            [ "It looks like I've created an empty transaction "
            , "that does not have any payments, withdrawals, delegations, "
            , "metadata nor minting. Include at least one of them."
            ]
        ErrConstructTxBody e -> toServerError e
        ErrConstructTxNoSuchWallet e -> (toServerError e)
            { errHTTPCode = 404
            , errReasonPhrase = errReasonPhrase err404
            }
        ErrConstructTxReadRewardAccount e -> toServerError e
        ErrConstructTxIncorrectTTL e -> toServerError e
        ErrConstructTxMultidelegationNotSupported ->
            apiError err403 CreatedMultidelegationTransaction $ mconcat
            [ "It looks like I've created a transaction "
            , "with multiple delegations, which is not supported at this moment. "
            , "Please use at most one delegation action: join, quit or none."
            ]
        ErrConstructTxMultiaccountNotSupported ->
            apiError err403 CreatedMultiaccountTransaction $ mconcat
            [ "It looks like I've created a transaction "
            , "with a delegation, which uses a stake key for the unsupported account. "
            , "Please use delegation action engaging '0H' account."
            ]
        ErrConstructTxWrongMintingBurningTemplate ->
            apiError err403 CreatedWrongPolicyScriptTemplate $ mconcat
            [ "It looks like I've created a transaction with a minting/burning "
            , "policy script that either does not pass validation, contains "
            , "more than one cosigner, or has a cosigner that is different "
            , "from cosigner#0."
            ]
        ErrConstructTxAssetNameTooLong ->
            apiError err403 AssetNameTooLong $ mconcat
            [ "Attempted to create a transaction with an asset name that is "
            , "too long. The maximum length is 32 bytes."
            ]
        ErrConstructTxMintOrBurnAssetQuantityOutOfBounds ->
            apiError err403 MintOrBurnAssetQuantityOutOfBounds $ mconcat
            [ "Attempted to mint or burn an asset quantity that is out of "
            , "bounds. The asset quantity must be greater than zero and must "
            , "not exceed 9223372036854775807 (2^63 - 1)."
            ]
        ErrConstructTxWrongValidityBounds ->
            apiError err403 InvalidValidityBounds $ T.unwords
            [ "Attempted to create a transaction with invalid validity bounds."
            , "Please make sure that the 'invalid_before' bound precedes the"
            , "'invalid_hereafter' bound, and that you have not used negative"
            , "time values."
            ]
        ErrConstructTxValidityIntervalNotWithinScriptTimelock ->
            apiError err403 ValidityIntervalNotInsideScriptTimelock $ T.unwords
            [ "Attempted to create a transaction with a validity interval"
            , "that is not a subinterval of an associated script's timelock"
            , "interval."
            ]
        ErrConstructTxSharedWalletIncomplete ->
            apiError err403 SharedWalletIncomplete $ T.unwords
            [ "I cannot construct a transaction for a shared wallet that is"
            , "in the 'incomplete' state. Please update your wallet accordingly"
            , "with"
            , "'PATCH /shared-wallets/{walletId}/payment-script-template'"
            , "or"
            , "'PATCH /shared-wallets/{walletId}/delegation-script-template'"
            , "to make it suitable for constructing transactions."
            ]
        ErrConstructTxStakingInvalid ->
            apiError err403 StakingInvalid $ T.unwords
            [ "I cannot construct a delegating transaction for a shared wallet "
            , "that is lacking a delegation script template."
            ]

instance IsServerError ErrGetPolicyId where
    toServerError = \case
        ErrGetPolicyIdReadPolicyPubliKey e -> toServerError e
        ErrGetPolicyIdWrongMintingBurningTemplate ->
            apiError err403 CreatedWrongPolicyScriptTemplate $ mconcat
            [ "It looks like policy id is requested for a "
            , "policy script that either does not pass validation, contains "
            , "more than one cosigner, or has a cosigner that is different "
            , "from cosigner#0."
            ]

instance IsServerError ErrDecodeTx where
    toServerError = \case
        ErrDecodeTxNoSuchWallet e -> (toServerError e)
            { errHTTPCode = 404
            , errReasonPhrase = errReasonPhrase err404
            }

instance IsServerError WriteTx.ErrInvalidTxOutInEra where
     toServerError = \case
         WriteTx.ErrInlineDatumNotSupportedInAlonzo ->
             apiError err400 BalanceTxInlineDatumsNotSupportedInAlonzo
                 "Inline datums are not supported in the Alonzo era."
         WriteTx.ErrInlineScriptNotSupportedInAlonzo ->
             apiError err400 BalanceTxInlineScriptsNotSupportedInAlonzo
                 "Inline scripts are not supported in the Alonzo era."

instance IsServerError ErrBalanceTx where
    toServerError = \case
        ErrOldEraNotSupported (Cardano.AnyCardanoEra era) ->
            apiError err403 BalanceTxEraNotSupported $ T.unwords
                [ "Balancing in ", showT era, " "
                , "is not supported."
                ]
        ErrBalanceTxUpdateError (ErrExistingKeyWitnesses n) ->
            apiError err403 BalanceTxExistingKeyWitnesses $ mconcat
                [ "The transaction could not be balanced, because it contains "
                , T.pack (show n), " "
                , "existing key-witnesses which would be invalid after "
                , "the transaction body is modified. "
                , "Please sign the transaction after it is balanced instead."
                ]
        ErrBalanceTxSelectAssets err -> toServerError err
        ErrBalanceTxAssignRedeemers err -> toServerError err
        ErrBalanceTxConflictingNetworks ->
            apiError err403 BalanceTxConflictingNetworks $ T.unwords
                [ "There are withdrawals for multiple networks (e.g. both"
                , "mainnet and testnet) in the provided transaction. This"
                , "makes no sense, and I'm confused."
                ]
        ErrBalanceTxExistingCollateral ->
            apiError err403 BalanceTxExistingCollateral
                "I cannot balance transactions with pre-defined collateral."

        ErrBalanceTxExistingTotalCollateral ->
            apiError err403 BalanceTxExistingTotalCollateral $ T.unwords
                [ "I cannot balance transactions"
                , "with pre-defined total collateral."
                ]
        ErrBalanceTxExistingReturnCollateral ->
            apiError err403 BalanceTxExistingReturnCollateral $ T.unwords
                [ "Balancing transactions with pre-defined"
                , "collateral return outputs is not yet supported."
                ]
        ErrBalanceTxInternalError e -> toServerError e
        ErrBalanceTxMaxSizeLimitExceeded ->
            apiError err403 BalanceTxMaxSizeLimitExceeded $ T.unwords
                [ "I was not able to balance the transaction without exceeding"
                , "the maximum transaction size."
                ]
        ErrBalanceTxUnresolvedInputs ins ->
            apiError err400 UnresolvedInputs $ T.unwords
                [ "There are inputs in the transaction for which corresponding"
                , "outputs could not be found:\n"
                , pretty $ NE.toList ins
                ]
        ErrBalanceTxInputResolutionConflicts conflicts -> do
            let conflictF (a, b) = build a <> "\nvs\n" <> build b
            apiError err400 InputResolutionConflicts $ mconcat
                [ "At least one of the inputs you've told me about has an"
                , "asset quantity or address that is different from that"
                , "recorded in the wallet's UTxO set."
                , "\n"
                , "The conflict(s) are:\n"
                , fmt $ blockListF' "-" conflictF conflicts
                ]

instance IsServerError ErrBalanceTxInternalError where
    toServerError = \case
        ErrUnderestimatedFee coin _st ->
            apiError err500 BalanceTxUnderestimatedFee $ T.unwords
                [ "I have somehow underestimated the fee of the transaction by"
                , pretty coin
                , "and cannot finish balancing."
                ]
        ErrFailedBalancing v ->
            apiError err500 BalanceTxInternalError $ T.unwords
                [ "I have somehow failed to balance the transaction."
                , "The balance is"
                , T.pack (show v)
                ]

instance IsServerError ErrRemoveTx where
    toServerError = \case
        ErrRemoveTxNoSuchWallet wid -> toServerError wid
        ErrRemoveTxNoSuchTransaction (ErrNoSuchTransaction _wid tid) ->
            apiError err404 NoSuchTransaction $ mconcat
                [ "I couldn't find a transaction with the given id: "
                , toText tid
                ]
        ErrRemoveTxAlreadyInLedger tid ->
            apiError err403 TransactionAlreadyInLedger $ mconcat
                [ "The transaction with id: ", toText tid,
                  " cannot be forgotten as it is already in the ledger."
                ]

instance IsServerError ErrPostTx where
    toServerError = \case
        ErrPostTxValidationError err ->
            apiError err500 CreatedInvalidTransaction $ mconcat
                [ "The submitted transaction was rejected by the local "
                , "node. Here's an error message that may help with "
                , "debugging:\n", err
                ]
        ErrPostTxMempoolFull ->
            apiError err425
            {errBody = "Mempool is full, please try resubmitting again later."}
                MempoolIsFull $ mconcat
                [ "The submitted transaction was rejected by the Cardano node "
                , "because its mempool was full."
                ]

instance IsServerError ErrSubmitTransaction where
    toServerError = \case
        ErrSubmitTransactionNoSuchWallet e -> (toServerError e)
            { errHTTPCode = 404
            , errReasonPhrase = errReasonPhrase err404
            }
        ErrSubmitTransactionForeignWallet ->
            apiError err403 ForeignTransaction $ mconcat
                [ "The transaction to be submitted is foreign to the current wallet "
                , "and cannot be sent. Submit a transaction that has either input "
                , "or withdrawal belonging to the wallet."
                ]
        ErrSubmitTransactionPartiallySignedOrNoSignedTx expectedWitsNo foundWitsNo ->
            apiError err403 MissingWitnessesInTransaction $ mconcat
                [ "The transaction expects ", toText expectedWitsNo
                , " witness(es) to be fully-signed but ", toText foundWitsNo, " was provided."
                , " Submit fully-signed transaction."
                ]
        ErrSubmitTransactionMultidelegationNotSupported ->
            apiError err403 CreatedMultidelegationTransaction $ mconcat
            [ "It looks like the transaction to be sent contains"
            , "multiple delegations, which is not supported at this moment."
            , "Please use at most one delegation action in a submitted transaction: join, quit or none."
            ]

instance IsServerError ErrSubmitTx where
    toServerError = \case
        ErrSubmitTxNetwork e -> toServerError e
        ErrSubmitTxNoSuchWallet e@ErrNoSuchWallet{} -> (toServerError e)
            { errHTTPCode = 404
            , errReasonPhrase = errReasonPhrase err404
            }
        ErrSubmitTxImpossible e -> toServerError e

instance IsServerError ErrUpdatePassphrase where
    toServerError = \case
        ErrUpdatePassphraseNoSuchWallet e -> toServerError e
        ErrUpdatePassphraseWithRootKey e  -> toServerError e

instance IsServerError ErrListTransactions where
    toServerError = \case
        ErrListTransactionsNoSuchWallet e -> toServerError e
        ErrListTransactionsStartTimeLaterThanEndTime e -> toServerError e
        ErrListTransactionsMinWithdrawalWrong ->
            apiError err400 MinWithdrawalWrong
            "The minimum withdrawal value must be at least 1 Lovelace."
        ErrListTransactionsPastHorizonException e -> toServerError e

instance IsServerError ErrStartTimeLaterThanEndTime where
    toServerError err = apiError err400 StartTimeLaterThanEndTime $ mconcat
        [ "The specified start time '"
        , toText $ Iso8601Time $ errStartTime err
        , "' is later than the specified end time '"
        , toText $ Iso8601Time $ errEndTime err
        , "'."
        ]

instance IsServerError PastHorizonException where
    toServerError _ = apiError err503 PastHorizon $ mconcat
        [ "Tried to convert something that is past the horizon"
        , " (due to uncertainty about the next hard fork)."
        , " Wait for the node to finish syncing to the hard fork."
        , " Depending on the blockchain, this process can take an"
        , " unknown amount of time."
        ]

instance IsServerError ErrGetTransaction where
    toServerError = \case
        ErrGetTransactionNoSuchWallet e -> toServerError e
        ErrGetTransactionNoSuchTransaction e -> toServerError e

instance IsServerError ErrNoSuchTransaction where
    toServerError = \case
        ErrNoSuchTransaction _wid tid ->
            apiError err404 NoSuchTransaction $ mconcat
                [ "I couldn't find a transaction with the given id: "
                , toText tid
                ]

instance IsServerError ErrStakePoolDelegation where
    toServerError = \case
        ErrStakePoolDelegationNoSuchWallet e -> toServerError e
        ErrStakePoolJoin e -> toServerError e
        ErrStakePoolQuit e -> toServerError e

instance IsServerError ErrCannotJoin where
    toServerError = \case
        ErrAlreadyDelegating pid ->
            apiError err403 PoolAlreadyJoined $ mconcat
                [ "I couldn't join a stake pool with the given id: "
                , toText pid
                , ". I have already joined this pool;"
                , " joining again would incur an unnecessary fee!"
                ]
        ErrNoSuchPool pid ->
            apiError err404 NoSuchPool $ mconcat
                [ "I couldn't find any stake pool with the given id: "
                , toText pid
                ]

instance IsServerError ErrCannotQuit where
    toServerError = \case
        ErrNotDelegatingOrAboutTo ->
            apiError err403 NotDelegatingTo $ mconcat
                [ "It seems that you're trying to retire from delegation "
                , "although you're not even delegating, nor won't be in an "
                , "immediate future."
                ]
        ErrNonNullRewards rewards ->
            apiError err403 NonNullRewards $ mconcat
                [ "It seems that you're trying to retire from delegation "
                , "although you've unspoiled rewards in your rewards "
                , "account! Make sure to withdraw your ", pretty rewards
                , " lovelace first."
                ]

instance IsServerError ErrFetchRewards where
    toServerError = \case
        ErrFetchRewardsReadRewardAccount e -> toServerError e

instance IsServerError ErrReadRewardAccount where
    toServerError = \case
        ErrReadRewardAccountNoSuchWallet e -> toServerError e
        ErrReadRewardAccountNotAShelleyWallet ->
            apiError err403 InvalidWalletType $ mconcat
                [ "It is regrettable but you've just attempted an operation "
                , "that is invalid for this type of wallet. Only new 'Shelley' "
                , "wallets can do something with rewards and this one isn't."
                ]

instance IsServerError ErrReadPolicyPublicKey where
    toServerError = \case
        ErrReadPolicyPublicKeyNoSuchWallet e -> toServerError e
        ErrReadPolicyPublicKeyNotAShelleyWallet ->
            apiError err403 InvalidWalletType $ mconcat
                [ "You have attempted an operation that is invalid for this "
                , "type of wallet. Only wallets from the Shelley era onwards "
                , "can have rewards, but this wallet is from an era before "
                , "Shelley."
                ]
        ErrReadPolicyPublicKeyAbsent ->
            apiError err403 MissingPolicyPublicKey $ T.unwords
                [ "It seems the wallet lacks a policy public key. Therefore"
                , "it's not possible to create a minting/burning"
                , "transaction or get a policy id. Please first POST to endpoint"
                , "/wallets/{walletId}/policy-key to set a policy key."
                ]

instance IsServerError ErrWritePolicyPublicKey where
    toServerError = \case
        ErrWritePolicyPublicKeyNoSuchWallet e -> toServerError e
        ErrWritePolicyPublicKeyWithRootKey  e -> toServerError e

instance IsServerError ErrCreateRandomAddress where
    toServerError = \case
        ErrCreateAddrNoSuchWallet e -> toServerError e
        ErrCreateAddrWithRootKey  e -> toServerError e
        ErrIndexAlreadyExists ix ->
            apiError err409 AddressAlreadyExists $ mconcat
                [ "I cannot derive a new unused address #", pretty (fromEnum ix)
                , " because I already know of such address."
                ]
        ErrCreateAddressNotAByronWallet ->
            apiError err403 InvalidWalletType $ mconcat
                [ "I cannot derive new address for this wallet type."
                , " Make sure to use Byron random wallet id."
                ]

instance IsServerError ErrImportRandomAddress where
    toServerError = \case
        ErrImportAddrNoSuchWallet e -> toServerError e
        ErrImportAddressNotAByronWallet ->
            apiError err403 InvalidWalletType $ mconcat
                [ "I cannot derive new address for this wallet type."
                , " Make sure to use Byron random wallet id."
                ]
        ErrImportAddr ErrAddrDoesNotBelong{} ->
            apiError err403 KeyNotFoundForAddress $ mconcat
                [ "I couldn't identify this address as one of mine. It likely "
                , "belongs to another wallet and I will therefore not import it."
                ]

instance IsServerError ErrNotASequentialWallet where
    toServerError = \case
        ErrNotASequentialWallet ->
            apiError err403 InvalidWalletType $ mconcat
                [ "I cannot derive new address for this wallet type. "
                , "Make sure to use a sequential wallet style, like Icarus."
                ]

instance IsServerError ErrWithdrawalNotBeneficial where
    toServerError = \case
        ErrWithdrawalNotBeneficial ->
            apiError err403 WithdrawalNotBeneficial $ mconcat
                [ "I've noticed that you're requesting a withdrawal from an "
                , "account that is either empty or doesn't have a balance big "
                , "enough to deserve being withdrawn. I won't proceed with that "
                , "request."
                ]

instance IsServerError ErrSignMetadataWith where
    toServerError = \case
        ErrSignMetadataWithRootKey e -> toServerError e
        ErrSignMetadataWithNoSuchWallet e -> toServerError e
        ErrSignMetadataWithInvalidIndex e -> toServerError e

instance IsServerError ErrReadAccountPublicKey where
    toServerError = \case
        ErrReadAccountPublicKeyRootKey e -> toServerError e
        ErrReadAccountPublicKeyNoSuchWallet e -> toServerError e
        ErrReadAccountPublicKeyInvalidAccountIndex e -> toServerError e
        ErrReadAccountPublicKeyInvalidPurposeIndex e -> toServerError e

instance IsServerError ErrDerivePublicKey where
    toServerError = \case
        ErrDerivePublicKeyNoSuchWallet e -> toServerError e
        ErrDerivePublicKeyInvalidIndex e -> toServerError e

instance IsServerError ErrAddCosignerKey where
    toServerError = \case
        ErrAddCosignerKeyNoSuchWallet e -> toServerError e
        ErrAddCosignerKeyWalletMetadataNotFound ->
            apiError err503 WalletMetadataNotFound $ T.unwords
                [ "It was not possible to find any metadata for the given"
                , "wallet within the database. This could be because the"
                , "wallet has yet to become active after being in the"
                , "incomplete state."
                ]
        ErrAddCosignerKey WalletAlreadyActive ->
            apiError err403 SharedWalletActive $ T.unwords
                [ "It looks like you've tried to add a cosigner key for a"
                , "shared wallet that is active. This can be done only for"
                , "an incomplete shared wallet."
                ]
        ErrAddCosignerKey NoDelegationTemplate ->
            apiError err403 SharedWalletNoDelegationTemplate $ T.unwords
                [ "It looks like you've tried to add a cosigner key to"
                , "a shared wallet's delegation template. This cannot be done"
                , "for a wallet that does not define any delegation template."
                ]
        ErrAddCosignerKey (KeyAlreadyPresent cred) ->
            apiError err403 SharedWalletKeyAlreadyExists $ T.unwords
                [ "It looks like you've tried to add a cosigner key to a"
                , "shared wallet's", toText cred, "template that is already"
                , "ascribed to another cosigner. Please make sure to assign a"
                , "different key to each cosigner."
                ]
        ErrAddCosignerKey (NoSuchCosigner credType (Cosigner cosignerIndex)) ->
            let errorInfo = SharedWalletNoSuchCosigner
                    ApiErrorSharedWalletNoSuchCosigner
                        { cosignerIndex =
                            ApiCosignerIndex cosignerIndex
                        , credentialType =
                            ApiCredentialType credType
                        }
                errorMessage = T.unwords
                    [ "It looks like you've tried to add a cosigner key to a"
                    , "shared wallet's"
                    , toText credType
                    , "template for a non-existing cosigner index:"
                    , pretty cosignerIndex
                    ]
            in
            apiError err403 errorInfo errorMessage
        ErrAddCosignerKey CannotUpdateSharedWalletKey ->
            apiError err403 SharedWalletCannotUpdateKey $ T.unwords
                [ "It looks like you've tried to update the key of a cosigner"
                , "having a shared wallet's account key. Only other cosigner"
                , "key(s) can be updated."
                ]

instance IsServerError ErrConstructSharedWallet where
    toServerError = \case
        ErrConstructSharedWalletWrongScriptTemplate (ErrScriptTemplateInvalid cred reason) ->
            handleTemplateErr cred (toText reason)
        ErrConstructSharedWalletWrongScriptTemplate (ErrScriptTemplateMissingKey cred reason) ->
            handleTemplateErr cred reason
        ErrConstructSharedWalletInvalidIndex e -> toServerError e
      where
          handleTemplateErr cred reason =
            apiError err403 SharedWalletScriptTemplateInvalid $ mconcat
                [ "It looks like you've tried to create a shared wallet "
                , "with a template script for ", toText cred, " credential that does not "
                , "pass validation. The problem is: ", reason
                ]

instance IsServerError (ErrInvalidDerivationIndex 'Soft level) where
    toServerError = \case
        ErrIndexOutOfBound minIx maxIx _ix ->
            apiError err403 SoftDerivationRequired $ mconcat
                [ "It looks like you've provided a derivation index that is "
                , "out of bound. The index is well-formed, but I require "
                , "indexes valid for soft derivation only. That is, indexes "
                , "between ", pretty minIx, " and ", pretty maxIx, " without a suffix."
                ]

instance IsServerError (SelectionOutputError WalletSelectionContext) where
    toServerError (SelectionOutputError index info) = case info of
        SelectionOutputCoinInsufficient e ->
            flip (apiError err403) selectionOutputCoinInsufficientMessage $
            UtxoTooSmall ApiErrorTxOutputLovelaceInsufficient
                { txOutputIndex =
                    flip fromJustNote (intCastMaybe @Int @Word32 index) $
                        unwords
                            [ "SelectionOutputError: index out of bounds:"
                            , show index
                            ]
                , txOutputLovelaceSpecified =
                    Coin.toQuantity $ TokenBundle.getCoin $ snd $ view #output e
                , txOutputLovelaceRequiredMinimum =
                    Coin.toQuantity $ view #minimumExpectedCoin e
                }
        SelectionOutputSizeExceedsLimit e ->
            toServerError e
        SelectionOutputTokenQuantityExceedsLimit e ->
            toServerError e
      where
        selectionOutputCoinInsufficientMessage = T.unwords
            [ "One of the outputs you've specified has an ada quantity that is"
            , "below the minimum required. Either increase the ada quantity to"
            , "at least the minimum, or specify an ada quantity of zero, in"
            , "which case the wallet will automatically assign the correct"
            , "minimum ada quantity to the output."
            ]

instance IsServerError
    (SelectionOutputSizeExceedsLimitError WalletSelectionContext)
  where
    toServerError e = apiError err403 OutputTokenBundleSizeExceedsLimit $
        mconcat
        [ "One of the outputs you've specified contains too many assets. "
        , "Try splitting these assets across two or more outputs. "
        , "Destination address: "
        , pretty (fst output)
        , ". Asset count: "
        , pretty (TokenMap.size $ snd output ^. #tokens)
        , "."
        ]
      where
        output = view #outputThatExceedsLimit e

instance IsServerError
    (SelectionOutputTokenQuantityExceedsLimitError WalletSelectionContext)
  where
    toServerError e = apiError err403 OutputTokenQuantityExceedsLimit $ mconcat
        [ "One of the token quantities you've specified is greater than the "
        , "maximum quantity allowed in a single transaction output. Try "
        , "splitting this quantity across two or more outputs. "
        , "Destination address: "
        , pretty (view #address e)
        , ". Token policy identifier: "
        , pretty (view #tokenPolicyId $ asset e)
        , ". Asset name: "
        , pretty (view #tokenName $ asset e)
        , ". Token quantity specified: "
        , pretty (view #quantity e)
        , ". Maximum allowable token quantity: "
        , pretty (view #quantityMaxBound e)
        , "."
        ]

instance IsServerError ErrCreateMigrationPlan where
    toServerError = \case
        ErrCreateMigrationPlanEmpty ->
            apiError err403 NothingToMigrate $ mconcat
                [ "I wasn't able to construct a migration plan. This could be "
                , "because your wallet is empty, or it could be because the "
                , "amount of ada in your wallet is insufficient to pay for "
                , "any of the funds to be migrated. Try adding some ada to "
                , "your wallet before trying again."
                ]
        ErrCreateMigrationPlanNoSuchWallet e -> toServerError e

instance IsServerError ErrSelectAssets where
    toServerError = \case
        ErrSelectAssetsPrepareOutputsError e -> toServerError e
        ErrSelectAssetsAlreadyWithdrawing tx ->
            apiError err403 AlreadyWithdrawing $ mconcat
                [ "I already know of a pending transaction with withdrawals: "
                , toText (tx ^. #txId)
                , ". Note that when I withdraw rewards, I "
                , "need to withdraw them fully for the Ledger to accept it. "
                , "There's therefore no point creating another conflicting "
                , "transaction; if, for some reason, you really want a new "
                , "transaction, then cancel the previous one first."
                ]
        ErrSelectAssetsSelectionError (SelectionBalanceErrorOf e) ->
            toServerError e
        ErrSelectAssetsSelectionError (SelectionCollateralErrorOf e) ->
            toServerError e
        ErrSelectAssetsSelectionError (SelectionOutputErrorOf e) ->
            toServerError e

instance IsServerError (SelectionBalanceError WalletSelectionContext) where
    toServerError = \case
        BalanceInsufficient e ->
            apiError err403 NotEnoughMoney $ mconcat
                [ "I can't process this payment as there are not "
                , "enough funds available in the wallet. I am "
                , "missing: ", pretty . Flat $ e ^. #utxoBalanceShortfall
                ]
        SelectionLimitReached e ->
            apiError err403 TransactionIsTooBig $ mconcat
                [ "I am not able to finalize the transaction "
                , "because I need to select additional inputs and "
                , "doing so will make the transaction too big. Try "
                , "sending a smaller amount. I had already selected "
                , showT (length $ view #inputsSelected e), " inputs."
                ]
        UnableToConstructChange e ->
            apiError err403 CannotCoverFee $ T.unwords
                [ "I am unable to finalize the transaction, as there"
                , "is not enough ada available to pay for the fee and"
                , "also pay for the minimum ada quantities of all"
                , "change outputs. I need approximately"
                , pretty (shortfall e)
                , "ada to proceed. Try increasing your wallet balance"
                , "or sending a smaller amount."
                ]
        EmptyUTxO ->
            apiError err403 NotEnoughMoney $ T.unwords
                [ "Cannot create a transaction because the wallet"
                , "has no UTxO entries. At least one UTxO entry is"
                , "required in order to create a transaction."
                ]

instance IsServerError (SelectionCollateralError WalletSelectionContext) where
    toServerError e =
        apiError err403 InsufficientCollateral $ T.unwords
            [ "I'm unable to create this transaction because the balance"
            , "of pure ada UTxOs in your wallet is insufficient to cover"
            , "the minimum amount of collateral required."
            , "I need an ada amount of at least:"
            , pretty (view #minimumSelectionAmount e)
            , "The largest combination of pure ada UTxOs I could find is:"
            , pretty $ listF $ L.sort $ F.toList $
                view #largestCombinationAvailable e
            , "To fix this, you'll need to add one or more pure ada UTxOs"
            , "to your wallet that can cover the minimum amount required."
            ]

instance IsServerError (ErrInvalidDerivationIndex 'Hardened level) where
    toServerError = \case
        ErrIndexOutOfBound (Index minIx) (Index maxIx) _ix ->
            apiError err403 HardenedDerivationRequired $ mconcat
                [ "It looks like you've provided a derivation index that is "
                , "out of bound. The index is well-formed, but I require "
                , "indexes valid for hardened derivation only. That is, indexes "
                , "between 0H and ", pretty (Index $ maxIx - minIx), "H."
                ]

instance IsServerError ErrUpdateSealedTx where
    toServerError = \case
        ErrExistingKeyWitnesses{} ->
            apiError err400 ExistingKeyWitnesses $ T.unwords
                [ "I cannot proceed with the request because there are key"
                , "witnesses defined in the input transaction and, adjusting"
                , "the transaction body will render witnesses invalid!"
                , "Please make sure to remove all key witnesses from the request."
                ]

instance IsServerError ErrAssignRedeemers where
    toServerError = \case
        ErrAssignRedeemersScriptFailure r failure ->
            apiError err400 RedeemerScriptFailure $ T.unwords
                [ "I was unable to assign execution units to one of your"
                , "redeemers:", pretty r <> ";"
                , "Its execution is failing with the following error:"
                , T.pack failure <> "."
                ]
        ErrAssignRedeemersTargetNotFound r ->
            apiError err400 RedeemerTargetNotFound $ T.unwords
                [ "I was unable to resolve one of your redeemers to the location"
                , "indicated in the request payload:", pretty r <> ";"
                , "Please double-check both your serialised transaction and"
                , "the provided redeemers."
                ]
        ErrAssignRedeemersInvalidData r _ ->
            apiError err400 RedeemerInvalidData $ T.unwords
                [ "It looks like you have provided an invalid 'data' payload"
                , "for one of your redeemers since I am unable to decode it"
                , "into a valid Plutus data:", pretty r <> "."
                ]
        ErrAssignRedeemersTranslationError (TranslationLogicMissingInput inp) ->
             -- Note that although this error is thrown from
             -- '_assignScriptRedeemers', it's more related to balanceTransaction
             -- in general than to assigning redeemers. Hence we don't mention
             -- redeemers in the message.
             apiError err400 UnresolvedInputs $ T.unwords
                 [ "The transaction I was given contains inputs I don't know"
                 , "about. Please ensure all foreign inputs are specified as "
                 , "part of the API request. The unknown input is:\n\n"
                 , T.pack $ show inp
                 ]
        ErrAssignRedeemersTranslationError (TimeTranslationPastHorizon t) ->
            -- We differentiate this from @TranslationError@ for partial API
            -- backwards compatibility.
            apiError err400 PastHorizon $ T.unwords
                [ "The transaction's validity interval is past the horizon"
                , "of safe slot-to-time conversions."
                , "This may happen when I know about a future era"
                , "which has not yet been confirmed on-chain. Try setting the"
                , "bounds of the validity interval to be earlier.\n\n"
                , "Here are the full details: " <> t
                ]
        ErrAssignRedeemersTranslationError e ->
            apiError err400 TranslationError $ T.unwords
                [ "The transaction I was given contains bits that cannot be"
                , "translated in the current era. The following is wrong:\n\n"
                , showT e
                ]

instance IsServerError (Request, ServerError) where
    toServerError (req, err@(ServerError code _ body _))
      | not (isJSON body) = case code of
        400 | "Failed reading" `BS.isInfixOf` BL.toStrict body ->
            apiError err BadRequest $ mconcat
                [ "I couldn't understand the content of your message. If your "
                , "message is intended to be in JSON format, please check that "
                , "the JSON is valid."
                ]
        400 -> apiError err BadRequest (utf8 body)
        404 -> apiError err NotFound $ mconcat
            [ "I couldn't find the requested endpoint. If the endpoint "
            , "contains path parameters, please ensure they are well-formed, "
            , "otherwise I won't be able to route them correctly."
            ]
        405 -> apiError err MethodNotAllowed $ mconcat
            [ "You've reached a known endpoint but I don't know how to handle "
            , "the HTTP method specified. Please double-check both the "
            , "endpoint and the method: one of them is likely to be incorrect "
            , "(for example: POST instead of PUT, or GET instead of POST...)."
            ]
        406 ->
            let cType =
                    -- FIXME: Ugly and not really scalable nor maintainable.
                    if ["wallets"] `isPrefixOf` pathInfo req
                    && ["signatures"] `isInfixOf` pathInfo req
                    then "application/octet-stream"
                    else "application/json"
            in apiError err NotAcceptable $ mconcat
            [ "It seems as though you don't accept '", cType,"', but "
            , "unfortunately I only speak '", cType,"'! Please "
            , "double-check your 'Accept' request header and make sure it's "
            , "set to '", cType,"'."
            ]
        415 ->
            let cType =
                    -- FIXME: Ugly and not really scalable nor maintainable.
                    if ["proxy", "transactions"] `isSubsequenceOf` pathInfo req
                        then "application/octet-stream"
                        else "application/json"
            in apiError err UnsupportedMediaType $ mconcat
            [ "I'm really sorry but I only understand '", cType, "'. I need you "
            , "to tell me what language you're speaking in order for me to "
            , "understand your message. Please double-check your 'Content-Type' "
            , "request header and make sure it's set to '", cType, "'."
            ]
        501 -> apiError err NotImplemented
            "I'm really sorry but this endpoint is not implemented yet."
        _ -> apiError err UnexpectedError $ mconcat
            [ "It looks like something unexpected went wrong. Unfortunately I "
            , "don't yet know how to handle this type of situation. Here's "
            , "some information about what happened: ", utf8 body
            ]
      | otherwise = err
      where
        utf8 = T.replace "\"" "'" . T.decodeUtf8 . BL.toStrict
        isJSON = isJust . Aeson.decode @Aeson.Value
