{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Wallet.Api.Http.Server.Error.AssignReedemers () where

import Prelude

import Cardano.Ledger.Alonzo.Plutus.TxInfo
    ( AlonzoContextError (..)
    )
import Cardano.Ledger.Babbage.TxInfo
    ( BabbageContextError (..)
    )
import Cardano.Ledger.Conway.TxInfo
    ( ConwayContextError (..)
    )
import Cardano.Wallet.Api.Http.Server.Error.IsServerError
    ( IsServerError (..)
    , apiError
    )
import Cardano.Wallet.Api.Types.Error
    ( ApiErrorInfo (..)
    )
import Fmt
    ( pretty
    )
import Internal.Cardano.Write.Tx
    ( IsRecentEra (..)
    , RecentEra (RecentEraBabbage, RecentEraConway)
    )
import Internal.Cardano.Write.Tx.Balance
    ( ErrAssignRedeemers (..)
    )
import Servant.Server
    ( ServerError
    , err400
    )

import qualified Data.Text as T

instance IsRecentEra era => IsServerError (ErrAssignRedeemers era) where
    toServerError = \case
        ErrAssignRedeemersScriptFailure r failure ->
            apiError err400 RedeemerScriptFailure
                $ T.unwords
                    [ "I was unable to assign execution units to one of your"
                    , "redeemers:"
                    , pretty r <> ";"
                    , "Its execution is failing with the following error:"
                    , T.pack failure <> "."
                    ]
        ErrAssignRedeemersTargetNotFound r ->
            apiError err400 RedeemerTargetNotFound
                $ T.unwords
                    [ "I was unable to resolve one of your redeemers to the location"
                    , "indicated in the request payload:"
                    , pretty r <> ";"
                    , "Please double-check both your serialised transaction and"
                    , "the provided redeemers."
                    ]
        ErrAssignRedeemersInvalidData r _ ->
            apiError err400 RedeemerInvalidData
                $ T.unwords
                    [ "It looks like you have provided an invalid 'data' payload"
                    , "for one of your redeemers since I am unable to decode it"
                    , "into a valid Plutus data:"
                    , pretty r <> "."
                    ]
        ErrAssignRedeemersTranslationError x -> case recentEra @era of
            RecentEraBabbage -> fromBabbageContextError x
            RecentEraConway -> fromConwayContextError x

fromAlonzoContextError :: AlonzoContextError era -> ServerError
fromAlonzoContextError = \case
    TranslationLogicMissingInput inp ->
        apiError err400 UnresolvedInputs
            $ T.unwords
                [ "The transaction I was given contains inputs I don't know"
                , "about. Please ensure all foreign inputs are specified as "
                , "part of the API request. The unknown input is:\n\n"
                , T.pack $ show inp
                ]
    TimeTranslationPastHorizon t ->
        apiError err400 PastHorizon
            $ T.unwords
                [ "The transaction's validity interval is past the horizon"
                , "of safe slot-to-time conversions."
                , "This may happen when I know about a future era"
                , "which has not yet been confirmed on-chain. Try setting the"
                , "bounds of the validity interval to be earlier.\n\n"
                , "Here are the full details: " <> t
                ]

fromBabbageContextError
    :: forall era
     . IsRecentEra era
    => BabbageContextError era
    -> ServerError
fromBabbageContextError = \case
    AlonzoContextError e -> fromAlonzoContextError e
    ByronTxOutInContext txOut ->
        apiError err400 TranslationByronTxOutInContext
            $ T.unwords
                [ "The transaction I was given contains a Byron-style TxOut"
                , "which is not supported when executing Plutus scripts. "
                , "The offending TxOut is:\n\n"
                , T.pack $ show txOut
                ]
    RedeemerPointerPointsToNothing ptr ->
        apiError err400 RedeemerTargetNotFound
            $ T.unwords
                [ "I was unable to resolve one of your redeemers to the location"
                , "indicated in the request payload:"
                , T.pack $ show ptr
                , "Please double-check both your serialised transaction and"
                , "the provided redeemers."
                ]
    InlineDatumsNotSupported x ->
        apiError err400 RedeemerInvalidData
            $ T.unwords
                [ "Inline datum is not supported in Plutus V1"
                , "The offending data is:\n\n"
                , T.pack $ show x
                ]
    ReferenceScriptsNotSupported x ->
        apiError err400 RedeemerScriptFailure
            $ T.unwords
                [ "Reference scripts are not supported in Plutus V1"
                , "The offending script is:\n\n"
                , T.pack $ show x
                ]
    ReferenceInputsNotSupported x ->
        apiError err400 UnresolvedInputs
            $ T.unwords
                [ "Reference inputs are not supported in Plutus V1"
                , "The offending input is:\n\n"
                , T.pack $ show x
                ]

fromConwayContextError
    :: forall era
     . IsRecentEra era
    => ConwayContextError era
    -> ServerError
fromConwayContextError = \case
    BabbageContextError e -> fromBabbageContextError e
    CertificateNotSupported x ->
        apiError err400 UnsupportedMediaType
            $ T.unwords
                [ "The transaction I was given contains a certificate which"
                , "is not supported in the current era."
                , "The offending certificate is:\n\n"
                , T.pack $ show x
                ]
    PlutusPurposeNotSupported x ->
        apiError err400 UnsupportedMediaType
            $ T.unwords
                [ "The transaction I was given contains a Plutus script"
                , "which is not supported in the current era."
                , "The offending script is:\n\n"
                , T.pack $ show x
                ]
