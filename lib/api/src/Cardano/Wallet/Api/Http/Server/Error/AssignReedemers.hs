{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Wallet.Api.Http.Server.Error.AssignReedemers () where

import Cardano.Wallet.Api.Http.Server.Error.IsServerError
    ( IsServerError (..)
    , apiError
    )
import Cardano.Wallet.Api.Types.Error
    ( ApiErrorInfo (..)
    )
import Cardano.Write.Eras
    ( IsRecentEra (..)
    )
import Fmt
    ( pretty
    )
import Internal.Cardano.Write.Tx.Balance
    ( ErrAssignRedeemers (..)
    )
import Servant.Server
    ( err400
    )
import Prelude

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
                    , T.pack (show failure) <> "."
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
