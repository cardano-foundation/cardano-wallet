{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Cardano.Wallet.UI.Deposit.Html.Pages.Payments.Page
    ( paymentsH
    , paymentsElementH
    , availableBalanceElementH
    , receiverAddressValidationH
    , receiverAmountValidationH
    , paymentsChangeH
    )
where

import Prelude

import Cardano.Wallet.Deposit.Pure
    ( CanSign (..)
    , InspectTx (..)
    )
import Cardano.Wallet.Deposit.Pure.State.Payment.Inspect
    ( transactionBalance
    )
import Cardano.Wallet.Read
    ( Coin (..)
    )
import Cardano.Wallet.UI.Common.Html.Htmx
    ( hxGet_
    , hxInclude_
    , hxPost_
    , hxSwapOob_
    , hxTarget_
    , hxTrigger_
    )
import Cardano.Wallet.UI.Common.Html.Lib
    ( WithCopy (..)
    , linkText
    , tdEnd
    , thEnd
    , truncatableText
    )
import Cardano.Wallet.UI.Common.Html.Modal
    ( mkModalButton
    )
import Cardano.Wallet.UI.Common.Html.Pages.Lib
    ( Striped (..)
    , Width (..)
    , alertH
    , box
    , field
    , record
    , simpleField
    , sseH
    )
import Cardano.Wallet.UI.Deposit.API
    ( modalLink
    , paymentsBalanceAvailableLink
    , paymentsDeleteReceiverLink
    , paymentsNewReceiverLink
    , paymentsReceiverAddressValidationLink
    , paymentsReceiverAmountValidationLink
    , paymentsResetLink
    , paymentsSignLink
    , paymentsSubmitLink
    )
import Cardano.Wallet.UI.Deposit.API.Payments
    ( Receivers
    , State
    , StateA (..)
    , Transaction
    )
import Cardano.Wallet.UI.Deposit.Handlers.Payments.Transaction
    ( AddressValidationResponse (..)
    , AmountValidationResponse (..)
    , PaymentHandlerResponse (..)
    , extractReceivers
    )
import Cardano.Wallet.UI.Deposit.Html.Common
    ( addressH
    , lovelaceH
    , txIdH
    )
import Cardano.Wallet.UI.Type
    ( WHtml
    )
import Control.Monad
    ( forM_
    , when
    )
import Data.Foldable
    ( Foldable (..)
    )
import Data.Maybe
    ( fromMaybe
    )
import Data.Semigroup
    ( Sum (..)
    )
import Data.Text
    ( Text
    )
import Data.Text.Class
    ( ToText (..)
    )
import Lucid
    ( Attribute
    , Html
    , ToHtml (..)
    , button_
    , class_
    , data_
    , div_
    , hidden_
    , i_
    , id_
    , input_
    , name_
    , placeholder_
    , span_
    , style_
    , table_
    , tbody_
    , thead_
    , tr_
    , type_
    , value_
    )
import Servant
    ( Link
    )

import qualified Data.Aeson as Aeson
import qualified Data.Map.Monoidal.Strict as MonoidalMap
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL

paymentsH :: Link -> WHtml ()
paymentsH paymentsLink = do
    sseH paymentsLink "payments-page" ["payments"]

paymentsChangeH :: Coin -> PaymentHandlerResponse -> Html ()
paymentsChangeH balance transaction = do
    case transaction of
        ResponseExceptionPayments paymentError -> do
            setError
                $ alertH
                $ toText paymentError
        ResponseSuccess canSign state -> do
            setError mempty
            setState [hxSwapOob_ "innerHTML"] $ fst <$> state
            case state of
                NoState -> do
                    setReceivers Nothing
                    setInspection Nothing
                    setBalance balance Nothing
                Unsigned (utx, inspect) -> do
                    setReceivers $ Just (signatureFormH utx canSign, inspect)
                    setInspection $ Just (inspect, utx, Nothing)
                    setBalance balance $ Just inspect
                Signed utx (stx, inspect) -> do
                    setReceivers $ Just (submitH, inspect)
                    setInspection $ Just (inspect, utx, Just stx)
                    setBalance balance $ Just inspect
                Submitted utx (stx, inspect) -> do
                    setReceivers $ Just (newH, inspect)
                    setInspection $ Just (inspect, utx, Just stx)
                    setBalance balance Nothing

setError :: Html () -> Html ()
setError = div_ [id_ "transaction-error", hxSwapOob_ "innerHTML"]

setState :: [Attribute] -> State -> Html ()
setState attrs state =
    div_ ([id_ "payment-state"] <> attrs)
        $ input_
            [ hidden_ ""
            , name_ "payment-state"
            , value_ $ TL.toStrict $ TL.decodeUtf8 $ Aeson.encode state
            ]

setReceivers :: Maybe (Html (), InspectTx) -> Html ()
setReceivers mInspect =
    div_ [id_ "receivers", hxSwapOob_ "innerHTML"]
        $ case mInspect of
            Nothing -> receiversH Nothing
            Just (canSign, inspect) -> do
                receiversH $ Just (extractReceivers inspect)
                canSign

setInspection
    :: Maybe (InspectTx, Transaction, Maybe Transaction)
    -> Html ()
setInspection inspect = do
    div_ [id_ "transaction-inspection", hxSwapOob_ "innerHTML"]
        $ foldMap transactionInspectionH inspect

setBalance :: Coin -> Maybe InspectTx -> Html ()
setBalance balance mInspect = do
    div_ [id_ "available-balance", hxSwapOob_ "innerHTML"]
        $ availableBalanceElementH balance
        $ case mInspect of
            Just inspect ->
                Just
                    $ fromIntegral
                    $ transactionBalance inspect
            _ -> Nothing

newReceiverH :: Html ()
newReceiverH = do
    let spanFlex = span_ [class_ "d-flex"]
    tbody_ [id_ "new-receiver-form"]
        $ tr_ [class_ "border-top pt-2"]
        $ do
            tdEnd
                $ spanFlex
                $ do
                    div_ [id_ "receiver-address-validation"] mempty
                    input_
                        [ class_ "form-control text-end"
                        , type_ "text"
                        , name_ "new-receiver-address"
                        , hxPost_
                            $ linkText
                                paymentsReceiverAddressValidationLink
                        , hxTarget_ "#receiver-address-validation"
                        , hxInclude_ "#new-receiver-form"
                        , hxTrigger_ "input"
                        , placeholder_ "payment address"
                        ]
            tdEnd
                $ spanFlex
                $ do
                    div_ [id_ "receiver-amount-validation"] mempty
                    input_
                        [ class_ "form-control text-end"
                        , type_ "text"
                        , name_ "new-receiver-amount"
                        , hxPost_
                            $ linkText
                                paymentsReceiverAmountValidationLink
                        , hxTarget_ "#receiver-amount-validation"
                        , hxInclude_ "#new-receiver-form"
                        , hxTrigger_ "input"
                        , placeholder_ "amount in ada"
                        ]

            tdEnd
                $ spanFlex
                $ button_
                    [ class_ "btn w-100"
                    , hxPost_ $ linkText paymentsNewReceiverLink
                    , hxInclude_ "#payment-state , #new-receiver-form"
                    , hxTarget_ "#none"
                    , id_ "new-receiver-button"
                    ]
                    mempty

receiversH :: Maybe Receivers -> Html ()
receiversH m = do
    div_ [class_ "d-flex justify-content-end"] $ do
        table_
            [ class_ "table table-sm table-borderless table-hover striped-columns"
            ]
            $ do
                thead_ $ do
                    tr_ $ do
                        thEnd Nothing "Address"
                        thEnd (Just 9) "Amount"
                        thEnd (Just 5) "Actions"
                tbody_ [id_ "payment-state"]
                    $ forM_ (MonoidalMap.assocs $ fold m)
                    $ \(address, Sum amount) -> do
                        tr_ $ do
                            tdEnd $ do
                                addressH WithCopy address
                            tdEnd $ lovelaceH amount
                            tdEnd
                                $ button_
                                    [ hxPost_
                                        $ linkText
                                        $ paymentsDeleteReceiverLink
                                        $ Just address
                                    , hxInclude_ "#payment-state"
                                    , hxTarget_ "#none"
                                    , class_ "btn w-100"
                                    ]
                                $ i_ [class_ "bi bi-trash"] mempty
                newReceiverH

ifNotEmpty :: (Foldable t, Monoid b) => t a -> b -> b
ifNotEmpty xs b = if null xs then mempty else b

transactionInspectionH
    :: (InspectTx, Transaction, Maybe Transaction)
    -> Html ()
transactionInspectionH (InspectTx{..}, utx, mstx) = do
    let table = table_ [class_ "table table-sm m-0"]
    div_ [class_ ""] $ do
        record (Just 7) Full Striped $ do
            field [] "unsigned transaction"
                $ transactionCBORH "unsigned-transaction-copy" utx
            case mstx of
                Just stx ->
                    field [] "signed transaction"
                        $ transactionCBORH "signed-transaction-copy" stx
                Nothing -> pure ()
            field [] "fee"
                $ lovelaceH
                $ fromIntegral fee
            field [] "our inputs"
                $ ifNotEmpty ourInputs
                $ table
                $ do
                    thead_ $ do
                        tr_ $ do
                            thEnd Nothing $ toHtml $ truncatableText WithoutCopy "" "Transaction"
                            thEnd (Just 4) "Index"
                            thEnd (Just 7) "Amount"
                    tbody_
                        $ forM_ ourInputs
                        $ \(txId, txIx, CoinC amount) -> do
                            tr_ $ do
                                tdEnd $ txIdH txId
                                tdEnd $ toHtml $ show $ fromEnum txIx
                                tdEnd $ lovelaceH $ fromIntegral amount
            field [] "other inputs"
                $ ifNotEmpty otherInputs
                $ table
                $ do
                    thead_ $ do
                        tr_ $ do
                            thEnd Nothing $ toHtml $ truncatableText WithoutCopy "" "Transaction"
                            thEnd (Just 4) "Index"
                    tbody_
                        $ forM_ otherInputs
                        $ \(txId, txIx) -> do
                            tr_ $ do
                                tdEnd $ txIdH txId
                                tdEnd $ toHtml $ show $ fromEnum txIx
            field [] "change"
                $ ifNotEmpty change
                $ table
                $ do
                    thead_ $ do
                        tr_ $ do
                            thEnd Nothing
                                $ toHtml
                                $ truncatableText WithoutCopy "" "Change Address"
                            thEnd (Just 7) "Amount"
                    tbody_
                        $ forM_ change
                        $ \(addr, CoinC amount) -> do
                            tr_ $ do
                                tdEnd $ addressH WithCopy addr
                                tdEnd $ lovelaceH $ fromIntegral amount
            field [] "customer outputs"
                $ ifNotEmpty ourOutputs
                $ table
                $ do
                    thead_ $ do
                        tr_ $ do
                            thEnd Nothing $ toHtml $ truncatableText WithoutCopy "" "Address"
                            thEnd (Just 6) "Customer"
                            thEnd (Just 7) "Amount"
                    tbody_
                        $ forM_ ourOutputs
                        $ \(addr, customer, CoinC amount) -> do
                            tr_ $ do
                                tdEnd $ addressH WithCopy addr
                                tdEnd $ toHtml $ show customer
                                tdEnd $ lovelaceH $ fromIntegral amount
            field [] "other outputs"
                $ ifNotEmpty otherOutputs
                $ table
                $ do
                    thead_ $ do
                        tr_ $ do
                            thEnd Nothing "Address"
                            thEnd (Just 7) "Amount"
                    tbody_
                        $ forM_ otherOutputs
                        $ \(addr, CoinC amount) -> do
                            tr_ $ do
                                tdEnd $ addressH WithCopy addr
                                tdEnd $ lovelaceH $ fromIntegral amount

transactionCBORH :: Text -> Transaction -> Html ()
transactionCBORH copyName cbor =
    truncatableText WithCopy copyName
        $ toHtml
        $ Aeson.encode cbor

signatureFormH :: Transaction -> CanSign -> Html ()
signatureFormH utx = \case
    CanSign -> do
        div_ [class_ "d-flex justify-content-end"] $ do
            div_ [class_ "input-group", style_ "max-width:35em"] $ do
                input_
                    [ id_ "signature-password"
                    , class_ "form-control text-end"
                    , type_ "password"
                    , name_ "passphrase"
                    , placeholder_ "passphrase"
                    ]
                button_
                    [ class_ "btn btn-secondary"
                    , hxPost_ $ linkText paymentsSignLink
                    , hxTarget_ "#none"
                    , hxInclude_ "#signature-password, #payment-state"
                    ]
                    "Sign"
    CannotSign -> do
        record (Just 15) Full Striped $ do
            field [] "unsigned transaction"
                $ transactionCBORH "unsigned-transaction-signature-copy" utx
        div_ [class_ "d-flex justify-content-end"] $ do
            div_ [class_ "input-group", style_ "max-width:35em"] $ do
                input_
                    [ id_ "signed-transaction"
                    , class_ "form-control text-end"
                    , name_ "signed-transaction"
                    , placeholder_ "signed transaction"
                    ]
            button_
                [ class_ "btn btn-secondary"
                , hxPost_ $ linkText paymentsSignLink
                , hxInclude_ "#payment-state, #signed-transaction"
                , hxTarget_ "#none"
                ]
                "Accept"

submitH :: Html ()
submitH = do
    div_ [class_ "input-group  d-flex justify-content-end"] $ do
        -- span_ [class_ "input-group-text"] "Submit"
        button_
            [ class_ "btn btn-secondary"
            , hxPost_ $ linkText paymentsSubmitLink
            , hxInclude_ "#payment-state"
            , hxTarget_ "#none"
            ]
            "Submit"

newH :: Html ()
newH = do
    div_ [class_ "input-group  d-flex justify-content-end"] $ do
        -- span_ [class_ "input-group-text"] "New transaction"
        button_
            [ class_ "btn btn-secondary"
            , hxPost_ $ linkText paymentsResetLink
            , hxTarget_ "#none"
            ]
            "Reset"

availableBalanceElementH :: Coin -> Maybe Coin -> Html ()
availableBalanceElementH balance mTxBalance =
    record Nothing Auto Striped $ do
        simpleField "Before transaction"
            $ div_ [class_ "d-flex justify-content-end"]
            $ lovelaceH
            $ fromIntegral balance
        simpleField "Transaction balance"
            $ div_ [class_ "d-flex justify-content-end"]
            $ lovelaceH
            $ fromIntegral
            $ fromMaybe 0 mTxBalance
        simpleField "After transaction"
            $ div_ [class_ "d-flex justify-content-end"]
            $ lovelaceH
            $ fromIntegral
            $ balance - fromMaybe 0 mTxBalance

{- restoreH :: Html ()
restoreH = div_ [class_ "input-group"] $ do
    input_
        [ class_ "form-control"
        , type_ "text"
        , name_ "restore-transaction"
        , placeholder_ "serialized tx"
        ]
    button_
        [ class_ "btn"
        , hxPost_ $ linkText paymentsRestoreLink
        , hxTarget_ "#receivers"
        , hxInclude_ "#restoration"
        ]
        $ i_ [class_ "bi bi-upload"] mempty -}

collapseBtn :: Text -> Html ()
collapseBtn identifier =
    button_
        [ class_ "btn"
        , type_ "button"
        , data_ "bs-toggle" "collapse"
        , data_
            "bs-target"
            $ "#" <> identifier
        ]
        $ i_ [class_ "bi bi-arrows-collapse"] mempty

paymentsElementH
    :: Html ()
paymentsElementH =
    div_
        [ class_ "row mt-3 gx-0"
        ]
        $ do
            div_ [id_ "none"] mempty
            box "New" mempty
                $ do
                    setState [] NoState
                    box "Transaction Creation" mempty
                        $ do
                            div_
                                [ id_ "receivers"
                                ]
                                $ receiversH Nothing
                            div_ [id_ "transaction-error"] mempty
                    div_ [id_ "copy-transaction"] mempty
                    box "Wallet Balance" (collapseBtn "available-balance")
                        $ div_
                            [ class_ "collapse d-flex justify-content-end"
                            , id_ "available-balance"
                            , hxTrigger_ "load"
                            , hxGet_ $ linkText paymentsBalanceAvailableLink
                            , hxInclude_ "#payment-state"
                            , hxTarget_ "#available-balance"
                            ]
                            mempty
                    box
                        "Transaction Content"
                        (collapseBtn "transaction-inspection")
                        $ div_
                            [ class_ "collapse"
                            , id_ "transaction-inspection"
                            ]
                            mempty

{-                     box
                        "Restoration"
                        (collapseBtn "restoration")
                        $ div_
                            [ class_ "collapse"
                            , id_ "restoration"
                            ]
                            restoreH -}

receiverAddressValidationH :: AddressValidationResponse -> Html ()
receiverAddressValidationH (ValidAddress _ m) =
    div_ [id_ "new-receiver-button", hxSwapOob_ "innerHTML"]
        $ when m
        $ i_ [class_ "bi bi-plus-lg"] mempty
receiverAddressValidationH (InvalidAddress e) = do
    validationFailedButton "Invalid Address" $ toText e
    div_ [id_ "new-receiver-button"] mempty

receiverAmountValidationH :: AmountValidationResponse -> Html ()
receiverAmountValidationH (ValidAmount _ m) = do
    div_ [id_ "new-receiver-button", hxSwapOob_ "innerHTML"]
        $ when m
        $ i_ [class_ "bi bi-plus-lg"] mempty
receiverAmountValidationH (InvalidAmount e) = do
    validationFailedButton "Invalid Amount" $ toText e
    div_ [id_ "new-receiver-button"] mempty

validationFailedButton :: Text -> Text -> Html ()
validationFailedButton t e =
    mkModalButton
        (modalLink (Just t) $ Just e)
        [class_ "btn px-1"]
        $ i_ [class_ "bi bi-exclamation-triangle text-danger-emphasis"] mempty
