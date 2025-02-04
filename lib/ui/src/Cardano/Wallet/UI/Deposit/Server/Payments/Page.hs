module Cardano.Wallet.UI.Deposit.Server.Payments.Page
    ( servePaymentsPage
    , servePaymentsNewReceiver
    , servePaymentsDeleteReceiver
    , servePaymentsBalanceAvailable
    , servePaymentsReceiverAddressValidation
    , servePaymentsReceiverAmountValidation
    , servePaymentsSign
    , servePaymentsSubmit
    , servePaymentsReset
    )
where

import Prelude

import Cardano.Wallet.Deposit.REST
    ( WalletResource
    )
import Cardano.Wallet.Deposit.Write
    ( Address
    )
import Cardano.Wallet.UI.Common.Handlers.Session
    ( withSessionLayer
    )
import Cardano.Wallet.UI.Common.Html.Html
    ( RawHtml (..)
    , renderHtml
    )
import Cardano.Wallet.UI.Common.Html.Pages.Lib
    ( alertH
    )
import Cardano.Wallet.UI.Common.Layer
    ( UILayer (..)
    )
import Cardano.Wallet.UI.Cookies
    ( CookieResponse
    , RequestCookies
    )
import Cardano.Wallet.UI.Deposit.API.Payments
    ( AddReceiverForm (..)
    , NewReceiver (..)
    , NewReceiverValidation
    , Signal (..)
    , SignatureForm (..)
    , State
    , StateA (..)
    , signatureFormState
    )
import Cardano.Wallet.UI.Deposit.Handlers.Payments.Balance
    ( getAvailableBalance
    )
import Cardano.Wallet.UI.Deposit.Handlers.Payments.Transaction
    ( receiverAddressValidation
    , receiverAmountValidation
    , signalHandler
    )
import Cardano.Wallet.UI.Deposit.Html.Pages.Payments.Page
    ( availableBalanceElementH
    , paymentsChangeH
    , paymentsElementH
    , receiverAddressValidationH
    , receiverAmountValidationH
    )
import Cardano.Wallet.UI.Deposit.Server.Lib
    ( renderSmoothHtml
    )
import Servant
    ( Handler
    )

servePaymentsPage
    :: UILayer WalletResource
    -> Maybe RequestCookies
    -> Handler (CookieResponse RawHtml)
servePaymentsPage ul = withSessionLayer ul $ \_layer -> do
    pure $ renderSmoothHtml paymentsElementH

servePaymentsNewReceiver
    :: UILayer WalletResource
    -> AddReceiverForm
    -> Maybe RequestCookies
    -> Handler (CookieResponse RawHtml)
servePaymentsNewReceiver ul (AddReceiverForm (NewReceiver receiver) state) =
    withSessionLayer ul $ \layer -> do
        renderHtml
            <$> signalHandler
                layer
                alertH
                paymentsChangeH
                state
                (AddReceiver receiver)

servePaymentsDeleteReceiver
    :: UILayer WalletResource
    -> State
    -> Maybe Address
    -> Maybe RequestCookies
    -> Handler (CookieResponse RawHtml)
servePaymentsDeleteReceiver ul state (Just receiver) =
    withSessionLayer ul $ \layer -> do
        renderHtml
            <$> signalHandler
                layer
                alertH
                paymentsChangeH
                state
                (DeleteReceiver receiver)
servePaymentsDeleteReceiver _ _ _ =
    error "servePaymentsDeleteReceiver: receiver-number is required"

servePaymentsBalanceAvailable
    :: UILayer WalletResource
    -> Maybe RequestCookies
    -> Handler (CookieResponse RawHtml)
servePaymentsBalanceAvailable ul = withSessionLayer ul $ \layer -> do
    renderSmoothHtml
        <$> getAvailableBalance
            layer
            (`availableBalanceElementH` Nothing)
            alertH

servePaymentsReceiverAddressValidation
    :: UILayer WalletResource
    -> NewReceiverValidation
    -> Maybe RequestCookies
    -> Handler (CookieResponse RawHtml)
servePaymentsReceiverAddressValidation ul receiver = withSessionLayer ul
    $ \layer -> do
        renderHtml
            <$> receiverAddressValidation
                layer
                alertH
                receiverAddressValidationH
                receiver

servePaymentsReceiverAmountValidation
    :: UILayer WalletResource
    -> NewReceiverValidation
    -> Maybe RequestCookies
    -> Handler (CookieResponse RawHtml)
servePaymentsReceiverAmountValidation ul amount = withSessionLayer ul
    $ \layer -> do
        renderHtml
            <$> receiverAmountValidation
                layer
                alertH
                receiverAmountValidationH
                amount

servePaymentsSign
    :: UILayer WalletResource
    -> SignatureForm
    -> Maybe RequestCookies
    -> Handler (CookieResponse RawHtml)
servePaymentsSign ul r = -- SignatureForm{signatureFormState, signaturePassword} =
    withSessionLayer ul $ \layer -> do
        renderHtml
            <$> signalHandler
                layer
                alertH
                paymentsChangeH
                (signatureFormState r)
                (case r of
                    SignatureForm _ s -> Sign s
                    ExternalSignatureForm _ s -> ExternallySign s
                )

servePaymentsSubmit
    :: UILayer WalletResource
    -> State
    -> Maybe RequestCookies
    -> Handler (CookieResponse RawHtml)
servePaymentsSubmit ul state =
    withSessionLayer ul $ \layer -> do
        renderHtml
            <$> signalHandler
                layer
                alertH
                paymentsChangeH
                state
                Submit

servePaymentsReset
    :: UILayer WalletResource
    -> Maybe RequestCookies
    -> Handler (CookieResponse RawHtml)
servePaymentsReset ul =
    withSessionLayer ul $ \layer -> do
        renderHtml
            <$> signalHandler
                layer
                alertH
                paymentsChangeH
                NoState
                Reset
