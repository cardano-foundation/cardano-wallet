{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Integration.Scenario.API.Byron.Transactions
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Api.Types
    ( ApiByronWallet
    , ApiTransaction
    , DecodeAddress
    , DecodeStakeAddress
    , WalletStyle (..)
    )
import Control.Monad
    ( forM_ )
import Control.Monad.IO.Class
    ( liftIO )
import Control.Monad.Trans.Resource
    ( runResourceT )
import Data.Generics.Internal.VL.Lens
    ( (^.) )
import Data.Quantity
    ( Quantity (..) )
import Data.Text.Class
    ( fromText )
import Test.Hspec
    ( SpecWith, describe )
import Test.Hspec.Expectations.Lifted
    ( shouldBe )
import Test.Hspec.Extra
    ( it )
import Test.Integration.Framework.DSL
    ( Context
    , Headers (..)
    , Payload (..)
    , emptyIcarusWallet
    , emptyRandomWallet
    , expectErrorMessage
    , expectField
    , expectListSize
    , expectResponseCode
    , faucetAmt
    , fixtureIcarusWallet
    , fixturePassphrase
    , fixtureRandomWallet
    , json
    , postByronWallet
    , request
    , toQueryString
    , verify
    , walletId
    )
import Test.Integration.Framework.Request
    ( RequestException )
import Test.Integration.Framework.TestData
    ( errMsg400StartTimeLaterThanEndTime, errMsg404NoWallet )

import qualified Cardano.Wallet.Api.Link as Link
import qualified Data.Text as T
import qualified Network.HTTP.Types.Status as HTTP

data TestCase a = TestCase
    { query :: T.Text
    , assertions :: [(HTTP.Status, Either RequestException a) -> IO ()]
    }

spec :: forall n t.
    ( DecodeAddress n
    , DecodeStakeAddress n
    ) => SpecWith (Context t)
spec = describe "BYRON_MIGRATIONS" $ do

    it "BYRON_RESTORE_08 - Icarus wallet with high indexes" $ \ctx -> runResourceT $ do
        -- NOTE
        -- Special Icarus mnemonic where address indexes are all after the index
        -- 500. Because we don't have the whole history, restoring sequential
        -- wallets like Icarus ones is tricky from just a snapshot and we need
        -- to use arbitrarily big address pool gaps.
        let mnemonics =
                [ "erosion", "ahead", "vibrant", "air", "day"
                , "timber", "thunder", "general", "dice", "into"
                , "chest", "enrich", "social", "neck", "shine"
                ] :: [T.Text]
        let payload = Json [json| {
                    "name": "High Index Wallet",
                    "mnemonic_sentence": #{mnemonics},
                    "passphrase": #{fixturePassphrase},
                    "style": "icarus"
                    } |]

        r <- postByronWallet ctx payload
        verify r
            [ expectResponseCode HTTP.status201
            , expectField (#balance . #available) (`shouldBe` Quantity faucetAmt)
            ]

    it "BYRON_RESTORE_09 - Ledger wallet" $ \ctx -> runResourceT $ do
        -- NOTE
        -- Special legacy wallets where addresses have been generated from a
        -- seed derived using the auxiliary method used by Ledger.
        let mnemonics =
                [ "vague" , "wrist" , "poet" , "crazy" , "danger" , "dinner"
                , "grace" , "home" , "naive" , "unfold" , "april" , "exile"
                , "relief" , "rifle" , "ranch" , "tone" , "betray" , "wrong"
                ] :: [T.Text]
        let payload = Json [json| {
                    "name": "Ledger Wallet",
                    "mnemonic_sentence": #{mnemonics},
                    "passphrase": #{fixturePassphrase},
                    "style": "ledger"
                    } |]

        r <- postByronWallet ctx payload
        verify r
            [ expectResponseCode HTTP.status201
            , expectField (#balance . #available) (`shouldBe` Quantity faucetAmt)
            ]

    it "BYRON_TX_LIST_01 - 0 txs on empty Byron wallet"
        $ \ctx -> runResourceT @IO $ forM_ [emptyRandomWallet, emptyIcarusWallet] $ \emptyByronWallet -> do
            w <- emptyByronWallet ctx
            let link = Link.listTransactions @'Byron w
            r <- request @([ApiTransaction n]) ctx link Default Empty
            verify r
                [ expectResponseCode HTTP.status200
                , expectListSize 0
                ]

    it "BYRON_TX_LIST_01 - Can list transactions on Byron Wallet"
        $ \ctx -> runResourceT @IO $ forM_ [fixtureRandomWallet, fixtureIcarusWallet]
        $ \fixtureByronWallet -> do
            w <- fixtureByronWallet ctx
            let link = Link.listTransactions @'Byron w
            r <- request @([ApiTransaction n]) ctx link Default Empty
            verify r
                [ expectResponseCode HTTP.status200
                , expectListSize 10
                ]

    describe "BYRON_TX_LIST_01 - Faulty start, end, order values" $ do
        let orderErr = "Please specify one of the following values:\
            \ ascending, descending."
        let startEndErr = "Expecting ISO 8601 date-and-time format\
            \ (basic or extended), e.g. 2012-09-25T10:15:00Z."
        let queries :: [TestCase [ApiTransaction n]] =
                [
                  TestCase
                    { query = toQueryString [ ("start", "2009") ]
                    , assertions =
                             [ expectResponseCode HTTP.status400
                             , expectErrorMessage startEndErr
                             ]

                    }
                 , TestCase
                     { query = toQueryString
                             [ ("start", "2012-09-25T10:15:00Z")
                             , ("end", "2016-11-21")
                             ]
                     , assertions =
                             [ expectResponseCode HTTP.status400
                             , expectErrorMessage startEndErr
                             ]

                     }
                 , TestCase
                     { query = toQueryString
                             [ ("start", "2012-09-25")
                             , ("end", "2016-11-21T10:15:00Z")
                             ]
                     , assertions =
                             [ expectResponseCode HTTP.status400
                             , expectErrorMessage startEndErr
                             ]

                     }
                 , TestCase
                     { query = toQueryString
                             [ ("end", "2012-09-25T10:15:00Z")
                             , ("start", "2016-11-21")
                             ]
                     , assertions =
                             [ expectResponseCode HTTP.status400
                             , expectErrorMessage startEndErr
                             ]

                     }
                 , TestCase
                     { query = toQueryString [ ("order", "scending") ]
                     , assertions =
                            [ expectResponseCode HTTP.status400
                            , expectErrorMessage orderErr
                            ]

                     }
                 , TestCase
                     { query = toQueryString
                             [ ("start", "2012-09-25T10:15:00Z")
                             , ("order", "asc")
                             ]
                     , assertions =
                             [ expectResponseCode HTTP.status400
                             , expectErrorMessage orderErr
                             ]
                     }
                ]

        let withQuery q (method, link) = (method, link <> q)

        forM_ queries $ \tc -> it (T.unpack $ query tc) $ \ctx -> runResourceT @IO $ do
            w <- emptyRandomWallet ctx
            let link = withQuery (query tc) $ Link.listTransactions @'Byron w
            r <- request @([ApiTransaction n]) ctx link Default Empty
            liftIO $ verify r (assertions tc)

    it "BYRON_TX_LIST_01 - Start time shouldn't be later than end time" $
        \ctx -> runResourceT @IO $ do
            w <- emptyRandomWallet ctx
            let startTime = "2009-09-09T09:09:09Z"
            let endTime = "2001-01-01T01:01:01Z"
            let link = Link.listTransactions' @'Byron w
                    Nothing
                    (either (const Nothing) Just $ fromText $ T.pack startTime)
                    (either (const Nothing) Just $ fromText $ T.pack endTime)
                    Nothing
            r <- request @([ApiTransaction n]) ctx link Default Empty
            expectResponseCode HTTP.status400 r
            expectErrorMessage
                (errMsg400StartTimeLaterThanEndTime startTime endTime) r

    it "BYRON_TX_LIST_04 - Deleted wallet" $ \ctx -> runResourceT @IO $ do
        w <- emptyRandomWallet ctx
        _ <- request @ApiByronWallet ctx
            (Link.deleteWallet @'Byron w) Default Empty
        let link = Link.listTransactions @'Byron w
        r <- request @([ApiTransaction n]) ctx link Default Empty
        expectResponseCode HTTP.status404 r
        expectErrorMessage (errMsg404NoWallet $ w ^. walletId) r
