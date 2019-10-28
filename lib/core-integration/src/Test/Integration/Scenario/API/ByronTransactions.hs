{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Integration.Scenario.API.ByronTransactions
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Api.Types
    ( ApiByronWallet, ApiTransaction )
import Cardano.Wallet.Primitive.AddressDerivation
    ( NetworkDiscriminant (..) )
import Control.Monad
    ( forM_ )
import Data.Generics.Internal.VL.Lens
    ( (^.) )
import Test.Hspec
    ( SpecWith, describe, it, pendingWith )
import Test.Integration.Framework.DSL
    ( Context
    , Headers (..)
    , Payload (..)
    , deleteByronWalletEp
    , emptyByronWallet
    , emptyWallet
    , expectErrorMessage
    , expectListSizeEqual
    , expectResponseCode
    , listByronTxEp
    , request
    , toQueryString
    , verify
    , walletId
    )
import Test.Integration.Framework.Request
    ( RequestException )
import Test.Integration.Framework.TestData
    ( errMsg400StartTimeLaterThanEndTime
    , errMsg404NoEndpoint
    , errMsg404NoWallet
    , falseWalletIds
    , getHeaderCases
    )

import qualified Data.Text as T
import qualified Network.HTTP.Types.Status as HTTP

data TestCase a = TestCase
    { query :: T.Text
    , assertions :: [(HTTP.Status, Either RequestException a) -> IO ()]
    }

spec :: forall t n. (n ~ 'Testnet) => SpecWith (Context t)
spec = do

    it "BYRON_TX_LIST_01 - 0 txs on empty Byron wallet" $ \ctx -> do
        w <- emptyByronWallet ctx
        r <- request @([ApiTransaction n]) ctx (listByronTxEp w mempty)
            Default Empty
        verify r
            [ expectResponseCode @IO HTTP.status200
            , expectListSizeEqual 0
            ]

    it "BYRON_TX_LIST_01 - Can list transactions on Byron Wallet" $ \ctx -> do
        pendingWith "Blocked by #849"
        -- TODO make it fixtureByronWallet after #849 and adjust expectations
        w <- emptyByronWallet ctx
        r <- request @([ApiTransaction n]) ctx (listByronTxEp w mempty)
            Default Empty
        verify r
            [ expectResponseCode @IO HTTP.status200
            , expectListSizeEqual 0
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
                             [ expectResponseCode @IO HTTP.status400
                             , expectErrorMessage startEndErr
                             ]

                    }
                 , TestCase
                     { query = toQueryString
                             [ ("start", "2012-09-25T10:15:00Z")
                             , ("end", "2016-11-21")
                             ]
                     , assertions =
                             [ expectResponseCode @IO HTTP.status400
                             , expectErrorMessage startEndErr
                             ]

                     }
                 , TestCase
                     { query = toQueryString
                             [ ("start", "2012-09-25")
                             , ("end", "2016-11-21T10:15:00Z")
                             ]
                     , assertions =
                             [ expectResponseCode @IO HTTP.status400
                             , expectErrorMessage startEndErr
                             ]

                     }
                 , TestCase
                     { query = toQueryString
                             [ ("end", "2012-09-25T10:15:00Z")
                             , ("start", "2016-11-21")
                             ]
                     , assertions =
                             [ expectResponseCode @IO HTTP.status400
                             , expectErrorMessage startEndErr
                             ]

                     }
                 , TestCase
                     { query = toQueryString [ ("order", "scending") ]
                     , assertions =
                            [ expectResponseCode @IO HTTP.status400
                            , expectErrorMessage orderErr
                            ]

                     }
                 , TestCase
                     { query = toQueryString
                             [ ("start", "2012-09-25T10:15:00Z")
                             , ("order", "asc")
                             ]
                     , assertions =
                             [ expectResponseCode @IO HTTP.status400
                             , expectErrorMessage orderErr
                             ]
                     }
                ]

        forM_ queries $ \tc -> it (T.unpack $ query tc) $ \ctx -> do
            w <- emptyByronWallet ctx
            r <- request @([ApiTransaction n]) ctx (listByronTxEp w (query tc))
                Default Empty
            verify r (assertions tc)

    it "BYRON_TX_LIST_01 - Start time shouldn't be later than end time" $
        \ctx -> do
              w <- emptyByronWallet ctx
              let startTime = "2009-09-09T09:09:09Z"
              let endTime = "2001-01-01T01:01:01Z"
              let q = toQueryString
                      [ ("start", T.pack startTime)
                      , ("end", T.pack endTime)
                      ]
              r <- request @([ApiTransaction n]) ctx (listByronTxEp w q)
                  Default Empty
              expectResponseCode @IO HTTP.status400 r
              expectErrorMessage
                  (errMsg400StartTimeLaterThanEndTime startTime endTime) r

    it "BYRON_TX_LIST_02 -\
        \ Byron endpoint does not list Shelley wallet transactions" $ \ctx -> do
        w <- emptyWallet ctx
        let wid = w ^. walletId
        let ep = ("GET", "v2/byron-wallets/" <> wid <> "/transactions")
        r <- request @([ApiTransaction n]) ctx ep Default Empty
        verify r
            [ expectResponseCode @IO HTTP.status404
            , expectErrorMessage (errMsg404NoWallet wid)
            ]

    it "BYRON_TX_LIST_03 -\
        \ Shelley endpoint does not list Byron wallet transactions" $ \ctx -> do
        w <- emptyByronWallet ctx
        let wid = w ^. walletId
        let ep = ("GET", "v2/wallets/" <> wid <> "/transactions")
        r <- request @([ApiTransaction n]) ctx ep Default Empty
        verify r
            [ expectResponseCode @IO HTTP.status404
            , expectErrorMessage (errMsg404NoWallet wid)
            ]

    it "BYRON_TX_LIST_04 - 'almost' valid walletId" $ \ctx -> do
        w <- emptyByronWallet ctx
        let endpoint = "v2/byron-wallets/"
                <> T.unpack (T.append (w ^. walletId) "0")
                <> "/transactions"
        r <- request @([ApiTransaction n]) ctx ("GET", T.pack endpoint)
                Default Empty
        expectResponseCode @IO HTTP.status404 r
        expectErrorMessage errMsg404NoEndpoint r

    it "BYRON_TX_LIST_04 - Deleted wallet" $ \ctx -> do
        w <- emptyByronWallet ctx
        _ <- request @ApiByronWallet ctx (deleteByronWalletEp w) Default Empty
        r <- request @([ApiTransaction n]) ctx (listByronTxEp w mempty)
            Default Empty
        expectResponseCode @IO HTTP.status404 r
        expectErrorMessage (errMsg404NoWallet $ w ^. walletId) r

    describe "BYRON_TX_LIST_04 - False wallet ids" $ do
        forM_ falseWalletIds $ \(title, walId) -> it title $ \ctx -> do
            let endpoint = "v2/byron-wallets/" <> walId <> "/transactions"
            r <- request @([ApiTransaction n]) ctx ("GET", T.pack endpoint)
                    Default Empty
            expectResponseCode HTTP.status404 r
            if (title == "40 chars hex") then
                expectErrorMessage (errMsg404NoWallet $ T.pack walId) r
            else
                expectErrorMessage errMsg404NoEndpoint r :: IO ()

    describe "BYRON_TX_LIST_05 - Request headers" $ do
        forM_ (getHeaderCases HTTP.status200)
            $ \(title, h, expec) -> it title $ \ctx -> do
            w <- emptyByronWallet ctx
            r <- request @([ApiTransaction n]) ctx (listByronTxEp w mempty) h Empty
            verify r expec
