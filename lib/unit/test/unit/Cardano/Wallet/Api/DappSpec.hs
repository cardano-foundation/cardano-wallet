{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Wallet.Api.DappSpec (spec) where

import Cardano.Wallet.Api.Http.Server
    ( dappCapabilitiesUnavailable
    )
import Cardano.Wallet.Api.Http.Server.Error
    ( IsServerError (toServerError)
    , dappServerError
    )
import Cardano.Wallet.Api.Types.Dapp
    ( ApiDappBackendBuild (..)
    , ApiDappCapabilities
    , makeApiDappCapabilities
    )
import Cardano.Wallet.Api.Types.Error
    ( ApiError (..)
    , ApiErrorInfo (..)
    , ApiErrorMessage (..)
    , DappError (..)
    )
import Data.Aeson
    ( eitherDecode
    , encode
    )
import Data.ByteString.Lazy
    ( ByteString
    )
import Data.List
    ( intercalate
    )
import Data.Text
    ( Text
    )
import Network.Wai
    ( defaultRequest
    , pathInfo
    )
import Servant.Server
    ( ServerError (..)
    , err400
    , err404
    , err500
    , runHandler
    )
import Test.Hspec
    ( Spec
    , describe
    , it
    , shouldBe
    , shouldReturn
    , shouldSatisfy
    )
import Prelude

import qualified Cardano.Wallet.Read as Read
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.Text as T

spec :: Spec
spec = do
    describe "dApp capability contract" $ do
        it "accepts and preserves the complete V1 document" $ do
            let decoded = eitherDecode validDocument :: Either String ApiDappCapabilities
            decoded `shouldSatisfy` isRight
            (eitherDecode . encode =<< decoded) `shouldBe` decoded

        it "retains a valid future capability only after full validation"
            $ ( eitherDecode
                    ( documentWithCapabilities
                        $ validCapabilities <> [capability "future-capability" 2]
                    )
                    :: Either String ApiDappCapabilities
              )
            `shouldSatisfy` isRight

        mapM_
            ( \(label, document) ->
                it ("rejects " <> label)
                    $ (eitherDecode document :: Either String ApiDappCapabilities)
                    `shouldSatisfy` isLeft
            )
            invalidDocuments

        it "constructs the complete response only for Conway" $ do
            let build = ApiDappBackendBuild "v2026-07-23" (T.pack $ forty 'a')
                make era =
                    makeApiDappCapabilities build 0 1 (T.pack $ sixtyFour 'b') era
            make (Read.EraValue Read.Conway) `shouldSatisfy` isJust
            make (Read.EraValue Read.Dijkstra) `shouldBe` Nothing
            make (Read.EraValue Read.Babbage) `shouldBe` Nothing
            makeApiDappCapabilities
                build
                2
                1
                (T.pack $ sixtyFour 'b')
                (Read.EraValue Read.Conway)
                `shouldBe` Nothing

        it "keeps the HTTP handler identical to an unmatched route"
            $ runHandler
                ( dappCapabilitiesUnavailable
                    $ ApiDappBackendBuild "version" (T.pack $ forty 'a')
                )
            `shouldReturn` Left err404

    describe "dApp backend errors"
        $ mapM_
            ( \(dappError, status, code, info, message) ->
                it (show dappError) $ do
                    let response = dappServerError dappError
                    errHTTPCode response `shouldBe` status
                    eitherDecode (errBody response)
                        `shouldBe` Right (ApiError info (ApiErrorMessage message))
                    BL8.unpack (errBody response)
                        `shouldSatisfy` isInfixOf ("\"code\":\"" <> code <> "\"")
                    BL8.unpack (errBody response)
                        `shouldSatisfy` not . containsSensitiveSentinel
            )
            dappErrors

    describe "transaction-context raw errors" $ do
        let request =
                defaultRequest
                    { pathInfo = ["v2", "wallets", "wallet", "transaction-context"]
                    }
        it "passes only exact fixed dApp errors" $ do
            let expected = dappServerError DappContextConflictError
            toServerError (request, expected) `shouldBe` expected
        it "normalizes arbitrary JSON errors" $ do
            let generic =
                    err400{errBody = "{\"code\":\"bad_request\",\"message\":\"details\"}"}
            toServerError (request, generic)
                `shouldBe` dappServerError InvalidDappRequest
        it "normalizes unexpected failures without leaking their body" $ do
            let generic =
                    err500
                        { errBody =
                            "{\"code\":\"internal_server_error\",\"message\":\"SENSITIVE_DAPP_SENTINEL\"}"
                        }
            toServerError (request, generic)
                `shouldBe` dappServerError DappInternalErrorResponse

validDocument :: ByteString
validDocument = documentWithCapabilities validCapabilities

documentWithCapabilities :: [String] -> ByteString
documentWithCapabilities capabilityDocuments =
    BL8.pack
        $ "{\"api_version\":1,\"backend_build\":{\"version\":\"v2026-07-23\",\"source_revision\":\""
            <> forty 'a'
            <> "\"},\"network\":{\"network_id\":0,\"network_magic\":1,\"genesis_hash\":\""
            <> sixtyFour 'b'
            <> "\",\"current_era\":\"conway\"},\"capabilities\":["
            <> intercalate "," capabilityDocuments
            <> "]}"

invalidDocuments :: [(String, ByteString)]
invalidDocuments =
    [
        ( "unknown top-level fields"
        , BL8.init validDocument <> ",\"unknown\":true}"
        )
    ,
        ( "unknown backend build fields"
        , replace
            ("\"source_revision\":\"" <> forty 'a' <> "\"")
            ("\"source_revision\":\"" <> forty 'a' <> "\",\"unknown\":true")
            validDocument
        )
    ,
        ( "unknown network fields"
        , replace
            "\"current_era\":\"conway\""
            "\"current_era\":\"conway\",\"unknown\":true"
            validDocument
        )
    ,
        ( "unknown capability fields"
        , replace
            "\"available_eras\":[\"conway\"]}"
            "\"available_eras\":[\"conway\"],\"unknown\":true}"
            validDocument
        )
    ,
        ( "uppercase source revisions"
        , replace (forty 'a') (forty 'A') validDocument
        )
    ,
        ( "out-of-range network magic"
        , replace
            "\"network_magic\":1"
            "\"network_magic\":4294967296"
            validDocument
        )
    ,
        ( "duplicate capabilities"
        , documentWithCapabilities
            $ validCapabilities <> [capability "transaction-context" 1]
        )
    ,
        ( "partial required capability sets"
        , documentWithCapabilities $ take 3 validCapabilities
        )
    ,
        ( "old required revisions"
        , documentWithCapabilities
            $ capability "transaction-context" 0 : drop 1 validCapabilities
        )
    ,
        ( "duplicate eras"
        , documentWithCapabilities
            $ capabilityWithEras "transaction-context" "[\"conway\",\"conway\"]"
                : drop 1 validCapabilities
        )
    ,
        ( "unsafe capability revisions"
        , documentWithCapabilities
            $ capability "transaction-context" 9007199254740992
                : drop 1 validCapabilities
        )
    ,
        ( "malformed capability names"
        , documentWithCapabilities
            $ capability "Transaction_Context" 1 : drop 1 validCapabilities
        )
    ]

validCapabilities :: [String]
validCapabilities =
    [ capability "transaction-context" 1
    , capability "reviewed-context-signing" 1
    , capability "cip8-cip95" 1
    , capability "durable-wallet-submit" 1
    ]

capability :: String -> Integer -> String
capability name revision =
    "{\"name\":\""
        <> name
        <> "\",\"revision\":"
        <> show revision
        <> ",\"available_eras\":[\"conway\"]}"

capabilityWithEras :: String -> String -> String
capabilityWithEras name eras =
    "{\"name\":\""
        <> name
        <> "\",\"revision\":1,\"available_eras\":"
        <> eras
        <> "}"

dappErrors :: [(DappError, Int, String, ApiErrorInfo, Text)]
dappErrors =
    [
        ( InvalidDappRequest
        , 400
        , "dapp_invalid_request"
        , DappInvalidRequest
        , "Invalid backend request"
        )
    ,
        ( DappContextConflictError
        , 400
        , "dapp_context_conflict"
        , DappContextConflict
        , "Backend context conflict"
        )
    ,
        ( DappIdentityConflictError
        , 400
        , "dapp_identity_conflict"
        , DappIdentityConflict
        , "Submission identity conflict"
        )
    ,
        ( DappAccountChangedError
        , 409
        , "dapp_account_changed"
        , DappAccountChanged
        , "Wallet or network changed"
        )
    ,
        ( DappContextUnavailableError
        , 503
        , "dapp_context_unavailable"
        , DappContextUnavailable
        , "Wallet context unavailable"
        )
    ,
        ( DappInternalErrorResponse
        , 500
        , "dapp_internal_error"
        , DappInternalError
        , "Backend operation failed"
        )
    ,
        ( DappTxProofGenerationError
        , 403
        , "dapp_tx_proof_generation"
        , DappTxProofGeneration
        , "Transaction proof unavailable"
        )
    ,
        ( DappDeprecatedCertificateError
        , 403
        , "dapp_deprecated_certificate"
        , DappDeprecatedCertificate
        , "Deprecated certificate"
        )
    ,
        ( DappDataProofGenerationError
        , 403
        , "dapp_data_proof_generation"
        , DappDataProofGeneration
        , "Data proof unavailable"
        )
    ,
        ( DappDataAddressNotPkError
        , 403
        , "dapp_data_address_not_pk"
        , DappDataAddressNotPk
        , "Address is not a public-key credential"
        )
    ,
        ( DappSubmissionFailedError
        , 409
        , "dapp_submission_failed"
        , DappSubmissionFailed
        , "Transaction submission failed"
        )
    ,
        ( DappSubmissionUnavailableError
        , 503
        , "dapp_submission_unavailable"
        , DappSubmissionUnavailable
        , "Transaction submission unavailable"
        )
    ]

containsSensitiveSentinel :: String -> Bool
containsSensitiveSentinel body = "SENSITIVE_DAPP_SENTINEL" `isInfixOf` body

replace :: String -> String -> ByteString -> ByteString
replace needle replacement =
    BL8.pack . replaceString needle replacement . BL8.unpack

replaceString :: String -> String -> String -> String
replaceString needle replacement input =
    case breakOn needle input of
        Nothing -> input
        Just (prefix, suffix) -> prefix <> replacement <> drop (length needle) suffix

breakOn :: String -> String -> Maybe (String, String)
breakOn needle = go []
  where
    go _ [] = Nothing
    go prefix rest@(x : xs)
        | needle `isPrefixOf` rest = Just (reverse prefix, rest)
        | otherwise = go (x : prefix) xs

forty :: Char -> String
forty character = replicate 40 character

sixtyFour :: Char -> String
sixtyFour character = replicate 64 character

isLeft :: Either a b -> Bool
isLeft = either (const True) (const False)

isRight :: Either a b -> Bool
isRight = not . isLeft

isJust :: Maybe a -> Bool
isJust = maybe False (const True)

isPrefixOf :: Eq a => [a] -> [a] -> Bool
isPrefixOf prefix value = take (length prefix) value == prefix

isInfixOf :: Eq a => [a] -> [a] -> Bool
isInfixOf needle value = any (isPrefixOf needle) (tails value)

tails :: [a] -> [[a]]
tails [] = [[]]
tails value@(_ : rest) = value : tails rest
