{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- The following is justified by the usage of the 'requestBody' field
-- accessor on the 'Request' object from the Network.Wai.Internal module as a
-- setter. The use of this field as a getter is deprecated, but we need it to
-- mount an existing request body in a request.
{-# OPTIONS_GHC -fno-warn-deprecations #-}

module Cardano.Wallet.ApiSpec
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Api
    ( Api )
import Cardano.Wallet.Api.Server
    ( LiftHandler (..) )
import Cardano.Wallet.Api.Types
    ( AllowedMnemonics
    , ApiAddress
    , ApiByronWallet
    , ApiByronWalletMigrationInfo
    , ApiCoinSelection
    , ApiEpochNumber
    , ApiFee
    , ApiNetworkInformation
    , ApiNetworkParameters
    , ApiNetworkTip
    , ApiSelectCoinsData
    , ApiStakePool
    , ApiT
    , ApiTransaction
    , ApiTxId
    , ApiUtxoStatistics
    , ApiWallet
    , ApiWalletPassphrase
    , ByronWalletPostData
    , ByronWalletStyle (..)
    , Iso8601Time
    , PostExternalTransactionData
    , PostTransactionData
    , PostTransactionFeeData
    , StyleSymbol
    , WalletPostData
    , WalletPutData
    , WalletPutPassphraseData
    )
import Cardano.Wallet.Primitive.AddressDerivation
    ( NetworkDiscriminant (..) )
import Cardano.Wallet.Primitive.Types
    ( PoolId, WalletId, walletNameMaxLength )
import Control.Arrow
    ( first )
import Control.Monad
    ( forM, forM_ )
import Control.Monad.IO.Class
    ( liftIO )
import Data.Aeson.QQ
    ( aesonQQ )
import Data.ByteString.Lazy
    ( ByteString )
import Data.Function
    ( (&) )
import Data.IORef
    ( IORef, atomicModifyIORef, modifyIORef, newIORef )
import Data.Kind
    ( Type )
import Data.List
    ( (\\) )
import Data.Map.Strict
    ( Map )
import Data.Proxy
    ( Proxy (..) )
import Data.String
    ( IsString )
import Data.Text
    ( Text )
import Data.Typeable
    ( Typeable, typeRep )
import Data.Void
    ( Void )
import Data.Word.Odd
    ( Word31 )
import GHC.TypeLits
    ( ErrorMessage (..), KnownSymbol, Nat, Symbol, TypeError, symbolVal )
import Network.HTTP.Types.Header
    ( hAccept, hContentLength, hContentType )
import Network.HTTP.Types.Method
    ( Method )
import Network.Wai
    ( Request
    , RequestBodyLength (..)
    , defaultRequest
    , pathInfo
    , requestBody
    , requestBodyLength
    , requestHeaders
    , requestMethod
    )
import Network.Wai.Middleware.ServerError
    ( handleRawError )
import Network.Wai.Test
    ( Session, assertBody, assertStatus, request, runSession )
import Servant
    ( Application
    , Header'
    , ReqBody
    , Server
    , StdMethod (..)
    , Verb
    , err501
    , serve
    , throwError
    )
import Servant.API
    ( (:<|>) (..), (:>), Capture )
import Servant.API.Verbs
    ( DeleteNoContent, GetNoContent, NoContentVerb, ReflectMethod (..), Verb )
import Servant.Links
    ( URI (..), allLinks', linkURI )
import System.IO.Unsafe
    ( unsafePerformIO )
import Test.Hspec
    ( Spec, SpecWith, describe, it, runIO, shouldBe, xdescribe, xit )
import Web.HttpApiData
    ( ToHttpApiData (..) )

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Servant


spec :: Spec
spec = describe "PATATE" $ do
    gSpec (everyPathParams api) spec_MalformedParam
    gSpec (everyBodyParams api) spec_MalformedParam
    gSpec (everyAllowedMethods api) spec_NotAllowedMethod

spec_MalformedParam :: Request -> ExpectedError -> Session ()
spec_MalformedParam malformedRequest (ExpectedError msg) = do
    response <- request malformedRequest
    response & assertStatus 400
    response & assertBody (Aeson.encode [aesonQQ|
        { "code": "bad_request"
        , "message": #{msg}
        }|])

spec_NotAllowedMethod :: Request -> ExpectedError -> Session ()
spec_NotAllowedMethod malformedRequest (ExpectedError msg) = do
    response <- request malformedRequest
    response & assertStatus 405
    response & assertBody (Aeson.encode [aesonQQ|
        { "code": "method_not_allowed"
        , "message": #{msg}
        }|])

--
-- Construction of well-formed and malformed parameters
--

newtype ExpectedError = ExpectedError String
    deriving newtype (IsString)

class WellFormed t where
    wellformed :: t

class Malformed t where
    malformed :: [(t, ExpectedError)]
    malformed = []

instance WellFormed (PathParam (ApiT WalletId)) where
    wellformed = PathParam $ T.replicate 40 "0"

instance Malformed (PathParam (ApiT WalletId)) where
    malformed = first PathParam <$>
        [ (T.replicate 40 "ś", msg)
        , (T.replicate 39 "1", msg)
        , (T.replicate 41 "1", msg)
        ]
      where
        msg = "wallet id should be a hex-encoded string of 40 characters"

instance WellFormed (PathParam ApiTxId) where
    wellformed = PathParam $ T.replicate 64 "0"

instance Malformed (PathParam ApiTxId) where
    malformed = first PathParam <$>
        [ (T.replicate 64 "ś", msg)
        , (T.replicate 63 "1", msg)
        , (T.replicate 65 "1", msg)
        ]
      where
        msg = "Invalid tx hash: expecting a hex-encoded value that is 32 bytes in length."

instance WellFormed (PathParam (ApiT PoolId)) where
    wellformed = PathParam $ T.replicate 64 "0"

instance Malformed (PathParam (ApiT PoolId)) where
    malformed = first PathParam <$>
        [ (T.replicate 64 "ś", msg)
        , (T.replicate 63 "1", msg)
        , (T.replicate 65 "1", msg)
        ]
      where
        msg = "Invalid stake pool id: expecting a hex-encoded value that is 32 bytes in length."

instance WellFormed (PathParam ApiEpochNumber) where
    wellformed = PathParam "latest"

instance Malformed (PathParam ApiEpochNumber) where
    malformed = first PathParam <$>
        [ ("earliest", msg)
        , (T.pack $ show $ (+1) $ fromIntegral @Word31 @Int maxBound, msg)
        , ("invalid", msg)
        , ("-1", msg)
        ]
      where
        msg = "I couldn't parse the given epoch number. I am expecting either the word 'latest' or, an integer from 0 to 2147483647."

instance Malformed (BodyParam WalletPostData) where
    malformed = first (BodyParam . Aeson.encode) <$>
        [ ( [aesonQQ|
            { "name": #{wName}
            , "mnemonic_sentence": "album execute kingdom dumb trip all salute busy case bring spell ugly umbrella choice shy"
            , "passphrase": #{wPassphrase}
            }|]
          , "Error in $['mnemonic_sentence']: parsing [] failed, expected Array, but encountered String"
          )
        , ( [aesonQQ|
            { "name": #{wName}
            , "mnemonic_sentence": 15
            , "passphrase": #{wPassphrase}
            }|]
          , "Error in $['mnemonic_sentence']: parsing [] failed, expected Array, but encountered Number"
          )
        , ( [aesonQQ|
            { "name": #{wName}
            , "passphrase": #{wPassphrase}
            }|]
          , "Error in $: parsing Cardano.Wallet.Api.Types.WalletPostData(WalletPostData) failed, key 'mnemonic_sentence' not found"
          )
        , ( [aesonQQ|
            { "name": #{wName}
            , "mnemonic_sentence": []
            , "passphrase": #{wPassphrase}
            }|]
          , "Error in $['mnemonic_sentence']: Invalid number of words: 15, 18, 21 or 24 words are expected."
          )
        , ( [aesonQQ|
            { "name": #{wName}
            , "mnemonic_sentence": #{mnemonics3}
            , "passphrase": #{wPassphrase}
            }|]
          , "Error in $['mnemonic_sentence']: Invalid number of words: 15, 18, 21 or 24 words are expected."
          )
        , ( [aesonQQ|
            { "name": #{wName}
            , "mnemonic_sentence": #{mnemonics6}
            , "passphrase": #{wPassphrase}
            }|]
          , "Error in $['mnemonic_sentence']: Invalid number of words: 15, 18, 21 or 24 words are expected."
          )
        , ( [aesonQQ|
            { "name": #{wName}
            , "mnemonic_sentence": #{mnemonics9}
            , "passphrase": #{wPassphrase}
            }|]
          , "Error in $['mnemonic_sentence']: Invalid number of words: 15, 18, 21 or 24 words are expected."
          )
        , ( [aesonQQ|
            { "name": #{wName}
            , "mnemonic_sentence": #{mnemonics12}
            , "passphrase": #{wPassphrase}
            }|]
          , "Error in $['mnemonic_sentence']: Invalid number of words: 15, 18, 21 or 24 words are expected."
          )
        , ( [aesonQQ|
            { "name": #{wName}
            , "mnemonic_sentence": #{invalidMnemonics15}
            , "passphrase": #{wPassphrase}
            }|]
          , "Error in $['mnemonic_sentence']: Invalid entropy checksum: please double-check the last word of your mnemonic sentence."
          )
        , ( [aesonQQ|
            { "name": #{wName}
            , "mnemonic_sentence": #{notInDictMnemonics15}
            , "passphrase": #{wPassphrase}
            }|]
          , "Error in $['mnemonic_sentence']: Found an unknown word not present in the pre-defined dictionary. The full dictionary is available here: https://github.com/input-output-hk/cardano-wallet/tree/master/specifications/mnemonic/english.txt"
          )
        , ( [aesonQQ|
            { "name": #{wName}
            , "mnemonic_sentence": #{specMnemonicSentence}
            , "passphrase": #{wPassphrase}
            }|]
          , "Error in $['mnemonic_sentence']: Invalid entropy checksum: please double-check the last word of your mnemonic sentence."
          )
        , ( [aesonQQ|
            { "name": #{wName}
            , "mnemonic_sentence": #{japaneseMnemonics12}
            , "passphrase": #{wPassphrase}
            }|]
          , "Error in $['mnemonic_sentence']: Invalid number of words: 15, 18, 21 or 24 words are expected."
          )
        , ( [aesonQQ|
            { "name": #{wName}
            , "mnemonic_sentence": #{japaneseMnemonics15}
            , "passphrase": #{wPassphrase}
            }|]
          , "Error in $['mnemonic_sentence']: Found an unknown word not present in the pre-defined dictionary. The full dictionary is available here: https://github.com/input-output-hk/cardano-wallet/tree/master/specifications/mnemonic/english.txt"
          )
        , ( [aesonQQ|
            { "name": #{wName}
            , "mnemonic_sentence": #{chineseMnemonics9}
            , "passphrase": #{wPassphrase}
            }|]
          , "Error in $['mnemonic_sentence']: Invalid number of words: 15, 18, 21 or 24 words are expected."
          )
        , ( [aesonQQ|
            { "name": #{wName}
            , "mnemonic_sentence": #{chineseMnemonics18}
            , "passphrase": #{wPassphrase}
            }|]
          , "Error in $['mnemonic_sentence']: Found an unknown word not present in the pre-defined dictionary. The full dictionary is available here: https://github.com/input-output-hk/cardano-wallet/tree/master/specifications/mnemonic/english.txt"
          )
        , ( [aesonQQ|
            { "name": #{wName}
            , "mnemonic_sentence": #{frenchMnemonics12}
            , "passphrase": #{wPassphrase}
            }|]
          , "Error in $['mnemonic_sentence']: Invalid number of words: 15, 18, 21 or 24 words are expected."
          )
        , ( [aesonQQ|
            { "name": #{wName}
            , "mnemonic_sentence": #{frenchMnemonics21}
            , "passphrase": #{wPassphrase}
            }|]
          , "Error in $['mnemonic_sentence']: Found an unknown word not present in the pre-defined dictionary. The full dictionary is available here: https://github.com/input-output-hk/cardano-wallet/tree/master/specifications/mnemonic/english.txt"
          )
        , ( [aesonQQ|
            { "name": #{wName}
            , "mnemonic_sentence": #{mnemonics15}
            , "mnemonic_second_factor": []
            , "passphrase": #{wPassphrase}
            }|]
          , "Error in $['mnemonic_second_factor']: Invalid number of words: 9 or 12 words are expected."
          )
        , ( [aesonQQ|
            { "name": #{wName}
            , "mnemonic_sentence": #{mnemonics15}
            , "mnemonic_second_factor": #{specMnemonicSecondFactor}
            , "passphrase": #{wPassphrase}
            }|]
          , "Error in $['mnemonic_second_factor']: Invalid entropy checksum: please double-check the last word of your mnemonic sentence."
          )
        , ( [aesonQQ|
            { "name": #{nameTooLong}
            , "mnemonic_sentence": #{mnemonics15}
            , "passphrase" :#{wPassphrase}
            }|]
          , "Error in $.name: name is too long: expected at most 255 characters"
          )
        , ( [aesonQQ|
            { "name": ""
            , "mnemonic_sentence": #{mnemonics15}
            , "passphrase" :#{wPassphrase}
            }|]
          , "Error in $.name: name is too short: expected at least 1 character"
          )
        , ( [aesonQQ|
            { "name": []
            , "mnemonic_sentence": #{mnemonics15}
            , "passphrase" :#{wPassphrase}
            }|]
          , "Error in $.name: parsing Text failed, expected String, but encountered Array"
          )
        , ( [aesonQQ|
            { "name": 123
            , "mnemonic_sentence": #{mnemonics15}
            , "passphrase" :#{wPassphrase}
            }|]
          , "Error in $.name: parsing Text failed, expected String, but encountered Number"
          )
        , ( [aesonQQ|
            { "mnemonic_sentence": #{mnemonics15}
            , "passphrase" :#{wPassphrase}
            }|]
          , "Error in $: parsing Cardano.Wallet.Api.Types.WalletPostData(WalletPostData) failed, key 'name' not found"
          )
        ]
      where
        wName :: Text
        wName =
            "Just a łallet"

        wPassphrase :: Text
        wPassphrase =
            "Secure Passphrase"

        nameTooLong :: Text
        nameTooLong =
            T.replicate (walletNameMaxLength + 1) "ę"

        mnemonics3 :: [Text]
        mnemonics3 =
            ["diamond", "flee", "window"]

        mnemonics6 :: [Text]
        mnemonics6 =
            ["tornado", "canvas", "peasant", "spike", "enrich", "dilemma"]

        mnemonics9 :: [Text]
        mnemonics9 =
            ["subway", "tourist", "abstract", "roast", "border", "curious","exercise", "work", "narrow"]

        mnemonics12 :: [Text]
        mnemonics12 =
            ["agent", "siren", "roof", "water", "giant", "pepper","obtain", "oxygen", "treat", "vessel", "hip", "garlic"]

        mnemonics15 :: [Text]
        mnemonics15 =
            ["network", "empty", "cause", "mean", "expire", "private",
            "finger", "accident", "session", "problem", "absurd", "banner", "stage",
            "void", "what"]

        mnemonics18 :: [Text]
        mnemonics18 =
            ["whisper", "control", "diary", "solid", "cattle", "salmon",
            "whale", "slender", "spread", "ice", "shock", "solve", "panel",
            "caution", "upon", "scatter", "broken", "tonight"]

        mnemonics21 :: [Text]
        mnemonics21 =
            ["click", "puzzle", "athlete", "morning", "fold", "retreat",
            "across", "timber", "essay", "drill", "finger", "erase", "galaxy",
            "spoon", "swift", "eye", "awesome", "shrimp", "depend", "zebra", "token"]

        mnemonics24 :: [Text]
        mnemonics24 =
            ["decade", "distance", "denial", "jelly", "wash", "sword",
            "olive", "perfect", "jewel", "renew", "wrestle", "cupboard", "record",
            "scale", "pattern", "invite", "other", "fruit", "gloom", "west", "oak",
            "deal", "seek", "hand"]

        invalidMnemonics12 :: [Text]
        invalidMnemonics12 =
            ["word","word","word","word"
            ,"word","word","word","word"
            ,"word","word","word","hill"
            ]

        invalidMnemonics15 :: [Text]
        invalidMnemonics15 =
            ["word","word","word","word","word"
            ,"word","word","word","word","word"
            ,"word","word","word","word","word"
            ]

        notInDictMnemonics15 :: [Text]
        notInDictMnemonics15 =
            ["one","two","three","four","five"
            ,"six","seven","eight","nine","diary"
            , "twenty", "coin", "regret", "cry", "thumb"
            ]

        specMnemonicSentence :: [Text]
        specMnemonicSentence =
            ["squirrel", "material", "silly", "twice", "direct"
            ,"slush", "pistol", "razor", "become", "junk"
            ,"kingdom", "flee","squirrel", "silly", "twice"
            ]

        specMnemonicByron :: [Text]
        specMnemonicByron =
            [ "squirrel", "material", "silly", "twice"
            , "direct", "slush","pistol", "razor"
            , "become", "junk", "kingdom", "junk"
            ]

        specMnemonicSecondFactor :: [Text]
        specMnemonicSecondFactor =
            ["squirrel", "material", "silly"
            , "twice","direct", "slush"
            , "pistol", "razor", "become"
            ]

        japaneseMnemonics12 :: [Text]
        japaneseMnemonics12 =
            ["そうだん",　"ひよう",　"にもつ",　"やさしい",　"きふく",　 "ねつい",　"だったい",　"けんてい",　"けいろ",　"ざつがく",　"ほうもん",　"すこし"]

        japaneseMnemonics15 :: [Text]
        japaneseMnemonics15 =
            ["うめる", "せんく", "えんぎ", "はんぺん", "おくりがな", "さんち", "きなが", "といれ", "からい", "らくだ", "うえる", "ふめん", "せびろ", "られつ", "なにわ"]

        chineseMnemonics9 :: [Text]
        chineseMnemonics9 =
            ["钢", "看", "磁", "塑", "凤", "魏", "世", "腐", "恶" ]

        chineseMnemonics18 :: [Text]
        chineseMnemonics18 =
            ["盗", "精", "序", "郎", "赋", "姿", "委", "善", "酵", "祥", "赛", "矩", "蜡", "注", "韦", "效", "义", "冻"]

        frenchMnemonics12 :: [Text]
        frenchMnemonics12 =
            ["palmarès", "supplier", "visuel", "gardien", "adorer", "cordage", "notifier", "réglage", "employer", "abandon", "scénario", "proverbe"]

        frenchMnemonics21 :: [Text]
        frenchMnemonics21 =
            [ "pliage", "exhorter", "brasier", "chausson", "bloquer"
            , "besace", "sorcier", "absurde", "neutron", "forgeron"
            , "geyser", "moulin", "cynique", "cloche", "baril"
            , "infliger", "rompre", "typique", "renifler", "creuser", "matière"
            ]

instance Malformed (BodyParam (ApiSelectCoinsData 'Testnet))

instance Malformed (BodyParam (PostTransactionData 'Testnet))

instance Malformed (BodyParam (PostTransactionFeeData 'Testnet))

instance Malformed (BodyParam (ByronWalletPostData (mw :: [Nat])))

instance Malformed (BodyParam WalletPutData)

instance Malformed (BodyParam WalletPutPassphraseData)

instance Malformed (BodyParam ApiNetworkTip)

instance Malformed (BodyParam ApiWalletPassphrase)

instance Malformed (BodyParam PostExternalTransactionData)

--
-- Generic API Spec
--

class Typeable api => GenericApiSpec api where
    gSpec :: api -> (Request -> ExpectedError -> Session ()) -> Spec
    gSpec _ _ = xdescribe (show $ typeRep $ Proxy @api) (pure ())

instance (GenericApiSpec a, GenericApiSpec b) => GenericApiSpec (a :<|> b) where
    gSpec (a :<|> b) toS = gSpec a toS >> gSpec b toS

instance GenericApiSpec Request where
    gSpec req _ = pure ()

instance
    ( Typeable a, Malformed (PathParam a)
    ) => GenericApiSpec (PathParam a -> Request)
  where
    gSpec toRequest toSession = describe "Malformed PathParam" $ do
        forM_ (first toRequest <$> malformed) $ \(req, msg) ->
            it (titleize (Proxy @a) req) $
                runSession (toSession req msg) application

instance
    ( Typeable a, Malformed (PathParam a), WellFormed (PathParam a)
    , GenericApiSpec (PathParam a -> Request)
    , Typeable b, Malformed (PathParam b), WellFormed (PathParam b)
    , GenericApiSpec (PathParam b -> Request)
    ) => GenericApiSpec (PathParam a -> PathParam b -> Request)
  where
    gSpec toRequest toSession = do
        gSpec (toRequest wellformed) toSession
        gSpec (`toRequest` wellformed) toSession

instance
    ( Typeable a, Malformed (BodyParam a)
    ) => GenericApiSpec (BodyParam a -> IO Request)
  where
    gSpec toRequest toSession = describe "Malformed BodyParam" $
        forM_ (first toRequest <$> malformed) $ \(newReq, msg) -> do
            req <- runIO newReq
            it (titleize (Proxy @a) req) $
                runSession (toSession req msg) application

instance GenericApiSpec (Map [Text] [Method])
  where
    gSpec allowedMethods toSession = describe "Not Allowed Methods" $
        forM_ (Map.toList allowedMethods) $ \(pathInfo, methods) ->
            forM_ (allMethods \\ methods) $ \requestMethod -> do
                let req = defaultRequest { pathInfo, requestMethod }
                it (titleize (Proxy @Void) req) $
                    runSession (toSession req msg) application
      where
        msg =
            "You've reached a known endpoint but I don't know how to handle the \
            \HTTP method specified. Please double-check both the endpoint and \
            \the method: one of them is likely to be incorrect (for example: \
            \POST instead of PUT, or GET instead of POST...)."

        allMethods :: [Method]
        allMethods =
            ["GET","PUT","POST","PATCH","DELETE","CONNECT","TRACE","OPTIONS"]

--
-- Construct test cases from the API
--

application :: Application
application = serve api server
    & handleRawError (curry handler)

api :: Proxy (Api Testnet)
api = Proxy @(Api Testnet)

server :: Server (Api Testnet)
server = error
    "No test from this module should actually reach handlers of the server. \
    \Tests are indeed all testing the internal machinery of Servant + Wai and \
    \the way they interact with the outside world. Only valid requests are \
    \delegated to our handlers."

newtype PathParam t = PathParam Text
    deriving (Typeable)

newtype BodyParam t = BodyParam ByteString
    deriving (Typeable)

everyPathParams :: GEveryParams api => Proxy api -> MkPathRequest api
everyPathParams proxy = gEveryPathParams proxy defaultRequest

everyBodyParams :: GEveryParams api => Proxy api -> MkBodyRequest api
everyBodyParams proxy = gEveryBodyParams proxy $ defaultRequest
    { requestHeaders =
        [ (hContentType, "application/json")
        , (hAccept, "application/json")
        ]
    }

everyAllowedMethods :: GEveryParams api => Proxy api -> Map [Text] [Method]
everyAllowedMethods proxy =
    Map.fromListWith (++) (toTuple <$> gEveryAllowedMethods proxy)
  where
    toTuple :: Request -> ([Text], [Method])
    toTuple req = (reverse (pathInfo req), [requestMethod req])

class GEveryParams api where
    type MkPathRequest api :: *
    gEveryPathParams :: Proxy api -> Request -> MkPathRequest api

    type MkBodyRequest api :: *
    gEveryBodyParams :: Proxy api -> Request -> MkBodyRequest api

    gEveryAllowedMethods :: Proxy api -> [Request]

    -- TODO
    -- Capture request query params as QueryParam
    --
    -- newtype QueryParam t = QueryParam Text
    --     deriving (Typeable)
    --
    -- type MkQueryRequest api :: *
    -- gEveryQueryParams :: Proxy api -> Request -> MkQueryRequest api

instance
    ( GEveryParams a
    , GEveryParams b
    ) => GEveryParams (a :<|> b)
  where
    type MkPathRequest (a :<|> b) = MkPathRequest a :<|> MkPathRequest b
    gEveryPathParams _ req = do
        gEveryPathParams (Proxy @a) req
      :<|>
        gEveryPathParams (Proxy @b) req

    type MkBodyRequest (a :<|> b) = MkBodyRequest a :<|> MkBodyRequest b
    gEveryBodyParams _ req = do
        gEveryBodyParams (Proxy @a) req
      :<|>
        gEveryBodyParams (Proxy @b) req

    gEveryAllowedMethods _ =
        gEveryAllowedMethods (Proxy @a)
      ++
        gEveryAllowedMethods (Proxy @b)

instance
    ( ReflectMethod m
    ) => GEveryParams (Verb (m :: StdMethod) s ct a)
  where
    type MkPathRequest (Verb m s ct a) = Request
    gEveryPathParams _ req =
        req { requestMethod = reflectMethod (Proxy @m) }

    type MkBodyRequest (Verb m s ct a) = Request
    gEveryBodyParams _ req =
        req { requestMethod = reflectMethod (Proxy @m) }

    gEveryAllowedMethods _ =
        [defaultRequest { requestMethod = reflectMethod (Proxy @m) }]

instance
    ( ReflectMethod m
    ) => GEveryParams (NoContentVerb (m :: StdMethod))
  where
    type MkPathRequest (NoContentVerb m) = Request
    gEveryPathParams _ req =
        req { requestMethod = reflectMethod (Proxy @m) }

    type MkBodyRequest (NoContentVerb m) = Request
    gEveryBodyParams _ req =
        req { requestMethod = reflectMethod (Proxy @m) }

    gEveryAllowedMethods _ =
        [defaultRequest { requestMethod = reflectMethod (Proxy @m) }]

instance
    ( WellFormed (PathParam t)
    , GEveryParams sub
    ) => GEveryParams (Capture p t :> sub)
  where
    type MkPathRequest (Capture p t :> sub) = PathParam t -> MkPathRequest sub
    gEveryPathParams _ req t =
        gEveryPathParams (Proxy @sub) (addPathFragment t req)

    type MkBodyRequest (Capture p t :> sub) = MkBodyRequest sub
    gEveryBodyParams _ req =
        gEveryBodyParams (Proxy @sub) (addPathFragment t req)
      where
        t = wellformed :: PathParam t

    gEveryAllowedMethods _ =
        addPathFragment t <$> gEveryAllowedMethods (Proxy @sub)
      where
        t = wellformed :: PathParam t

instance
    ( KnownSymbol s
    , GEveryParams sub
    ) => GEveryParams (s :> sub)
  where
    type MkPathRequest (s :> sub) = MkPathRequest sub
    gEveryPathParams _ req =
        gEveryPathParams (Proxy @sub) (addPathFragment str req)
      where
        str = PathParam $ T.pack $ symbolVal (Proxy @s)

    type MkBodyRequest (s :> sub) = MkBodyRequest sub
    gEveryBodyParams _ req =
        gEveryBodyParams (Proxy @sub) (addPathFragment t req)
      where
        t = PathParam $ T.pack $ symbolVal (Proxy @s)

    gEveryAllowedMethods _ =
        addPathFragment t <$> gEveryAllowedMethods (Proxy @sub)
      where
        t = PathParam $ T.pack $ symbolVal (Proxy @s)


instance
    ( GEveryParams sub
    ) => GEveryParams (ReqBody a b :> sub)
  where
    type MkPathRequest (ReqBody a b :> sub) = MkPathRequest sub
    gEveryPathParams _ =
        gEveryPathParams (Proxy @sub)

    type MkBodyRequest (ReqBody a b :> sub) = BodyParam b -> IO (MkBodyRequest sub)
    gEveryBodyParams _ req b =
        gEveryBodyParams (Proxy @sub) <$> (setRequestBody b req)

    gEveryAllowedMethods _ =
        gEveryAllowedMethods (Proxy @sub)

instance
    ( GEveryParams sub
    ) => GEveryParams (Servant.QueryParam a b :> sub)
  where
    type MkPathRequest (Servant.QueryParam a b :> sub) = MkPathRequest sub
    gEveryPathParams _ =
        gEveryPathParams (Proxy @sub)

    type MkBodyRequest (Servant.QueryParam a b :> sub) = MkBodyRequest sub
    gEveryBodyParams _ =
        gEveryBodyParams (Proxy @sub)

    gEveryAllowedMethods _ =
        gEveryAllowedMethods (Proxy @sub)

--
-- Helpers
--

addPathFragment :: PathParam t -> Request -> Request
addPathFragment (PathParam fragment) req = req
    { pathInfo = pathInfo req ++ [fragment] }

setRequestBody :: BodyParam b -> Request -> IO Request
setRequestBody (BodyParam bytes) req = do
    ref <- newIORef $ BL.toChunks bytes
    pure req
        { requestBodyLength = KnownLength $ fromIntegral $ BL.length bytes
        , requestBody = atomicModifyIORef ref $ \case
            []  -> ([], mempty)
            h:q -> (q, h)
        }

titleize
    :: forall t. (Typeable t)
    => Proxy t
    -> Request
    -> String
titleize proxy req = unwords
    [ if proxyStr == "(Void)" then "" else proxyStr
    , B8.unpack (requestMethod req)
    , "/" <> T.unpack (T.intercalate "/" $ pathInfo req)
    ]
  where
    proxyStr = "(" <> show (typeRep proxy) <> ")"
