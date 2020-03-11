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
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
-- |
-- Copyright: © 2020 IOHK
-- License: Apache-2.0
--
-- This modules defines test scenarios for various parameters present in the API
-- (path parameters, body parameters, headers, etc ...).
--
-- The 'Wellformed' and 'Malformed' interfaces below are used in
-- @Cardano.Wallet.ApiSpec@ to generically derive a set of unit tests from the
-- API servant type. These tests enforces that the API behaves as expected when
-- present with malformed data, and do so consistently across each endpoint.


-- TODO: Temporary until all 'integration' scenarios have been move here.
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module Cardano.Wallet.Api.Malformed
    (
    -- * Data Types
      Header (..)
    , PathParam (..)
    , BodyParam (..)
    , ExpectedError (..)

    -- * Interface
    , Wellformed
    , wellformed
    , Malformed
    , malformed
    ) where

import Prelude

import Cardano.Wallet.Api.Types
    ( ApiEpochNumber
    , ApiNetworkTip
    , ApiPoolId
    , ApiSelectCoinsData
    , ApiT (..)
    , ApiTxId
    , ApiWalletPassphrase
    , PostExternalTransactionData
    , PostTransactionData
    , PostTransactionFeeData
    , SomeByronWalletPostData
    , WalletOrAccountPostData
    , WalletPutData
    , WalletPutPassphraseData
    )
import Cardano.Wallet.Primitive.AddressDerivation
    ( NetworkDiscriminant (..) )
import Cardano.Wallet.Primitive.Types
    ( WalletId, walletNameMaxLength )
import Control.Arrow
    ( first )
import Data.Aeson.QQ
    ( aesonQQ )
import Data.ByteString.Lazy
    ( ByteString )
import Data.String
    ( IsString )
import Data.Text
    ( Text )
import Data.Typeable
    ( Typeable )
import Data.Word.Odd
    ( Word31 )
import GHC.TypeLits
    ( Symbol )
import Servant
    ( JSON, OctetStream )

import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.Text as T

--
-- Parameter Data Types
--

newtype ExpectedError = ExpectedError String
    deriving newtype (IsString)

newtype PathParam (t :: *) = PathParam Text
    deriving (Typeable)

newtype BodyParam (t :: *) = BodyParam ByteString
    deriving (Typeable)

newtype Header (headerName :: Symbol) (contentType :: *) =
    Header BS.ByteString
    deriving (Typeable)

--
-- Class Declaration
--

class Wellformed t where
    wellformed :: t

class Malformed t where
    malformed :: [(t, ExpectedError)]
    malformed = []
--
-- Class instances (PathParam)
--
instance Wellformed (PathParam (ApiT WalletId)) where
    wellformed = PathParam $ T.replicate 40 "0"

instance Malformed (PathParam (ApiT WalletId)) where
    malformed = first PathParam <$>
        [ (T.replicate 40 "ś", msg)
        , (T.replicate 39 "1", msg)
        , (T.replicate 41 "1", msg)
        ]
      where
        msg = "wallet id should be a hex-encoded string of 40 characters"

instance Wellformed (PathParam ApiTxId) where
    wellformed = PathParam $ T.replicate 64 "0"

instance Malformed (PathParam ApiTxId) where
    malformed = first PathParam <$>
        [ (T.replicate 64 "ś", msg)
        , (T.replicate 63 "1", msg)
        , (T.replicate 65 "1", msg)
        ]
      where
        msg = "Invalid tx hash: expecting a hex-encoded value that is 32 bytes in length."

instance Wellformed (PathParam ApiPoolId) where
    wellformed = PathParam $ T.replicate 64 "0"

instance Malformed (PathParam ApiPoolId) where
    malformed = first PathParam <$>
        [ (T.replicate 64 "ś", msg)
        , (T.replicate 63 "1", msg)
        , (T.replicate 65 "1", msg)
        ]
      where
        msg = "Invalid stake pool id: expecting a hex-encoded value that is 32 bytes in length."

instance Wellformed (PathParam ApiEpochNumber) where
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

--
-- Class instances (BodyParam)
--
instance Malformed (BodyParam SomeByronWalletPostData) where
    malformed = jsonValid ++ jsonInvalid
     where
         jsonInvalid = first BodyParam <$>
            [ ("1020344", "Error in $: parsing SomeByronWallet failed, expected Object, but encountered Number")
            , ("\"1020344\"", "Error in $: parsing SomeByronWallet failed, expected Object, but encountered String")
            , ("\"slot_number : \"random\"}", "trailing junk after valid JSON: endOfInput")
            , ("{style = \"random\"}", msgJsonInvalid)
            ]
         jsonValid = first (BodyParam . Aeson.encode) <$>
            [ -- style
              ( [aesonQQ|
                { "style": "radom"
                , "name": #{wName}
                , "mnemonic_sentence": "album execute kingdom dumb trip all salute busy case bring spell ugly umbrella choice shy"
                , "passphrase": #{wPassphrase}
                }|]
              , "Error in $: unrecognized wallet's style."
              )
            , ( [aesonQQ|
                { "style": 1
                , "name": #{wName}
                , "mnemonic_sentence": "album execute kingdom dumb trip all salute busy case bring spell ugly umbrella choice shy"
                , "passphrase": #{wPassphrase}
                }|]
              , "Error in $.style: parsing Text failed, expected String, but encountered Number"
              )
            -- mnemonic_sentence
            , ( [aesonQQ|
                { "style": "random"
                , "name": #{wName}
                , "mnemonic_sentence": "album execute kingdom dumb trip all salute busy case bring spell ugly umbrella choice shy"
                , "passphrase": #{wPassphrase}
                }|]
              , "Error in $['mnemonic_sentence']: parsing [] failed, expected Array, but encountered String"
              )
            , ( [aesonQQ|
                { "style": "random"
                , "name": #{wName}
                , "mnemonic_sentence": #{mnemonics15}
                , "passphrase": #{wPassphrase}
                }|]
              , "Error in $['mnemonic_sentence']: Invalid number of words: 12 words are expected."
              )
            , ( [aesonQQ|
                { "style": "icarus"
                , "name": #{wName}
                , "mnemonic_sentence": #{mnemonics12}
                , "passphrase": #{wPassphrase}
                }|]
              , "Error in $['mnemonic_sentence']: Invalid number of words: 15 words are expected."
              )
            , ( [aesonQQ|
                { "style": "trezor"
                , "name": #{wName}
                , "mnemonic_sentence": #{mnemonics9}
                , "passphrase": #{wPassphrase}
                }|]
              , "Error in $['mnemonic_sentence']: Invalid number of words: 12, 15, 18, 21 or 24 words are expected."
              )
            , ( [aesonQQ|
                { "style": "ledger"
                , "name": #{wName}
                , "mnemonic_sentence": []
                , "passphrase": #{wPassphrase}
                }|]
              , "Error in $['mnemonic_sentence']: Invalid number of words: 12, 15, 18, 21 or 24 words are expected."
              )
            , ( [aesonQQ|
                { "style": "random"
                , "name": #{wName}
                , "mnemonic_sentence": #{mnemonics15}
                , "passphrase": #{wPassphrase}
                }|]
              , "Error in $['mnemonic_sentence']: Invalid number of words: 12 words are expected."
              )
            , ( [aesonQQ|
                { "style": "icarus"
                , "name": #{wName}
                , "mnemonic_sentence": #{invalidMnemonics15}
                , "passphrase": #{wPassphrase}
                }|]
              , "Error in $['mnemonic_sentence']: Invalid entropy checksum: please double-check the last word of your mnemonic sentence."
              )
            , ( [aesonQQ|
                { "style": "ledger"
                , "name": #{wName}
                , "mnemonic_sentence": #{notInDictMnemonics15}
                , "passphrase": #{wPassphrase}
                }|]
              , "Error in $['mnemonic_sentence']: Found an unknown word not present in the pre-defined dictionary. The full dictionary is available here: https://github.com/input-output-hk/cardano-wallet/tree/master/specifications/mnemonic/english.txt"
              )
            , ( [aesonQQ|
                { "style": "trezor"
                , "name": #{wName}
                , "mnemonic_sentence": #{specMnemonicSentence}
                , "passphrase": #{wPassphrase}
                }|]
              , "Error in $['mnemonic_sentence']: Invalid entropy checksum: please double-check the last word of your mnemonic sentence."
              )
            -- name
            , ( [aesonQQ|
                { "style": "trezor"
                , "name": #{nameTooLong}
                , "mnemonic_sentence": #{mnemonics15}
                , "passphrase" :#{wPassphrase}
                }|]
              , "Error in $.name: name is too long: expected at most 255 characters"
              )
            , ( [aesonQQ|
                { "style": "random"
                , "name": ""
                , "mnemonic_sentence": #{mnemonics12}
                , "passphrase" :#{wPassphrase}
                }|]
              , "Error in $.name: name is too short: expected at least 1 character"
              )
            -- passphrase
            , ( [aesonQQ|
                { "style": "random"
                , "name": #{wName}
                , "mnemonic_sentence": #{mnemonics12}
                , "passphrase" : 100
                }|]
              , "Error in $.passphrase: parsing Text failed, expected String, but encountered Number"
              )
            , ( [aesonQQ|
                { "style": "icarus"
                , "name": #{wName}
                , "mnemonic_sentence": #{mnemonics15}
                , "passphrase" :[""]
                }|]
              , "Error in $.passphrase: parsing Text failed, expected String, but encountered Array"
              )
            , ( [aesonQQ|
                { "style": "trezor"
                , "name": #{wName}
                , "mnemonic_sentence": #{mnemonics12}
                , "passphrase" :""
                }|]
              , "Error in $.passphrase: passphrase is too short: expected at least 10 characters"
              )
            , ( [aesonQQ|
                { "style": "ledger"
                , "name": #{wName}
                , "mnemonic_sentence": #{mnemonics12}
                , "passphrase" :"123456789"
                }|]
              , "Error in $.passphrase: passphrase is too short: expected at least 10 characters"
              )
            , ( [aesonQQ|
                { "style": "random"
                , "name": #{wName}
                , "mnemonic_sentence": #{mnemonics12}
                , "passphrase" : #{nameTooLong}
                }|]
              , "Error in $.passphrase: passphrase is too long: expected at most 255 characters"
              )
            , ( [aesonQQ|
                { "style": "random"
                , "name": #{wName}
                , "mnemonic_sentence": #{mnemonics12}
                }|]
              , "Error in $: parsing Cardano.Wallet.Api.Types.ByronWalletPostData(ByronWalletPostData) failed, key 'passphrase' not found"
              )
            ]

instance Malformed (BodyParam WalletOrAccountPostData) where
    malformed = jsonValid ++ jsonInvalid
     where
         jsonInvalid = first BodyParam <$>
            [ ("1020344", "Error in $: parsing postData failed, expected Object, but encountered Number")
            , ("\"1020344\"", "Error in $: parsing postData failed, expected Object, but encountered String")
            , ("\"slot_number : \"random\"}", "trailing junk after valid JSON: endOfInput")
            , ("{style = \"random\"}", msgJsonInvalid)
            ]
         jsonValid = first (BodyParam . Aeson.encode) <$>
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
            -- address_pool_gap
            , ( [aesonQQ|
                { "name": #{wName}
                , "mnemonic_sentence": #{mnemonics15}
                , "passphrase" : #{wPassphrase}
                , "address_pool_gap" : ["20"]
                }|]
              , "Error in $['address_pool_gap']: parsing Integer failed, expected Number, but encountered Array"
              )
            , ( [aesonQQ|
                { "name": #{wName}
                , "mnemonic_sentence": #{mnemonics15}
                , "passphrase" : #{wPassphrase}
                , "address_pool_gap" : "20"
                }|]
              , "Error in $['address_pool_gap']: parsing Integer failed, expected Number, but encountered String"
              )
            , ( [aesonQQ|
                { "name": #{wName}
                , "mnemonic_sentence": #{mnemonics15}
                , "passphrase" : #{wPassphrase}
                , "address_pool_gap" : 2.5
                }|]
              , "Error in $['address_pool_gap']: parsing Integer failed, unexpected floating number 2.5"
              )
            , ( [aesonQQ|
                { "name": #{wName}
                , "mnemonic_sentence": #{mnemonics15}
                , "passphrase" : #{wPassphrase}
                , "address_pool_gap" : -2.5
                }|]
              , "Error in $['address_pool_gap']: parsing Integer failed, unexpected floating number -2.5"
              )
            , ( [aesonQQ|
                { "name": #{wName}
                , "mnemonic_sentence": #{mnemonics15}
                , "passphrase" : #{wPassphrase}
                , "address_pool_gap" : 0
                }|]
              , "Error in $['address_pool_gap']: An address pool gap must be a natural number between 10 and 100."
              )
            , ( [aesonQQ|
                { "name": #{wName}
                , "mnemonic_sentence": #{mnemonics15}
                , "passphrase" : #{wPassphrase}
                , "address_pool_gap" : -1000
                }|]
              , "Error in $['address_pool_gap']: An address pool gap must be a natural number between 10 and 100."
              )
            , ( [aesonQQ|
                { "name": #{wName}
                , "mnemonic_sentence": #{mnemonics15}
                , "passphrase" : #{wPassphrase}
                , "address_pool_gap" : -132323000
                }|]
              , "Error in $['address_pool_gap']: An address pool gap must be a natural number between 10 and 100."
              )
            , ( [aesonQQ|
                { "name": #{wName}
                , "mnemonic_sentence": #{mnemonics15}
                , "passphrase" : #{wPassphrase}
                , "address_pool_gap" : 9
                }|]
              , "Error in $['address_pool_gap']: An address pool gap must be a natural number between 10 and 100."
              )
            , ( [aesonQQ|
                { "name": #{wName}
                , "mnemonic_sentence": #{mnemonics15}
                , "passphrase" : #{wPassphrase}
                , "address_pool_gap" : 101
                }|]
              , "Error in $['address_pool_gap']: An address pool gap must be a natural number between 10 and 100."
              )
            , ( [aesonQQ|
                { "name": #{wName}
                , "mnemonic_sentence": #{mnemonics15}
                , "passphrase" : #{wPassphrase}
                , "address_pool_gap" : 1000
                }|]
              , "Error in $['address_pool_gap']: An address pool gap must be a natural number between 10 and 100."
              )
            , ( [aesonQQ|
                { "name": #{wName}
                , "mnemonic_sentence": #{mnemonics15}
                , "passphrase" : #{wPassphrase}
                , "address_pool_gap" : 132323000
                }|]
              , "Error in $['address_pool_gap']: An address pool gap must be a natural number between 10 and 100."
              )
            -- passphrase
            , ( [aesonQQ|
                { "name": #{wName}
                , "mnemonic_sentence": #{mnemonics21}
                , "passphrase" : 100
                }|]
              , "Error in $.passphrase: parsing Text failed, expected String, but encountered Number"
              )
            , ( [aesonQQ|
                { "name": #{wName}
                , "mnemonic_sentence": #{mnemonics15}
                , "passphrase" :[""]
                }|]
              , "Error in $.passphrase: parsing Text failed, expected String, but encountered Array"
              )
            , ( [aesonQQ|
                { "name": #{wName}
                , "mnemonic_sentence": #{mnemonics15}
                , "passphrase" :""
                }|]
              , "Error in $.passphrase: passphrase is too short: expected at least 10 characters"
              )
            , ( [aesonQQ|
                { "name": #{wName}
                , "mnemonic_sentence": #{mnemonics15}
                , "passphrase" :"123456789"
                }|]
              , "Error in $.passphrase: passphrase is too short: expected at least 10 characters"
              )
            , ( [aesonQQ|
                { "name": #{wName}
                , "mnemonic_sentence": #{mnemonics15}
                , "passphrase" : #{nameTooLong}
                }|]
              , "Error in $.passphrase: passphrase is too long: expected at most 255 characters"
              )
            , ( [aesonQQ|
                { "name": #{wName}
                , "mnemonic_sentence": #{mnemonics15}
                }|]
              , "Error in $: parsing Cardano.Wallet.Api.Types.WalletPostData(WalletPostData) failed, key 'passphrase' not found"
              )
            -- HW Wallets (account_public_key)
            , ( [aesonQQ|
                { "name": #{wName}
                , "account_public_key" : ["11111"]
                }|]
              , "Error in $['account_public_key']: parsing Text failed, expected String, but encountered Array"
              )
            , ( [aesonQQ|
                { "name": #{wName}
                , "account_public_key" : 11111
                }|]
              , "Error in $['account_public_key']: parsing Text failed, expected String, but encountered Number"
              )
            , ( [aesonQQ|
                { "name": #{wName}
                , "account_public_key" :#{accountPublicKeyInvalid}
                }|]
              , "Error in $['account_public_key']: AccountPublicKey: unable to deserialize ShelleyKey from json. Expecting hex-encoded string of 128 characters."
              )
            , ( [aesonQQ|
                { "name": #{wName}
                , "account_public_key" :#{accountPublicKeyTooLong}
                }|]
              , "Error in $['account_public_key']: AccountPublicKey: unable to deserialize ShelleyKey from json. Expecting hex-encoded string of 128 characters."
              )
             , ( [aesonQQ|
                 { "name": #{wName}
                 , "account_public_key" :#{accountPublicKeyTooShort}
                 }|]
               , "Error in $['account_public_key']: AccountPublicKey: unable to deserialize ShelleyKey from json. Expecting hex-encoded string of 128 characters."
               )
            ]

instance Malformed (BodyParam WalletPutPassphraseData) where
    malformed = jsonValid ++ jsonInvalid
     where
         jsonInvalid = first BodyParam <$>
            [ ("1020344", "Error in $: parsing Cardano.Wallet.Api.Types.WalletPutPassphraseData(WalletPutPassphraseData) failed, expected Object, but encountered Number")
            , ("\"1020344\"", "Error in $: parsing Cardano.Wallet.Api.Types.WalletPutPassphraseData(WalletPutPassphraseData) failed, expected Object, but encountered String")
            , ("\"slot_number : \"random\"}", "trailing junk after valid JSON: endOfInput")
            , ("{old_passphrase = \"random\"}", msgJsonInvalid)
            ]
         jsonValid = first (BodyParam . Aeson.encode) <$>
            [ ( [aesonQQ|
                { "old_passphrase": #{wPassphrase}
                , "new_passphrase" : 100
                }|]
              , "Error in $['new_passphrase']: parsing Text failed, expected String, but encountered Number"
              )
            , ( [aesonQQ|
                { "old_passphrase": []
                , "new_passphrase" : #{wPassphrase}
                }|]
              , "Error in $['old_passphrase']: parsing Text failed, expected String, but encountered Array"
              )
            , ( [aesonQQ|
                { "old_passphrase": ""
                , "new_passphrase" : #{wPassphrase}
                }|]
              , "Error in $['old_passphrase']: passphrase is too short: expected at least 10 characters"
              )
            , ( [aesonQQ|
                { "old_passphrase": #{wPassphrase}
                , "new_passphrase" : "123456789"
                }|]
              , "Error in $['new_passphrase']: passphrase is too short: expected at least 10 characters"
              )
            , ( [aesonQQ|
                { "old_passphrase": #{wPassphrase}
                , "new_passphrase" : #{nameTooLong}
                }|]
              , "Error in $['new_passphrase']: passphrase is too long: expected at most 255 characters"
              )
            , ( [aesonQQ|
                { "old_passphrase": #{nameTooLong}
                , "new_passphrase" : #{wPassphrase}
                }|]
              , "Error in $['old_passphrase']: passphrase is too long: expected at most 255 characters"
              )
            , ( [aesonQQ|
                { "old_passphrase": #{wPassphrase}
                }|]
              , "Error in $: parsing Cardano.Wallet.Api.Types.WalletPutPassphraseData(WalletPutPassphraseData) failed, key 'new_passphrase' not found"
              )
            , ( [aesonQQ|
                { "new_passphrase": #{wPassphrase}
                }|]
              , "Error in $: parsing Cardano.Wallet.Api.Types.WalletPutPassphraseData(WalletPutPassphraseData) failed, key 'old_passphrase' not found"
              )
            ]

instance Malformed (BodyParam WalletPutData) where
    malformed = jsonValid ++ jsonInvalid
     where
         jsonInvalid = first BodyParam <$>
            [ ("1020344", "Error in $: parsing Cardano.Wallet.Api.Types.WalletPutData(WalletPutData) failed, expected Object, but encountered Number")
            , ("\"1020344\"", "Error in $: parsing Cardano.Wallet.Api.Types.WalletPutData(WalletPutData) failed, expected Object, but encountered String")
            , ("\"slot_number : \"random\"}", "trailing junk after valid JSON: endOfInput")
            , ("{\"name : \"random\"}", msgJsonInvalid)
            ]
         jsonValid =
            first (BodyParam . Aeson.encode) <$>
            [ ( [aesonQQ| { "name": "" }|]
              , "Error in $.name: name is too short: expected at least 1 character"
              )
            , ( [aesonQQ| { "name": #{nameTooLong} }|]
              , "Error in $.name: name is too long: expected at most 255 characters"
              )
            , ( [aesonQQ| { "name": 123 }|]
              , "Error in $.name: parsing Text failed, expected String, but encountered Number"
              )
            , ( [aesonQQ| { "name": [] }|]
              , "Error in $.name: parsing Text failed, expected String, but encountered Array"
              )
            , ( [aesonQQ| { "name": 1.5 }|]
              , "Error in $.name: parsing Text failed, expected String, but encountered Number"
              )
            ]

instance Malformed (BodyParam ApiWalletPassphrase) where
    malformed = jsonValid ++ jsonInvalid
     where
         jsonInvalid = first BodyParam <$>
            [ ("1020344", "Error in $: parsing Cardano.Wallet.Api.Types.ApiWalletPassphrase(ApiWalletPassphrase) failed, expected Object, but encountered Number")
            , ("\"1020344\"", "Error in $: parsing Cardano.Wallet.Api.Types.ApiWalletPassphrase(ApiWalletPassphrase) failed, expected Object, but encountered String")
            , ("\"slot_number : \"random\"}", "trailing junk after valid JSON: endOfInput")
            , ("{\"name : \"random\"}", msgJsonInvalid)
            ]
         jsonValid = first (BodyParam . Aeson.encode) <$>
            [ ( [aesonQQ| { "passphrase": "" }|]
              , "Error in $.passphrase: passphrase is too short: expected at least 10 characters"
              )
            , ( [aesonQQ| { "passphrase": "123456789" }|]
              , "Error in $.passphrase: passphrase is too short: expected at least 10 characters"
              )
            , ( [aesonQQ| { "passphrase": #{nameTooLong} }|]
              , "Error in $.passphrase: passphrase is too long: expected at most 255 characters"
              )
            , ( [aesonQQ| { "passphrase": 123 }|]
              , "Error in $.passphrase: parsing Text failed, expected String, but encountered Number"
              )
            , ( [aesonQQ| { "passphrase": [] }|]
              , "Error in $.passphrase: parsing Text failed, expected String, but encountered Array"
              )
            , ( [aesonQQ| { "passphrase": 1.5 }|]
              , "Error in $.passphrase: parsing Text failed, expected String, but encountered Number"
              )
            ]

instance Malformed (BodyParam (ApiSelectCoinsData 'Testnet)) where
    malformed = jsonValid ++ jsonInvalid
     where
         jsonInvalid = first BodyParam <$>
            [ ("1020344", "Error in $: parsing Cardano.Wallet.Api.Types.ApiSelectCoinsData(ApiSelectCoinsData) failed, expected Object, but encountered Number")
            , ("\"1020344\"", "Error in $: parsing Cardano.Wallet.Api.Types.ApiSelectCoinsData(ApiSelectCoinsData) failed, expected Object, but encountered String")
            , ("\"slot_number : \"random\"}", "trailing junk after valid JSON: endOfInput")
            , ("{\"payments : [], \"random\"}", msgJsonInvalid)
            ]
         jsonValid = first (BodyParam . Aeson.encode) <$> paymentCases

instance Malformed (BodyParam (PostTransactionData 'Testnet)) where
    malformed = jsonValid ++ jsonInvalid
     where
         jsonInvalid = first BodyParam <$>
            [ ("1020344", "Error in $: parsing Cardano.Wallet.Api.Types.PostTransactionData(PostTransactionData) failed, expected Object, but encountered Number")
            , ("\"1020344\"", "Error in $: parsing Cardano.Wallet.Api.Types.PostTransactionData(PostTransactionData) failed, expected Object, but encountered String")
            , ("{\"payments : [], \"random\"}", msgJsonInvalid)
            ]
         jsonValid = first (BodyParam . Aeson.encode) <$> paymentCases ++
            [ -- passphrase
              ( [aesonQQ|
                { "payments": [
                    {
                        "address": #{addrValid},
                        "amount": {
                            "quantity": 42000000,
                            "unit": "lovelace"
                        }
                    }
                   ]
                }|]
              , "Error in $: parsing Cardano.Wallet.Api.Types.PostTransactionData(PostTransactionData) failed, key 'passphrase' not found"
              )
            , ( [aesonQQ|
               { "payments": [
                   {
                       "address": #{addrValid},
                       "amount": {
                           "quantity": 42000000,
                           "unit": "lovelace"
                       }
                   }
                  ],
                  "passphrase": "123"
               }|]
               , "Error in $.passphrase: passphrase is too short: expected at least 10 characters"
              )
            , ( [aesonQQ|
               { "payments": [
                   {
                       "address": #{addrValid},
                       "amount": {
                           "quantity": 42000000,
                           "unit": "lovelace"
                       }
                   }
                  ],
                  "passphrase": #{nameTooLong}
               }|]
               , "Error in $.passphrase: passphrase is too long: expected at most 255 characters"
              )
            ]

instance Malformed (BodyParam (PostTransactionFeeData 'Testnet)) where
    malformed = jsonValid ++ jsonInvalid
     where
         jsonInvalid = first BodyParam <$>
            [ ("1020344", "Error in $: parsing Cardano.Wallet.Api.Types.PostTransactionFeeData(PostTransactionFeeData) failed, expected Object, but encountered Number")
            , ("\"1020344\"", "Error in $: parsing Cardano.Wallet.Api.Types.PostTransactionFeeData(PostTransactionFeeData) failed, expected Object, but encountered String")
            , ("{\"payments : [], \"random\"}", msgJsonInvalid)
            , ("\"slot_number : \"random\"}", "trailing junk after valid JSON: endOfInput")
            ]
         jsonValid = first (BodyParam . Aeson.encode) <$> paymentCases

instance Malformed (BodyParam ApiNetworkTip) where
    malformed = jsonValid ++ jsonInvalid
     where
         jsonInvalid = first BodyParam <$>
            [ ("1020344", "Error in $: parsing Cardano.Wallet.Api.Types.ApiNetworkTip(ApiNetworkTip) failed, expected Object, but encountered Number")
            , ("\"1020344\"", "Error in $: parsing Cardano.Wallet.Api.Types.ApiNetworkTip(ApiNetworkTip) failed, expected Object, but encountered String")
            , ("{\"slot_number : \"random\"}", msgJsonInvalid)
            , ("\"slot_number : \"random\"}", "trailing junk after valid JSON: endOfInput")
            ]
         jsonValid = first (BodyParam . Aeson.encode) <$>
            [ ( [aesonQQ|
                { "slot_number": 0
                , "epoch_number" : 1.5
                }|]
              , "Error in $['epoch_number']: parsing Word32 failed, value is either floating or will cause over or underflow 1.5"
              )
            , ( [aesonQQ|
                { "slot_number": -100
                , "epoch_number" : 0
                }|]
              , "Error in $['slot_number']: parsing Word32 failed, value is either floating or will cause over or underflow -100.0"
              )
            , ( [aesonQQ|
                { "slot_number": ""
                , "epoch_number" : 0
                }|]
              , "Error in $['slot_number']: parsing Word32 failed, expected Number, but encountered String"
              )
            , ( [aesonQQ|
                { "slot_number": 0
                , "epoch_number" : ["123456789"]
                }|]
              , "Error in $['epoch_number']: parsing Word32 failed, expected Number, but encountered Array"
              )
            , ( [aesonQQ|
                { "slot_number": 0
                }|]
              , "Error in $: parsing Cardano.Wallet.Api.Types.ApiNetworkTip(ApiNetworkTip) failed, key 'epoch_number' not found"
              )
            , ( [aesonQQ|
                { "epoch_number": 0
                }|]
              , "Error in $: parsing Cardano.Wallet.Api.Types.ApiNetworkTip(ApiNetworkTip) failed, key 'slot_number' not found"
              )
            ]

instance Malformed (BodyParam PostExternalTransactionData)
-- no cases here as all bad requests are served by ErrDecodeSignedTxWrongPayload
-- in Server.hs. Tested by integration tests.

--
-- Class instances (Header)
--
instance Wellformed (Header "Content-Type" JSON) where
    wellformed =
        Header "application/json"

instance Malformed (Header "Content-Type" JSON) where
    malformed = first Header <$>
        [ ( "plain/text"
          , "I'm really sorry but I only understand 'application/json'. I need you to tell me what language you're speaking in order for me to understand your message. Please double-check your 'Content-Type' request header and make sure it's set to 'application/json'."
          )
        ]

instance Wellformed (Header "Content-Type" OctetStream) where
    wellformed =
        Header "application/octet-stream"

instance Malformed (Header "Content-Type" OctetStream) where
    malformed = first Header <$>
        [ ( "plain/text"
          , "I'm really sorry but I only understand 'application/octet-stream'. I need you to tell me what language you're speaking in order for me to understand your message. Please double-check your 'Content-Type' request header and make sure it's set to 'application/octet-stream'."
          )
        ]

instance Wellformed (Header "Accept" JSON) where
    wellformed =
        Header "application/json"

instance Malformed (Header "Accept" JSON) where
    malformed = first Header <$>
        [ ( "plain/text"
          , "It seems as though you don't accept 'application/json', but unfortunately I only speak 'application/json'! Please double-check your 'Accept' request header and make sure it's set to 'application/json'."
          )
        ]

--
-- Test Data
--
msgJsonInvalid :: ExpectedError
msgJsonInvalid = "I couldn't understand the content of your message. \
    \If your message is intended to be in JSON format, please check that \
    \the JSON is valid."

addrValid :: Text
addrValid = "addr1snfkjmygacv2xjdqxgvy750pxacq7v9r4ya5tyj3pmjas9mkvuh2uq0a3d2wj240n8ye5r2z52gd5m3rh0fyh5dq9p2ynae3m9p33lgg06zgge"

addrTooLong :: Text
addrTooLong = T.replicate 5000 "1"

addrInvalid :: Text
addrInvalid = T.replicate 104 "ś"

accountPublicKeyInvalid :: Text
accountPublicKeyInvalid = T.replicate 128 "ś"

accountPublicKeyTooLong :: Text
accountPublicKeyTooLong = T.replicate 129 "1"

accountPublicKeyTooShort :: Text
accountPublicKeyTooShort = "1"

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
    [ "subway", "tourist", "abstract"
    , "roast", "border", "curious"
    , "exercise", "work", "narrow"
    ]

mnemonics12 :: [Text]
mnemonics12 =
    [ "agent", "siren", "roof", "water"
    , "giant", "pepper","obtain", "oxygen"
    , "treat", "vessel", "hip", "garlic"
    ]

mnemonics15 :: [Text]
mnemonics15 =
    [ "network", "empty", "cause", "mean", "expire"
    , "private", "finger", "accident", "session", "problem"
    , "absurd", "banner", "stage", "void", "what"
    ]

mnemonics18 :: [Text]
mnemonics18 =
    [ "whisper", "control", "diary", "solid", "cattle", "salmon"
    , "whale", "slender", "spread", "ice", "shock", "solve"
    , "panel", "caution", "upon", "scatter", "broken", "tonight"
    ]

mnemonics21 :: [Text]
mnemonics21 =
    [ "click", "puzzle", "athlete", "morning", "fold", "retreat"
    , "across", "timber", "essay", "drill", "finger", "erase"
    , "galaxy", "spoon", "swift", "eye", "awesome", "shrimp"
    , "depend", "zebra", "token"
    ]

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
    [ "squirrel", "material", "silly"
    , "twice","direct", "slush"
    , "pistol", "razor", "become"
    ]

japaneseMnemonics12 :: [Text]
japaneseMnemonics12 =
    [ "そうだん",　"ひよう",　"にもつ",　"やさしい"
    , "きふく", "ねつい",　"だったい",　"けんてい"
    ,　"けいろ",　"ざつがく",　"ほうもん",　"すこし"
    ]

japaneseMnemonics15 :: [Text]
japaneseMnemonics15 =
    [ "うめる", "せんく", "えんぎ", "はんぺん", "おくりがな"
    , "さんち", "きなが", "といれ", "からい", "らくだ"
    , "うえる", "ふめん", "せびろ", "られつ", "なにわ"
    ]

chineseMnemonics9 :: [Text]
chineseMnemonics9 =
    ["钢", "看", "磁", "塑", "凤", "魏", "世", "腐", "恶" ]

chineseMnemonics18 :: [Text]
chineseMnemonics18 =
    [ "盗", "精", "序", "郎", "赋", "姿"
    , "委", "善", "酵", "祥", "赛", "矩"
    , "蜡", "注", "韦", "效", "义", "冻"
    ]

frenchMnemonics12 :: [Text]
frenchMnemonics12 =
    [ "palmarès", "supplier", "visuel", "gardien"
    , "adorer", "cordage", "notifier", "réglage"
    , "employer", "abandon", "scénario", "proverbe"
    ]

frenchMnemonics21 :: [Text]
frenchMnemonics21 =
    [ "pliage", "exhorter", "brasier", "chausson", "bloquer"
    , "besace", "sorcier", "absurde", "neutron", "forgeron"
    , "geyser", "moulin", "cynique", "cloche", "baril"
    , "infliger", "rompre", "typique", "renifler", "creuser", "matière"
    ]

paymentCases :: [(Aeson.Value, ExpectedError)]
paymentCases =
    [ -- address
      ( [aesonQQ|
        { "payments": [
            {
                "address": "1",
                "amount": {
                    "quantity": 42000000,
                    "unit": "lovelace"
                }
            }
           ]
        }|]
      , "Error in $.payments[0].address: Unable to decode Address: neither Bech32-encoded nor a valid Byron Address."
      )
    , ( [aesonQQ|
        { "payments": [
            {
                "address": #{addrInvalid},
                "amount": {
                    "quantity": 42000000,
                    "unit": "lovelace"
                }
            }
           ]
        }|]
      , "Error in $.payments[0].address: Unable to decode Address: encoding is neither Bech32 nor Base58."
      )
    , ( [aesonQQ|
        { "payments": [
            {
                "address": #{addrTooLong},
                "amount": {
                    "quantity": 42000000,
                    "unit": "lovelace"
                }
            }
           ]
        }|]
      , "Error in $.payments[0].address: Unable to decode Address: neither Bech32-encoded nor a valid Byron Address."
      )
    , ( [aesonQQ|
        { "payments": [
            {
                "address": 123,
                "amount": {
                    "quantity": 42000000,
                    "unit": "lovelace"
                }
            }
           ]
        }|]
      , "Error in $.payments[0].address: parsing Text failed, expected String, but encountered Number"
      )
    , ( [aesonQQ|
        { "payments": [
            {
                "amount": {
                    "quantity": 42000000,
                    "unit": "lovelace"
                }
            }
           ]
        }|]
      , "Error in $.payments[0]: parsing Cardano.Wallet.Api.Types.AddressAmount(AddressAmount) failed, key 'address' not found"
      )
    -- amount
    , ( [aesonQQ|
        { "payments": [
            {
                "address": #{addrValid},
                "amount": {
                    "quantity": 42000000,
                    "unit": "lovelaces"
                }
            }
           ]
        }|]
      , "Error in $.payments[0].amount: failed to parse quantified value. Expected value in 'lovelace' (e.g. { 'unit': 'lovelace', 'quantity': ... }) but got something else."
      )
    , ( [aesonQQ|
        { "payments": [
            {
                "address": #{addrValid},
                "amount": {
                    "quantity": 42000000
                }
            }
           ]
        }|]
      , "Error in $.payments[0].amount: key 'unit' not found"
      )
    , ( [aesonQQ|
        { "payments": [
            {
                "address": #{addrValid}
            }
           ]
        }|]
      , "Error in $.payments[0]: parsing Cardano.Wallet.Api.Types.AddressAmount(AddressAmount) failed, key 'amount' not found"
      )
    , ( [aesonQQ|
        { "payments": [
            {
                "address": #{addrValid},
                "amount": {
                    "unit": "lovelace"
                }
            }
           ]
        }|]
      , "Error in $.payments[0].amount: key 'quantity' not found"
      )
    , ( [aesonQQ|
        { "payments": [
            {
                "address": #{addrValid},
                "amount": 42000000
            }
           ]
        }|]
      , "Error in $.payments[0].amount: parsing Quantity failed, expected Object, but encountered Number"
      )
    , ( [aesonQQ|
        { "payments": [
            {
                "address": #{addrValid},
                "amount": {
                    "quantity": "123",
                    "unit": "lovelace"
                }
            }
           ]
        }|]
      , "Error in $.payments[0].amount.quantity: parsing Natural failed, expected Number, but encountered String"
      )
    , ( [aesonQQ|
        { "payments": [
            {
                "address": #{addrValid},
                "amount": {
                    "quantity": -1,
                    "unit": "lovelace"
                }
            }
           ]
        }|]
      , "Error in $.payments[0].amount.quantity: parsing Natural failed, unexpected negative number -1"
      )
    , ( [aesonQQ|
        { "payments": [
            {
                "address": #{addrValid},
                "amount": {
                    "quantity": 4200.12,
                    "unit": "lovelace"
                }
            }
           ]
        }|]
      , "Error in $.payments[0].amount.quantity: parsing Natural failed, unexpected floating number 4200.12"
      )
    , ( [aesonQQ|
        { "payments": [ ]
        }|]
      , "Error in $.payments: parsing NonEmpty failed, unexpected empty list"
      )
    ]
