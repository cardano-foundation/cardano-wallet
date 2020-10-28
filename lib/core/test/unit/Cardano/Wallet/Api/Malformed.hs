{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
    ( ApiAddressInspectData
    , ApiCredentials
    , ApiPoolId
    , ApiPostRandomAddressData
    , ApiPutAddressesData
    , ApiScript
    , ApiSelectCoinsData
    , ApiSlotReference
    , ApiT (..)
    , ApiTxId
    , ApiWalletMigrationPostData
    , ApiWalletPassphrase
    , ApiWalletSignData
    , ByronWalletPutPassphraseData
    , PostExternalTransactionData
    , PostTransactionData
    , PostTransactionFeeData
    , SettingsPutData (..)
    , SomeByronWalletPostData
    , WalletOrAccountPostData
    , WalletPutData
    , WalletPutPassphraseData
    )
import Cardano.Wallet.Primitive.AddressDerivation
    ( AccountingStyle (..), DerivationIndex (..), NetworkDiscriminant (..) )
import Cardano.Wallet.Primitive.Types
    ( Address, WalletId, walletNameMaxLength )
import Control.Arrow
    ( first )
import Data.Aeson.QQ
    ( aesonQQ )
import Data.ByteString.Lazy
    ( ByteString )
import Data.Proxy
    ( Proxy (..) )
import Data.String
    ( IsString )
import Data.Text
    ( Text )
import Data.Typeable
    ( Typeable )
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
    wellformed :: [t]

class Malformed t where
    malformed :: [(t, ExpectedError)]
    malformed = []
--
-- Class instances (PathParam)
--
instance Wellformed (PathParam (ApiT WalletId)) where
    wellformed = [PathParam $ T.replicate 40 "0"]

instance Malformed (PathParam (ApiT WalletId)) where
    malformed = first PathParam <$>
        [ (T.replicate 40 "ś", msg)
        , (T.replicate 39 "1", msg)
        , (T.replicate 41 "1", msg)
        ]
      where
        msg = "wallet id should be a hex-encoded string of 40 characters"

instance Wellformed (PathParam ApiTxId) where
    wellformed = [PathParam $ T.replicate 64 "0"]

instance Malformed (PathParam ApiTxId) where
    malformed = first PathParam <$>
        [ (T.replicate 64 "ś", msg)
        , (T.replicate 63 "1", msg)
        , (T.replicate 65 "1", msg)
        ]
      where
        msg = "Invalid tx hash: expecting a hex-encoded value that is 32 bytes in length."

instance Wellformed (PathParam ApiPoolId) where
    wellformed = PathParam <$>
        [ T.replicate 64 "0"
        , "pool1wqaz0q0zhtxlgn0ewssevn2mrtm30fgh2g7hr7z9rj5856457mm"
        ]

instance Malformed (PathParam ApiPoolId) where
    malformed = first PathParam <$>
        [ (T.replicate 64 "ś", msg)
        , (T.replicate 63 "1", msg)
        , (T.replicate 65 "1", msg)
        ]
      where
        msg = "Invalid stake pool id: expecting a Bech32 encoded value with human readable part of 'pool'."

instance Wellformed (PathParam (ApiT Address, Proxy ('Testnet 0))) where
    wellformed = [PathParam
        "FHnt4NL7yPY7JbfJYSadQVSGJG7EKkN4kpVJMhJ8CN3uDNymGnJuuwcHmyP4ouZ"]

instance Malformed (PathParam (ApiT Address, Proxy ('Testnet 0))) where
    malformed = []

instance Wellformed (PathParam ApiAddressInspectData) where
    wellformed = PathParam <$>
        [ "Ae2tdPwUPEYz6ExfbWubiXPB6daUuhJxikMEb4eXRp5oKZBKZwrbJ2k7EZe"
        ]

instance Malformed (PathParam ApiAddressInspectData) where
    malformed = []

instance Wellformed (PathParam (ApiT AccountingStyle)) where
    wellformed = PathParam <$>
        [ "utxo_internal"
        , "utxo_external"
        , "mutable_account"
        ]

instance Malformed (PathParam (ApiT AccountingStyle)) where
    malformed = first PathParam <$>
        [ ( "patate", msgMalformed )
        , ( "💩", msgMalformed )
        , ( "utxoInternal", msgMalformed )
        ]
      where
        msgMalformed =
            "Unable to decode the given text value. Please specify \
            \one of the following values: utxo_external, utxo_internal, \
            \mutable_account, multisig_script."

instance Wellformed (PathParam (ApiT DerivationIndex)) where
    wellformed = PathParam <$>
        [ "0"
        , "1234"
        , "2147483647"
        , "0H"
        , "1234H"
        ]

instance Malformed (PathParam (ApiT DerivationIndex)) where
    malformed = first PathParam <$>
        [ ( "patate", msgMalformed )
        , ( "💩", msgMalformed )
        , ( "2147483648", msgMalformed )
        , ( "1234H1234", msgMalformed )
        , ( "H", msgMalformed )
        ]
      where
        msgMalformed =
            "A derivation index must be a natural number between \
            \0 and 2147483647 with an optional 'H' suffix \
            \(e.g. '1815H' or '44'). \
            \Indexes without suffixes are called 'Soft' \
            \Indexes with suffixes are called 'Hardened'."

--
-- Class instances (BodyParam)
--

instance Malformed (BodyParam ApiWalletSignData) where
    malformed = first BodyParam <$>
        [ ( ""
          , "not enough input"
          )
        , ( Aeson.encode [aesonQQ|
            { "metadata": null
            , "passphrase": #{wPassphrase}
            }|]
          , "Error in $.metadata: The JSON metadata top level must be a map (JSON object) from word to value."
          )
        , ( Aeson.encode [aesonQQ|
            { "metadata": { "0": { "string": "metadata" } }
            , "passphrase": 100
            }|]
          , "Error in $.passphrase: parsing Text failed, expected String, but encountered Number"
          )
        , ( Aeson.encode [aesonQQ|
            { "metadata": { "0": { "string": "metadata" } }
            }|]
          , "Error in $: parsing Cardano.Wallet.Api.Types.ApiWalletSignData(ApiWalletSignData) failed, key 'passphrase' not found"
          )
        , ( Aeson.encode [aesonQQ|
            { "passphrase": #{wPassphrase}
            }|]
          , "Error in $: parsing Cardano.Wallet.Api.Types.ApiWalletSignData(ApiWalletSignData) failed, key 'metadata' not found"
          )
        ]

instance Malformed (BodyParam ApiScript) where
    malformed = first BodyParam <$>
        [ ( Aeson.encode [aesonQQ|
            { "something": []
            }|]
          , "Error in $: key 'script' not found"
          )
        , ( Aeson.encode [aesonQQ|
            { "script": {}
            }|]
          , "Error in $: key 'some' not found"
          )
        , ( Aeson.encode [aesonQQ|
            { "script": 2
            }|]
          , "Error in $: parsing Script SomeOf failed, expected Object, but encountered Number"
          )
        , ( Aeson.encode [aesonQQ|
            { "script": { "some" : 2 }
            }|]
          , "Error in $.some: parsing HashMap ~Text failed, expected Object, but encountered Number"
          )
        , ( Aeson.encode [aesonQQ|
            { "script": { "some" : { "something": 2, "at_least": 0 } }
            }|]
          , "Error in $: key 'from' not found"
          )
        ]

instance Malformed (BodyParam ApiCredentials) where
    malformed = first BodyParam <$>
        [ ( Aeson.encode [aesonQQ|
            {}|]
          , "Error in $: ApiCredentials must have at least one credential."
          )
        , ( Aeson.encode [aesonQQ|
            { "script": {}
            }|]
          , "Error in $: ApiCredentials must have at least one credential."
          )
        , ( Aeson.encode [aesonQQ|
            { "staking": {}
            }|]
          , "Error in $: key 'script' not found"
          )
        , ( Aeson.encode [aesonQQ|
            { "spending": {}
            }|]
          , "Error in $: key 'script' not found"
          )
        , ( Aeson.encode [aesonQQ|
            { "staking": 2
            }|]
          , "Error in $: parsing ApiCredential failed, expected Object, but encountered Number"
          )
        , ( Aeson.encode [aesonQQ|
            { "spending": 2
            }|]
          , "Error in $: parsing ApiCredential failed, expected Object, but encountered Number"
          )
        ]
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
                , "mnemonic_sentence": #{mnemonics9}
                , "passphrase": #{wPassphrase}
                }|]
              , "Error in $['mnemonic_sentence']: Invalid number of words: 12, 15, 18, 21 or 24 words are expected."
              )
            , ( [aesonQQ|
                { "style": "icarus"
                , "name": #{wName}
                , "mnemonic_sentence": #{mnemonics9}
                , "passphrase": #{wPassphrase}
                }|]
              , "Error in $['mnemonic_sentence']: Invalid number of words: 12, 15, 18, 21 or 24 words are expected."
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
                , "mnemonic_sentence": #{invalidMnemonics15}
                , "passphrase": #{wPassphrase}
                }|]
              , "Error in $['mnemonic_sentence']: Invalid entropy checksum: please double-check the last word of your mnemonic sentence."
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
              , "Error in $['address_pool_gap']: An address pool gap must be a natural number between 10 and 100000."
              )
            , ( [aesonQQ|
                { "name": #{wName}
                , "mnemonic_sentence": #{mnemonics15}
                , "passphrase" : #{wPassphrase}
                , "address_pool_gap" : -1000
                }|]
              , "Error in $['address_pool_gap']: An address pool gap must be a natural number between 10 and 100000."
              )
            , ( [aesonQQ|
                { "name": #{wName}
                , "mnemonic_sentence": #{mnemonics15}
                , "passphrase" : #{wPassphrase}
                , "address_pool_gap" : -132323000
                }|]
              , "Error in $['address_pool_gap']: An address pool gap must be a natural number between 10 and 100000."
              )
            , ( [aesonQQ|
                { "name": #{wName}
                , "mnemonic_sentence": #{mnemonics15}
                , "passphrase" : #{wPassphrase}
                , "address_pool_gap" : 9
                }|]
              , "Error in $['address_pool_gap']: An address pool gap must be a natural number between 10 and 100000."
              )
            , ( [aesonQQ|
                { "name": #{wName}
                , "mnemonic_sentence": #{mnemonics15}
                , "passphrase" : #{wPassphrase}
                , "address_pool_gap" : 100001
                }|]
              , "Error in $['address_pool_gap']: An address pool gap must be a natural number between 10 and 100000."
              )
            , ( [aesonQQ|
                { "name": #{wName}
                , "mnemonic_sentence": #{mnemonics15}
                , "passphrase" : #{wPassphrase}
                , "address_pool_gap" : 132323000
                }|]
              , "Error in $['address_pool_gap']: An address pool gap must be a natural number between 10 and 100000."
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
              , "Error in $['account_public_key']: Invalid account public key: expecting a hex-encoded value that is 64 bytes in length."
              )
            , ( [aesonQQ|
                { "name": #{wName}
                , "account_public_key" :#{accountPublicKeyTooLong}
                }|]
              , "Error in $['account_public_key']: Invalid account public key: expecting a hex-encoded value that is 64 bytes in length."
              )
             , ( [aesonQQ|
                 { "name": #{wName}
                 , "account_public_key" :#{accountPublicKeyTooShort}
                 }|]
               , "Error in $['account_public_key']: Invalid account public key: expecting a hex-encoded value that is 64 bytes in length."
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

instance Malformed (BodyParam ByronWalletPutPassphraseData) where
    malformed = jsonValid ++ jsonInvalid
     where
         jsonInvalid = first BodyParam <$>
            [ ("1020344", "Error in $: parsing Cardano.Wallet.Api.Types.ByronWalletPutPassphraseData(ByronWalletPutPassphraseData) failed, expected Object, but encountered Number")
            , ("\"1020344\"", "Error in $: parsing Cardano.Wallet.Api.Types.ByronWalletPutPassphraseData(ByronWalletPutPassphraseData) failed, expected Object, but encountered String")
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
              , "Error in $: parsing Cardano.Wallet.Api.Types.ByronWalletPutPassphraseData(ByronWalletPutPassphraseData) failed, key 'new_passphrase' not found"
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
            [ ( [aesonQQ| { "passphrase": #{nameTooLong} }|]
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

instance Malformed (BodyParam (ApiSelectCoinsData ('Testnet pm))) where
    malformed = jsonValid ++ jsonInvalid
     where
         jsonInvalid = first BodyParam <$>
            [ ("1020344", "Error in $: parsing DelegationAction failed, expected Object, but encountered Number")
            , ("\"1020344\"", "Error in $: parsing DelegationAction failed, expected Object, but encountered String")
            , ("\"slot_number : \"random\"}", "trailing junk after valid JSON: endOfInput")
            , ("{\"payments : [], \"random\"}", msgJsonInvalid)
            , ("join", "I couldn't understand the content of your message. If your message is intended to be in JSON format, please check that the JSON is valid.")
            , ("quit", msgJsonInvalid)
            ]
         jsonValid = (first (BodyParam . Aeson.encode) <$> paymentCases) <> jsonValidAction
         jsonValidAction = first (BodyParam . Aeson.encode) <$>
            [ ( [aesonQQ| { "action": "join" }|]
              , "Error in $: No valid parse for ApiSelectCoinsPayments or ApiSelectCoinsAction"
              )
            , ( [aesonQQ| { "action": "" }|]
              , "Error in $: No valid parse for ApiSelectCoinsPayments or ApiSelectCoinsAction"
              )
            , ( [aesonQQ| { "action": "join", "pool": "" }|]
              , "Error in $: No valid parse for ApiSelectCoinsPayments or ApiSelectCoinsAction"
              )
            , ( [aesonQQ| { "action": "join", "pool": "1" }|]
              , "Error in $: No valid parse for ApiSelectCoinsPayments or ApiSelectCoinsAction"
              )
            , ( [aesonQQ| { "pool": "pool1wqaz0q0zhtxlgn0ewssevn2mrtm30fgh2g7hr7z9rj5856457mm" }|]
              , "Error in $: No valid parse for ApiSelectCoinsPayments or ApiSelectCoinsAction"
              )
            ]

instance Malformed (BodyParam (PostTransactionData ('Testnet pm))) where
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
                        "address": #{addrPlaceholder},
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
                       "address": #{addrPlaceholder},
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

instance Malformed (BodyParam (PostTransactionFeeData ('Testnet pm))) where
    malformed = jsonValid ++ jsonInvalid
     where
         jsonInvalid = first BodyParam <$>
            [ ("1020344", "Error in $: parsing Cardano.Wallet.Api.Types.PostTransactionFeeData(PostTransactionFeeData) failed, expected Object, but encountered Number")
            , ("\"1020344\"", "Error in $: parsing Cardano.Wallet.Api.Types.PostTransactionFeeData(PostTransactionFeeData) failed, expected Object, but encountered String")
            , ("{\"payments : [], \"random\"}", msgJsonInvalid)
            , ("\"slot_number : \"random\"}", "trailing junk after valid JSON: endOfInput")
            ]
         jsonValid = first (BodyParam . Aeson.encode) <$> paymentCases

instance Malformed (BodyParam (ApiWalletMigrationPostData ('Testnet pm) "lenient")) where
    malformed = jsonValid ++ jsonInvalid
     where
         jsonInvalid = first BodyParam <$>
            [ ("1020344", "Error in $: parsing Cardano.Wallet.Api.Types.ApiWalletMigrationPostData(ApiWalletMigrationPostData) failed, expected Object, but encountered Number")
            , ("\"1020344\"", "Error in $: parsing Cardano.Wallet.Api.Types.ApiWalletMigrationPostData(ApiWalletMigrationPostData) failed, expected Object, but encountered String")
            , ("{\"payments : [], \"random\"}", msgJsonInvalid)
            , ("\"slot_number : \"random\"}", "trailing junk after valid JSON: endOfInput")
            ]
         jsonValid = first (BodyParam . Aeson.encode) <$> migrateDataCases

instance Malformed (BodyParam (ApiWalletMigrationPostData ('Testnet pm) "raw")) where
    malformed = jsonValid ++ jsonInvalid
     where
         jsonInvalid = first BodyParam <$>
            [ ("1020344", "Error in $: parsing Cardano.Wallet.Api.Types.ApiWalletMigrationPostData(ApiWalletMigrationPostData) failed, expected Object, but encountered Number")
            , ("\"1020344\"", "Error in $: parsing Cardano.Wallet.Api.Types.ApiWalletMigrationPostData(ApiWalletMigrationPostData) failed, expected Object, but encountered String")
            , ("{\"payments : [], \"random\"}", msgJsonInvalid)
            , ("\"slot_number : \"random\"}", "trailing junk after valid JSON: endOfInput")
            ]
         jsonValid = first (BodyParam . Aeson.encode) <$> migrateDataCases

instance Malformed (BodyParam (ApiPutAddressesData ('Testnet pm))) where
    malformed = jsonValid ++ jsonInvalid
     where
         jsonInvalid = first BodyParam <$>
            [ ("1020344", "Error in $: parsing Cardano.Wallet.Api.Types.ApiPutAddressesData(ApiPutAddressesData) failed, expected Object, but encountered Number")
            , ("\"1020344\"", "Error in $: parsing Cardano.Wallet.Api.Types.ApiPutAddressesData(ApiPutAddressesData) failed, expected Object, but encountered String")
            , ("{\"payments : [], \"random\"}", msgJsonInvalid)
            , ("\"slot_number : \"random\"}", "trailing junk after valid JSON: endOfInput")
            ]
         jsonValid = first (BodyParam . Aeson.encode) <$> putAddressesDataCases

instance Malformed (BodyParam ApiSlotReference) where
    malformed = jsonValid ++ jsonInvalid
     where
         jsonInvalid = first BodyParam <$>
            [ ("1020344", "Error in $: parsing Cardano.Wallet.Api.Types.ApiSlotReference(ApiSlotReference) failed, expected Object, but encountered Number")
            , ("\"1020344\"", "Error in $: parsing Cardano.Wallet.Api.Types.ApiSlotReference(ApiSlotReference) failed, expected Object, but encountered String")
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
              , "Error in $: parsing Cardano.Wallet.Api.Types.ApiSlotReference(ApiSlotReference) failed, key 'epoch_number' not found"
              )
            , ( [aesonQQ|
                { "epoch_number": 0
                }|]
              , "Error in $: parsing Cardano.Wallet.Api.Types.ApiSlotReference(ApiSlotReference) failed, key 'slot_number' not found"
              )
            ]

instance Malformed (BodyParam PostExternalTransactionData)
-- no cases here as all bad requests are served by ErrDecodeSignedTxWrongPayload
-- in Server.hs. Tested by integration tests.

instance Malformed (BodyParam ApiPostRandomAddressData) where
    malformed = first (BodyParam . Aeson.encode) <$>
        [ ( [aesonQQ|
            { "passphrase": "Secure Passphrase"
            , "address_index": "not_a_number"
            }|]
          , "Error in $['address_index']: parsing Int failed, expected Number, but encountered String"
          )
        , ( [aesonQQ|
            { "address_index": 0
            }|]
          , "Error in $: parsing Cardano.Wallet.Api.Types.ApiPostRandomAddressData(ApiPostRandomAddressData) failed, key 'passphrase' not found"
          )
        ]

instance Malformed (BodyParam SettingsPutData) where
    malformed = first (BodyParam . Aeson.encode) <$>
        [ ( [aesonQQ|
            { "settings": {
                          "pool_metadata_source": "not_a_uri"
                          }
            }|]
          , "Error in $.settings['pool_metadata_source']: Not a valid absolute URI."
          )
        ]

--
-- Class instances (Header)
--
instance Wellformed (Header "Content-Type" JSON) where
    wellformed =
        [Header "application/json"]

instance Malformed (Header "Content-Type" JSON) where
    malformed = first Header <$>
        [ ( "plain/text"
          , "I'm really sorry but I only understand 'application/json'. I need you to tell me what language you're speaking in order for me to understand your message. Please double-check your 'Content-Type' request header and make sure it's set to 'application/json'."
          )
        ]

instance Wellformed (Header "Content-Type" OctetStream) where
    wellformed =
        [Header "application/octet-stream"]

instance Malformed (Header "Content-Type" OctetStream) where
    malformed = first Header <$>
        [ ( "plain/text"
          , "I'm really sorry but I only understand 'application/octet-stream'. I need you to tell me what language you're speaking in order for me to understand your message. Please double-check your 'Content-Type' request header and make sure it's set to 'application/octet-stream'."
          )
        ]

instance Wellformed (Header "Accept" JSON) where
    wellformed =
        [Header "application/json"]

instance Malformed (Header "Accept" JSON) where
    malformed = first Header <$>
        [ ( "plain/text"
          , "It seems as though you don't accept 'application/json', but unfortunately I only speak 'application/json'! Please double-check your 'Accept' request header and make sure it's set to 'application/json'."
          )
        ]

instance Wellformed (Header "Accept" OctetStream) where
    wellformed =
        [Header "application/octet-stream"]

instance Malformed (Header "Accept" OctetStream) where
    malformed = first Header <$>
        [ ( "application/json"
          , "It seems as though you don't accept 'application/octet-stream', but unfortunately I only speak 'application/octet-stream'! Please double-check your 'Accept' request header and make sure it's set to 'application/octet-stream'."
          )
        ]

--
-- Test Data
--
msgJsonInvalid :: ExpectedError
msgJsonInvalid = "I couldn't understand the content of your message. \
    \If your message is intended to be in JSON format, please check that \
    \the JSON is valid."

addrPlaceholder :: Text
addrPlaceholder = "<addr>"

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
                "address": #{addrPlaceholder},
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
                "address": #{addrPlaceholder},
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
                "address": #{addrPlaceholder}
            }
           ]
        }|]
      , "Error in $.payments[0]: parsing Cardano.Wallet.Api.Types.AddressAmount(AddressAmount) failed, key 'amount' not found"
      )
    , ( [aesonQQ|
        { "payments": [
            {
                "address": #{addrPlaceholder},
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
                "address": #{addrPlaceholder},
                "amount": 42000000
            }
           ]
        }|]
      , "Error in $.payments[0].amount: parsing Quantity failed, expected Object, but encountered Number"
      )
    , ( [aesonQQ|
        { "payments": [
            {
                "address": #{addrPlaceholder},
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
                "address": #{addrPlaceholder},
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
                "address": #{addrPlaceholder},
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

migrateDataCases :: [(Aeson.Value, ExpectedError)]
migrateDataCases =
    [
      ( [aesonQQ|
        { "passphrase": #{wPassphrase}
        , "addresses": "not_a_array"
        }|]
      , "Error in $.addresses: parsing [] failed, expected Array, but encountered String"
      )
    , ( [aesonQQ|
        { "passphrase": #{wPassphrase}
        , "addresses": 1
        }|]
      , "Error in $.addresses: parsing [] failed, expected Array, but encountered Number"
      )
    , ( [aesonQQ|
        { "passphrase": #{wPassphrase}
        }|]
      , "Error in $: parsing Cardano.Wallet.Api.Types.ApiWalletMigrationPostData(ApiWalletMigrationPostData) failed, key 'addresses' not found"
      )
    , ( [aesonQQ|
        { "addresses": 1
        }|]
      , "Error in $: parsing Cardano.Wallet.Api.Types.ApiWalletMigrationPostData(ApiWalletMigrationPostData) failed, key 'passphrase' not found"
      )
    , ( [aesonQQ|
        { "passphrase": 1
        , "addresses": [ #{addrPlaceholder} ]
        }|]
      , "Error in $.passphrase: parsing Text failed, expected String, but encountered Number"
      )
    , ( [aesonQQ|
        { "passphrase": [ ]
        , "addresses": [ #{addrPlaceholder} ]
        }|]
      , "Error in $.passphrase: parsing Text failed, expected String, but encountered Array"
      )
    ]

putAddressesDataCases :: [(Aeson.Value, ExpectedError)]
putAddressesDataCases =
    [
      ( [aesonQQ|
        { "addresses": "not_a_array"
        }|]
      , "Error in $.addresses: parsing [] failed, expected Array, but encountered String"
      )
    , ( [aesonQQ|
        { "addresses": 1
        }|]
      , "Error in $.addresses: parsing [] failed, expected Array, but encountered Number"
      )
    , ( [aesonQQ|
        {
        }|]
      , "Error in $: parsing Cardano.Wallet.Api.Types.ApiPutAddressesData(ApiPutAddressesData) failed, key 'addresses' not found"
      )
    ]
