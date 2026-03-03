{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Copyright: © 2026 Cardano Foundation
-- License: Apache-2.0
--
-- E2E round-trip test for the wallet-key-export executable.
-- Generates random Shelley and Byron root keys, stores them in a temp
-- SQLite DB, runs wallet-key-export, and verifies the extracted hex
-- matches the expected decrypted XPrv.
module Main (main) where

import Cardano.Crypto.Wallet
    ( unXPrv
    , xPrvChangePass
    )
import Cardano.DB.Sqlite
    ( runQuery
    , withSqliteContextFile
    )
import Cardano.DB.Sqlite.Migration.Old
    ( noManualMigration
    )
import Cardano.Mnemonic
    ( SomeMnemonic (..)
    )
import Cardano.Wallet.Address.Derivation.Shelley
    ( generateKeyFromSeed
    )
import Cardano.Wallet.Address.Keys.PersistPrivateKey
    ( serializeXPrv
    )
import Cardano.Wallet.Address.Keys.WalletKey
    ( getRawKey
    )
import Cardano.Wallet.DB.Sqlite.Schema
    ( PrivateKey (..)
    , Wallet (..)
    , migrateAll
    )
import Cardano.Wallet.DB.Sqlite.Types
    ( BlockId (..)
    )
import Cardano.Wallet.Flavor
    ( KeyFlavorS (..)
    )
import Cardano.Wallet.Gen
    ( genMnemonic
    , genWalletId
    )
import Cardano.Wallet.Primitive.Passphrase
    ( encryptPassphrase
    , preparePassphrase
    )
import Cardano.Wallet.Primitive.Passphrase.Legacy
    ( encryptPassphraseTestingOnly
    )
import Cardano.Wallet.Primitive.Passphrase.Types
    ( Passphrase (..)
    , PassphraseScheme (..)
    )
import Cardano.Wallet.Primitive.Types
    ( WalletId
    )
import Cardano.Wallet.Primitive.Types.Hash
    ( Hash (..)
    )
import Cardano.Wallet.Unsafe
    ( unsafeFromHex
    )
import Control.Tracer
    ( nullTracer
    )
import Data.Text.Class
    ( toText
    )
import Data.Time.Clock.POSIX
    ( posixSecondsToUTCTime
    )
import Database.Persist
    ( insert_
    )
import Database.Persist.Sql
    ( SqlPersistT
    )
import System.Directory
    ( findExecutable
    )
import System.Exit
    ( ExitCode (..)
    , exitFailure
    )
import System.IO
    ( hClose
    )
import System.IO.Temp
    ( withSystemTempFile
    )
import System.Process
    ( proc
    , readCreateProcessWithExitCode
    )
import Test.QuickCheck
    ( Gen
    , Property
    , choose
    , counterexample
    , forAll
    , ioProperty
    , oneof
    , vectorOf
    , withMaxSuccess
    , (.&&.)
    , (===)
    )
import Test.QuickCheck.Test
    ( isSuccess
    , quickCheckResult
    )
import Prelude

import qualified Cardano.Wallet.Address.Derivation.Byron as Byron
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

main :: IO ()
main = do
    exe <-
        findExecutable "wallet-key-export" >>= \case
            Nothing -> do
                putStrLn "wallet-key-export not found in PATH"
                exitFailure
            Just path -> pure path
    results <-
        mapM
            quickCheckResult
            [ withMaxSuccess 20 $ prop_shelleyRoundtrip exe
            , withMaxSuccess 20 $ prop_byronRoundtrip exe
            ]
    if all isSuccess results
        then putStrLn "All tests passed."
        else exitFailure

-- | Generate a user passphrase of 10–50 lowercase ASCII chars.
genUserPassphrase :: Gen T.Text
genUserPassphrase = do
    n <- choose (10 :: Int, 50)
    chars <- vectorOf n (choose ('a', 'z'))
    pure $ T.pack chars

-- | Generate a Shelley mnemonic (15 or 24 words).
genShelleyMnemonic :: Gen SomeMnemonic
genShelleyMnemonic =
    oneof
        [ SomeMnemonic <$> genMnemonic @15
        , SomeMnemonic <$> genMnemonic @24
        ]

-- | Generate an optional second-factor mnemonic for Shelley.
genSecondFactor :: Gen (Maybe SomeMnemonic)
genSecondFactor =
    oneof
        [ pure Nothing
        , Just . SomeMnemonic <$> genMnemonic @9
        , Just . SomeMnemonic <$> genMnemonic @12
        ]

-- | Generate a Byron mnemonic (12 or 15 words).
genByronMnemonic :: Gen SomeMnemonic
genByronMnemonic =
    oneof
        [ SomeMnemonic <$> genMnemonic @12
        , SomeMnemonic <$> genMnemonic @15
        ]

-- | Shelley: PBKDF2 passphrase scheme with optional second factor.
prop_shelleyRoundtrip :: FilePath -> Property
prop_shelleyRoundtrip exe =
    forAll
        ( (,,,)
            <$> genShelleyMnemonic
            <*> genSecondFactor
            <*> genUserPassphrase
            <*> genWalletId
        )
        $ \(mnemonic, secondFactor, passphrase, wid) ->
            ioProperty $ do
                let userPass = mkUserPass passphrase
                    encPass =
                        preparePassphrase EncryptWithPBKDF2 userPass
                    key =
                        generateKeyFromSeed
                            (mnemonic, secondFactor)
                            encPass
                (_, passHash) <- encryptPassphrase userPass
                let (rootHex, hashHex) =
                        serializeXPrv ShelleyKeyS (key, passHash)
                    xprv = getRawKey ShelleyKeyS key
                    expected =
                        B16.encode
                            ( unXPrv
                                ( xPrvChangePass
                                    encPass
                                    emptyPass
                                    xprv
                                )
                            )
                runExportTest
                    exe
                    wid
                    rootHex
                    hashHex
                    passphrase
                    expected

-- | Byron: Scrypt passphrase scheme with varied mnemonic sizes.
prop_byronRoundtrip :: FilePath -> Property
prop_byronRoundtrip exe =
    forAll
        ( (,,)
            <$> genByronMnemonic
            <*> genUserPassphrase
            <*> genWalletId
        )
        $ \(mnemonic, passphrase, wid) ->
            ioProperty $ do
                let userPass = mkUserPass passphrase
                    encPass =
                        preparePassphrase EncryptWithScrypt userPass
                    key = Byron.generateKeyFromSeed mnemonic encPass
                passHash <- encryptPassphraseTestingOnly 64 encPass
                let (rootHex, hashHex) =
                        serializeXPrv ByronKeyS (key, passHash)
                    xprv = getRawKey ByronKeyS key
                    expected =
                        B16.encode
                            ( unXPrv
                                ( xPrvChangePass
                                    encPass
                                    emptyPass
                                    xprv
                                )
                            )
                runExportTest
                    exe
                    wid
                    rootHex
                    hashHex
                    passphrase
                    expected

mkUserPass :: T.Text -> Passphrase "user"
mkUserPass t = Passphrase (BA.convert (T.encodeUtf8 t))

emptyPass :: Passphrase "encryption"
emptyPass = mempty

-- | Shared test logic: write key to temp DB, run the tool,
-- compare output.
runExportTest
    :: FilePath
    -> WalletId
    -> BS.ByteString
    -> BS.ByteString
    -> T.Text
    -> BS.ByteString
    -> IO Property
runExportTest exe wid rootHex hashHex passphrase expected =
    withSystemTempFile "wallet-export-test.db"
        $ \dbPath handle -> do
            hClose handle
            dbResult <-
                withSqliteContextFile
                    nullTracer
                    dbPath
                    noManualMigration
                    migrateAll
                    $ \db ->
                        runQuery db $ do
                            insertWallet wid
                            insert_
                                $ PrivateKey wid rootHex hashHex
            case dbResult of
                Left err ->
                    pure
                        $ counterexample
                            ("DB migration error: " <> show err)
                        $ False === True
                Right () -> do
                    let widHex = T.unpack (toText wid)
                    (exitCode, stdout_, _stderr) <-
                        readCreateProcessWithExitCode
                            (proc exe [dbPath, widHex])
                            (T.unpack passphrase ++ "\n")

                    let extractedHex =
                            extractHexFromOutput stdout_

                    pure
                        $ counterexample
                            ("stdout: " <> stdout_)
                        $ counterexample
                            ("exit: " <> show exitCode)
                        $ (exitCode === ExitSuccess)
                        .&&. (extractedHex === expected)

-- | Extract the hex line from wallet-key-export output.
-- Looks for the line after "=== Raw XPrv (hex) ===".
extractHexFromOutput :: String -> B8.ByteString
extractHexFromOutput output =
    case dropWhile
        (/= "=== Raw XPrv (hex) ===")
        (lines output) of
        (_ : hexLine : _) -> B8.pack hexLine
        _ ->
            error
                $ "Could not parse hex from output:\n"
                    <> output

-- | Insert a wallet row to satisfy foreign key constraints.
insertWallet :: WalletId -> SqlPersistT IO ()
insertWallet wid =
    insert_
        $ Wallet
            { walId = wid
            , walName = "test-wallet"
            , walCreationTime =
                posixSecondsToUTCTime 1506203091
            , walPassphraseLastUpdatedAt = Nothing
            , walPassphraseScheme = Nothing
            , walGenesisHash = BlockId dummyHash
            , walGenesisStart =
                posixSecondsToUTCTime 1506203091
            }
  where
    dummyHash =
        Hash
            $ unsafeFromHex
                "5f20df933584822601f9e3f8c024eb5eb252fe8cefb24d1317dc3d432e940ebb"
