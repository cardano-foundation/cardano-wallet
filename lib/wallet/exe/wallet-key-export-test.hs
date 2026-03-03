{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Copyright: © 2024 Cardano Foundation
-- License: Apache-2.0
--
-- E2E round-trip test for the wallet-key-export executable.
-- Generates a random Shelley root key, stores it in a temp SQLite DB,
-- runs wallet-key-export, and verifies the extracted hex matches
-- the expected decrypted XPrv.
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

import qualified Data.ByteArray as BA
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
    result <-
        quickCheckResult $ withMaxSuccess 20 $ prop_shelleyRoundtrip exe
    if isSuccess result
        then putStrLn "All tests passed."
        else exitFailure

-- | Generate a user passphrase of 10–50 lowercase ASCII chars.
genUserPassphrase :: Gen T.Text
genUserPassphrase = do
    n <- choose (10 :: Int, 50)
    chars <- vectorOf n (choose ('a', 'z'))
    pure $ T.pack chars

-- | Property: storing a Shelley root key in a SQLite DB and running
-- wallet-key-export recovers the same decrypted XPrv.
prop_shelleyRoundtrip :: FilePath -> Property
prop_shelleyRoundtrip exe =
    forAll
        ( (,,)
            <$> (SomeMnemonic <$> genMnemonic @12)
            <*> genUserPassphrase
            <*> genWalletId
        )
        $ \(mnemonic, passText, wid) ->
            ioProperty $ shelleyRoundtrip exe mnemonic passText wid

shelleyRoundtrip
    :: FilePath -> SomeMnemonic -> T.Text -> WalletId -> IO Property
shelleyRoundtrip exe mnemonic passText wid = do
    let userPass =
            Passphrase (BA.convert (T.encodeUtf8 passText))
                :: Passphrase "user"
        encPass = preparePassphrase EncryptWithPBKDF2 userPass
        key = generateKeyFromSeed (mnemonic, Nothing) encPass
    (_, passHash) <- encryptPassphrase userPass
    let (rootHex, hashHex) = serializeXPrv ShelleyKeyS (key, passHash)

    withSystemTempFile "wallet-export-test.db" $ \dbPath handle -> do
        hClose handle
        -- Create schema and insert test data
        dbResult <-
            withSqliteContextFile
                nullTracer
                dbPath
                noManualMigration
                migrateAll
                $ \db ->
                    runQuery db $ do
                        insertWallet wid
                        insert_ $ PrivateKey wid rootHex hashHex
        case dbResult of
            Left err ->
                pure
                    $ counterexample ("DB migration error: " <> show err)
                    $ False === True
            Right () -> do
                let widHex = T.unpack (toText wid)
                (exitCode, stdout_, _stderr) <-
                    readCreateProcessWithExitCode
                        (proc exe [dbPath, widHex])
                        (T.unpack passText ++ "\n")

                let extractedHex = extractHexFromOutput stdout_
                    xprv = getRawKey ShelleyKeyS key
                    emptyPass =
                        mempty :: Passphrase "encryption"
                    expected =
                        B16.encode
                            (unXPrv (xPrvChangePass encPass emptyPass xprv))

                pure
                    $ counterexample ("stdout: " <> stdout_)
                    $ counterexample ("exit: " <> show exitCode)
                    $ (exitCode === ExitSuccess)
                    .&&. (extractedHex === expected)

-- | Extract the hex line from wallet-key-export output.
-- Looks for the line after "=== Raw XPrv (hex) ===".
extractHexFromOutput :: String -> B8.ByteString
extractHexFromOutput output =
    case dropWhile (/= "=== Raw XPrv (hex) ===") (lines output) of
        (_ : hexLine : _) -> B8.pack hexLine
        _ -> error $ "Could not parse hex from output:\n" <> output

-- | Insert a wallet row to satisfy foreign key constraints.
insertWallet :: WalletId -> SqlPersistT IO ()
insertWallet wid =
    insert_
        $ Wallet
            { walId = wid
            , walName = "test-wallet"
            , walCreationTime = posixSecondsToUTCTime 1506203091
            , walPassphraseLastUpdatedAt = Nothing
            , walPassphraseScheme = Nothing
            , walGenesisHash = BlockId dummyHash
            , walGenesisStart = posixSecondsToUTCTime 1506203091
            }
  where
    dummyHash =
        Hash
            $ unsafeFromHex
                "5f20df933584822601f9e3f8c024eb5eb252fe8cefb24d1317dc3d432e940ebb"
