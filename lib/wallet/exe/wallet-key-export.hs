{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Copyright: © 2024 Cardano Foundation
-- License: Apache-2.0
--
-- Extract a wallet root private key from a cardano-wallet SQLite database.
-- Auto-detects Byron vs Shelley key format, verifies the spending passphrase,
-- decrypts the XPrv, and outputs both raw hex and cardano-cli text envelope.
module Main (main) where

import Cardano.Crypto.Wallet
    ( unXPrv
    , xPrvChangePass
    )
import Cardano.Wallet.Address.Keys.PersistPrivateKey
    ( unsafeDeserializeXPrv
    )
import Cardano.Wallet.Address.Keys.WalletKey
    ( getRawKey
    )
import Cardano.Wallet.Flavor
    ( KeyFlavorS (..)
    )
import Cardano.Wallet.Primitive.Passphrase
    ( checkPassphrase
    , preparePassphrase
    )
import Cardano.Wallet.Primitive.Passphrase.Types
    ( ErrWrongPassphrase (..)
    , Passphrase (..)
    , PassphraseScheme (..)
    )
import Cardano.Wallet.Primitive.Types.Credentials
    ( HashedCredentials (..)
    )
import Control.Monad
    ( when
    )
import Control.Monad.Logger
    ( runNoLoggingT
    )
import Control.Monad.Reader
    ( runReaderT
    )
import Control.Monad.Trans.Resource
    ( runResourceT
    )
import Data.ByteString
    ( ByteString
    )
import Database.Persist.Sql
    ( PersistValue (..)
    , Single (..)
    , rawSql
    )
import Database.Persist.Sqlite
    ( withSqliteConn
    )
import System.Environment
    ( getArgs
    )
import System.Exit
    ( exitFailure
    )
import System.IO
    ( hFlush
    , hGetEcho
    , hIsTerminalDevice
    , hSetEcho
    , stdin
    , stdout
    )
import Prelude

import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as TIO

main :: IO ()
main = do
    args <- getArgs
    case args of
        [dbPath, walletId] -> run dbPath (T.pack walletId)
        _ -> do
            putStrLn "Usage: wallet-key-export <path-to-wallet.db> <wallet-id>"
            exitFailure

run :: FilePath -> T.Text -> IO ()
run dbPath walletId = do
    -- Read root key and hash from the SQLite database
    (rootHex, hashHex) <- readPrivateKey dbPath walletId

    -- Auto-detect key type from the serialized key column.
    let isV2 = "V2:" `BS.isPrefixOf` rootHex
        isByron = not isV2 && isByronV1RootKey rootHex

    putStrLn
        $ "Key type: "
            <> if isByron then "Byron" else "Shelley"

    when isV2 $ do
        putStrLn "Error: V2 (Argon2id) keys cannot be exported in XPrv format"
        exitFailure

    -- Prompt for passphrase
    userPass <- promptPassphrase

    -- Deserialize, verify passphrase, and decrypt
    xprv <-
        if isByron
            then case unsafeDeserializeXPrv ByronKeyS (rootHex, hashHex) of
                HashedCredentialsV1 key passHash -> do
                    case checkPassphrase EncryptWithScrypt userPass passHash of
                        Left ErrWrongPassphrase -> do
                            putStrLn "Error: wrong passphrase"
                            exitFailure
                        Right () -> pure ()
                    let prepared = preparePassphrase EncryptWithScrypt userPass
                        emptyPass = mempty :: Passphrase "encryption"
                    pure $ xPrvChangePass prepared emptyPass (getRawKey ByronKeyS key)
                HashedCredentialsV2{} -> do
                    putStrLn "Error: V2 (Argon2id) keys cannot be exported in XPrv format"
                    exitFailure
            else case unsafeDeserializeXPrv ShelleyKeyS (rootHex, hashHex) of
                HashedCredentialsV1 key passHash -> do
                    case checkPassphrase EncryptWithPBKDF2 userPass passHash of
                        Left ErrWrongPassphrase -> do
                            putStrLn "Error: wrong passphrase"
                            exitFailure
                        Right () -> pure ()
                    let prepared = preparePassphrase EncryptWithPBKDF2 userPass
                        emptyPass = mempty :: Passphrase "encryption"
                    pure $ xPrvChangePass prepared emptyPass (getRawKey ShelleyKeyS key)
                HashedCredentialsV2{} -> do
                    putStrLn "Error: V2 (Argon2id) keys cannot be exported in XPrv format"
                    exitFailure

    -- Output raw hex (128 bytes = 256 hex chars)
    let hexKey = B16.encode (unXPrv xprv)
    putStrLn ""
    putStrLn "=== Raw XPrv (hex) ==="
    B8.putStrLn hexKey

    -- Output cardano-cli text envelope JSON
    let (envType, cborPrefix) =
            if isByron
                then
                    ( "PaymentSigningKeyByron_ed25519_bip32" :: String
                    , "5880"
                    )
                else
                    ( "PaymentExtendedSigningKeyShelley_ed25519_bip32"
                    , "5880"
                    )
        cborHex = B8.pack cborPrefix <> hexKey
    putStrLn ""
    putStrLn "=== Text Envelope ==="
    putStrLn "{"
    putStrLn $ "    \"type\": \"" <> envType <> "\","
    putStrLn "    \"description\": \"Payment Signing Key\","
    putStrLn $ "    \"cborHex\": \"" <> B8.unpack cborHex <> "\""
    putStrLn "}"

readPrivateKey :: FilePath -> T.Text -> IO (ByteString, ByteString)
readPrivateKey dbPath walletId = do
    result <-
        runResourceT
            . runNoLoggingT
            $ withSqliteConn (T.pack dbPath)
            $ runReaderT
            $ rawSql
                "SELECT root, hash FROM private_key WHERE wallet_id = ?"
                [PersistText walletId]
    case result of
        [(Single root, Single hash)] ->
            pure (T.encodeUtf8 root, T.encodeUtf8 hash)
        [] -> do
            putStrLn
                $ "Error: no private key found for wallet "
                    <> T.unpack walletId
            exitFailure
        _ -> do
            putStrLn "Error: unexpected number of rows in private_key table"
            exitFailure

isByronV1RootKey :: ByteString -> Bool
isByronV1RootKey rootHex =
    case B8.split ':' rootHex of
        [keyHex, _payloadHex] -> BS.length keyHex == 256
        _ -> False

promptPassphrase :: IO (Passphrase "user")
promptPassphrase = do
    isTerm <- hIsTerminalDevice stdin
    when isTerm $ do
        putStr "Enter spending passphrase: "
        hFlush stdout
    oldEcho <-
        if isTerm
            then Just <$> (hGetEcho stdin <* hSetEcho stdin False)
            else pure Nothing
    input <- TIO.getLine
    case oldEcho of
        Just e -> hSetEcho stdin e >> putStrLn ""
        Nothing -> pure ()
    when (T.null input) $ do
        putStrLn "Error: empty passphrase"
        exitFailure
    pure $ Passphrase $ BA.convert $ T.encodeUtf8 input
