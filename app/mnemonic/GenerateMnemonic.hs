{-# LANGUAGE DataKinds #-}

-- |
-- Copyright: © 2018-2019 IOHK
-- License: MIT
--
-- | Mnemonic generation executable


import Prelude

import Cardano.Wallet.Mnemonic
    ( Mnemonic, entropyToMnemonic, genEntropy, mnemonicToText )
import Data.Function
    ( flip )
import Data.Text
    ( Text )

import qualified Data.Text as T
import qualified Data.Text.IO as T

main
    :: IO ()
main = do
    backupPhrase <- generateBackupPhrase
    let backupPhraseString = backupPhraseToString backupPhrase
    T.putStrLn $ formatOutput backupPhraseString

generateBackupPhrase
    :: IO (Mnemonic 15)
generateBackupPhrase =
    entropyToMnemonic <$> genEntropy

backupPhraseToString
    :: Mnemonic 15
    -> [Text]
backupPhraseToString = mnemonicToText

formatOutput
    :: [Text]
    -> Text
formatOutput =
      flip T.snoc ']'
    . T.append "["
    . T.intercalate ","
    . map (T.append "\"" . flip T.snoc '"' )
