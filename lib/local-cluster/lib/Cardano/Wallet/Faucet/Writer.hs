{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | This module contains functionality to generate faucet addresses.
     It is not used at the moment by the local cluster, but it might be useful
     for troubleshooting to introspect addresses derived from mnemonics.
-}
module Cardano.Wallet.Faucet.Writer
    ( genByronFaucets
    , genIcarusFaucets
    , genShelleyFaucets
    , genMaryAllegraFaucets
    ) where

import Prelude

import Cardano.Address
    ( Address
    , base58
    , unAddress
    )
import Cardano.Mnemonic
    ( Mnemonic
    , SomeMnemonic (..)
    , mnemonicToText
    )
import Control.Monad
    ( forM
    , forM_
    )
import Data.ByteArray.Encoding
    ( Base (Base16)
    , convertToBase
    )
import Data.Text
    ( Text
    )

import qualified Cardano.Address as CA
import qualified Cardano.Wallet.Faucet.Addresses as Addresses
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as TIO

-- | Generate faucets addresses and mnemonics to a file.
--
-- >>> genMnemonics 100 >>= genByronFaucets "byron-faucets.yaml"
genByronFaucets :: CA.NetworkTag -> FilePath -> [Mnemonic 12] -> IO [[Text]]
genByronFaucets = genFaucet base58 . Addresses.byron

-- | Generate faucets addresses and mnemonics to a file.
--
-- >>> genMnemonics 100 >>= genIcarusFaucets (CA.NetworkTag 42) "icarus-faucets.yaml"
genIcarusFaucets :: CA.NetworkTag -> FilePath -> [Mnemonic 15] -> IO [[Text]]
genIcarusFaucets = genFaucet base58 . Addresses.icarus

-- | Generate faucet addresses and mnemonics to a file.
--
-- >>> genMnemonics 100 >>= genShelleyFaucets "shelley-faucets.yaml"
genShelleyFaucets :: FilePath -> [Mnemonic 15] -> IO [[Text]]
genShelleyFaucets =
    genFaucet encodeAddressHex (Addresses.shelley . SomeMnemonic)

-- | Generate faucet addresses and mnemonics to a file.
--
-- >>> genMnemonics 100 >>= genMaryAllegraFaucets "ma-faucets.yaml"
genMaryAllegraFaucets :: FilePath -> [Mnemonic 24] -> IO [[Text]]
genMaryAllegraFaucets =
    genFaucet encodeAddressHex (Addresses.shelley . SomeMnemonic)

-- | Abstract function for generating a faucet as a YAML file.
--
-- Returns the generated mnemonics as Text.
genFaucet
    :: forall a mw
     . (a -> Text)
    -> (Mnemonic mw -> [a])
    -> FilePath
    -> [Mnemonic mw]
    -> IO [[Text]]
genFaucet encodeAddress genAddresses file mnemonics = do
    TIO.writeFile file ""
    forM [(mnemonicToText m, take 10 (genAddresses m)) | m <- mnemonics]
        $ \(mnem, addrs) -> do
            let comment = ("# " <>)
                    $ T.intercalate ", "
                    $ map (surroundedBy '"') mnem
            appendToFile file comment
            forM_ addrs (appendToFile file . encodeFaucet)
            pure mnem
  where
    surroundedBy :: Char -> Text -> Text
    surroundedBy c txt = T.singleton c <> txt <> T.singleton c

    encodeFaucet :: a -> Text
    encodeFaucet addr =
        "  " <> encodeAddress addr <> ": " <> T.pack (show faucetAmount)
      where
        faucetAmount :: Int = ada 100_000 where ada = (* 1000_000)

    appendToFile :: FilePath -> Text -> IO ()
    appendToFile f txt = TIO.appendFile f (txt <> "\n")

--------------------------------------------------------------------------------
-- Utility functions -----------------------------------------------------------

encodeAddressHex :: Address -> Text
encodeAddressHex = T.decodeUtf8 . convertToBase Base16 . unAddress
