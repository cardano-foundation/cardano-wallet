{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.UI.Common.Handlers.Wallet
    ( pickMnemonic
    )
where

import Cardano.Mnemonic
    ( entropyToMnemonic
    , genEntropy
    , mnemonicToText
    )
import Data.Text
    ( Text
    )
import Prelude

-- | Suggest a fresh recovery phrase, drawing entropy from the system
-- cryptographically-secure RNG via 'genEntropy', consistent with how the
-- wallet API and CLI generate recovery phrases.
pickMnemonic :: Int -> Maybe Bool -> IO (Maybe [Text])
pickMnemonic _ (Just True) = pure Nothing
pickMnemonic n _ = case n of
    15 -> Just . mnemonicToText @15 . entropyToMnemonic <$> genEntropy
    18 -> Just . mnemonicToText @18 . entropyToMnemonic <$> genEntropy
    21 -> Just . mnemonicToText @21 . entropyToMnemonic <$> genEntropy
    24 -> Just . mnemonicToText @24 . entropyToMnemonic <$> genEntropy
    _ -> pure Nothing
