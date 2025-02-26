{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.UI.Common.Handlers.Wallet
    ( pickMnemonic
    )
where

import Prelude hiding
    ( lookup
    )

import Cardano.Mnemonic
    ( MkSomeMnemonic (mkSomeMnemonic)
    )
import Cardano.Wallet.Api.Types
    ( AllowedMnemonics
    , WalletStyle (..)
    )
import Control.Monad
    ( replicateM
    )
import Data.Text
    ( Text
    )
import System.Random.Stateful
    ( randomRIO
    )

import Cardano.Wallet.UI.Static
    ( englishWords
    )

pickMnemonic :: Int -> Maybe Bool -> IO (Maybe [Text])
pickMnemonic _n (Just True) = pure Nothing
pickMnemonic n _ = do
    let loop = do
            xs <- replicateM n $ do
                i <- randomRIO (0, length englishWords - 1)
                pure $ englishWords !! i
            case mkSomeMnemonic @(AllowedMnemonics 'Shelley) xs of
                Left _ -> loop
                Right _ -> pure xs
    Just <$> loop
