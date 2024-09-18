{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.UI.Common.Handlers.Wallet where

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
import Paths_cardano_wallet_ui
    ( getDataFileName
    )
import System.Random.Stateful
    ( randomRIO
    )

import qualified Data.Text as T

pickMnemonic :: Int -> Maybe Bool -> IO (Maybe [Text])
pickMnemonic _n (Just True) = pure Nothing
pickMnemonic n _ = do
    wordsFile <- getDataFileName "data/english.txt"
    dict <- fmap T.pack . words <$> readFile wordsFile

    let loop = do
            xs <- replicateM n $ do
                i <- randomRIO (0, length dict - 1)
                pure $ dict !! i
            case mkSomeMnemonic @(AllowedMnemonics 'Shelley) xs of
                Left _ -> loop
                Right _ -> pure xs
    Just <$> loop
