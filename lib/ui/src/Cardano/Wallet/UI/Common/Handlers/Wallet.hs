{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
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
import Data.ByteString
    ( ByteString
    )
import Data.FileEmbed
    ( embedFile
    , makeRelativeToProject
    )
import Data.Text
    ( Text
    )
import System.Random.Stateful
    ( randomRIO
    )

import qualified Data.ByteString.Char8 as B8
import qualified Data.Text.Encoding as T

pickMnemonic :: Int -> Maybe Bool -> IO (Maybe [Text])
pickMnemonic _n (Just True) = pure Nothing
pickMnemonic n _ = do
    let wordsList :: ByteString
        wordsList =
            $(makeRelativeToProject "data/english.txt" >>= embedFile)
    let dict = fmap T.decodeUtf8 . B8.words $ wordsList

    let loop = do
            xs <- replicateM n $ do
                i <- randomRIO (0, length dict - 1)
                pure $ dict !! i
            case mkSomeMnemonic @(AllowedMnemonics 'Shelley) xs of
                Left _ -> loop
                Right _ -> pure xs
    Just <$> loop
