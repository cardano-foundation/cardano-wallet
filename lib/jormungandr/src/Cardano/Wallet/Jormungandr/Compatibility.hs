{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Copyright: © 2018-2019 IOHK
-- License: Apache-2.0
--
-- Contains various implementation decision that are specific to a particular
-- network / protocol. This allows us to easily select a particular backend
-- (Byron, Shelley-Rust, Shelley-Haskell) and isolate the bits that vary between
-- those backends.

module Cardano.Wallet.Jormungandr.Compatibility
    ( -- * Target
      Jormungandr
    , softTxMaxSize

      -- * Node's Configuration
    , BaseUrl (..)
    , Scheme (..)
    , localhostBaseUrl
    , baseUrlToText
    ) where

import Prelude

import Cardano.Wallet.DB.Sqlite
    ( PersistTx (..) )
import Cardano.Wallet.Jormungandr.Primitive.Types
    ( Tx (..) )
import Cardano.Wallet.Primitive.Types
    ( DefineTx, invariant )
import Control.Arrow
    ( second )
import Data.Maybe
    ( fromJust, isJust )
import Data.Quantity
    ( Quantity (..) )
import Data.Word
    ( Word16 )
import Servant.Client.Core
    ( BaseUrl (..), Scheme (..), showBaseUrl )

import qualified Cardano.Wallet.Primitive.Types as W
import qualified Data.Text as T

-- | A type representing the Jormungandr as a backend target. This has an
-- influence on binary serializer & network primitives. See also 'DefineTx'
data Jormungandr

-- | Jörmugandr's chain parameter doesn't include a transaction max size. The
-- actual hard-limit for the size is constrained by the binary format and
-- numbers used to represent the number of inputs and outputs (Word8), yet
-- there's also a soft-limit of 8kb which results in much smaller transactions
-- in the end.
softTxMaxSize :: Quantity "byte" Word16
softTxMaxSize = Quantity 8192

instance DefineTx Jormungandr where
    type Tx Jormungandr = Tx
    inputs = fmap fst . inputs
    outputs = outputs
    -- The corresponding rust implementation is:
    -- https://github.com/input-output-hk/rust-cardano/blob/e5d974f7bedeb00c9c9d688ac66094a34bf8f40d/chain-impl-mockchain/src/transaction/transaction.rs#L115-L119
    txId = txid

instance PersistTx Jormungandr where
    resolvedInputs = map (second Just) . inputs
    mkTx tid inps = Tx tid ((second unsafeFromMaybe) <$> inps)
      where
        unsafeFromMaybe amt = fromJust $ invariant
            ("PersistTx Jormungandr: invariant violation, tried to \
            \reconstruct a 'Tx' from the database that has resolved inputs \
            \without any amount: " <> show inps)
            amt
            isJust

{-------------------------------------------------------------------------------
                                     Base URL
-------------------------------------------------------------------------------}

localhostBaseUrl :: Int -> BaseUrl
localhostBaseUrl port = BaseUrl Http "127.0.0.1" port ""

-- | Format an API 'BaseUrl', for logging, etc.
baseUrlToText :: BaseUrl -> T.Text
baseUrlToText = T.pack . showBaseUrl
