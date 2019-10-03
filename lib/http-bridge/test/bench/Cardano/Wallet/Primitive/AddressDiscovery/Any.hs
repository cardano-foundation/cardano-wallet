{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Copyright: © 2018-2019 IOHK
-- License: Apache-2.0
--
-- Custom address discovery schemes used for testing and benchmarking.
--

module Cardano.Wallet.Primitive.AddressDiscovery.Any
    ( AnyAddressState (..)
    , initAnyState
    ) where

import Prelude

import Cardano.Wallet.DB.Sqlite
    ( PersistState (..) )
import Cardano.Wallet.Primitive.AddressDiscovery
    ( CompareDiscovery (..)
    , GenChange (..)
    , IsOurs (..)
    , IsOwned (..)
    , KnownAddresses (..)
    )
import Cardano.Wallet.Primitive.Types
    ( Address (..), WalletId (..), WalletName (..) )
import Control.DeepSeq
    ( NFData )
import Control.Monad.Trans.Maybe
    ( MaybeT (..) )
import Crypto.Hash
    ( hash )
import Data.Digest.CRC32
    ( crc32 )
import Data.Text
    ( Text )
import Data.Word
    ( Word32 )
import Database.Persist.Sql
    ( entityVal, insert_, selectFirst, (==.) )
import GHC.Generics
    ( Generic )

import qualified Cardano.Wallet.Primitive.AddressDiscovery.Any.TH as DB
import qualified Data.ByteString.Char8 as B8

----------------------------------------------------------------------------

-- | Any Address Derivation
--
-- An arbitrary fraction of addreses are recognized as "ours". This is done by
-- looking at a checksum of the address.
newtype AnyAddressState = AnyAddressState
    { oursProportion :: Double
    }
    deriving stock (Generic, Show)

instance NFData AnyAddressState

instance IsOurs AnyAddressState where
    isOurs (Address addr) s@(AnyAddressState p) = (crc32 addr < p', s)
        where
          p' = floor (fromIntegral (maxBound :: Word32) * p)

instance IsOwned AnyAddressState key where
    isOwned _ _ _ = Nothing

instance GenChange AnyAddressState where
    genChange _ = error
        "GenChange.genChange: trying to generate change for \
        \an incompatible scheme 'AnyAddressState'. Please don't."

instance CompareDiscovery AnyAddressState where
    compareDiscovery _ _ _ = error
        "CompareDiscovery.compareDiscovery: trying to generate change for \
        \an incompatible scheme 'AnyAddressState'. Please don't."

instance KnownAddresses AnyAddressState where
    knownAddresses _ = error
        "KnownAddresses.knownAddresses: trying to generate change for \
        \an incompatible scheme 'AnyAddressState'. Please don't."

instance PersistState AnyAddressState where
    insertState (wid, sl) (AnyAddressState s) =
        insert_ (DB.AnyAddressState wid sl s)
    selectState (wid, sl) = runMaybeT $ do
        DB.AnyAddressState _ _ s <- MaybeT $ fmap entityVal <$> selectFirst
            [ DB.AnyAddressStateWalletId ==. wid
            , DB.AnyAddressStateCheckpointSlot ==. sl
            ] []
        return (AnyAddressState s)

initAnyState :: Text -> Double -> (WalletId, WalletName, AnyAddressState)
initAnyState wname p = (walletId cfg, WalletName wname, cfg)
  where cfg = AnyAddressState p

walletId :: Show a => a -> WalletId
walletId = WalletId . hash . B8.pack . show
