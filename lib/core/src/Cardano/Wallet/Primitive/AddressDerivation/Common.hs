{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Copyright: © 2018-2019 IOHK
-- License: MIT
--
-- Common functions for the address derivation implementations.
--
-- Use the types from "Cardano.Wallet.Primitive.AddressDerivation" or the key
-- generation or derivation functions from
-- "Cardano.Wallet.Primitive.AddressDerivation.Sequential" or
-- "Cardano.Wallet.Primitive.AddressDerivation.Random" instead of this module.
--

module Cardano.Wallet.Primitive.AddressDerivation.Common
    ( fromHexText
    , toHexText
    ) where

import Prelude

import Data.ByteArray.Encoding
    ( Base (..), convertFromBase, convertToBase )
import Data.ByteString
    ( ByteString )

fromHexText :: ByteString -> Either String ByteString
fromHexText = convertFromBase Base16

toHexText :: ByteString -> ByteString
toHexText = convertToBase Base16
