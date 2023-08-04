{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Cardano.Wallet.Primitive.Types.Tx.TxIn.Gen
  ( coarbitraryTxIn
  , genTxHash
  , genTxIndex
  , genTxIn
  , genTxInFunction
  , genTxInLargeRange
  , shrinkTxHash
  , shrinkTxIndex
  , shrinkTxIn
  )
where

import Cardano.Wallet.Primitive.Types.Hash
  ( Hash (..)
  )
import Cardano.Wallet.Primitive.Types.Tx.TxIn
  ( TxIn (..)
  )
import Control.Monad
  ( replicateM
  )
import Data.ByteString.Char8 qualified as B8
import Data.Either
  ( fromRight
  )
import Data.Text qualified as T
import Data.Text.Class
  ( FromText (..)
  )
import Data.Word
  ( Word16
  , Word32
  )
import Test.QuickCheck
  ( Gen
  , arbitrary
  , coarbitrary
  , elements
  , sized
  )
import Test.QuickCheck.Extra
  ( genFunction
  , genSized2With
  , shrinkInterleaved
  )
import Prelude

--------------------------------------------------------------------------------
-- Transaction hashes generated according to the size parameter
--------------------------------------------------------------------------------

genTxHash :: Gen (Hash "Tx")
genTxHash = sized $ \size -> elements $ take (max 1 size) txHashes

shrinkTxHash :: Hash "Tx" -> [Hash "Tx"]
shrinkTxHash x
  | x == simplest = []
  | otherwise = [simplest]
  where
    simplest = head txHashes

txHashes :: [Hash "Tx"]
txHashes = mkTxHash <$> ['0' .. '9'] <> ['A' .. 'F']

--------------------------------------------------------------------------------
-- Transaction hashes chosen from a large range (to minimize collisions)
--------------------------------------------------------------------------------

genTxHashLargeRange :: Gen (Hash "Tx")
genTxHashLargeRange = Hash . B8.pack <$> replicateM 32 arbitrary

--------------------------------------------------------------------------------
-- Transaction indices generated according to the size parameter
--------------------------------------------------------------------------------

genTxIndex :: Gen Word32
genTxIndex = sized $ \size -> elements $ take (max 1 size) txIndices

shrinkTxIndex :: Word32 -> [Word32]
shrinkTxIndex 0 = []
shrinkTxIndex _ = [0]

txIndices :: [Word32]
txIndices =
  let
    w16range = [0 ..] :: [Word16]
  in
    fromIntegral <$> w16range

--------------------------------------------------------------------------------
-- Transaction inputs generated according to the size parameter
--------------------------------------------------------------------------------

genTxIn :: Gen TxIn
genTxIn = genSized2With TxIn genTxHash genTxIndex

shrinkTxIn :: TxIn -> [TxIn]
shrinkTxIn (TxIn h i) =
  uncurry TxIn
    <$> shrinkInterleaved
      (h, shrinkTxHash)
      (i, shrinkTxIndex)

--------------------------------------------------------------------------------
-- Transaction input functions
--------------------------------------------------------------------------------

coarbitraryTxIn :: TxIn -> Gen a -> Gen a
coarbitraryTxIn = coarbitrary . show

genTxInFunction :: Gen a -> Gen (TxIn -> a)
genTxInFunction = genFunction coarbitraryTxIn

--------------------------------------------------------------------------------
-- Transaction inputs chosen from a large range (to minimize collisions)
--------------------------------------------------------------------------------

genTxInLargeRange :: Gen TxIn
genTxInLargeRange =
  TxIn
    <$> genTxHashLargeRange
    -- Note that we don't need to choose indices from a large range, as hashes
    -- are already chosen from a large range:
    <*> genTxIndex

--------------------------------------------------------------------------------
-- Internal utilities
--------------------------------------------------------------------------------

-- The input must be a character in the range [0-9] or [A-F].
--
mkTxHash :: Char -> Hash "Tx"
mkTxHash c =
  fromRight reportError
    $ fromText
    $ T.pack
    $ replicate txHashHexStringLength c
  where
    reportError =
      error
        $ "Unable to generate transaction hash from character: " <> show c

txHashHexStringLength :: Int
txHashHexStringLength = 64
