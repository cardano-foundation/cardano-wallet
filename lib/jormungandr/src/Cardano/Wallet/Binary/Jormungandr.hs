{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}

-- |
-- Copyright: © 2018-2019 IOHK
-- License: MIT
--
-- The format is for the Shelley era as implemented by the Jörmungandr node.

module Cardano.Wallet.Binary.Jormungandr
    ( getBlockHeader
    , getBlock
    , Message (..)
    , BlockHeader (..)

     -- * Re-export
    , runGet

    ) where

import Prelude

import Cardano.Wallet.Primitive.Types
    ( Hash (..), SlotId (..) )
import Data.Binary.Get
    ( Get
    , getByteString
    , getWord16be
    , getWord32be
    , getWord8
    , isEmpty
    , isolate
    , runGet
    , skip
    )
import Data.Word
    ( Word16, Word32 )


-- | Messages are what the block body consists of.
--
-- Every message is prefixed with a message header.
--
--  Following, as closely as possible:
-- https://github.com/input-output-hk/rust-cardano/blob/e0616f13bebd6b908320bddb1c1502dea0d3305a/chain-impl-mockchain/src/message/mod.rs#L22-L29
data Message
    = Initial [ConfigParam]
    | OldUtxoDeclaration TODO
    | Transaction TODO
    | Certificate TODO
    | UpdateProposal SignedUpdateProposal
    | UpdateVote SignedVote
    | UnimplementedMessage Int -- For development. Remove later.
    deriving Show


data BlockHeader = BlockHeader
    { version :: Word16
    , contentSize :: Word32
    , slot :: SlotId
    , chainLength :: Word32
    , contentHash :: Hash "content"
    , parentHeaderHash :: Hash "parentHeader"
    } deriving (Show, Eq)

data Block = Block BlockHeader [Message]
    deriving Show

data SignedUpdateProposal = SignedUpdateProposal
    deriving Show
data TODO = TODO
    deriving Show
data SignedVote = SignedVote
    deriving Show
data ConfigParam = ConfigParam
    deriving Show

{-# ANN getBlockHeader ("HLint: ignore Use <$>" :: String) #-}
getBlockHeader :: Get BlockHeader
getBlockHeader =  (fromIntegral <$> getWord16be) >>= \s -> isolate s $ do
    version <- getWord16be
    contentSize <- getWord32be
    slotEpoch <- fromIntegral <$> getWord32be
    slotId <- fromIntegral <$> getWord32be
    chainLength <- getWord32be
    contentHash <- Hash <$> getByteString 32 -- or 256 bits
    parentHeaderHash <- Hash <$> getByteString 32

    -- TODO: Handle special case for BFT
    -- TODO: Handle special case for Praos/Genesis

    return $ BlockHeader
        { version = version
        , contentSize = contentSize
        , slot = (SlotId slotId slotEpoch)
        , chainLength = chainLength
        , contentHash = contentHash
        , parentHeaderHash = parentHeaderHash
        }

getBlock :: Get Block
getBlock = do
    header <- getBlockHeader
    msgs <- isolate (fromIntegral $ contentSize header)
        $ whileM (not <$> isEmpty) getMessage
    return $ Block header msgs

getMessage :: Get Message
getMessage = do
    size <- fromIntegral <$> getWord16be
    contentType <- fromIntegral <$> getWord8
    let remaining = size - 1
    let unimpl = skip remaining >> return (UnimplementedMessage contentType)
    isolate remaining $ case contentType of
        0 -> unimpl
        1 -> unimpl
        2 -> unimpl
        3 -> unimpl
        4 -> unimpl
        5 -> unimpl
        other -> fail $ "Unexpected content type tag " ++ show other

{-------------------------------------------------------------------------------
                              Helpers
-------------------------------------------------------------------------------}

whileM :: Monad m => m Bool -> m a -> m [a]
whileM cond next = go
  where
    go = do
        c <- cond
        if c then do
            a <- next
            as <- go
            return (a : as)
        else return []
