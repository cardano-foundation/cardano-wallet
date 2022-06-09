{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Cardano.Wallet.Shelley.Network.Blockfrost.Error where

import Prelude

import Cardano.Wallet.Primitive.Types
    ( EpochNo )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin )
import Control.Exception
    ( Exception )
import Data.Text
    ( Text )
import Data.Text.Class
    ( TextDecodingError )

import qualified Blockfrost.Client as BF
import qualified Servant.Client as Servant

data BlockfrostError
    = ClientError Servant.ClientError
    | NoSlotError BF.Block
    | IntegralCastError String
    | InvalidBlockHash BF.BlockHash TextDecodingError
    | InvalidTxMetadataLabel String
    | InvalidTxMetadataValue String
    | InvalidTxHash Text TextDecodingError
    | InvalidAddress Text TextDecodingError
    | InvalidStakeAddress Text TextDecodingError
    | InvalidPoolId Text TextDecodingError
    | PoolStakePercentageError Coin Coin
    | InvalidDecentralizationLevelPercentage Double
    | InvalidUtxoInputAmount BF.UtxoInput
    | InvalidUtxoOutputAmount BF.UtxoOutput
    | UnknownEraForEpoch EpochNo
    deriving (Show, Eq)

newtype BlockfrostException = BlockfrostException BlockfrostError
    deriving stock (Show)
    deriving anyclass (Exception)
