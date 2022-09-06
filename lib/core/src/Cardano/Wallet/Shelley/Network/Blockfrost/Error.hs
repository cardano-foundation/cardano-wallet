{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}

module Cardano.Wallet.Shelley.Network.Blockfrost.Error where

import Prelude

import Cardano.Wallet.Primitive.Types
    ( EpochNo, PoolId )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin )
import Control.Exception
    ( Exception, throwIO )
import Control.Monad
    ( (<=<) )
import Control.Monad.Error.Class
    ( MonadError (throwError) )
import Control.Monad.Trans.Except
    ( ExceptT, runExceptT )
import Data.Bits
    ( Bits )
import Data.IntCast
    ( intCastMaybe )
import Data.Quantity
    ( MkPercentageError )
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
    | InvalidStakePoolMetadataHash Text TextDecodingError
    | PoolRegistrationIsMissing PoolId
    | PoolRetirementCertificateNotFound PoolId BF.PoolUpdate
    | PoolUpdateCertificateNotFound PoolId BF.PoolUpdate
    | InvalidTxHash Text TextDecodingError
    | InvalidAddress Text TextDecodingError
    | InvalidStakeAddress Text TextDecodingError
    | InvalidPoolId Text TextDecodingError
    | InvalidTokenPolicyId Text TextDecodingError
    | InvalidTokenName Text TextDecodingError
    | PoolStakePercentageError Coin Coin
    | InvalidDecentralizationLevelPercentage Double
    | InvalidPercentage Double MkPercentageError
    | InvalidUtxoInputAmount BF.UtxoInput
    | InvalidUtxoOutputAmount BF.UtxoOutput
    | UnknownEraForEpoch EpochNo
    deriving (Show, Eq)

newtype BlockfrostException = BlockfrostException BlockfrostError
    deriving stock (Show)
    deriving anyclass (Exception)

-- | Raises an error in case of an absent value
(<?>) :: MonadError e m => Maybe a -> e -> m a
(<?>) Nothing e = throwError e
(<?>) (Just a) _ = pure a

infixl 8 <?>

{-# INLINE (<?>) #-}

-- | Casts integral values safely or raises an `IntegralCastError`
(<?#>)
    :: (MonadError BlockfrostError m, Integral a, Integral b, Bits a, Bits b)
    => a
    -> String
    -> m b
(<?#>) a e = intCastMaybe a <?> IntegralCastError e

infixl 8 <?#>

{-# INLINE (<?#>) #-}

throwBlockfrostError :: ExceptT BlockfrostError IO a -> IO a
throwBlockfrostError =
    either (throwIO . BlockfrostException) pure <=< runExceptT
