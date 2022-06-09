{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.Shelley.Network.Blockfrost.Conversion where

import Prelude

import Cardano.Wallet.Api.Types
    ( decodeAddress, decodeStakeAddress )
import Cardano.Wallet.Primitive.AddressDerivation
    ( RewardAccount )
import Cardano.Wallet.Primitive.Types
    ( StakePoolMetadataHash )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (Coin) )
import Cardano.Wallet.Shelley.Network.Blockfrost.Error
    ( BlockfrostError (..), (<?#>) )
import Cardano.Wallet.Shelley.Network.Discriminant
    ( SomeNetworkDiscriminant (..) )
import Control.Monad.Error.Class
    ( MonadError (throwError) )
import Data.IntCast
    ( intCast )
import Data.Proxy
    ( Proxy (..) )
import Data.Quantity
    ( Percentage, mkPercentage )
import Data.Text
    ( Text )
import Data.Text.Class
    ( fromText )

import qualified Blockfrost.Client as BF
import Cardano.Wallet.Primitive.Types.Address
    ( Address )

fromBfLovelaces :: MonadError BlockfrostError m => BF.Lovelaces -> m Coin
fromBfLovelaces lovs = Coin <$> (intCast @_ @Integer lovs <?#> "Lovelaces")

fromBfAddress
    :: MonadError BlockfrostError m
    => SomeNetworkDiscriminant
    -> BF.Address
    -> m Address
fromBfAddress (SomeNetworkDiscriminant (Proxy :: Proxy nd)) (BF.Address addr) =
    case decodeAddress @nd addr of
        Left e -> throwError (InvalidAddress addr e)
        Right a -> pure a

fromBfStakeAddress
    :: MonadError BlockfrostError m
    => SomeNetworkDiscriminant
    -> BF.Address
    -> m RewardAccount
fromBfStakeAddress (SomeNetworkDiscriminant (Proxy :: Proxy nd)) (BF.Address addr) =
    case decodeStakeAddress @nd addr of
        Left e -> throwError (InvalidStakeAddress addr e)
        Right a -> pure a

percentageFromDouble :: MonadError BlockfrostError m => Double -> m Percentage
percentageFromDouble d =
    case mkPercentage (toRational d) of
        Left e -> throwError (InvalidPercentage d e)
        Right a -> pure a

stakePoolMetadataHashFromText
    :: MonadError BlockfrostError m => Text -> m StakePoolMetadataHash
stakePoolMetadataHashFromText text =
    case fromText text of
        Left e -> throwError (InvalidStakePoolMetadataHash text e)
        Right a -> pure a
