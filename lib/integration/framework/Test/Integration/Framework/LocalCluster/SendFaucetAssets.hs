module Test.Integration.Framework.LocalCluster.SendFaucetAssets
    ( sendFaucetAssets
    ) where

import Cardano.Wallet.Launch.Cluster.Process
    ( FaucetQ (..)
    , RunFaucetQ (..)
    , SendFaucetAssets (..)
    )
import Cardano.Wallet.Primitive.Types.Address
    ( Address (..)
    )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin
    )
import Data.Bifunctor
    ( first
    )
import Prelude

import qualified Cardano.Address as Address
import qualified Cardano.Wallet.Faucet as Faucet

sendFaucetAssets
    :: RunFaucetQ m
    -- ^ local cluster faucet client
    -> Int
    -- ^ Batch size
    -> (Int, Coin, [Address.Address])
    -> m ()
sendFaucetAssets (RunFaucetQ queryFaucet) batchSize' (nPerAddr, c, addrs) = do
    queryFaucet
        $ SendFaucetAssetsQ
        $ SendFaucetAssets batchSize'
        $ first toPrimitiveAddress
            <$> Faucet.seaHorseTestAssets nPerAddr c addrs

toPrimitiveAddress :: Address.Address -> Address
toPrimitiveAddress = Address . Address.unAddress
