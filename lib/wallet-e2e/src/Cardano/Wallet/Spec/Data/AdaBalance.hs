module Cardano.Wallet.Spec.Data.AdaBalance where

data AdaBalance = AdaBalance
    { adaAvailable :: Natural
    , adaInRewards :: Natural
    , adaTotal :: Natural
    }
    deriving stock (Show, Eq, Ord)

zeroAdaBalance :: AdaBalance
zeroAdaBalance = AdaBalance{adaAvailable = 0, adaInRewards = 0, adaTotal = 0}
