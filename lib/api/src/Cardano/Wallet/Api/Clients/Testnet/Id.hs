{-# LANGUAGE DataKinds #-}

module Cardano.Wallet.Api.Clients.Testnet.Id
    ( Testnet42
    )
where

import Cardano.Wallet.Primitive.NetworkId
    ( NetworkDiscriminant (..)
    )

type Testnet42 = 'Testnet 42
