{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Integration.Scenario.API.Shelley.StakePools
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Api.Types
    ( ApiT (..), ApiWallet, DecodeAddress, EncodeAddress, WalletStyle (..) )
import Cardano.Wallet.Primitive.AddressDerivation
    ( PaymentAddress )
import Cardano.Wallet.Primitive.AddressDerivation.Shelley
    ( ShelleyKey )
import Cardano.Wallet.Primitive.Types
    ( PoolId (..) )
import Data.Generics.Internal.VL.Lens
    ( (^.) )
import Data.Text.Class
    ( toText )
import Test.Hspec
    ( SpecWith, it )
import Test.Integration.Framework.DSL
    ( Context (..)
    , Headers (..)
    , Payload (..)
    , emptyWallet
    , expectErrorMessage
    , expectResponseCode
    , fixturePassphrase
    , fixtureWallet
    , joinStakePool
    , request
    , walletId
    )
import Test.Integration.Framework.TestData
    ( errMsg403WrongPass, errMsg404NoSuchPool, errMsg404NoWallet )

import qualified Cardano.Wallet.Api.Link as Link
import qualified Data.ByteString as BS
import qualified Network.HTTP.Types.Status as HTTP


spec :: forall n t.
    ( DecodeAddress n
    , EncodeAddress n
    , PaymentAddress n ShelleyKey
    ) => SpecWith (Context t)
spec = do
    it "STAKE_POOLS_JOIN_01 - Cannot join with empty wallet" $ \ctx -> do
        w <- emptyWallet ctx
        let wid = w ^. walletId
        _ <- request @ApiWallet ctx
            (Link.deleteWallet @'Shelley w) Default Empty
        let poolIdAbsent = PoolId $ BS.pack $ replicate 32 1
        r <- joinStakePool @n ctx (ApiT poolIdAbsent) (w, fixturePassphrase)
        expectResponseCode HTTP.status404 r
        expectErrorMessage (errMsg404NoWallet wid) r

    it "STAKE_POOLS_JOIN_01 - Cannot join non-existant stakepool" $ \ctx -> do
        w <- fixtureWallet ctx
        let poolIdAbsent = PoolId $ BS.pack $ replicate 32 1
        r <- joinStakePool @n ctx (ApiT poolIdAbsent) (w, fixturePassphrase)
        expectResponseCode HTTP.status404 r
        expectErrorMessage (errMsg404NoSuchPool (toText poolIdAbsent)) r

    it "STAKE_POOLS_JOIN_01 - Cannot join existant stakepool when wrong password" $ \ctx -> do
        w <- fixtureWallet ctx
        let poolIdMock = PoolId $ BS.pack $ replicate 32 0
        r <- joinStakePool @n ctx (ApiT poolIdMock) (w, "Wrong Passphrase")
        expectResponseCode HTTP.status403 r
        expectErrorMessage errMsg403WrongPass r

    it "STAKE_POOLS_JOIN_01 - Can join existant stakepool" $ \ctx -> do
        w <- fixtureWallet ctx
        let poolIdMock = PoolId $ BS.pack $ replicate 32 0
        r <- joinStakePool @n ctx (ApiT poolIdMock) (w, fixturePassphrase)
        expectResponseCode HTTP.status403 r
        expectErrorMessage errMsg403WrongPass r
