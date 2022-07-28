{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}

import Prelude

import Data.Proxy
import qualified Data.Aeson.Types as A
import Cardano.Wallet.Api
import Cardano.Wallet.Api.Types (ApiStakePool)
import qualified Cardano.Wallet.Api.Types
import qualified Cardano.Wallet.Primitive.Types.Address
import Cardano.Wallet.Primitive.AddressDerivation (NetworkDiscriminant(..))

import ServantTS.Convert
import ServantTS.Output.RequestFlavors.Fetch (Fetch)
import ServantTS.Output.TSFunctions
import ServantTS.Output.Docs
import Typescript

type MainnetApi = Cardano.Wallet.Api.ApiV2 'Mainnet ApiStakePool

--deriving instance TypescriptType MainnetApi

deriving instance TypescriptType (Cardano.Wallet.Api.Types.ApiT Cardano.Wallet.Primitive.Types.Address.Address, Proxy 'Mainnet)

-- now:
--
-- >     • No instance for (TypescriptType
-- >                          (Cardano.Wallet.Api.Types.ApiWalletMigrationPostData
-- >                             'Mainnet "lenient"))
--
-- so we should specify those by hand, or use
-- `Data.Aeson.TypeScript.Recursive.recursivelyDeriveMissingTypeScriptInstancesFor`
-- which doesn’t seem to want to work with the 'Mainnet parameter

main :: IO ()
main = apiToTSDocs asTS reqToTSFunction outputFileLocs
  where
    outputFileLocs = OutputFileNames "tmp/types.tsx" "tmp/api.tsx"
    asTS = servantToReqTS (Proxy :: Proxy FpTs) (Proxy :: Proxy MainnetApi)
    reqToTSFunction = defaultReqToTSFunction (Proxy @Fetch)
