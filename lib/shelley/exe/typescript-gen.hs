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
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Prelude

import Data.Proxy
import qualified Data.Aeson.Types as A
import Cardano.Wallet.Api
import Servant.TypeScript
import Cardano.Wallet.Api.Types (ApiStakePool)
import qualified Cardano.Wallet.Api.Types
import qualified Cardano.Wallet.Primitive.Types
import Cardano.Wallet.Primitive.AddressDerivation (NetworkDiscriminant(..))
import Data.Aeson.TypeScript.Recursive (recursivelyDeriveMissingTypeScriptInstancesFor)
import Data.Aeson.TypeScript.TH (deriveJSONAndTypeScript, TypeScript)
import Servant ((:<|>)(..), (:>)(..))

type SmallerApiV2 = "v2" :> SmallerApi

-- FIXME: cardano-wallet API without:
--   • parts referencing (n :: NetworkDiscriminant)
--   • and apiPool
--   • and endpoints returning `OctetStream`
--       So for now, we commented out `PostExternalTransaction` and `SignMetadata`
--
--       We would (maybe) like to propose a JSON representation of OctetStream along the lines of:
--
--       > {"data":"base64-encoded-string"}
type SmallerApi =
         Wallets
    :<|> WalletKeys
    :<|> Assets
    :<|> ByronWallets
    :<|> ByronAssets
    :<|> Network
    :<|> Settings
    :<|> SMASH
    :<|> SharedWallets
    :<|> SharedWalletKeys

-- TODO: pls, help us get the whole `MainnetApi` instead of `SmallerApi` here :pray:
--type MainnetApi = Cardano.Wallet.Api.ApiV2 'Mainnet ApiStakePool

deriving instance TypeScript Cardano.Wallet.Primitive.Types.WalletId

-- ↑ results in:
-- • No instance for (TypeScript
--                      (cryptonite-0.27:Crypto.Hash.Types.Digest
--                         cryptonite-0.27:Crypto.Hash.Blake2b.Blake2b_160))

$(recursivelyDeriveMissingTypeScriptInstancesFor ''SmallerApi (deriveJSONAndTypeScript A.defaultOptions))

main :: IO ()
main = writeTypeScriptLibrary (Proxy :: Proxy SmallerApi) "tmp"
