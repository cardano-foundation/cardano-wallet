{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

{-# LANGUAGE TemplateHaskell #-}

import Prelude

import Data.Proxy
import qualified Data.Aeson.Types as A
import Cardano.Wallet.Api
import Servant.TypeScript
import Cardano.Wallet.Api.Types (ApiStakePool)
import Cardano.Wallet.Primitive.AddressDerivation (NetworkDiscriminant(..))
import Data.Aeson.TypeScript.Recursive (recursivelyDeriveMissingTypeScriptInstancesFor)
import Data.Aeson.TypeScript.TH (deriveJSONAndTypeScript)

type MainnetApi = Cardano.Wallet.Api.ApiV2 'Mainnet ApiStakePool

$(recursivelyDeriveMissingTypeScriptInstancesFor ''MainnetApi (deriveJSONAndTypeScript A.defaultOptions))

main :: IO ()
main = writeTypeScriptLibrary (Proxy :: Proxy MainnetApi) "tmp"

-- But OctetStream (ByteString) cannot be processed, since it’s not JSON:
--
-- > exe/typescript-gen.hs:28:8: error:
-- >     • servant-0.19:Servant.API.ContentTypes.JSON expected in list '[servant-0.19:Servant.API.ContentTypes.OctetStream]
--
-- And `Data.Aeson.TypeScript.TH` is a “library provides a way to
-- generate TypeScript .d.ts files that match your existing Aeson
-- ToJSON instances.”,
-- cf. <https://hackage.haskell.org/package/aeson-typescript-0.4.0.0/docs/Data-Aeson-TypeScript-TH.html>
