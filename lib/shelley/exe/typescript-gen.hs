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

import Prelude

import Data.Proxy
import qualified Data.Aeson.Types as A
import Cardano.Wallet.Api
import Servant.TypeScript
import Cardano.Wallet.Api.Types (ApiStakePool)
import qualified Cardano.Wallet.Api.Types
import Cardano.Wallet.Primitive.AddressDerivation (NetworkDiscriminant(..))
import Data.Aeson.TypeScript.Recursive (recursivelyDeriveMissingTypeScriptInstancesFor)
import Data.Aeson.TypeScript.TH (deriveJSONAndTypeScript, TypeScript)

type MainnetApi = Cardano.Wallet.Api.ApiV2 'Mainnet ApiStakePool

$(recursivelyDeriveMissingTypeScriptInstancesFor ''MainnetApi (deriveJSONAndTypeScript A.defaultOptions))

-- Hmmm… The 'Mainnet parameter is not handled by `recursivelyDeriveMissingTypeScriptInstancesFor`?
instance TypeScript (Cardano.Wallet.Api.Types.ApiWalletMigrationPostData 'Mainnet "lenient")
instance TypeScript (Cardano.Wallet.Api.Types.ApiWalletMigrationPostData 'Mainnet "user")
instance TypeScript (Cardano.Wallet.Api.Types.ApiAddress 'Mainnet)
-- etc. …
-- No instance for (TypeScript (Cardano.Wallet.Api.Types.ApiSelectCoinsData 'Mainnet))
-- etc. … so we’d have to specify all of them by hand…

-- Why is this needed???
instance TypeScript (Proxy 'Mainnet)

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
--
-- So for now, we commented out `PostExternalTransaction` and `SignMetadata`
--
-- We would (maybe) like to propose a JSON representation of OctetStream along the lines of:
--
-- > {"data":"base64-encoded-string"}
