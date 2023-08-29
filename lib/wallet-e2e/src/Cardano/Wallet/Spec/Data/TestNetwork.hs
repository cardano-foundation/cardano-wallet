module Cardano.Wallet.Spec.Data.TestNetwork where

import qualified Data.Text as T

data TestNetwork
    = -- | Automatically manage a local test cluster.
      Local
    | -- | Automatically manage a preprod node and wallet.
      Preprod
    | -- | Rely on a node and wallet managed manually (externally).
      Manual
    deriving stock (Show)

fromText :: Text -> Either String TestNetwork
fromText text =
    case T.toLower (T.strip text) of
        "local" -> pure Local
        "preprod" -> pure Preprod
        "manual" -> pure Manual
        _ -> fail "Invalid test network, must be one of: local, preprod, manual"
