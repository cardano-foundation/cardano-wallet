module Cardano.Wallet.Launch.Cluster.Aeson
    ( decodeFileThrow
    , ChangeValue
    )
where

import Data.Aeson
    ( FromJSON
    , Value
    , eitherDecodeFileStrict
    )
import Prelude

type ChangeValue = Value -> Value

decodeFileThrow :: FromJSON a => FilePath -> IO a
decodeFileThrow fp = do
    e <- eitherDecodeFileStrict fp
    case e of
        Left err -> fail err
        Right a -> return a
