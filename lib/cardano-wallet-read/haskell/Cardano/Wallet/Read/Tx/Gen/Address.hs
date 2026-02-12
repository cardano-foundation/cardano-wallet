module Cardano.Wallet.Read.Tx.Gen.Address
    ( decodeShelleyAddress
    , decodeByronAddress
    )
where

import Cardano.Ledger.Api
    ( Addr
    )
import Cardano.Wallet.Read.Tx.Gen.TxParameters
    ( Address (..)
    )
import Prelude

import Cardano.Chain.Common qualified as Byron
import Cardano.Ledger.Api.Tx.Address qualified as Shelley
import Cardano.Ledger.Binary qualified as Binary

fromRight :: String -> Either a p -> p
fromRight _ (Right x) = x
fromRight e (Left _) = error e

decodeShelleyAddress :: Address -> Addr
decodeShelleyAddress (ByronAddress b) =
    fromRight "expected valid Byron address"
        $ Shelley.decodeAddrEither b
decodeShelleyAddress (ShelleyAddress b) =
    fromRight "expected valid Shelley address"
        $ Shelley.decodeAddrEither b

decodeByronAddress :: Address -> Byron.Address
decodeByronAddress (ByronAddress b) =
    fromRight "expected a valid Byron address"
        $ Binary.decodeFull' Binary.byronProtVer b
decodeByronAddress (ShelleyAddress _) =
    error "expected a Byron address"
