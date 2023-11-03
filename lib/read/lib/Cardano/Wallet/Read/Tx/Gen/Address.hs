module Cardano.Wallet.Read.Tx.Gen.Address
    ( decodeShelleyAddress
    , decodeByronAddress
    )
where

import Prelude

import Cardano.Ledger.Api
    ( Addr
    , StandardCrypto
    )
import Cardano.Wallet.Read.Tx.Gen.TxParameters
    ( Address (..)
    )

import qualified Cardano.Chain.Common as Byron
import qualified Cardano.Ledger.Api.Tx.Address as Shelley
import qualified Cardano.Ledger.Binary as Binary

fromRight :: String -> Either a p -> p
fromRight _ (Right x) = x
fromRight e (Left _) = error e

decodeShelleyAddress :: Address -> Addr StandardCrypto
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
