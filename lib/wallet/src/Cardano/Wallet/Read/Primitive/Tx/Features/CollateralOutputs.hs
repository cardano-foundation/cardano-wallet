{-# LANGUAGE TypeFamilies #-}

module Cardano.Wallet.Read.Primitive.Tx.Features.CollateralOutputs
    ( getCollateralOutputs
    )
    where

import Prelude

import Cardano.Wallet.Read.Eras
    ( EraFun (..), K (..) )
import Cardano.Wallet.Read.Primitive.Tx.Features.Outputs
    ( fromCardanoValue, fromShelleyAddress )
import Cardano.Wallet.Read.Tx.CollateralOutputs
    ( CollateralOutputs (..) )
import Data.Maybe.Strict
    ( strictMaybeToMaybe )
import Ouroboros.Consensus.Shelley.Eras
    ( StandardBabbage )

import qualified Cardano.Api.Shelley as Cardano
import qualified Cardano.Ledger.Babbage as Babbage
import qualified Cardano.Ledger.Babbage.TxBody as Babbage
import qualified Cardano.Wallet.Primitive.Types.Tx.TxOut as W

getCollateralOutputs :: EraFun CollateralOutputs (K (Maybe W.TxOut))
getCollateralOutputs = EraFun
    { byronFun = \_ -> K Nothing
    , shelleyFun = \_ -> K Nothing
    , allegraFun = \_ -> K Nothing
    , maryFun = \_ -> K Nothing
    , alonzoFun = \_ -> K Nothing
    , babbageFun = \(CollateralOutputs mo)
        -> K $ fromBabbageTxOut <$> strictMaybeToMaybe mo
    }

fromBabbageTxOut
    :: Babbage.BabbageTxOut StandardBabbage
    -> W.TxOut
fromBabbageTxOut (Babbage.BabbageTxOut addr value _datum _refScript) =
    W.TxOut (fromShelleyAddress addr) $
    fromCardanoValue $ Cardano.fromMaryValue value
