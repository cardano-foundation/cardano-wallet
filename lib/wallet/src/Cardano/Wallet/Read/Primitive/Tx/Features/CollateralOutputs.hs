{-# LANGUAGE TypeFamilies #-}

module Cardano.Wallet.Read.Primitive.Tx.Features.CollateralOutputs
  ( getCollateralOutputs
  )
where

import Cardano.Api.Shelley qualified as Cardano
import Cardano.Ledger.Babbage qualified as Babbage
import Cardano.Ledger.Babbage.TxBody qualified as Babbage
import Cardano.Wallet.Primitive.Types.Tx.TxOut qualified as W
import Cardano.Wallet.Read.Eras
  ( EraFun (..)
  , K (..)
  )
import Cardano.Wallet.Read.Primitive.Tx.Features.Outputs
  ( fromCardanoValue
  , fromShelleyAddress
  )
import Cardano.Wallet.Read.Tx.CollateralOutputs
  ( CollateralOutputs (..)
  )
import Data.Maybe.Strict
  ( strictMaybeToMaybe
  )
import Ouroboros.Consensus.Shelley.Eras
  ( StandardBabbage
  , StandardConway
  )
import Prelude

getCollateralOutputs :: EraFun CollateralOutputs (K (Maybe W.TxOut))
getCollateralOutputs =
  EraFun
    { byronFun = \_ -> K Nothing
    , shelleyFun = \_ -> K Nothing
    , allegraFun = \_ -> K Nothing
    , maryFun = \_ -> K Nothing
    , alonzoFun = \_ -> K Nothing
    , babbageFun = \(CollateralOutputs mo) ->
        K $ fromBabbageTxOut <$> strictMaybeToMaybe mo
    , conwayFun = \(CollateralOutputs mo) ->
        K $ fromConwayTxOut <$> strictMaybeToMaybe mo
    }

fromBabbageTxOut
  :: Babbage.BabbageTxOut StandardBabbage
  -> W.TxOut
fromBabbageTxOut (Babbage.BabbageTxOut addr value _datum _refScript) =
  W.TxOut (fromShelleyAddress addr)
    $ fromCardanoValue
    $ Cardano.fromMaryValue value

fromConwayTxOut
  :: Babbage.BabbageTxOut StandardConway
  -> W.TxOut
fromConwayTxOut (Babbage.BabbageTxOut addr value _datum _refScript) =
  W.TxOut (fromShelleyAddress addr)
    $ fromCardanoValue
    $ Cardano.fromMaryValue value
