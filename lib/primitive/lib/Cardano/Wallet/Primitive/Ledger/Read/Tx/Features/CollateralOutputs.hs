{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Wallet.Primitive.Ledger.Read.Tx.Features.CollateralOutputs
    ( getCollateralOutputs
    )
    where

import Prelude

import Cardano.Wallet.Primitive.Ledger.Convert
    ( toWalletTokenBundle
    )
import Cardano.Wallet.Primitive.Ledger.Read.Tx.Features.Outputs
    ( fromShelleyAddress
    )
import Cardano.Wallet.Read.Eras
    ( Era (..)
    , IsEra (..)
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

import qualified Cardano.Ledger.Babbage as Babbage
import qualified Cardano.Ledger.Babbage.TxBody as Babbage
import qualified Cardano.Wallet.Primitive.Types.Tx.TxOut as W

getCollateralOutputs
    :: forall era
     . IsEra era
    => CollateralOutputs era
    -> Maybe W.TxOut
getCollateralOutputs = case theEra @era of
    Byron -> \_ -> Nothing
    Shelley -> \_ -> Nothing
    Allegra -> \_ -> Nothing
    Mary -> \_ -> Nothing
    Alonzo -> \_ -> Nothing
    Babbage -> \(CollateralOutputs mo) ->
        fromBabbageTxOut <$> strictMaybeToMaybe mo
    Conway -> \(CollateralOutputs mo) ->
        fromConwayTxOut <$> strictMaybeToMaybe mo

fromBabbageTxOut
    :: Babbage.BabbageTxOut StandardBabbage
    -> W.TxOut
fromBabbageTxOut (Babbage.BabbageTxOut addr value _datum _refScript) =
    W.TxOut (fromShelleyAddress addr) (toWalletTokenBundle value)

fromConwayTxOut
    :: Babbage.BabbageTxOut StandardConway
    -> W.TxOut
fromConwayTxOut (Babbage.BabbageTxOut addr value _datum _refScript) =
    W.TxOut (fromShelleyAddress addr) (toWalletTokenBundle value)
