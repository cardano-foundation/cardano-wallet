{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Wallet.Primitive.Ledger.Read.Tx.Features.Outputs
    ( getOutputs
    , fromShelleyTxOut
    , fromMaryTxOut
    , fromAllegraTxOut
    , fromAlonzoTxOut
    , fromBabbageTxOut
    , fromConwayTxOut
    , fromCardanoValue
    , fromShelleyAddress
    , fromByronTxOut
    )
    where

import Prelude

import Cardano.Binary
    ( serialize'
    )
import Cardano.Chain.Common
    ( unsafeGetLovelace
    )
import Cardano.Ledger.Alonzo
    ( AlonzoScript
    )
import Cardano.Ledger.Shelley.API
    ( StrictMaybe (SJust, SNothing)
    )
import Cardano.Read.Ledger.Tx.Outputs
    ( Outputs (..)
    )
import Cardano.Wallet.Primitive.Ledger.Convert
    ( toWalletTokenBundle
    )
import Cardano.Wallet.Read
    ( Era (..)
    , theEra
    )
import Cardano.Wallet.Read.Eras
    ( IsEra
    )
import Cardano.Wallet.Util
    ( internalError
    )
import Data.Foldable
    ( toList
    )
import GHC.Stack
    ( HasCallStack
    )
import Cardano.Ledger.Allegra
    ( AllegraEra
    )
import Cardano.Ledger.Alonzo
    ( AlonzoEra
    )
import Cardano.Ledger.Babbage
    ( BabbageEra
    )
import Cardano.Ledger.Conway
    ( ConwayEra
    )
import Cardano.Ledger.Mary
    ( MaryEra
    )
import Cardano.Ledger.Shelley
    ( ShelleyEra
    )

import qualified Cardano.Api.Shelley as Cardano
import qualified Cardano.Chain.UTxO as Byron
import qualified Cardano.Ledger.Address as SL
import qualified Cardano.Ledger.Alonzo as Alonzo
import qualified Cardano.Ledger.Alonzo.TxBody as Alonzo
import qualified Cardano.Ledger.Babbage as Babbage
import qualified Cardano.Ledger.Babbage.TxBody as Babbage
import qualified Cardano.Ledger.Conway as Conway
import qualified Cardano.Ledger.Crypto as SL
import qualified Cardano.Ledger.Shelley.API as SL
import qualified Cardano.Wallet.Primitive.Ledger.Convert as Ledger
import qualified Cardano.Wallet.Primitive.Types.Address as W
import qualified Cardano.Wallet.Primitive.Types.AssetId as W
import qualified Cardano.Wallet.Primitive.Types.AssetName as W
import qualified Cardano.Wallet.Primitive.Types.Coin as Coin
import qualified Cardano.Wallet.Primitive.Types.Coin as W
import qualified Cardano.Wallet.Primitive.Types.Hash as W
import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle
import qualified Cardano.Wallet.Primitive.Types.TokenPolicyId as W
import qualified Cardano.Wallet.Primitive.Types.TokenQuantity as W
import qualified Cardano.Wallet.Primitive.Types.Tx.TxOut as W
import qualified GHC.IsList as GHC
    ( toList
    )

{-# INLINABLE getOutputs #-}
getOutputs :: forall era . IsEra era => Outputs era -> [W.TxOut]
getOutputs = case theEra @era of
    Byron -> \(Outputs os) -> fromByronTxOut <$> toList os
    Shelley -> \(Outputs os) -> fromShelleyTxOut <$> toList os
    Allegra -> \(Outputs os) -> fromAllegraTxOut <$> toList os
    Mary -> \(Outputs os) -> fromMaryTxOut <$> toList os
    Alonzo -> \(Outputs os) -> fromAlonzoTxOut <$> toList os
    Babbage -> \(Outputs os) -> fst . fromBabbageTxOut <$> toList os
    Conway -> \(Outputs os) -> fst . fromConwayTxOut <$> toList os

fromShelleyAddress :: SL.Addr -> W.Address
fromShelleyAddress = W.Address . SL.serialiseAddr

fromShelleyTxOut :: SL.ShelleyTxOut ShelleyEra -> W.TxOut
fromShelleyTxOut (SL.ShelleyTxOut addr amount) = W.TxOut
    (fromShelleyAddress addr)
    (TokenBundle.fromCoin $ Ledger.toWalletCoin amount)

fromAllegraTxOut :: SL.ShelleyTxOut AllegraEra -> W.TxOut
fromAllegraTxOut (SL.ShelleyTxOut addr amount) = W.TxOut
    (fromShelleyAddress addr)
    (TokenBundle.fromCoin $ Ledger.toWalletCoin amount)

fromMaryTxOut :: SL.ShelleyTxOut MaryEra -> W.TxOut
fromMaryTxOut (SL.ShelleyTxOut addr value) =
    W.TxOut (fromShelleyAddress addr) (toWalletTokenBundle value)

fromAlonzoTxOut
    :: Alonzo.AlonzoTxOut AlonzoEra
    -> W.TxOut
fromAlonzoTxOut (Alonzo.AlonzoTxOut addr value _) =
    W.TxOut (fromShelleyAddress addr) (toWalletTokenBundle value)

fromBabbageTxOut
    :: Babbage.BabbageTxOut BabbageEra
    -> (W.TxOut, Maybe (AlonzoScript Babbage.BabbageEra))
fromBabbageTxOut (Babbage.BabbageTxOut addr value _datum refScript) =
    ( W.TxOut (fromShelleyAddress addr) (toWalletTokenBundle value)
    , case refScript of
          SJust s -> Just s
          SNothing -> Nothing
    )

fromConwayTxOut
    :: Babbage.BabbageTxOut ConwayEra
    -> (W.TxOut, Maybe (AlonzoScript Conway.ConwayEra))
fromConwayTxOut (Babbage.BabbageTxOut addr value _datum refScript) =
    ( W.TxOut (fromShelleyAddress addr) (toWalletTokenBundle value)
    , case refScript of
          SJust s -> Just s
          SNothing -> Nothing
    )

-- Lovelace to coin. Quantities from ledger should always fit in Word64.
fromCardanoLovelace :: HasCallStack => SL.Coin -> W.Coin
fromCardanoLovelace =
    Coin.unsafeFromIntegral . unQuantity . Cardano.lovelaceToQuantity
  where
    unQuantity (Cardano.Quantity q) = q

fromCardanoValue :: HasCallStack => Cardano.Value -> TokenBundle.TokenBundle
fromCardanoValue = uncurry TokenBundle.fromFlatList . extract
  where
    extract value =
        ( fromCardanoLovelace $ Cardano.selectLovelace value
        , mkBundle $ GHC.toList value
        )

    -- Do Integer to Natural conversion. Quantities from ledger TxOuts can
    -- never be negative (but unminted values could be negative).
    mkQuantity :: Integer -> W.TokenQuantity
    mkQuantity = W.TokenQuantity . checkBounds
      where
        checkBounds n
          | n >= 0 = fromIntegral n
          | otherwise = internalError "negative token quantity"

    mkBundle assets =
        [ (W.AssetId (mkPolicyId p) (mkAssetName n), mkQuantity q)
        | (Cardano.AssetId p n, Cardano.Quantity q) <- assets
        ]

    mkPolicyId = W.UnsafeTokenPolicyId . W.Hash . Cardano.serialiseToRawBytes
    mkAssetName = W.UnsafeAssetName . Cardano.serialiseToRawBytes

fromByronTxOut :: Byron.TxOut -> W.TxOut
fromByronTxOut (Byron.TxOut addr coin) = W.TxOut
    { W.address = W.Address (serialize' addr)
    , W.tokens = TokenBundle.fromCoin $ Coin.fromWord64 $ unsafeGetLovelace coin
    }
