{-# LANGUAGE TypeFamilies #-}

module Cardano.Wallet.Read.Primitive.Tx.Features.Outputs
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

import Cardano.Api.Shelley qualified as Cardano
import Cardano.Binary
  ( serialize'
  )
import Cardano.Chain.Common
  ( unsafeGetLovelace
  )
import Cardano.Chain.UTxO qualified as Byron
import Cardano.Ledger.Address qualified as SL
import Cardano.Ledger.Alonzo
  ( AlonzoScript
  )
import Cardano.Ledger.Alonzo qualified as Alonzo
import Cardano.Ledger.Alonzo.TxBody qualified as Alonzo
import Cardano.Ledger.Babbage qualified as Babbage
import Cardano.Ledger.Babbage.TxBody qualified as Babbage
import Cardano.Ledger.Conway qualified as Conway
import Cardano.Ledger.Crypto qualified as SL
import Cardano.Ledger.Shelley.API
  ( StrictMaybe (SJust, SNothing)
  )
import Cardano.Ledger.Shelley.API qualified as SL
import Cardano.Wallet.Primitive.Types.Address qualified as W
import Cardano.Wallet.Primitive.Types.Coin qualified as Coin
import Cardano.Wallet.Primitive.Types.Coin qualified as W
import Cardano.Wallet.Primitive.Types.Hash qualified as W
import Cardano.Wallet.Primitive.Types.TokenBundle qualified as TokenBundle
import Cardano.Wallet.Primitive.Types.TokenPolicy qualified as W
import Cardano.Wallet.Primitive.Types.TokenQuantity qualified as W
import Cardano.Wallet.Primitive.Types.Tx.TxOut qualified as W
import Cardano.Wallet.Read.Eras
  ( EraFun (..)
  , K (..)
  )
import Cardano.Wallet.Read.Tx.Outputs
  ( Outputs (..)
  )
import Cardano.Wallet.Shelley.Compatibility.Ledger qualified as Ledger
import Cardano.Wallet.Util
  ( internalError
  )
import Data.Foldable
  ( toList
  )
import GHC.Stack
  ( HasCallStack
  )
import Ouroboros.Consensus.Shelley.Eras
  ( StandardAllegra
  , StandardAlonzo
  , StandardBabbage
  , StandardConway
  , StandardMary
  , StandardShelley
  )
import Prelude

getOutputs :: EraFun Outputs (K [W.TxOut])
getOutputs =
  EraFun
    { byronFun = \(Outputs os) -> K . fmap fromByronTxOut $ toList os
    , shelleyFun = \(Outputs os) -> K . fmap fromShelleyTxOut $ toList os
    , allegraFun = \(Outputs os) -> K . fmap fromAllegraTxOut $ toList os
    , maryFun = \(Outputs os) -> K . fmap fromMaryTxOut $ toList os
    , alonzoFun = \(Outputs os) -> K . fmap fromAlonzoTxOut $ toList os
    , babbageFun = \(Outputs os) -> K . fmap (fst . fromBabbageTxOut) $ toList os
    , conwayFun = \(Outputs os) -> K . fmap (fst . fromConwayTxOut) $ toList os
    }

fromShelleyAddress :: SL.Addr crypto -> W.Address
fromShelleyAddress = W.Address . SL.serialiseAddr

fromShelleyTxOut :: SL.ShelleyTxOut StandardShelley -> W.TxOut
fromShelleyTxOut (SL.ShelleyTxOut addr amount) =
  W.TxOut
    (fromShelleyAddress addr)
    (TokenBundle.fromCoin $ Ledger.toWalletCoin amount)

fromAllegraTxOut :: SL.ShelleyTxOut StandardAllegra -> W.TxOut
fromAllegraTxOut (SL.ShelleyTxOut addr amount) =
  W.TxOut
    (fromShelleyAddress addr)
    (TokenBundle.fromCoin $ Ledger.toWalletCoin amount)

fromMaryTxOut :: SL.ShelleyTxOut StandardMary -> W.TxOut
fromMaryTxOut (SL.ShelleyTxOut addr value) =
  W.TxOut (fromShelleyAddress addr)
    $ fromCardanoValue
    $ Cardano.fromMaryValue value

fromAlonzoTxOut
  :: Alonzo.AlonzoTxOut StandardAlonzo
  -> W.TxOut
fromAlonzoTxOut (Alonzo.AlonzoTxOut addr value _) =
  W.TxOut (fromShelleyAddress addr)
    $ fromCardanoValue
    $ Cardano.fromMaryValue value

fromBabbageTxOut
  :: Babbage.BabbageTxOut StandardBabbage
  -> (W.TxOut, Maybe (AlonzoScript (Babbage.BabbageEra SL.StandardCrypto)))
fromBabbageTxOut (Babbage.BabbageTxOut addr value _datum refScript) =
  ( W.TxOut (fromShelleyAddress addr)
      $ fromCardanoValue
      $ Cardano.fromMaryValue value
  , case refScript of
      SJust s -> Just s
      SNothing -> Nothing
  )

fromConwayTxOut
  :: Babbage.BabbageTxOut StandardConway
  -> (W.TxOut, Maybe (AlonzoScript (Conway.ConwayEra SL.StandardCrypto)))
fromConwayTxOut (Babbage.BabbageTxOut addr value _datum refScript) =
  ( W.TxOut (fromShelleyAddress addr)
      $ fromCardanoValue
      $ Cardano.fromMaryValue value
  , case refScript of
      SJust s -> Just s
      SNothing -> Nothing
  )

-- Lovelace to coin. Quantities from ledger should always fit in Word64.
fromCardanoLovelace :: HasCallStack => Cardano.Lovelace -> W.Coin
fromCardanoLovelace =
  Coin.unsafeFromIntegral . unQuantity . Cardano.lovelaceToQuantity
  where
    unQuantity (Cardano.Quantity q) = q

fromCardanoValue :: HasCallStack => Cardano.Value -> TokenBundle.TokenBundle
fromCardanoValue = uncurry TokenBundle.fromFlatList . extract
  where
    extract value =
      ( fromCardanoLovelace $ Cardano.selectLovelace value
      , mkBundle $ Cardano.valueToList value
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
      [ (TokenBundle.AssetId (mkPolicyId p) (mkTokenName n), mkQuantity q)
      | (Cardano.AssetId p n, Cardano.Quantity q) <- assets
      ]

    mkPolicyId = W.UnsafeTokenPolicyId . W.Hash . Cardano.serialiseToRawBytes
    mkTokenName = W.UnsafeTokenName . Cardano.serialiseToRawBytes

fromByronTxOut :: Byron.TxOut -> W.TxOut
fromByronTxOut (Byron.TxOut addr coin) =
  W.TxOut
    { W.address = W.Address (serialize' addr)
    , W.tokens = TokenBundle.fromCoin $ Coin.fromWord64 $ unsafeGetLovelace coin
    }
