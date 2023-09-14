-- | Assessing sizes of token bundles
module Cardano.Wallet.Write.Tx.Balance.TokenBundleSize
    ( TokenBundleSizeAssessor (..)
    , mkTokenBundleSizeAssessor
    , computeTokenBundleSerializedLengthBytes
    )
    where

import Prelude

import Cardano.CoinSelection.Size
    ( TokenBundleSizeAssessment (..), TokenBundleSizeAssessor (..) )
import Cardano.Ledger.Api
    ( StandardCrypto, ppMaxValSizeL, ppProtocolVersionL )
import Cardano.Ledger.BaseTypes
    ( ProtVer (pvMajor) )
import Cardano.Ledger.Binary
    ( serialize )
import Cardano.Wallet.Primitive.Types.Tx.Constraints
    ( TxSize (..) )
import Cardano.Wallet.Shelley.Compatibility.Ledger
    ( toLedger )
import Cardano.Wallet.Write.Tx
    ( PParams, RecentEra, ShelleyLedgerEra, Value, Version, withConstraints )
import Control.Lens
    ( (^.) )
import Data.IntCast
    ( intCastMaybe )

import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle
import qualified Data.ByteString.Lazy as BL

-- | Assesses a token bundle size in relation to the maximum size that can be
--   included in a transaction output.
--
-- See 'TokenBundleSizeAssessor' for the expected properties of this function.
--
mkTokenBundleSizeAssessor
    :: RecentEra era
    -> PParams (ShelleyLedgerEra era)
    -> TokenBundleSizeAssessor
mkTokenBundleSizeAssessor era pp = TokenBundleSizeAssessor $ \tb ->
    if computeTokenBundleSerializedLengthBytes tb ver > maxValSize
    then TokenBundleSizeExceedsLimit
    else TokenBundleSizeWithinLimit
  where
    maxValSize :: TxSize
    maxValSize = TxSize $ withConstraints era $ pp ^. ppMaxValSizeL

    ver :: Version
    ver = withConstraints era $ pvMajor $ pp ^. ppProtocolVersionL

computeTokenBundleSerializedLengthBytes
    :: TokenBundle.TokenBundle
    -> Version
    -> TxSize
computeTokenBundleSerializedLengthBytes tb ver = serSize (toLedger tb)
  where
    serSize :: Value StandardCrypto -> TxSize
    serSize v = maybe err TxSize
        . intCastMaybe
        . BL.length
        $ serialize ver v
      where
        err = error $ "negative serialized size of value: " <> show v
