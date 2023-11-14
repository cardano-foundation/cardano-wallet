-- | Assessing sizes of token bundles
module Internal.Cardano.Write.Tx.Balance.TokenBundleSize
    ( TokenBundleSizeAssessor (..)
    , mkTokenBundleSizeAssessor
    , computeTokenBundleSerializedLengthBytes
    )
    where

import Prelude

import Cardano.CoinSelection.Size
    ( TokenBundleSizeAssessment (..)
    , TokenBundleSizeAssessor (..)
    )
import Cardano.Ledger.Api
    ( ppMaxValSizeL
    , ppProtocolVersionL
    )
import Cardano.Ledger.BaseTypes
    ( ProtVer (pvMajor)
    )
import Cardano.Ledger.Binary
    ( serialize
    )
import Cardano.Wallet.Primitive.Types.Tx.Constraints
    ( TxSize (..)
    )
import Control.Lens
    ( (^.)
    )
import Data.IntCast
    ( intCastMaybe
    )
import Internal.Cardano.Write.Tx
    ( PParams
    , RecentEra
    , Value
    , Version
    , withConstraints
    )

import qualified Cardano.Api.Shelley as CardanoApi
import qualified Cardano.Wallet.Primitive.Types.TokenBundle as W
    ( TokenBundle
    )
import qualified Cardano.Wallet.Shelley.Compatibility.Ledger as Convert
import qualified Data.ByteString.Lazy as BL

-- | Assesses a token bundle size in relation to the maximum size that can be
--   included in a transaction output.
--
-- See 'TokenBundleSizeAssessor' for the expected properties of this function.
--
mkTokenBundleSizeAssessor
    :: RecentEra era
    -> PParams (CardanoApi.ShelleyLedgerEra era)
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
    :: W.TokenBundle
    -> Version
    -> TxSize
computeTokenBundleSerializedLengthBytes tb ver = serSize (Convert.toLedger tb)
  where
    serSize :: Value -> TxSize
    serSize v = maybe err TxSize
        . intCastMaybe
        . BL.length
        $ serialize ver v
      where
        err = error $ "negative serialized size of value: " <> show v
