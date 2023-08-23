{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}

-- | Assessing sizes of token bundles
module Cardano.Wallet.Write.Tx.Balance.TokenBundleSize
    ( TokenBundleSizeAssessor (..)
    , TokenBundleMaxSize (..)
    , computeTokenBundleSerializedLengthBytes
    , mkTokenBundleSizeAssessor
    , getTokenBundleMaxSize
    )
    where

import Prelude

import Cardano.CoinSelection.Size
    ( TokenBundleSizeAssessment (..), TokenBundleSizeAssessor (..) )
import Cardano.Ledger.Api
    ( ppMaxValSizeL )
import Cardano.Ledger.Binary
    ( serialize', shelleyProtVer )
import Cardano.Wallet.Primitive.Types.Tx.Constraints
    ( TxSize (..) )
import Cardano.Wallet.Shelley.Compatibility
    ( toCardanoValue )
import Cardano.Wallet.Write.Tx
    ( PParams, RecentEra, ShelleyLedgerEra, withConstraints )
import Control.DeepSeq
    ( NFData )
import Control.Lens
    ( (^.) )
import GHC.Generics
    ( Generic )
import Numeric.Natural
    ( Natural )

import qualified Cardano.Api.Shelley as Cardano
import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle
import qualified Data.ByteString as BS

-- | The maximum size of a serialized 'TokenBundle'.
-- ('_maxValSize' in the Alonzo ledger)
newtype TokenBundleMaxSize = TokenBundleMaxSize
    { unTokenBundleMaxSize :: TxSize }
    deriving (Eq, Generic, Show)

instance NFData TokenBundleMaxSize

-- | Assesses a token bundle size in relation to the maximum size that can be
--   included in a transaction output.
--
-- See 'W.TokenBundleSizeAssessor' for the expected properties of this function.
--
mkTokenBundleSizeAssessor :: TokenBundleMaxSize -> TokenBundleSizeAssessor
mkTokenBundleSizeAssessor maxSize =
    TokenBundleSizeAssessor { assessTokenBundleSize }
  where
    assessTokenBundleSize tb
        | serializedLengthBytes <= maxSize' =
            TokenBundleSizeWithinLimit
        | otherwise =
            TokenBundleSizeExceedsLimit
      where
        serializedLengthBytes :: TxSize
        serializedLengthBytes = computeTokenBundleSerializedLengthBytes tb

        maxSize' :: TxSize
        maxSize' = unTokenBundleMaxSize maxSize

computeTokenBundleSerializedLengthBytes :: TokenBundle.TokenBundle -> TxSize
computeTokenBundleSerializedLengthBytes =
    TxSize
        . safeCast
        . BS.length
        . serialize' shelleyProtVer
        . Cardano.toMaryValue
        . toCardanoValue
  where
    safeCast :: Int -> Natural
    safeCast = fromIntegral

-- | Get a 'TokenBundleMaxSize' from a 'PParam era'.
getTokenBundleMaxSize
    :: RecentEra era
    -> PParams (ShelleyLedgerEra era)
    -> TokenBundleMaxSize
getTokenBundleMaxSize era pp = withConstraints era $
    TokenBundleMaxSize $ TxSize $ pp ^. ppMaxValSizeL
