{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}

module Cardano.Wallet.Primitive.Ledger.Read.Block
    ( primitiveBlock
    , fromCardanoBlock
    )
where

import Prelude hiding
    ( id
    , (.)
    )

import Cardano.Wallet.Primitive.Ledger.Read.Block.Header
    ( primitiveBlockHeader
    )
import Cardano.Wallet.Primitive.Ledger.Read.Tx
    ( primitiveTx
    )
import Cardano.Wallet.Primitive.Ledger.Read.Tx.Features.Certificates
    ( primitiveCertificates
    )
import Cardano.Wallet.Read
    ( Block
    , ConsensusBlock
    , IsEra
    , fromConsensusBlock
    , (:.:) (Comp)
    )
import Cardano.Wallet.Read.Block.Txs
    ( getEraTransactions
    )
import Cardano.Wallet.Read.Eras
    ( applyEraFun
    )
import Cardano.Wallet.Read.Tx.Certificates
    ( getEraCertificates
    )
import Control.Category
    ( Category (..)
    )
import Control.Error
    ( partitionEithers
    )

import qualified Cardano.Wallet.Primitive.Types.Block as W
import qualified Cardano.Wallet.Primitive.Types.Certificates as W
import qualified Cardano.Wallet.Primitive.Types.Hash as W
import qualified Cardano.Wallet.Primitive.Types.Tx as W

-- | Compute a wallet primitive  'W.Block' from a ledger 'Block'
primitiveBlock
    :: IsEra era
    => W.Hash "Genesis"
    -> Block era
    -> (W.Block, [W.PoolCertificate])
primitiveBlock hg = do
    header <- primitiveBlockHeader hg
    (transactions, certificates) <- unzip <$> getTxsAndCertificates
    let (delegations, pools) = pickWalletCertificates $ concat certificates
    pure
        ( W.Block header transactions delegations
        , pools
        )

getTxsAndCertificates :: IsEra era => Block era -> [(W.Tx, [W.Certificate])]
getTxsAndCertificates block =
    let Comp txs = getEraTransactions block
        ptxs = primitiveTx <$> txs
        pcts = primitiveCertificates . getEraCertificates <$> txs
    in  zip ptxs pcts

pickWalletCertificates
    :: [W.Certificate]
    -> ([W.DelegationCertificate], [W.PoolCertificate])
pickWalletCertificates xs = partitionEithers $ do
    x <- xs
    case x of
        W.CertificateOfDelegation _ cert -> pure $ Left cert
        W.CertificateOfPool cert -> pure $ Right cert
        _otherCerts -> []

fromCardanoBlock
    :: W.Hash "Genesis"
    -> ConsensusBlock
    -> (W.Block, [W.PoolCertificate])
fromCardanoBlock gp = applyEraFun (primitiveBlock gp) . fromConsensusBlock
