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
    , fromConsensusBlock
    , (:.:) (Comp)
    )
import Cardano.Wallet.Read.Block.Txs
    ( getEraTransactions
    )
import Cardano.Wallet.Read.Eras
    ( K (..)
    , applyEraFun
    , extractEraValue
    )
import Cardano.Wallet.Read.Eras.EraFun
    ( EraFun
    , mkEraFunK
    , runEraFun
    , runEraFunK
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
    :: W.Hash "Genesis"
    -> EraFun Block (K (W.Block, [W.PoolCertificate]))
primitiveBlock hg = mkEraFunK $ do
    header <- runEraFunK $ primitiveBlockHeader hg
    (transactions, certificates) <- unzip <$> runEraFunK getTxsAndCertificates
    let (delegations, pools) = pickWalletCertificates $ concat certificates
    pure
        ( W.Block header transactions delegations
        , pools
        )

getTxsAndCertificates :: EraFun Block (K [(W.Tx, [W.Certificate])])
getTxsAndCertificates = mkEraFunK $ \block ->
    let Comp txs = runEraFun getEraTransactions block
        ptxs = runEraFunK primitiveTx <$> txs
        pcts = runEraFunK primitiveCertificates
            . runEraFun getEraCertificates <$> txs
    in zip ptxs pcts

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
fromCardanoBlock gp =
    let primitiveBlock' = primitiveBlock gp
    in  extractEraValue
            . applyEraFun primitiveBlock'
            . fromConsensusBlock
