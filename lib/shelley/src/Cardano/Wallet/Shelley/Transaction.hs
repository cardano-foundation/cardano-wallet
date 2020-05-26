{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Copyright: © 2020 IOHK
-- License: Apache-2.0
--
-- Working with Shelley transactions.

module Cardano.Wallet.Shelley.Transaction
    ( newTransactionLayer
    ) where

import Prelude

import Cardano.Address.Derivation
    ( XPrv )
import Cardano.Binary
    ( serialize' )
import Cardano.Crypto.DSIGN
    ( DSIGNAlgorithm (..), SignedDSIGN (..) )
import Cardano.Crypto.DSIGN.Ed25519
import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (..), NetworkDiscriminant (..), Passphrase, WalletKey (..) )
import Cardano.Wallet.Primitive.CoinSelection
    ( CoinSelection (..) )
import Cardano.Wallet.Primitive.Types
    ( Address (..)
    , Coin (..)
    , EpochLength (..)
    , ProtocolMagic (..)
    , SealedTx
    , SlotId (..)
    , Tx (..)
    , TxIn
    , TxOut (..)
    )
import Cardano.Wallet.Shelley.Compatibility
    ( Shelley
    , TPraosStandardCrypto
    , toCardanoLovelace
    , toCardanoTxIn
    , toCardanoTxOut
    , toSealed
    , toSlotNo
    )
import Cardano.Wallet.Shelley.Transaction.Size
    ( sizeOfSignedTx )
import Cardano.Wallet.Transaction
    ( ErrMkTx (..), ErrValidateSelection, TransactionLayer (..) )
import Crypto.Error
    ( throwCryptoError )
import Data.ByteString
    ( ByteString )
import Data.Maybe
    ( fromMaybe )
import Data.Proxy
    ( Proxy )
import Data.Quantity
    ( Quantity (..) )
import Data.Word
    ( Word16, Word8 )
import Fmt
    ( Buildable (..) )
import GHC.Stack
    ( HasCallStack )

import qualified Cardano.Api as Cardano
import qualified Cardano.Crypto.Wallet as CC
import qualified Crypto.PubKey.Ed25519 as Ed25519
import qualified Data.ByteString as BS
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Shelley.Spec.Ledger.BaseTypes as SL
import qualified Shelley.Spec.Ledger.Keys as SL
import qualified Shelley.Spec.Ledger.Tx as SL
import qualified Shelley.Spec.Ledger.UTxO as SL

newTransactionLayer
    :: forall (n :: NetworkDiscriminant) k t.
        ( t ~ IO Shelley
        , WalletKey k
        )
    => Proxy n
    -> ProtocolMagic
    -> EpochLength
    -> TransactionLayer t k
newTransactionLayer _proxy _protocolMagic epochLength = TransactionLayer
    { mkStdTx = _mkStdTx
    , mkDelegationJoinTx = notImplemented "mkDelegationJoinTx"
    , mkDelegationQuitTx = notImplemented "mkDelegationQuitTx"
    , decodeSignedTx = notImplemented "decodeSignedTx"
    , estimateSize = _estimateSize
    , estimateMaxNumberOfInputs = _estimateMaxNumberOfInputs
    , validateSelection = const $ return ()
    , allowUnbalancedTx = True
    }
  where
    _mkStdTx
        :: (Address -> Maybe (k 'AddressK XPrv, Passphrase "encryption"))
        -> SlotId -- ^ The current slot
        -> [(TxIn, TxOut)]
        -> [TxOut]
        -> Either ErrMkTx (Tx, SealedTx)
    _mkStdTx keyFrom slot ownedIns outs = do
        let ins' = map (toCardanoTxIn . fst) ownedIns
        let outs' = map toCardanoTxOut outs
        let fee = Coin $ sum (map (getCoin . coin . snd) ownedIns)
                - sum (map (getCoin . coin) outs)
        let certs = []

        -- TODO: The SlotId-SlotNo conversion based on epoch length would not
        -- work if the epoch length changed in a hard fork.
        let timeToLive = (toSlotNo epochLength slot) + 7200
        let Cardano.TxUnsignedShelley unsigned = Cardano.buildShelleyTransaction
                ins'
                outs'
                timeToLive
                (toCardanoLovelace fee)
                certs
                Nothing -- update
        let bytes = serialize' (SL.hashTxBody unsigned)
        keyWits <- mapM (fmap (signBody bytes) . lookupPrivateKey . address . snd) ownedIns
        let stx = SL.Tx
                unsigned
                (Set.fromList keyWits)
                Map.empty    -- script witnesses
                SL.SNothing  -- metadata
        return $ toSealed stx
      where
        lookupPrivateKey
            :: Address
            -> Either ErrMkTx (k 'AddressK XPrv, Passphrase "encryption")
        lookupPrivateKey addr =
            maybe (Left $ ErrKeyNotFoundForAddress addr) Right (keyFrom addr)

        signBody :: ByteString -> (k 'AddressK XPrv, Passphrase "encryption") -> SL.WitVKey TPraosStandardCrypto
        signBody body k =
            let
                sig = SignedDSIGN $ fromMaybe (error "error converting signatures")
                        $ rawDeserialiseSigDSIGN
                        $ k `sign` body
                vKey = SL.VKey $ VerKeyEd25519DSIGN $  throwCryptoError $ Ed25519.publicKey $ BS.take 32 $ CC.unXPub $ CC.toXPub $ getRawKey $ fst k
            in
                SL.WitVKey vKey sig

        sign
            :: (k 'AddressK XPrv, Passphrase "encryption")
            -> ByteString
            -> ByteString
        sign (k, pass) = CC.unXSignature . CC.sign pass (getRawKey k)


    _estimateSize
        :: CoinSelection
        -> Quantity "byte" Int
    _estimateSize (CoinSelection inps outs chngs) = Quantity $
        sizeOfSignedTx (fst <$> inps) (outs <> map dummyOutput chngs)
      where
        dummyOutput :: Coin -> TxOut
        dummyOutput = TxOut $ Address "\130\ACKX \217\NULYA\205=\179d\160d\203\232\SYN\138C\SOH\\MGO\141\201\228\161\138\146\136\179\235M\217v"

    _estimateMaxNumberOfInputs
        :: Quantity "byte" Word16
        -- ^ Transaction max size in bytes
        -> Word8
        -- ^ Number of outputs in transaction
        -> Word8
    _estimateMaxNumberOfInputs _ _ =
        -- FIXME Implement.
        100

--------------------------------------------------------------------------------
-- Extra validations on coin selection
--

-- | Transaction with 0 output amount is tried
data ErrInvalidTxOutAmount -- FIXME: = ErrInvalidTxOutAmount

instance Buildable ErrInvalidTxOutAmount where
    build _ = "Invalid coin selection: at least one output is null."

type instance ErrValidateSelection (IO Shelley) = ErrInvalidTxOutAmount

notImplemented :: HasCallStack => String -> a
notImplemented what = error ("Not implemented: " <> what)
