module Cardano.Wallet.Deposit.Pure.State.Signing
    ( getBIP32PathsForOwnedInputs
    , signTx
    , Passphrase
    ) where

import Prelude

import Cardano.Crypto.Wallet
    ( xPrvChangePass
    )
import Cardano.Wallet.Address.BIP32
    ( BIP32Path (..)
    )
import Cardano.Wallet.Address.BIP32_Ed25519
    ( deriveXPrvBIP32Path
    )
import Cardano.Wallet.Deposit.Pure.State.Submissions
    ( availableUTxO
    )
import Cardano.Wallet.Deposit.Pure.State.Type
    ( WalletState (..)
    )
import Data.Maybe
    ( mapMaybe
    )
import Data.Set
    ( Set
    )
import Data.Text
    ( Text
    )

import qualified Cardano.Wallet.Deposit.Pure.Address as Address
import qualified Cardano.Wallet.Deposit.Pure.UTxO as UTxO
import qualified Cardano.Wallet.Deposit.Read as Read
import qualified Cardano.Wallet.Deposit.Write as Write
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as T

getBIP32PathsForOwnedInputs :: Write.Tx -> WalletState -> [BIP32Path]
getBIP32PathsForOwnedInputs tx w =
    getBIP32Paths w $ resolveInputAddresses inputs
  where
    inputs = Read.getInputs tx <> Read.getCollateralInputs tx

    resolveInputAddresses :: Set Read.TxIn -> [Read.Address]
    resolveInputAddresses ins =
        map (Read.address . snd)
            . UTxO.toList
            $ UTxO.restrictedBy (availableUTxO w) ins

getBIP32Paths :: WalletState -> [Read.Address] -> [BIP32Path]
getBIP32Paths w =
    mapMaybe $ Address.getBIP32Path (addresses w)

type Passphrase = Text

-- | Sign the transaction if 'rootXSignKey' is 'Just'.
signTx :: Write.Tx -> Passphrase -> WalletState -> Maybe Write.Tx
signTx tx passphrase w = signTx' <$> rootXSignKey w
  where
    signTx' encryptedXPrv =
        foldr Write.addSignature tx keys
      where
        unencryptedXPrv =
            xPrvChangePass
                (T.encodeUtf8 passphrase)
                BS.empty
                encryptedXPrv
        keys = deriveXPrvBIP32Path unencryptedXPrv
            <$> getBIP32PathsForOwnedInputs tx w
