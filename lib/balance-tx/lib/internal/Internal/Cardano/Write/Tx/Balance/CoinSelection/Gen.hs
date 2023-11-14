module Internal.Cardano.Write.Tx.Balance.CoinSelection.Gen
    ( coarbitraryWalletUTxO
    , genWalletUTxO
    , genWalletUTxOFunction
    , genWalletUTxOLargeRange
    , shrinkWalletUTxO
    )
    where

import Prelude

import Generics.SOP
    ( NP (..)
    )
import Internal.Cardano.Write.Tx.Balance.CoinSelection
    ( WalletUTxO (..)
    )
import Test.QuickCheck
    ( Gen
    , coarbitrary
    )
import Test.QuickCheck.Extra
    ( genFunction
    , genSized2
    , genericRoundRobinShrink
    , (<:>)
    , (<@>)
    )

import qualified Cardano.Wallet.Primitive.Types.Address.Gen as W
import qualified Cardano.Wallet.Primitive.Types.Tx.TxIn.Gen as W

--------------------------------------------------------------------------------
-- Wallet UTxO identifiers chosen according to the size parameter
--------------------------------------------------------------------------------

coarbitraryWalletUTxO :: WalletUTxO -> Gen a -> Gen a
coarbitraryWalletUTxO = coarbitrary . show

genWalletUTxO :: Gen WalletUTxO
genWalletUTxO = uncurry WalletUTxO <$> genSized2 W.genTxIn W.genAddress

shrinkWalletUTxO :: WalletUTxO -> [WalletUTxO]
shrinkWalletUTxO = genericRoundRobinShrink
    <@> W.shrinkTxIn
    <:> W.shrinkAddress
    <:> Nil

genWalletUTxOFunction :: Gen a -> Gen (WalletUTxO -> a)
genWalletUTxOFunction = genFunction coarbitraryWalletUTxO

--------------------------------------------------------------------------------
-- Wallet UTxO identifiers chosen from a large range
--------------------------------------------------------------------------------

genWalletUTxOLargeRange :: Gen WalletUTxO
genWalletUTxOLargeRange = WalletUTxO <$> W.genTxInLargeRange <*> W.genAddress
