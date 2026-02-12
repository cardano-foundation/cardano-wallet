{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Wallet.Read.Tx.TxOutSpec
    ( spec
    ) where

import Cardano.Ledger.Address
    ( Addr (Addr)
    )
import Cardano.Ledger.Api
    ( mkBasicTxOut
    )
import Cardano.Ledger.BaseTypes
    ( Network (Mainnet)
    )
import Cardano.Ledger.Credential
    ( Credential (KeyHashObj)
    , PaymentCredential
    , StakeReference (StakeRefNull)
    )
import Cardano.Ledger.Keys
    ( KeyHash (KeyHash)
    )
import Cardano.Read.Ledger.Tx.Output
    ( Output (..)
    )
import Cardano.Wallet.Read.Eras
    ( Era (..)
    , EraValue (..)
    , IsEra (theEra)
    , knownEras
    )
import Cardano.Wallet.Read.Hash
    ( hashFromBytesAsHex
    )
import Cardano.Wallet.Read.Tx
    ( TxOut
    , getValue
    , mkEraTxOut
    , upgradeTxOutToBabbageOrLater
    )
import Cardano.Wallet.Read.Value
    ( Coin (CoinC)
    , Value (getCoin)
    , injectCoin
    , toMaryValue
    )
import Data.ByteString
    ( ByteString
    )
import Data.Maybe
    ( fromMaybe
    )
import Test.Hspec
    ( Spec
    , describe
    , it
    )
import Test.QuickCheck
    ( Gen
    , arbitrary
    , elements
    , forAll
    , getPositive
    , vectorOf
    , (===)
    )
import Prelude

import Data.ByteString.Char8 qualified as B8

spec :: Spec
spec =
    describe "upgradeTxOutToBabbageOrLater"
        $ it "preserves getEraValue"
        $ forAll genTxOut
        $ \txout ->
            getValue txout
                === getValue (upgradeTxOutToBabbageOrLater txout)

{-----------------------------------------------------------------------------
    Generators
------------------------------------------------------------------------------}

genAddr :: Gen Addr
genAddr =
    mkAddr . mkPaymentCred . B8.pack
        <$> vectorOf (2 * 28) (elements $ ['0' .. '9'] <> ['a' .. 'f'])

genValue :: Gen Value
genValue = injectCoin . CoinC . getPositive <$> arbitrary

genNonByronEra :: Gen (EraValue Era)
genNonByronEra = case knownEras of
    (_ : rest) -> elements rest
    [] -> error "genNonByronEra: knownEras is empty"

genTxOut :: Gen TxOut
genTxOut = do
    EraValue (_ :: Era era) <- genNonByronEra
    (output :: Output era) <- mkBasicOutput <$> genAddr <*> genValue
    pure $ mkEraTxOut output

{-----------------------------------------------------------------------------
    Constructors
------------------------------------------------------------------------------}
mkBasicOutput
    :: forall era
     . IsEra era
    => Addr -> Value -> Output era
mkBasicOutput addr value = case theEra :: Era era of
    Byron -> error "not implemented"
    Shelley -> Output $ mkBasicTxOut addr (getCoin value)
    Allegra -> Output $ mkBasicTxOut addr (getCoin value)
    Mary -> Output $ mkBasicTxOut addr (toMaryValue value)
    Alonzo -> Output $ mkBasicTxOut addr (toMaryValue value)
    Babbage -> Output $ mkBasicTxOut addr (toMaryValue value)
    Conway -> Output $ mkBasicTxOut addr (toMaryValue value)

mkPaymentCred :: ByteString -> PaymentCredential
mkPaymentCred =
    KeyHashObj
        . KeyHash
        . fromMaybe (error "paymentCred: invalid hex length")
        . hashFromBytesAsHex

mkAddr :: PaymentCredential -> Addr
mkAddr x = Addr Mainnet x StakeRefNull
