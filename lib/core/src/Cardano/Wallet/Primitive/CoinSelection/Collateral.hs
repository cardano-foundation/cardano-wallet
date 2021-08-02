{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- For a UTxO to be considered a suitable collateral input, it must:
--    - Be a pure ADA UTxO (no tokens)
--    - Require a verification key witness to be spent
--    - Have an output address that is not any of:
--      - a native script address
--      - a bootstrap address
--      - a plutus script address
--
-- UTxOs of this kind are sometimes referred to as "VK" inputs.

module Cardano.Wallet.Primitive.CoinSelection.Collateral
    ( classifyCollateralAddress
    , pureAdaValue
    , asCollateral
    ) where

import Prelude

import Cardano.Wallet.Primitive.Types.Address
    ( Address (..) )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin )
import Cardano.Wallet.Primitive.Types.TokenBundle
    ( TokenBundle )
import Cardano.Wallet.Primitive.Types.Tx
    ( TxIn (..), TxOut (..) )

import qualified Cardano.Ledger.Address as L
import qualified Cardano.Ledger.Credential as L
import qualified Cardano.Ledger.Crypto as L
import qualified Cardano.Ledger.Hashes as L
import qualified Cardano.Ledger.Keys as L
import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle

-- | If the given @(TxIn, TxOut)@ represents a UTxO that is suitable for use as
-- a collateral input, returns @Just@ along with the total ADA value of the
-- UTxO. Otherwise returns @Nothing@ if it is not a suitable collateral value.
asCollateral
    :: (TxIn, TxOut)
    -- ^ TxIn, TxOut representing the value of a UTxO
    -> Maybe Coin
    -- ^ The total ADA value of that UTxO if it is suitable for collateral,
    -- otherwise Nothing.
asCollateral (_txIn, txOut) = do
   coin <- pureAdaValue $ tokens txOut

   case classifyCollateralAddress (address txOut) of
     Left (IsABootstrapAddr _) ->
         Nothing
     Left (IsAScriptAddr _) ->
         Nothing
     Left (IsAMalformedOrUnknownAddr) ->
         Nothing
     Right _addr ->
         Just coin

-- | Returns the total ADA value of the TokenBundle iff the TokenBundle only
-- contains ADA and no other tokens.
pureAdaValue :: TokenBundle -> Maybe Coin
pureAdaValue toks
    | TokenBundle.isCoin toks
        = Just $ TokenBundle.coin toks
    | otherwise
        = Nothing

-- | Reasons why an address might be considered unsuitable for a collateral
-- input.
data AddrNotSuitableForCollateral
    = IsABootstrapAddr (L.BootstrapAddress L.StandardCrypto)
    -- ^ The address is a bootstrap address
    | IsAScriptAddr (L.ScriptHash L.StandardCrypto)
    -- ^ The address is some form of script address
    | IsAMalformedOrUnknownAddr
    -- ^ The address could not be parsed
    deriving (Eq, Show)

-- | Analyze an address to determine if it's funds are suitable for use as a
-- collateral input.
classifyCollateralAddress
    :: Address
    -> Either AddrNotSuitableForCollateral Address
classifyCollateralAddress addr =
    case L.deserialiseAddr addrBytes of
        -- If we couldn't deserialise the address, it's either a malformed
        -- address or an address the Ledger doesn't know about.
        Nothing ->
            Left IsAMalformedOrUnknownAddr

        -- This is a bootstrap address, therefore not a suitable collateral
        -- input.
        Just (L.AddrBootstrap bootstrapAddr) ->
            Left $ IsABootstrapAddr bootstrapAddr

        -- Otherwise, we further analyze the address.
        Just (L.Addr _network payCred _stakeRef) ->
            case (payCred :: L.Credential 'L.Payment L.StandardCrypto) of
                -- Check if this is a script address.
                L.ScriptHashObj scriptHash ->
                    -- This is a native script address or a Plutus script
                    -- address, therefore not a suitable collateral input.
                    Left $ IsAScriptAddr scriptHash

                -- Otherwise, this is an address that corresponds with a
                -- suitable collateral input
                L.KeyHashObj (_keyHash :: L.KeyHash 'L.Payment L.StandardCrypto) ->
                    Right addr

    where
        addrBytes = unAddress addr
