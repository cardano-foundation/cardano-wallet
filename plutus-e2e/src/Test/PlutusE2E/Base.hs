{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ViewPatterns        #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:debug-context #-}

module Test.PlutusE2E.Base where

import Data.Aeson
    ( (.=) )
import Data.Generics.Internal.VL.Lens
    ( view, (^.) )
import Data.List.NonEmpty (NonEmpty(..))
import Codec.Serialise (serialise)
import Control.Monad.IO.Class (MonadIO)
import Cardano.Wallet.Primitive.Types.Hash (Hash(..))
import Data.Quantity
    ( Quantity (..) )
import Cardano.Wallet.Api.Types (ApiWallet, DecodeAddress, DecodeStakeAddress, EncodeStakeAddress, EncodeAddress, ApiCoinSelection, WalletStyle(..), AddressAmount(..), ApiT(..), ApiWalletInput(..))
import Control.Monad.Trans.Resource (ResourceT, runResourceT, MonadUnliftIO)
import Ledger
    ( POSIXTime, PaymentPubKeyHash
    )
import Cardano.Wallet.Primitive.Types.TokenPolicy
import Cardano.Wallet.Primitive.Types.TokenQuantity (TokenQuantity(..))
import Cardano.Wallet.Shelley
    ( SomeNetworkDiscriminant (..)
    , Tracers
    , serveWallet
    , setupTracers
    , tracerSeverities
    )
import Test.Hspec.Core.Spec
    ( Spec, SpecWith, describe, parallel, sequential )
import Test.Hspec.Extra
    ( aroundAll, hspecMain, it )
import Data.IORef
    ( IORef, atomicModifyIORef', newIORef )
import Ledger.Constraints.OffChain
    ( tx )
import Data.Proxy (Proxy(..))
import UnliftIO.Exception
    ( fromEither )
import Cardano.Wallet.Unsafe
    ( unsafeMkMnemonic )
import Cardano.Mnemonic
    ( Mnemonic(..), SomeMnemonic (..))
import Plutus.Contracts.Crowdfunding (Campaign(..), CampaignAction(..), mkValidator)
import PlutusTx.Prelude hiding (Applicative (..), Semigroup (..), return, (<$>), (>>), (>>=), error)
import Prelude (Semigroup (..))
import Control.Arrow
    ( first, second )
import Cardano.Wallet.Primitive.AddressDerivation
    ( PaymentAddress, HardDerivation (..), Role(..), getRawKey, publicKey)
import Cardano.Wallet.Primitive.AddressDerivation.Icarus
    ( IcarusKey )
import Control.Tracer
    ( Tracer (..), contramap, traceWith )
import Cardano.CLI
    ( LogOutput (..)
    , Port (..)
    )
import Network.HTTP.Client
    ( defaultManagerSettings
    , managerResponseTimeout
    , newManager
    , responseTimeoutMicro
    )

import qualified Test.Integration.Framework.DSL as DSL
import qualified Data.Text as T
import qualified Data.Vector as Vector
import qualified Data.ByteString.Lazy as LBS
import qualified Codec.CBOR.Write as CBORWrite
import qualified Codec.Serialise.Class as Serialise
import qualified Cardano.Wallet.Primitive.AddressDerivation.Shelley as Shelley
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Base16 as Base16
import qualified Data.List.NonEmpty as NE
import qualified Data.HashMap.Strict as HM
import qualified Data.Text.Encoding as T
import qualified Ledger.Constraints as Constraints
import qualified Cardano.Wallet.Primitive.Types.TokenMap as TokenMap
import qualified Ledger.Interval as Interval
import qualified Ledger.Typed.Scripts as Scripts hiding
    ( validatorHash )
import qualified Plutus.V1.Ledger.Value as Plutus
import qualified Plutus.V1.Ledger.Ada as Plutus
import qualified PlutusTx
import qualified Prelude as Haskell
import qualified Ledger as Plutus
import qualified Cardano.Crypto.Wallet
import qualified PlutusTx.AssocMap as AssocMap
import qualified Data.Map.Strict as Map

data Crowdfunding
instance Scripts.ValidatorTypes Crowdfunding where
    type instance RedeemerType Crowdfunding = CampaignAction
    type instance DatumType Crowdfunding = PaymentPubKeyHash

typedValidator :: Campaign -> Scripts.TypedValidator Crowdfunding
typedValidator = Scripts.mkTypedValidatorParam @Crowdfunding
    $$(PlutusTx.compile [|| mkValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = Scripts.wrapValidator

-- | A sample campaign
theCampaign :: Actor -> POSIXTime -> Campaign
theCampaign owner startTime = Campaign
    { campaignDeadline = startTime + 20000
    , campaignCollectionDeadline = startTime + 30000
    , campaignOwner = actorPaymentPublicKeyHash owner
    }

-- newActor :: Context -> Actor
-- actorWallet :: Actor -> Wallet
-- actorWalletMnemonics :: Actor -> Mnemonic 15
-- actorWalletRootKey :: Actor -> k 'RootK XPrv
-- actorPaymentPublicKey :: Actor -> Plutus.PaymentPublicKey
-- actorPaymentPublicKeyHash :: Actor -> Plutus.PaymentPublicKeyHash
-- actorPaymentPrivateKey :: Actor -> Plutus.PaymentPrivateKey

data Actor = Actor { actorWallet :: ApiWallet
                   , actorMnemonic :: Mnemonic 15
                   }

newActor :: MonadIO m => DSL.Context -> ResourceT m Actor
newActor ctx = do
    (w, mw) <- second (unsafeMkMnemonic @15) Haskell.<$> DSL.fixtureWalletWithMnemonics (Proxy @"shelley") ctx
    Haskell.pure $ Actor w mw

actorPaymentPublicKeyHash :: Actor -> Plutus.PaymentPubKeyHash
actorPaymentPublicKeyHash a =
    let
        mnemonic = actorMnemonic a
        pw = Haskell.mempty
        rootSk = Shelley.generateKeyFromSeed (SomeMnemonic mnemonic, Nothing) pw
        acctSk = deriveAccountPrivateKey pw rootSk Haskell.minBound
        addrSk = getRawKey $ deriveAddressPrivateKey pw acctSk UtxoExternal Haskell.minBound
    in
        Plutus.PaymentPubKeyHash . Plutus.pubKeyHash . Plutus.toPublicKey $ addrSk

getActorFreshUTxO
    :: forall n m
    . ( MonadIO m
      , DecodeAddress n
      , EncodeAddress n
      , DecodeStakeAddress n
      , MonadUnliftIO m
      )
    => DSL.Context -> Actor -> ResourceT m (ApiWalletInput n)
getActorFreshUTxO ctx a = do
    let
        w = actorWallet a
    (addr,proxy) <- view #id . head Haskell.<$> DSL.listAddresses @n ctx w
    let getFreshUTxO = do
            -- To obtain a fresh UTxO, we perform
            -- coin selection and just pick the first input
            -- that has been selected.
            let singleton = Haskell.pure :: a -> NonEmpty a
            (_, result) <- DSL.selectCoins @_ @'Shelley ctx w $
                singleton $ AddressAmount
                    { address = (addr, proxy)
                    , amount  = Quantity 10_000_000
                    , assets  = ApiT TokenMap.empty
                    }
            Haskell.pure $ head . view #inputs Haskell.<$> result
    txOutRef <- fromEither =<< getFreshUTxO
    Haskell.pure txOutRef

spec :: forall n.
    ( DecodeAddress n
    , DecodeStakeAddress n
    , EncodeStakeAddress n
    , EncodeAddress n
    , PaymentAddress n IcarusKey
    ) => SpecWith DSL.Context
spec = describe "Plutus E2E" $ do
    it "does things" $ \ctx -> runResourceT $ do
        alice   <- newActor ctx
        bob     <- newActor ctx
        charlie <- newActor ctx

        -- txId <- getApiT Haskell.<$> view #id Haskell.<$> getActorFreshUTxO @n ctx alice

        let
            campaign :: Campaign
            campaign = theCampaign alice 10000

            contributionBob :: Constraints.UnbalancedTx
            contributionBob =
                contribute campaign
                           (actorPaymentPublicKeyHash bob)
                           (Plutus.toValue $ Plutus.Lovelace 10_000)

        submitUnbalancedTx bob contributionBob

        Haskell.pure ()

submitUnbalancedTx actor unbalancedTx = do
    let
        w = actorWallet actor

        outs = Plutus.txOutputs (unbalancedTx ^. tx)
        outsValue = foldMap Plutus.txOutValue outs

        adaQty = Plutus.fromValue outsValue
        nonAdaQty = AssocMap.fromList $ filter (\(k,v) -> k /= Plutus.adaSymbol) $ AssocMap.toList $ Plutus.getValue outsValue

        x :: AssocMap.Map Plutus.CurrencySymbol (AssocMap.Map Plutus.TokenName Integer) -> TokenMap.TokenMap
        x = TokenMap.fromNestedList . fmap (second NE.fromList) . fmap (first y) . fmap (fmap (fmap w')) . AssocMap.toList . fmap (AssocMap.toList)

        y :: Plutus.CurrencySymbol -> TokenPolicyId
        y = UnsafeTokenPolicyId . Hash . LBS.toStrict . serialise

        z :: Plutus.TokenName -> TokenName
        z = UnsafeTokenName . LBS.toStrict . serialise

        w' :: (Plutus.TokenName, Integer) -> (TokenName, TokenQuantity)
        w' (name, qty) = (z name, TokenQuantity $ Haskell.fromIntegral qty)

        addrAmt = AddressAmount
            { address = Haskell.undefined
            , amount = Quantity $ Haskell.fromIntegral $ Plutus.getLovelace adaQty
            , assets = ApiT $ x nonAdaQty
            }

    Haskell.undefined

    -- (addr,proxy) <- view #id . head Haskell.<$> DSL.listAddresses @n ctx w
    -- let getFreshUTxO = do
    --         -- To obtain a fresh UTxO, we perform
    --         -- coin selection and just pick the first input
    --         -- that has been selected.
    --         let singleton = Haskell.pure :: a -> NonEmpty a
    --         (_, result) <- DSL.selectCoins @_ @'Shelley ctx w $
    --             singleton $ AddressAmount
    --                 { address = (addr, proxy)
    --                 , amount  = Quantity 10_000_000
    --                 , assets  = ApiT TokenMap.empty
    --                 }
    --         Haskell.pure $ head . view #inputs Haskell.<$> result
    -- txOutRef <- fromEither =<< getFreshUTxO

    -- let
    --     unbalancedTxHex =
    --         T.decodeUtf8
    --         . Base16.encode
    --         . CBORWrite.toStrictByteString
    --         . Serialise.encode
    --         $ unbalancedTx ^. tx

        -- apiData = Aeson.Object $ HM.fromList
        --     [ "transaction" .= Aeson.String unbalancedTxHex
        --     , "inputs" .= Aeson.toJSON [ Aeson.object
        --         [ "id" .= view #id input
        --         , "index" .= view #index input
        --         , "address" .= view #address input
        --         , "amount" .= view #amount input
        --         , "assets" .= Aeson.Array Vector.empty
        --         ] ]
        --    -- The contribution action does not try to spend a UTxO at a script
        --    -- address, it only puts funds to the script address, so datum and
        --    -- redeemer are not required.
        --    , "redeemers" .= (Aeson.toJSON ([] :: [()]))
        --    ]

     _

contribute :: Campaign -> PaymentPubKeyHash -> Plutus.Value -> Constraints.UnbalancedTx
contribute cmp contributor value =
    let
        inst = typedValidator cmp

        constraints
            = Constraints.mustPayToTheScript contributor value
            <> Constraints.mustValidateIn (Interval.to (campaignDeadline cmp))

        unbalancedTx =
            either (Haskell.error "Failed to create Tx from constraints") Haskell.id
            $ Constraints.mkTx
                (Constraints.typedValidatorLookups inst)
                constraints

        -- import Codec.CBOR.Write qualified as Write
        -- import Codec.Serialise.Class (Serialise, encode)
        -- unbalancedTxHex =
        --     T.decodeUtf8
        --     . Base16.encode
        --     . CBORWrite.toStrictByteString
        --     . Serialise.encode
        --     $ unbalancedTx ^. tx
    in
        unbalancedTx
        -- Aeson.Object $ HM.fromList
        --   [ "transaction" .= Aeson.String unbalancedTxHex
        --   , "inputs" .= Aeson.toJSON [ Aeson.object
        --       [ "id" .= view #id input
        --       , "index" .= view #index input
        --       , "address" .= view #address input
        --       , "amount" .= view #amount input
        --       , "assets" .= Aeson.Array Vector.empty
        --       ] ]
        --  -- The contribution action does not try to spend a UTxO at a script
        --  -- address, it only puts funds to the script address, so datum and
        --  -- redeemer are not required.
        --  , "redeemers" .= (Aeson.toJSON ([] :: [()]))
        --  ]

    -- Tx in UnbalancedTx has no extra required signatories.
    -- But is available in UnbalancedTx
    -- So BalanceTx first, get sealed Tx back, then add required signatories
    -- before calling signTransaction.

    -- TODO: Add reqSignerHashes to TxUpdate
    -- TODO: Change test DSL to take UnBalancedTxs to handle this natively?


            -- w <- fixtureWallet ctx
