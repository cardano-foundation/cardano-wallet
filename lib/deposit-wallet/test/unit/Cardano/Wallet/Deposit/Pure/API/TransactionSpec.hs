{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Cardano.Wallet.Deposit.Pure.API.TransactionSpec
    ( spec
    )
where

import Prelude

import Cardano.Ledger.Api
    ( ppMaxTxSizeL
    , ppMaxValSizeL
    )
import Cardano.Ledger.BaseTypes
    ( EpochSize (..)
    )
import qualified Cardano.Ledger.BaseTypes as Ledger
import qualified Cardano.Ledger.Core as Ledger
import qualified Cardano.Ledger.Shelley.API.Mempool as Ledger
import qualified Cardano.Ledger.Shelley.LedgerState as Ledger
import qualified Cardano.Ledger.Shelley.Rules as Ledger
import qualified Cardano.Slotting.EpochInfo as Slotting
import Cardano.Slotting.Time
    ( SlotLength
    , SystemStart (..)
    , mkSlotLength
    )
import qualified Cardano.Wallet.Deposit.Pure.Address as Address
import Cardano.Wallet.Deposit.Pure.API.Address
    ( encodeAddress
    )
import Cardano.Wallet.Deposit.Pure.State.Creation
    ( accountXPubFromCredentials
    , createMnemonicFromWords
    , credentialsFromMnemonics
    )
import Cardano.Wallet.Deposit.PureSpec
    ( testOnWallet
    )
import Cardano.Wallet.Deposit.Read
    ( Address
    , Conway
    , NetworkTag (..)
    , UTxO
    , mkEnterpriseAddress
    )
import Cardano.Wallet.Deposit.Testing.DSL
    ( assert
    , balance
    , block
    , deposit
    , existsTx
    , rollForward
    , sign
    , spend
    , utxo
    , wallet
    )
import Cardano.Wallet.Deposit.Write
    ( Tx
    )
import qualified Cardano.Wallet.Deposit.Write as Write
import Cardano.Wallet.Read
    ( NetworkId (..)
    )
import qualified Cardano.Wallet.Read as Read
import Control.Lens
    ( (&)
    , (.~)
    )
import qualified Data.ByteString.Short as SBS
import Data.Default
    ( Default (..)
    )
import Data.Maybe
    ( fromMaybe
    )
import Data.Text
    ( Text
    )
import qualified Data.Text.Lazy as TL
import Data.Time.Clock.POSIX
    ( posixSecondsToUTCTime
    )
import Test.Cardano.Ledger.Core.Arbitrary
    ()
import Test.Hspec
    ( Spec
    , describe
    , it
    , shouldBe
    )
import Text.Pretty.Simple
    ( pShow
    )

address :: Address
address = mockAddress

mockAddress :: Address
mockAddress =
    mkEnterpriseAddress
        MainnetTag
        (SBS.toShort "12345678901234567890123456789012")

defaultPParams :: Ledger.PParams Conway
defaultPParams =
    def
        & ppMaxTxSizeL .~ 16_384
        & ppMaxValSizeL .~ 1_000_000_000

-- | Create a new ledger env from given protocol parameters.
newLedgerEnv :: Ledger.PParams Conway -> Ledger.LedgerEnv Conway
newLedgerEnv protocolParams =
    Ledger.LedgerEnv
        { Ledger.ledgerSlotNo = 0
        , -- NOTE: This can probably stay at 0 forever. This is used internally by the
          -- node's mempool to keep track of transaction seen from peers. Transactions
          -- in Hydra do not go through the node's mempool and follow a different
          -- consensus path so this will remain unused.
          Ledger.ledgerIx = minBound
        , -- NOTE: This keeps track of the ledger's treasury and reserve which are
          -- both unused in Hydra. There might be room for interesting features in the
          -- future with these two but for now, we'll consider them empty.
          Ledger.ledgerAccount = Ledger.AccountState mempty mempty
        , Ledger.ledgerPp = protocolParams
        , Ledger.ledgerMempool = False
        }

defaultLedgerEnv :: Ledger.LedgerEnv Conway
defaultLedgerEnv = newLedgerEnv defaultPParams

defaultGlobals :: Ledger.Globals
defaultGlobals =
    Ledger.Globals
        { Ledger.epochInfo = Slotting.fixedEpochInfo epochSize slotLength
        , Ledger.slotsPerKESPeriod = 20
        , Ledger.stabilityWindow = 33
        , Ledger.randomnessStabilisationWindow = 33
        , Ledger.securityParameter = 10
        , Ledger.maxKESEvo = 10
        , Ledger.quorum = 5
        , Ledger.maxLovelaceSupply = 45 * 1000 * 1000 * 1000 * 1000 * 1000
        , Ledger.activeSlotCoeff =
            Ledger.mkActiveSlotCoeff . unsafeBoundRational $ 0.9
        , Ledger.networkId = Ledger.Mainnet
        , Ledger.systemStart = SystemStart $ posixSecondsToUTCTime 0
        }
  where
    unsafeBoundRational r =
        fromMaybe (error $ "Could not convert from Rational: " <> show r)
            $ Ledger.boundRational r

epochSize :: EpochSize
epochSize = EpochSize 100

slotLength :: SlotLength
slotLength = mkSlotLength 1

applyTx
    :: UTxO
    -> Write.Tx
    -> Either
        (Ledger.ApplyTxError Conway)
        ()
applyTx utxos (Read.Tx tx) =
    case Ledger.applyTx defaultGlobals defaultLedgerEnv memPoolState tx of
        Left err -> Left err
        Right _ -> Right ()
  where
    memPoolState =
        Ledger.LedgerState
            { Ledger.lsUTxOState =
                def{Ledger.utxosUtxo = Write.toConwayUTxO utxos}
            , Ledger.lsCertState = def
            }
newtype Ledger = Ledger
    { validate :: Tx -> Either (Ledger.ApplyTxError Conway) ()
    }

ledgerFrom :: UTxO -> Ledger
ledgerFrom = Ledger . applyTx

accepts :: Ledger -> Tx -> IO ()
accepts l t = case validate l t of
    Left err ->
        error
            $ TL.unpack
            $ "Transaction was not accepted by the ledger: \n"
                <> pShow defaultPParams
                <> "\n"
                <> pShow t
                <> "\n"
                <> pShow err
    Right _ -> pure ()

mnemonics :: Text
mnemonics = "vital minimum victory start lunch find city peanut shiver soft hedgehog artwork mushroom loud found"

spec :: Spec
spec = do
    describe "balanced transaction" $ do
        it "has correct witness for one tx-in"
            $ testOnWallet
            $ do
                wallet 17 mnemonics "passphrase"
                tx1 <- existsTx
                u1 <- deposit tx1 1 100
                b1 <- block [tx1]
                rollForward [b1]
                spending <- existsTx
                spend spending address 10
                balanced <- balance spending
                utxos <- utxo u1
                signedTx <- sign balanced "passphrase"
                assert $ ledgerFrom utxos `accepts` signedTx

    -- cat root1.prv
    --  | cardano-address key child 1857H/1815H/0H/0/0 \
    --  | cardano-address key public --with-chain-code \
    --  | cardano-address address payment --network-tag mainnet
    describe "generated address match golden cases" $ do
        it "with empty passphrase in mainnet" $ do
            let
                Right seed = createMnemonicFromWords mnemonics
                address0 = "addr1v8th5554xvd2us9hwh72p3yt9rg7uw9v7tk49t3yw3wrcgc3drxft"
                creds = credentialsFromMnemonics seed mempty
                xpub = accountXPubFromCredentials creds
                addr =
                    encodeAddress
                        $ snd
                        $ head
                        $ Address.listCustomers
                        $ Address.fromXPubAndCount Mainnet xpub 1

            addr `shouldBe` address0
