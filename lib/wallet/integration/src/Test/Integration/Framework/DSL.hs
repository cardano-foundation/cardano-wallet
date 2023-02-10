{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-simplifiable-class-constraints #-}

module Test.Integration.Framework.DSL
    ( Context(..)
    , MnemonicLength(..)
    , TxDescription(..)

    -- * Steps
    , request
    , rawRequest
    , unsafeRequest
    , unsafeResponse

    -- * Decoding errors
    , decodeErrorInfo

    -- * Expectations
    , expectPathEventuallyExist
    , expectSuccess
    , expectError
    , expectErrorMessage
    , expectField
    , expectListField
    , expectListSize
    , expectListSizeSatisfy
    , expectResponseCode
    , expectValidJSON
    , expectCliField
    , expectCliListField
    , expectWalletUTxO
    , between
    , counterexample
    , (.>=)
    , (.<=)
    , (.>)
    , (.<)
    , verify
    , verifyMsg
    , Headers(..)
    , Payload(..)
    , RequestException(..)

    -- * Lens
    , walletId

    -- * Constants
    , minUTxOValue
    , slotLengthValue
    , securityParameterValue
    , epochLengthValue
    , defaultTxTTL
    , maximumCollateralInputCountByEra
    , minimumCollateralPercentageByEra

    -- * Create wallets
    , restoreWalletFromPubKey
    , emptyRandomWallet
    , emptyRandomWalletMws
    , emptyRandomWalletWithPasswd
    , emptyIcarusWallet
    , emptyIcarusWalletMws
    , emptyByronWalletWith
    , postWallet
    , postWallet'
    , postByronWallet
    , emptyWallet
    , emptyWalletWith
    , emptyWalletAndMnemonic
    , emptyWalletAndMnemonicAndSndFactor
    , emptyWalletAndMnemonicWith
    , emptyByronWalletFromXPrvWith
    , rewardWallet
    , emptySharedWallet
    , emptySharedWalletDelegating
    , fundSharedWallet
    , fixtureSharedWallet
    , fixtureSharedWalletDelegating
    , postSharedWallet
    , deleteSharedWallet
    , getSharedWallet
    , patchSharedWallet
    , getSharedWalletKey
    , postAccountKeyShared
    , getAccountKeyShared
    , getSomeVerificationKey

    -- * Wallet helpers
    , listFilteredWallets
    , listFilteredByronWallets
    , listFilteredSharedWallets
    , getWalletIdFromSharedWallet

    -- * Helpers
    , (</>)
    , (!!)
    , computeApiCoinSelectionFee
    , isValidDerivationPath
    , isValidRandomDerivationPath
    , genMnemonics
    , genMnemonics'
    , getFromResponse
    , getFromResponseList
    , json
    , joinStakePool
    , joinStakePoolUnsigned
    , delegationFee
    , quitStakePool
    , quitStakePoolUnsigned
    , selectCoins
    , selectCoinsWith
    , listAddresses
    , signTx
    , signSharedTx
    , submitTx
    , submitTxWithWid
    , getWallet
    , listTransactions
    , listAllTransactions
    , deleteAllWallets
    , fixtureRawTx
    , fixtureRandomWallet
    , fixtureRandomWalletMws
    , fixtureRandomWalletAddrs
    , fixtureRandomWalletWith
    , fixtureIcarusWallet
    , fixtureIcarusWalletMws
    , fixtureIcarusWalletAddrs
    , fixtureIcarusWalletWith
    , fixtureWallet
    , fixtureWalletWith
    , fixtureWalletWithMnemonics
    , fixtureMultiAssetWallet
    , fixtureMultiAssetRandomWallet
    , fixtureMultiAssetIcarusWallet
    , constFixtureWalletNoWait
    , faucetAmt
    , faucetUtxoAmt
    , proc'
    , postTx
    , pickAnAsset
    , mkTxPayloadMA
    , waitForServer
    , for
    , utcIso8601ToText
    , eventually
    , eventuallyUsingDelay
    , fixturePassphrase
    , fixturePassphraseEncrypted
    , waitForNextEpoch
    , waitForTxImmutability
    , waitAllTxsInLedger
    , toQueryString
    , withMethod
    , withPathParam
    , addField
    , icarusAddresses
    , randomAddresses
    , shelleyAddresses
    , pubKeyFromMnemonics
    , rootPrvKeyFromMnemonics
    , unsafeGetTransactionTime
    , getTxId
    , oneSecond
    , getTTLSlots
    , updateMetadataSource
    , bracketSettings
    , verifyMetadataSource
    , triggerMaintenanceAction
    , verifyMaintenanceAction
    , genXPubs
    , genXPubsBech32
    , hexText
    , fromHexText
    , bech32Text
    , accPubKeyFromMnemonics
    , sharedAccPubKeyFromMnemonics
    , submitSharedTxWithWid

    -- * Delegation helpers
    , notDelegating
    , delegating
    , getSlotParams
    , arbitraryStake

    -- * CLI
    , commandName
    , command
    , cardanoWalletCLI
    , generateMnemonicsViaCLI
    , createWalletViaCLI
    , createWalletFromPublicKeyViaCLI
    , deleteWalletViaCLI
    , getWalletUtxoSnapshotViaCLI
    , getWalletUtxoStatisticsViaCLI
    , getWalletViaCLI
    , createAddressViaCLI
    , importAddressViaCLI
    , listAddressesViaCLI
    , listStakePoolsViaCLI
    , listWalletsViaCLI
    , updateWalletNameViaCLI
    , updateWalletPassphraseViaCLI
    , updateWalletPassphraseWithMnemonicViaCLI
    , postTransactionViaCLI
    , postTransactionFeeViaCLI
    , listTransactionsViaCLI
    , postExternalTransactionViaCLI
    , deleteTransactionViaCLI
    , getTransactionViaCLI

    -- utilities
    , getRetirementEpoch
    , replaceStakeKey
     -- * Re-exports
    , runResourceT
    , ResourceT
    ) where

import Prelude

import Cardano.Address.Derivation
    ( XPub, xpubFromBytes, xpubToBytes )
import Cardano.CLI
    ( Port (..) )
import Cardano.Mnemonic
    ( ConsistentEntropy
    , EntropySize
    , MkSomeMnemonic (..)
    , Mnemonic
    , MnemonicWords
    , SomeMnemonic (..)
    , entropyToMnemonic
    , genEntropy
    , mnemonicToText
    )
import Cardano.Pool.Metadata.Types
    ( PoolMetadataGCStatus (..) )
import Cardano.Pool.Types
    ( PoolId (..) )
import Cardano.Wallet.Api.Types
    ( AddressAmount
    , ApiAccountKeyShared
    , ApiAccountSharedPublicKey (..)
    , ApiActiveSharedWallet
    , ApiAddress
    , ApiBlockReference (..)
    , ApiByronWallet
    , ApiCoinSelection
    , ApiEra (..)
    , ApiFee (..)
    , ApiMaintenanceAction (..)
    , ApiNetworkInformation
    , ApiNetworkParameters (..)
    , ApiPoolSpecifier
    , ApiSerialisedTransaction
    , ApiSharedWallet (..)
    , ApiT (..)
    , ApiTransaction
    , ApiTxId (ApiTxId)
    , ApiUtxoStatistics (..)
    , ApiVerificationKeyShared
    , ApiVerificationKeyShelley (..)
    , ApiWallet
    , ApiWalletDelegation (..)
    , ApiWalletDelegationNext (..)
    , ApiWalletDelegationStatus (..)
    , DecodeAddress (..)
    , DecodeStakeAddress (..)
    , EncodeAddress (..)
    , Iso8601Time (..)
    , KeyFormat
    , SettingsPutData (..)
    , WalletStyle (..)
    , insertedAt
    )
import Cardano.Wallet.Api.Types.Error
    ( ApiErrorInfo (..) )
import Cardano.Wallet.Api.Types.SchemaMetadata
    ( TxMetadataSchema, toSimpleMetadataFlag )
import Cardano.Wallet.Compat
    ( (^?) )
import Cardano.Wallet.Pools
    ( EpochInfo, StakePool )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (..)
    , DerivationIndex (..)
    , DerivationType (..)
    , HardDerivation (..)
    , Index (..)
    , NetworkDiscriminant (..)
    , PaymentAddress (..)
    , PersistPublicKey (..)
    , Role (..)
    , WalletKey (..)
    , fromHex
    , hex
    )
import Cardano.Wallet.Primitive.AddressDerivation.Byron
    ( ByronKey (..) )
import Cardano.Wallet.Primitive.AddressDerivation.Icarus
    ( IcarusKey )
import Cardano.Wallet.Primitive.AddressDerivation.Shelley
    ( ShelleyKey )
import Cardano.Wallet.Primitive.AddressDiscovery.Sequential
    ( coinTypeAda )
import Cardano.Wallet.Primitive.AddressDiscovery.Shared
    ( CredentialType (..) )
import Cardano.Wallet.Primitive.Passphrase
    ( Passphrase (..), preparePassphrase )
import Cardano.Wallet.Primitive.Passphrase.Legacy
    ( encryptPassphraseTestingOnly )
import Cardano.Wallet.Primitive.SyncProgress
    ( SyncProgress (..) )
import Cardano.Wallet.Primitive.Types
    ( ActiveSlotCoefficient (..)
    , EpochLength (..)
    , EpochNo
    , PoolMetadataSource
    , Settings
    , SlotLength (..)
    , SlotNo (..)
    , SlottingParameters (..)
    , SortOrder (..)
    , WalletId (..)
    )
import Cardano.Wallet.Primitive.Types.Address
    ( Address (..) )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..) )
import Cardano.Wallet.Primitive.Types.Hash
    ( Hash (..) )
import Cardano.Wallet.Primitive.Types.Tx
    ( SealedTx (..), TxStatus (..) )
import Cardano.Wallet.Primitive.Types.Tx.Constraints
    ( txOutMaxCoin, txOutMinCoin )
import Cardano.Wallet.Primitive.Types.Tx.TxIn
    ( TxIn (..) )
import Cardano.Wallet.Primitive.Types.Tx.TxOut
    ( TxOut (..) )
import Cardano.Wallet.Primitive.Types.UTxO
    ( UTxO (..) )
import Cardano.Wallet.Primitive.Types.UTxOStatistics
    ( HistogramBar (..), UTxOStatistics (..) )
import "cardano-addresses" Codec.Binary.Encoding
    ( AbstractEncoding (..), encode )
import Control.Arrow
    ( second )
import Control.Monad
    ( forM_, join, replicateM, unless, void, (>=>) )
import Control.Monad.IO.Unlift
    ( MonadIO, MonadUnliftIO (..), liftIO )
import Control.Monad.Trans.Resource
    ( ResourceT, allocate, runResourceT )
import Control.Retry
    ( capDelay, constantDelay, retrying )
import Crypto.Hash
    ( Blake2b_160, Digest, digestFromByteString )
import Crypto.Hash.Utils
    ( blake2b224 )
import Data.Aeson
    ( FromJSON, ToJSON, Value, (.=) )
import Data.Aeson.QQ
    ( aesonQQ )
import Data.ByteString
    ( ByteString )
import Data.Either.Combinators
    ( swapEither )
import Data.Either.Extra
    ( eitherToMaybe )
import Data.Foldable
    ( toList )
import Data.Function
    ( (&) )
import Data.Functor.Identity
    ( Identity (..) )
import Data.Generics.Internal.VL.Lens
    ( Lens', lens, set, view, (^.) )
import Data.Generics.Internal.VL.Traversal
    ( Traversal' )
import Data.Generics.Labels
    ()
import Data.Generics.Product.Typed
    ( HasType, typed )
import Data.IORef
    ( newIORef, readIORef, writeIORef )
import Data.List
    ( isPrefixOf )
import Data.List.NonEmpty
    ( NonEmpty )
import Data.Maybe
    ( fromJust, fromMaybe )
import Data.Monoid
    ( Sum (..) )
import Data.Proxy
    ( Proxy (..) )
import Data.Quantity
    ( Quantity (..) )
import Data.Set
    ( Set )
import Data.Text
    ( Text )
import Data.Text.Class
    ( FromText (..), ToText (..) )
import Data.Time
    ( NominalDiffTime, UTCTime )
import Data.Time.Text
    ( iso8601ExtendedUtc, utcTimeToText )
import Data.Word
    ( Word16, Word32 )
import Fmt
    ( indentF, (+|), (|+) )
import Language.Haskell.TH.Quote
    ( QuasiQuoter )
import Network.HTTP.Types.Method
    ( Method )
import Numeric.Natural
    ( Natural )
import System.Command
    ( CmdOption (..), CmdResult, Exit (..), Stderr, Stdout (..), command )
import System.Directory
    ( doesPathExist )
import System.Exit
    ( ExitCode (..) )
import System.IO
    ( hClose, hFlush, hPutStr )
import Test.Hspec
    ( Expectation, HasCallStack )
import Test.Hspec.Expectations.Lifted
    ( expectationFailure, shouldBe, shouldContain, shouldNotBe, shouldSatisfy )
import Test.Hspec.Extra
    ( appendFailureReason, counterexample )
import Test.HUnit.Lang
    ( HUnitFailure (..) )
import Test.Integration.Faucet
    ( NextWallet, nextTxBuilder, nextWallet, seqMnemonics )
import Test.Integration.Framework.Context
    ( Context (..), TxDescription (..) )
import Test.Integration.Framework.Request
    ( Headers (..)
    , Payload (..)
    , RequestException (..)
    , rawRequest
    , request
    , unsafeRequest
    )
import Test.Utils.Pretty
    ( Pretty (..), pShowBuilder )
import UnliftIO.Async
    ( async, race, wait )
import UnliftIO.Concurrent
    ( threadDelay )
import UnliftIO.Exception
    ( Exception (..), SomeException (..), catch, throwIO, throwString, try )
import UnliftIO.Process
    ( CreateProcess (..)
    , StdStream (..)
    , proc
    , waitForProcess
    , withCreateProcess
    )
import Web.HttpApiData
    ( ToHttpApiData (..) )

import qualified Cardano.Wallet.Api.Link as Link
import qualified Cardano.Wallet.Primitive.AddressDerivation.Byron as Byron
import qualified Cardano.Wallet.Primitive.AddressDerivation.Icarus as Icarus
import qualified Cardano.Wallet.Primitive.AddressDerivation.Shared as Shared
import qualified Cardano.Wallet.Primitive.AddressDerivation.Shelley as Shelley
import qualified Cardano.Wallet.Primitive.Passphrase.Types as W
import qualified Cardano.Wallet.Primitive.Types.Coin as Coin
import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle
import qualified Cardano.Wallet.Primitive.Types.TokenMap as TokenMap
import qualified Cardano.Wallet.Primitive.Types.TokenQuantity as TokenQuantity
import qualified Cardano.Wallet.Primitive.Types.UTxOStatistics as UTxOStatistics
import qualified Codec.Binary.Bech32 as Bech32
import qualified Codec.Binary.Bech32.TH as Bech32
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Aeson
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.Foldable as F
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as TIO
import qualified Network.HTTP.Types.Status as HTTP

--
-- API response expectations
--

-- | Expect a successful response, without any further assumptions.
expectSuccess :: MonadIO m => (s, Either RequestException a) -> m ()
expectSuccess = either wantedSuccessButError (const $ pure ()) . snd

-- | Expect an error response, without any further assumptions.
expectError :: (MonadIO m, Show a) => (s, Either RequestException a) -> m ()
expectError = either (const $ pure ()) wantedErrorButSuccess . snd

-- | Expect an error response, without any further assumptions.
expectErrorMessage
    :: (HasCallStack, MonadIO m, Show a)
    => String
    -> (s, Either RequestException a)
    -> m ()
expectErrorMessage want = either expectation wantedErrorButSuccess . snd
  where
    expectation msg = fmt msg `shouldContain` want
    fmt = \case
        DecodeFailure res msg -> msg ++ "\n" ++ BL8.unpack res
        ClientError val       -> BL8.unpack $ Aeson.encode val
        RawClientError val    -> BL8.unpack val
        HttpException err     -> show err

-- | Decodes the information about an error into an 'ApiErrorInfo' value.
decodeErrorInfo
    :: (HasCallStack, Show a)
    => (s, Either RequestException a)
    -> ApiErrorInfo
decodeErrorInfo (_, response) =
    case response of
        Left (ClientError value) ->
            fromMaybe decodeFailure $ Aeson.decode $ Aeson.encode value
        somethingElse ->
            error $ unwords
                [ "decodeErrorInfo:"
                , "Expected a 'ClientError', but encountered something else:"
                , show somethingElse
                ]
  where
    decodeFailure = error $ unwords
        [ "decodeErrorInfo:"
        , "Unable to decode 'ApiErrorInfo' value"
        ]

-- | Expect a given response code on the response.
expectResponseCode
    :: (HasCallStack, MonadUnliftIO m, Show a)
    => HTTP.Status
    -> (HTTP.Status, a)
    -> m ()
expectResponseCode expected (actual, a) =
    counterexample ("From the following response: " <> show (Pretty a)) $
    if actual == expected
        then pure ()
        else actual `shouldBe` expected

expectField
    :: (HasCallStack, MonadIO m)
    => Traversal' s a
    -> (a -> Expectation)
    -> (HTTP.Status, Either RequestException s)
    -> m ()
expectField getter predicate (_, res) = case res of
    Left e  -> wantedSuccessButError e
    Right s -> liftIO $ case s ^? getter of
        Nothing -> expectationFailure
            "could not traverse response to test expectations"
        Just a -> predicate a

expectListField
    :: (HasCallStack, MonadIO m)
    => Int
    -> Traversal' s a
    -> (a -> Expectation)
    -> (HTTP.Status, Either RequestException [s])
    -> m ()
expectListField i getter predicate (c, res) = liftIO $ case res of
    Left e -> wantedSuccessButError e
    Right xs
        | length xs > i ->
            expectField getter predicate (c, Right (xs !! i))
        | otherwise -> expectationFailure $
            "expectListField: trying to access the #" <> show i <>
            " element from a list but there's none! "

-- | Expects data list returned by the API to be of certain length
expectListSize
    :: (HasCallStack, MonadIO m, Foldable xs)
    => Int
    -> (HTTP.Status, Either RequestException (xs a))
    -> m ()
expectListSize l (_, res) = liftIO $ case res of
    Left e   -> wantedSuccessButError e
    Right xs -> length (toList xs) `shouldBe` l

-- | Expects data list returned by the API to be of certain length
expectListSizeSatisfy
    :: (HasCallStack, MonadIO m, Foldable xs)
    => (Int -> Bool)
    -> (HTTP.Status, Either RequestException (xs a))
    -> m ()
expectListSizeSatisfy cond (_, res) = liftIO $ case res of
    Left e   -> wantedSuccessButError e
    Right xs -> length (toList xs) `shouldSatisfy` cond

-- | Expects wallet UTxO statistics from the request to be equal to
-- pre-calculated statistics.
expectWalletUTxO
    :: (HasCallStack, MonadIO m)
    => [Natural]
    -> Either RequestException ApiUtxoStatistics
    -> m ()
expectWalletUTxO coins = \case
    Left e  -> wantedSuccessButError e
    Right stats -> do
        let addr = Address "ARBITRARY"
        let constructUtxoEntry idx c =
                ( TxIn (Hash "ARBITRARY") idx
                , TxOut addr (TokenBundle.fromCoin $ Coin c)
                )
        let utxo = UTxO $ Map.fromList $ zipWith constructUtxoEntry [0..] coins
        let (UTxOStatistics hist stakes bType) = UTxOStatistics.compute utxo
        let distr = Map.fromList $ map (\(HistogramBar k v)-> (k,v)) hist
        (ApiUtxoStatistics (Quantity (fromIntegral stakes)) (ApiT bType) distr)
            `shouldBe` stats

--
-- CLI output expectations
--

-- | Expects a given string to be a valid JSON output corresponding to some
-- given data-type 'a'. Returns this type if successful.
expectValidJSON
    :: forall m a. (HasCallStack, FromJSON a, MonadIO m)
    => Proxy a
    -> String
    -> m a
expectValidJSON _ str = liftIO $
    case Aeson.eitherDecode @a (BL.fromStrict $ T.encodeUtf8 $ T.pack str) of
        Left e ->
            expectationFailure' $ "expected valid JSON but failed decoding: " <> show e
        Right a -> return a

expectCliListField
    :: (HasCallStack, MonadIO m)
    => Int
    -> Lens' s a
    -> (a -> Expectation)
    -> [s]
    -> m ()
expectCliListField i getter predicate xs
        | length xs > i = expectCliField getter predicate (xs !! i)
        | otherwise = liftIO . expectationFailure $
            "expectCliListField: trying to access the #" <> show i <>
            " element from a list but there's none! "

expectCliField :: MonadIO m => Lens' s a -> (a -> Expectation) -> s -> m ()
expectCliField getter predicate out = do
    let a = (view getter out)
    liftIO $ predicate a

-- | A file is eventually created on the given location
expectPathEventuallyExist :: HasCallStack => FilePath -> IO ()
expectPathEventuallyExist filepath = do
    handle <- async doesPathExistNow
    winner <- race (threadDelay (60 * oneSecond)) (wait handle)
    case winner of
        Left _ -> expectationFailure $
            "waited more than 60s for " <> filepath <> " to be created!"
        Right _ ->
            return ()
  where
    doesPathExistNow = do
        doesPathExist filepath >>= \case
            True ->
                return ()
            False ->
                threadDelay (oneSecond `div` 2) >> doesPathExistNow

-- Lenses
--

walletId :: HasType (ApiT WalletId) s => Lens' s Text
walletId =
    lens _get _set
  where
    _get :: HasType (ApiT WalletId) s => s -> Text
    _get = T.pack . show . getWalletId . getApiT . view typed
    _set :: HasType (ApiT WalletId) s => s -> Text -> s
    _set s v = set typed (ApiT $ WalletId (unsafeCreateDigest v)) s

--
-- Constants
--

-- | Minimum UTxO parameter for the test cluster.
--
-- The value returned by this function is only appropriate for a very minimal
-- output, where:
--
-- - the output has no assets other than ada.
-- - the output uses a Shelley-era address of the length typically used in
--   the integration test suite.
-- - the output has no datum hash.
--
-- This value will almost certainly not be correct for outputs with non-ada
-- assets, for outputs with longer addresses, or outputs with a datum hash.
--
-- In those cases, a larger value will be required. The precise value can be
-- determined by calling one of the endpoints that returns an 'ApiFee' object,
-- and inspecting the 'minimumCoins' field.
--
minUTxOValue :: ApiEra -> Natural
minUTxOValue e
    | e >= ApiBabbage =   978_370
    | e >= ApiAlonzo  =   999_978
    | otherwise       = 1_000_000

-- | Parameter in test cluster shelley genesis.
--
-- This space left blank intentionally.
slotLengthValue :: NominalDiffTime
slotLengthValue =  0.2

-- | Parameter in test cluster shelley genesis.
securityParameterValue :: Word32
securityParameterValue = 10

-- | Parameter in test cluster shelley genesis.
epochLengthValue :: Word32
epochLengthValue = 160

-- | Wallet server's chosen transaction TTL value (in seconds) when none is
-- given.
defaultTxTTL :: NominalDiffTime
defaultTxTTL = 7_200

maximumCollateralInputCountByEra :: ApiEra -> Word16
maximumCollateralInputCountByEra = \case
    ApiByron   -> 0
    ApiShelley -> 0
    ApiAllegra -> 0
    ApiMary    -> 0
    -- value from alonzo-genesis.yaml:
    ApiAlonzo  -> 3
    ApiBabbage -> 3

minimumCollateralPercentageByEra :: ApiEra -> Natural
minimumCollateralPercentageByEra = \case
    ApiByron   -> 0
    ApiShelley -> 0
    ApiAllegra -> 0
    ApiMary    -> 0
    -- value from alonzo-genesis.yaml:
    ApiAlonzo  -> 150
    ApiBabbage -> 150

--
-- Helpers
--

-- | Computes the effective fee for an `ApiCoinSelection` value.
--
-- The `ApiCoinSelection` type doesn't include a field to indicate the fee.
--
-- This function computes the effective fee by subtracting the total output
-- value from the total input value.
--
computeApiCoinSelectionFee :: ApiCoinSelection n -> Coin
computeApiCoinSelectionFee selection
    | feeIsValid =
        Coin $ fromIntegral fee
    | otherwise =
        error $ unlines
            [ "Unable to compute fee of ApiCoinSelection:"
            , "fee:"
            , show fee
            , "balanceOfInputs:"
            , show balanceOfInputs
            , "balanceOfOutputs:"
            , show balanceOfOutputs
            , "balanceOfChange:"
            , show balanceOfChange
            , "balanceOfRewardWithdrawals"
            , show balanceOfRewardWithdrawals
            , "balanceOfDeposits"
            , show balanceOfDeposits
            ]
  where
    fee :: Integer
    fee
        = balanceOfInputs
        + balanceOfRewardWithdrawals
        - balanceOfOutputs
        - balanceOfChange
        - balanceOfDeposits
    feeIsValid :: Bool
    feeIsValid = (&&)
        (fee >= Coin.toInteger txOutMinCoin)
        (fee <= Coin.toInteger txOutMaxCoin)
    balanceOfInputs
        = selection
        & view #inputs
        & F.foldMap (Sum . quantityToInteger . view #amount)
        & getSum
    balanceOfOutputs
        = selection
        & view #outputs
        & F.foldMap (Sum . quantityToInteger . view #amount)
        & getSum
    balanceOfChange
        = selection
        & view #change
        & F.foldMap (Sum . quantityToInteger . view #amount)
        & getSum
    balanceOfRewardWithdrawals
        = selection
        & view #withdrawals
        & F.foldMap (Sum . quantityToInteger . view #amount)
        & getSum
    balanceOfDeposits
        = selection
        & view #depositsTaken
        & F.foldMap (Sum . quantityToInteger)
        & getSum
    quantityToInteger :: Quantity "lovelace" Natural -> Integer
    quantityToInteger (Quantity n) = fromIntegral n

isValidDerivationPath
    :: Index 'Hardened 'PurposeK
    -> NonEmpty (ApiT DerivationIndex)
    -> Bool
isValidDerivationPath purpose path =
    ( length path == 5 )
    &&
    ( [ ApiT $ DerivationIndex $ getIndex purpose
      , ApiT $ DerivationIndex $ getIndex coinTypeAda
      , ApiT $ DerivationIndex $ getIndex @'Hardened minBound
      ] `isPrefixOf` NE.toList path
    )

isValidRandomDerivationPath :: NonEmpty (ApiT DerivationIndex) -> Bool
isValidRandomDerivationPath path =
    ( length path == 2 )
    &&
    ( [ ApiT $ DerivationIndex $ getIndex @'Hardened minBound
      ] `isPrefixOf` NE.toList path
    )


pickAnAsset :: TokenMap.TokenMap -> ((Text, Text), Natural)
pickAnAsset tm = case TokenMap.toFlatList tm of
    (TokenBundle.AssetId pid an, TokenQuantity.TokenQuantity q):_ ->
        ((toText pid, toText an), q)
    _ -> error "pickAnAsset: empty TokenMap"

-- Like mkTxPayload, except that assets are included in the payment.
-- Asset amounts are specified by ((PolicyId Hex, AssetName Hex), amount).
mkTxPayloadMA
    :: forall n m.
        ( MonadUnliftIO m
        , EncodeAddress n
        )
    => (ApiT Address, Proxy n)
    -> Natural
    -> [((Text, Text), Natural)]
    -> Text
    -> m Payload
mkTxPayloadMA destination coin val passphrase = do
    let assetJson ((pid, name), q) = [aesonQQ|{
                "policy_id": #{pid},
                "asset_name": #{name},
                "quantity": #{q}
            }|]
    return $ Json [aesonQQ|{
            "payments": [{
                "address": #{destination},
                "amount": {
                    "quantity": #{coin},
                    "unit": "lovelace"
                },
                "assets": #{map assetJson val}
            }],
            "passphrase": #{passphrase}
        }|]

postTx
    :: forall n w m.
        ( DecodeAddress n
        , DecodeStakeAddress n
        , EncodeAddress n
        , MonadUnliftIO m
        )
    => Context
    -> (w, w -> (Method, Text), Text)
    -> ApiWallet
    -> Natural
    -> m (HTTP.Status, Either RequestException (ApiTransaction n))
postTx ctx (wSrc, postTxEndp, pass) wDest amt = do
    addrs <- listAddresses @n ctx wDest
    let destination = (addrs !! 1) ^. #id
    let payload = Json [aesonQQ|{
            "payments": [{
                "address": #{destination},
                "amount": {
                    "quantity": #{amt},
                    "unit": "lovelace"
                }
            }],
            "passphrase": #{pass}
        }|]
    r <- request @(ApiTransaction n) ctx (postTxEndp wSrc) Default payload
    expectResponseCode HTTP.status202 r
    return r

updateMetadataSource :: MonadUnliftIO m => Context -> Text -> m ()
updateMetadataSource ctx t = do
    r <- request @SettingsPutData ctx Link.putSettings Default payload
    expectResponseCode HTTP.status204 r
 where
  payload = Json [aesonQQ| {
       "settings": {
           "pool_metadata_source": #{t}
            }
       } |]

bracketSettings :: MonadUnliftIO m => Context -> m () -> m ()
bracketSettings ctx action = do
    r@(_, response) <- request @(ApiT Settings) ctx Link.getSettings Default Empty
    expectResponseCode HTTP.status200 r
    case response of
        Left e -> wantedSuccessButError e
        Right s -> do
            action
            r' <- request @SettingsPutData ctx Link.putSettings Default
                (Json $ Aeson.toJSON $ SettingsPutData s)
            expectResponseCode HTTP.status204 r'

verifyMetadataSource
    :: MonadUnliftIO m
    => Context
    -> PoolMetadataSource
    -> m ()
verifyMetadataSource ctx s = do
    r <- request @(ApiT Settings) ctx Link.getSettings Default Empty
    expectResponseCode HTTP.status200 r
    expectField (#getApiT . #poolMetadataSource) (`shouldBe` s) r

triggerMaintenanceAction :: MonadUnliftIO m => Context -> Text -> m ()
triggerMaintenanceAction ctx a = do
    r <- request @ApiMaintenanceAction ctx Link.postPoolMaintenance Default payload
    expectResponseCode HTTP.status204 r
 where
   payload = Json [aesonQQ| { "maintenance_action": #{a} } |]

verifyMaintenanceAction
    :: MonadUnliftIO m
    => Context
    -> PoolMetadataGCStatus
    -> m ()
verifyMaintenanceAction ctx s = do
    r <- request @ApiMaintenanceAction ctx Link.getPoolMaintenance Default Empty
    expectResponseCode HTTP.status200 r
    expectField (#gcStakePools . #getApiT) (`shouldBe` s) r

data MnemonicLength = M9 | M12 | M15 | M18 | M21 | M24 deriving (Show)

genMnemonics :: MnemonicLength -> IO [Text]
genMnemonics M9 = genMnemonics' @9
genMnemonics M12 = genMnemonics' @12
genMnemonics M15 = genMnemonics' @15
genMnemonics M18 = genMnemonics' @18
genMnemonics M21 = genMnemonics' @21
genMnemonics M24 = genMnemonics' @24

genMnemonics'
   :: forall mw ent csz m.
       ( ConsistentEntropy ent mw csz
       , ent ~ EntropySize mw
       , mw ~ MnemonicWords ent
       , MonadIO m
       )
   => m [Text]
genMnemonics' =
    liftIO $ mnemonicToText . entropyToMnemonic @mw <$> genEntropy

accPubKeyFromMnemonics
    :: SomeMnemonic
    -> Maybe SomeMnemonic
    -> Word32
    -> Passphrase "encryption"
    -> Text
accPubKeyFromMnemonics mnemonic1 mnemonic2 ix passphrase =
    T.decodeUtf8 $ serializeXPub $ publicKey $
        deriveAccountPrivateKey passphrase rootXPrv (Index $ 2_147_483_648 + ix)
  where
    rootXPrv = Shared.generateKeyFromSeed (mnemonic1, mnemonic2) passphrase

sharedAccPubKeyFromMnemonics
    :: SomeMnemonic
    -> Maybe SomeMnemonic
    -> Word32
    -> Passphrase "encryption"
    -> Text
sharedAccPubKeyFromMnemonics mnemonic1 mnemonic2 ix passphrase =
    T.decodeUtf8 $ encode (EBech32 hrp) $ xpubToBytes $ getRawKey $ publicKey $
        deriveAccountPrivateKey passphrase rootXPrv (Index $ 2_147_483_648 + ix)
  where
    hrp = [Bech32.humanReadablePart|acct_shared_xvk|]
    rootXPrv = Shared.generateKeyFromSeed (mnemonic1, mnemonic2) passphrase

genXPubs :: Int -> IO [(XPub,Text)]
genXPubs num =
    replicateM num genXPub
  where
    genXPub = do
        m15txt <- genMnemonics M15
        m12txt <- genMnemonics M12
        let (Right m15) = mkSomeMnemonic @'[ 15 ] m15txt
        let (Right m12) = mkSomeMnemonic @'[ 12 ] m12txt
        let accXPubTxt = accPubKeyFromMnemonics m15 (Just m12) 10 mempty
        let (Just accXPub) = xpubFromText accXPubTxt
        return (accXPub, accXPubTxt)

    xpubFromText :: Text -> Maybe XPub
    xpubFromText = fmap eitherToMaybe fromHexText >=> xpubFromBytes

genXPubsBech32 :: Int -> IO [(XPub,Text)]
genXPubsBech32 num =
    replicateM num genXPub
  where
    genXPub = do
        m15txt <- genMnemonics M15
        m12txt <- genMnemonics M12
        let (Right m15) = mkSomeMnemonic @'[ 15 ] m15txt
        let (Right m12) = mkSomeMnemonic @'[ 12 ] m12txt
        let accXPubTxt = sharedAccPubKeyFromMnemonics m15 (Just m12) 10 mempty
        let (Just accXPub) = xpubFromText accXPubTxt
        return (accXPub, accXPubTxt)

    xpubFromText :: Text -> Maybe XPub
    xpubFromText = fmap (getApiT . sharedKey) . fmap eitherToMaybe
        (fromText @ApiAccountSharedPublicKey)

fromHexText :: Text -> Either String ByteString
fromHexText = fromHex . T.encodeUtf8

hexText :: ByteString -> Text
hexText = T.decodeLatin1 . hex

bech32Text :: Bech32.HumanReadablePart -> ByteString -> Text
bech32Text hrp = T.decodeUtf8 . encode (EBech32 hrp)

getTxId :: (ApiTransaction n) -> String
getTxId tx = T.unpack $ toUrlPiece $ ApiTxId (tx ^. #id)

unsafeGetTransactionTime :: MonadUnliftIO m => [ApiTransaction n] -> m UTCTime
unsafeGetTransactionTime txs =
    case fmap time . insertedAt <$> txs of
        (Just t):_ -> pure t
        _ -> throwString "Expected at least one transaction with a time."

waitAllTxsInLedger
    :: forall n m.
        ( DecodeAddress n
        , DecodeStakeAddress n
        , MonadUnliftIO m
        )
    => Context
    -> ApiWallet
    -> m ()
waitAllTxsInLedger ctx w = eventually "waitAllTxsInLedger: all txs in ledger" $ do
    let ep = Link.listTransactions @'Shelley w
    r <- request @[ApiTransaction n] ctx ep Default Empty
    expectResponseCode HTTP.status200 r
    let txs = getFromResponse id r
    view (#status . #getApiT) <$> txs `shouldSatisfy` all (== InLedger)

waitForNextEpoch :: MonadUnliftIO m => Context -> m ()
waitForNextEpoch ctx = do
    epoch <- getFromResponse (#nodeTip . #slotId . #epochNumber) <$>
        request @ApiNetworkInformation ctx Link.getNetworkInfo Default Empty
    eventually "waitForNextEpoch: goes to next epoch" $ do
        epoch' <- getFromResponse (#nodeTip . #slotId . #epochNumber) <$>
            request @ApiNetworkInformation ctx Link.getNetworkInfo Default Empty
        unless (getApiT epoch' > getApiT epoch) $ expectationFailure "not yet"

-- Sometimes, we need to wait long-enough for transactions to become immutable.
-- What long enough is rather empirical here, but it must satisfies one important
-- criteria: it must be long-enough to assume that all transactions are no longer
-- in mempools AND, for those who've been inserted, they are now deep enough to
-- not be affected by rollbacks.
--
-- The stability windows in Praos is defined as: `3k/f` slots, thus we can consider
-- transactions immutable after:
--
--    ---> 3k / f * slot_length
--
-- (about 12s with the current parameters for the test cluster).
--
-- Beside, everything runs on the same machine and the information is propagated
-- rather fast; transactions are usually inserted within a few blocks. We'll
-- therefore consider that transactions are typically inserted within 10 blocks
-- yet still consider an extra delay of:
--
--    ---> 10 / f * slot_length
--
-- (about 4s with the current parameters for the test cluster).
waitForTxImmutability :: (MonadIO m) => Context -> m ()
waitForTxImmutability _ctx = liftIO $ do
    -- FIXME: #2226
    --
    -- (_, params) <- unsafeRequest @ApiNetworkParameters ctx Link.getNetworkParams Empty
    --
    -- let picoToMicro = (`div` 1_000_000)
    --
    -- let sl = params ^. (#slotLength . #getQuantity) & fromEnum & picoToMicro & fromIntegral
    -- let k  = params ^. (#epochStability . #getQuantity) & fromIntegral
    -- let f  = params ^. (#activeSlotCoefficient . #getQuantity) / 100
    --
    -- let stabilityDelay   = round (sl * 3 * k / f)
    -- let txInsertionDelay = round (sl * 10 / f)
    let stabilityDelay   = 6 * oneSecond
    let txInsertionDelay = 4 * oneSecond

    threadDelay $ stabilityDelay + txInsertionDelay

between :: (Ord a, Show a) => (a, a) -> a -> Expectation
between (min', max') x
    | min' <= x && x <= max'
        = return ()
    | otherwise
        = expectationFailure $ mconcat
            [ show x
            , " ∉ ["
            , show min'
            , ", "
            , show max'
            ]

(.>) :: (Ord a, Show a) => a -> a -> Expectation
x .> bound
    | x > bound
        = return ()
    | otherwise
        = expectationFailure $ mconcat
            [ show x
            , " does not satisfy (> "
            , show bound
            , ")"
            ]

(.<) :: (Ord a, Show a) => a -> a -> Expectation
x .< bound
    | x < bound
        = return ()
    | otherwise
        = expectationFailure $ mconcat
            [ show x
            , " does not satisfy (< "
            , show bound
            , ")"
            ]


(.>=) :: (Ord a, Show a) => a -> a -> Expectation
a .>= b
    | a >= b
        = return ()
    | otherwise
        = expectationFailure $ mconcat
            [ show a
            , " does not satisfy (>= "
            , show b
            , ")"
            ]

(.<=) :: (Ord a, Show a) => a -> a -> Expectation
a .<= b
    | a <= b
        = return ()
    | otherwise
        = expectationFailure $ mconcat
            [ show a
            , " does not satisfy (<= "
            , show b
            , ")"
            ]

-- | Like @expectationFailure@, but with a @IO a@ return type instead of @IO
-- ()@.
expectationFailure' :: HasCallStack => String -> IO a
expectationFailure' msg = do
    expectationFailure msg
    fail "expectationFailure': impossible"

-- Retry the given action a couple of time until it doesn't throw, or until it
-- has been retried enough.
--
-- It is like 'eventuallyUsingDelay', but with the default delay of 500 ms
-- and timeout of 90s between retries.
-- NOTE
-- This __90s__ is mostly justified by the parameters in the shelley
-- genesis. The longest action we have two wait for are about 2 epochs,
-- which corresponds to 20s with the current parameters. Using something
-- much longer than that isn't really useful (in particular, this doesn't
-- depend on the host machine running the test, because the protocol moves
-- forward at the same speed regardless...)
eventually :: MonadIO m => String -> IO a -> m a
eventually = eventuallyUsingDelay (500 * ms) 90
  where
    ms = 1_000

-- Retry the given action a couple of time until it doesn't throw, or until it
-- has been retried enough.
--
-- It sleeps for a specified delay between retries and fails after timeout.
eventuallyUsingDelay
    :: MonadIO m
    => Int -- ^ Delay in microseconds
    -> Int -- ^ Timeout in seconds
    -> String -- ^ Brief description of the IO action
    -> IO a
    -> m a
eventuallyUsingDelay delay timeout desc io = liftIO $ do
    lastErrorRef <- newIORef Nothing
    winner <- race (threadDelay $ timeout * oneSecond) (trial lastErrorRef)
    case winner of
        Left () -> do
            lastError <- readIORef lastErrorRef
            let msg = "Waited longer than " ++ show timeout ++
                      "s to resolve action: " ++ show desc ++ "."
            case fromException @HUnitFailure =<< lastError of
                Just lastError' -> throwIO $ appendFailureReason msg lastError'
                Nothing ->
                    expectationFailure' $ mconcat
                        [ msg
                        , " Error condition: "
                        , show lastError
                        ]
        Right a ->
            return a
  where
    trial lastErrorRef = loop
      where
        loop = io `catch` \(e :: SomeException) -> do
            writeIORef lastErrorRef (Just e)
            threadDelay delay
            loop

utcIso8601ToText :: UTCTime -> Text
utcIso8601ToText = utcTimeToText iso8601ExtendedUtc

-- Functions for creating wallets.
--
-- Wallets are cleaned up automatically at the end of @runResourceT@ in your
-- integration test.
--
-- Do not try to POST a wallet in any other way, since you'd then need to handle
-- the cleanup manually.

-- | Restore HW Wallet from pub key
--
restoreWalletFromPubKey
    :: forall w (style :: WalletStyle) m.
        ( Link.Discriminate style
        , Link.PostWallet style
        , HasType (ApiT WalletId) w
        , HasType (ApiT SyncProgress) w
        , Show w
        , FromJSON w
        , MonadIO m
        , MonadUnliftIO m
        )
    => Context
    -> Text
    -> Text
    -> ResourceT m w
restoreWalletFromPubKey ctx pubKey name = snd <$> allocate create destroy
  where
    create = do
        let payloadRestore = Json [aesonQQ| {
                "name": #{name},
                "account_public_key": #{pubKey}
            }|]
        r <- request @w ctx (Link.postWallet @style) Default payloadRestore
        expectResponseCode HTTP.status201 r
        let wid = getFromResponse id r
        eventually "restoreWalletFromPubKey: wallet is 100% synced " $ do
            rg <- request @w ctx (Link.getWallet @style wid) Default Empty
            expectField (typed @(ApiT SyncProgress) . #getApiT) (`shouldBe` Ready) rg
        return wid
    destroy w = void $ request @Aeson.Value ctx
      (Link.deleteWallet @style w) Default Empty

-- | Create an empty wallet
emptyRandomWallet
    :: MonadUnliftIO m
    => Context
    -> ResourceT m ApiByronWallet
emptyRandomWallet ctx = do
    mnemonic <- liftIO $ mnemonicToText @12 . entropyToMnemonic <$> genEntropy
    emptyByronWalletWith ctx "random"
        ("Random Wallet", mnemonic, fixturePassphrase)

emptyRandomWalletMws
    :: MonadUnliftIO m
    => Context
    -> ResourceT m (ApiByronWallet, Mnemonic 12)
emptyRandomWalletMws ctx = do
    mnemonic <- liftIO $ entropyToMnemonic <$> genEntropy
    (,mnemonic) <$> emptyByronWalletWith ctx "random"
        ("Random Wallet", mnemonicToText @12 mnemonic, fixturePassphrase)

emptyIcarusWallet
    :: MonadUnliftIO m
    => Context
    -> ResourceT m ApiByronWallet
emptyIcarusWallet ctx = do
    mnemonic <- liftIO $ mnemonicToText @15 . entropyToMnemonic <$> genEntropy
    emptyByronWalletWith ctx "icarus"
        ("Icarus Wallet", mnemonic, fixturePassphrase)

emptyIcarusWalletMws
    :: MonadUnliftIO m
    => Context
    -> ResourceT m (ApiByronWallet, Mnemonic 15)
emptyIcarusWalletMws ctx = do
    mnemonic <- liftIO $ entropyToMnemonic <$> genEntropy
    (,mnemonic) <$> emptyByronWalletWith ctx "icarus"
        ("Icarus Wallet",mnemonicToText @15 mnemonic, fixturePassphrase)

emptyRandomWalletWithPasswd
    :: MonadUnliftIO m
    => Context
    -> Text
    -> ResourceT m ApiByronWallet
emptyRandomWalletWithPasswd ctx rawPwd = do
    let pwd = preparePassphrase W.EncryptWithScrypt
            $ Passphrase
            $ BA.convert
            $ T.encodeUtf8 rawPwd
    seed <- liftIO $ SomeMnemonic @12 . entropyToMnemonic <$> genEntropy
    let key = T.decodeUtf8
            $ hex
            $ Byron.getKey
            $ Byron.generateKeyFromSeed seed pwd
    pwdH <- liftIO $ toText <$> encryptPassphraseTestingOnly pwd
    emptyByronWalletFromXPrvWith ctx "random" ("Random Wallet", key, pwdH)

postWallet'
    :: MonadUnliftIO m
    => Context
    -> Headers
    -> Payload
    -> ResourceT m (HTTP.Status, Either RequestException ApiWallet)
postWallet' ctx headers payload = snd <$> allocate create (free . snd)
  where
    create =
        request @ApiWallet ctx (Link.postWallet @'Shelley) headers payload

    free (Right w) = void $ request @Aeson.Value ctx
        (Link.deleteWallet @'Shelley w) Default Empty
    free (Left _) = return ()

postWallet
    :: MonadUnliftIO m
    => Context
    -> Payload
    -> ResourceT m (HTTP.Status, Either RequestException ApiWallet)
postWallet ctx = postWallet' ctx Default


postByronWallet
    :: MonadUnliftIO m
    => Context
    -> Payload
    -> ResourceT m (HTTP.Status, Either RequestException ApiByronWallet)
postByronWallet ctx payload = snd <$> allocate create (free . snd)
  where
    create =
        request @ApiByronWallet ctx (Link.postWallet @'Byron) Default payload

    free (Right w) = void $ request @Aeson.Value ctx
        (Link.deleteWallet @'Byron w) Default Empty
    free (Left _) = return ()

emptyByronWalletWith
    :: forall m. MonadUnliftIO m
    => Context
    -> String
    -> (Text, [Text], Text)
    -> ResourceT m ApiByronWallet
emptyByronWalletWith ctx style (name, mnemonic, pass) = do
    let payload = Json [aesonQQ| {
            "name": #{name},
            "mnemonic_sentence": #{mnemonic},
            "passphrase": #{pass},
            "style": #{style}
        }|]
    r <- postByronWallet ctx payload
    expectResponseCode HTTP.status201 r
    return (getFromResponse id r)

emptyByronWalletFromXPrvWith
    :: forall m. MonadUnliftIO m
    => Context
    -> String
    -> (Text, Text, Text)
    -> ResourceT m ApiByronWallet
emptyByronWalletFromXPrvWith ctx style (name, key, passHash) = do
    let payload = Json [aesonQQ| {
            "name": #{name},
            "encrypted_root_private_key": #{key},
            "passphrase_hash": #{passHash},
            "style": #{style}
        }|]
    r <- postByronWallet ctx payload
    expectResponseCode HTTP.status201 r
    return (getFromResponse id r)


emptyWalletAndMnemonic
    :: MonadUnliftIO m
    => Context
    -> ResourceT m (ApiWallet, [Text])
emptyWalletAndMnemonic ctx = do
    mnemonic <- liftIO $ (mnemonicToText . entropyToMnemonic) <$> genEntropy @160
    let payload = Json [aesonQQ| {
            "name": "Empty Wallet",
            "mnemonic_sentence": #{mnemonic},
            "passphrase": #{fixturePassphrase}
        }|]
    r <- postWallet ctx payload
    expectResponseCode HTTP.status201 r
    return (getFromResponse id r, mnemonic)

emptyWalletAndMnemonicAndSndFactor
    :: MonadUnliftIO m
    => Context
    -> ResourceT m (ApiWallet, [Text], [Text])
emptyWalletAndMnemonicAndSndFactor ctx = do
    mnemonic <-
        liftIO $ (mnemonicToText . entropyToMnemonic) <$> genEntropy @160
    sndFactor <-
        liftIO $ (mnemonicToText . entropyToMnemonic) <$> genEntropy @128
    let payload = Json [aesonQQ| {
            "name": "Empty Wallet",
            "mnemonic_sentence": #{mnemonic},
            "mnemonic_second_factor": #{sndFactor},
            "passphrase": #{fixturePassphrase}
        }|]
    r <- postWallet ctx payload
    expectResponseCode HTTP.status201 r
    return (getFromResponse id r, mnemonic, sndFactor)

-- | Create an empty wallet
emptyWallet
    :: MonadUnliftIO m
    => Context
    -> ResourceT m ApiWallet
emptyWallet ctx = do
    mnemonic <- liftIO $ (mnemonicToText . entropyToMnemonic) <$> genEntropy @160
    let payload = Json [aesonQQ| {
            "name": "Empty Wallet",
            "mnemonic_sentence": #{mnemonic},
            "passphrase": #{fixturePassphrase}
        }|]
    r <- postWallet ctx payload
    expectResponseCode HTTP.status201 r
    return (getFromResponse id r)

-- | Create an empty wallet
emptyWalletWith
    :: MonadUnliftIO m
    => Context
    -> (Text, Text, Int)
    -> ResourceT m ApiWallet
emptyWalletWith ctx (name, passphrase, addrPoolGap) = do
    mnemonic <- liftIO $ (mnemonicToText . entropyToMnemonic) <$> genEntropy @160
    let payload = Json [aesonQQ| {
            "name": #{name},
            "mnemonic_sentence": #{mnemonic},
            "passphrase": #{passphrase},
            "address_pool_gap" : #{addrPoolGap}
        }|]
    r <- postWallet ctx payload
    expectResponseCode HTTP.status201 r
    return (getFromResponse id r)

emptyWalletAndMnemonicWith
    :: MonadUnliftIO m
    => Context
    -> (Text, Text, Int)
    -> ResourceT m (ApiWallet, [Text])
emptyWalletAndMnemonicWith ctx (name, passphrase, addrPoolGap) = do
    mnemonic <- liftIO $ (mnemonicToText . entropyToMnemonic) <$> genEntropy @160
    let payload = Json [aesonQQ| {
            "name": #{name},
            "mnemonic_sentence": #{mnemonic},
            "passphrase": #{passphrase},
            "address_pool_gap" : #{addrPoolGap}
        }|]
    r <- postWallet ctx payload
    expectResponseCode HTTP.status201 r
    return (getFromResponse id r, mnemonic)

rewardWallet
    :: MonadUnliftIO m
    => Context
    -> ResourceT m (ApiWallet, Mnemonic 24)
rewardWallet ctx = do
    mw <- liftIO $ nextWallet @"reward" (_faucet ctx)
    let mnemonic = mnemonicToText mw
    let payload = Json [aesonQQ|{
            "name": "MIR Wallet",
            "mnemonic_sentence": #{mnemonic},
            "passphrase": #{fixturePassphrase}
        }|]
    r <- postWallet ctx payload
    expectResponseCode HTTP.status201 r
    let w = getFromResponse id r
    waitForNextEpoch ctx
    eventually "MIR wallet: wallet is 100% synced " $ do
        rg <- request @ApiWallet ctx (Link.getWallet @'Shelley w) Default Empty
        verify rg
            [ expectField (#balance . #available . #getQuantity) (.> 0)
            , expectField (#balance . #reward . #getQuantity) (.> 0)
            ]
        pure (getFromResponse id rg, mw)

fixtureMultiAssetWallet
    :: MonadIO m
    => Context
    -> ResourceT m ApiWallet
fixtureMultiAssetWallet = fmap fst . fixtureWalletWithMnemonics (Proxy @"ma")

fixtureMultiAssetRandomWallet
    :: forall n m.
        ( DecodeAddress n
        , DecodeStakeAddress n
        , EncodeAddress n
        , MonadIO m
        , MonadUnliftIO m
        )
    => Context
    -> ResourceT m ApiByronWallet
fixtureMultiAssetRandomWallet ctx = do
    wMA <- fixtureMultiAssetWallet ctx
    wB <- fixtureRandomWallet ctx

    -- create Byron address
    let p = Json [aesonQQ| { "passphrase": #{fixturePassphrase} }|]
    r <- request @(ApiAddress n) ctx (Link.postRandomAddress wB) Default p
    expectSuccess r

    -- pick out assets to send
    let assetsSrc = wMA ^. #assets . #total . #getApiT
    assetsSrc `shouldNotBe` mempty
    let val = minUTxOValue (_mainEra ctx) <$ pickAnAsset assetsSrc

    rL <- request @[ApiAddress n] ctx (Link.listAddresses @'Byron wB) Default Empty
    let addrs = getFromResponse id rL
    let destination = (addrs !! 1) ^. #id
    payload <- mkTxPayloadMA @n destination 0 [val] fixturePassphrase

    -- send assets to Byron wallet
    rtx <- request @(ApiTransaction n) ctx
        (Link.createTransactionOld @'Shelley wMA) Default payload
    expectResponseCode HTTP.status202 rtx

    eventually "Byron wallet has assets" $ do
        rb <- request @ApiByronWallet ctx
            (Link.getWallet @'Byron wB) Default Empty
        verify rb
            [ expectField (#assets . #available . #getApiT)
                (`shouldNotBe` TokenMap.empty)
            , expectField (#assets . #total . #getApiT)
                (`shouldNotBe` TokenMap.empty)
            , expectField (#state . #getApiT) (`shouldBe` Ready)
            ]
        return (getFromResponse id rb)

fixtureMultiAssetIcarusWallet
    :: forall n m.
        ( DecodeAddress n
        , DecodeStakeAddress n
        , EncodeAddress n
        , MonadIO m
        , MonadUnliftIO m
        )
    => Context
    -> ResourceT m ApiByronWallet
fixtureMultiAssetIcarusWallet ctx = do
    wMA <- fixtureMultiAssetWallet ctx
    wB <- fixtureIcarusWallet ctx

    -- pick out assets to send
    let assetsSrc = wMA ^. #assets . #total . #getApiT
    assetsSrc `shouldNotBe` mempty
    let val = minUTxOValue (_mainEra ctx) <$ pickAnAsset assetsSrc

    rL <- request @[ApiAddress n] ctx (Link.listAddresses @'Byron wB) Default Empty
    let addrs = getFromResponse id rL
    let destination = (addrs !! 1) ^. #id
    payload <- mkTxPayloadMA @n destination 0 [val] fixturePassphrase

    -- send assets to Icarus wallet
    rtx <- request @(ApiTransaction n) ctx
        (Link.createTransactionOld @'Shelley wMA) Default payload
    expectResponseCode HTTP.status202 rtx

    eventually "Icarus wallet has assets" $ do
        rb <- request @ApiByronWallet ctx
            (Link.getWallet @'Byron wB) Default Empty
        verify rb
            [ expectField (#assets . #available . #getApiT)
                (`shouldNotBe` TokenMap.empty)
            , expectField (#assets . #total . #getApiT)
                (`shouldNotBe` TokenMap.empty)
            ]
        return (getFromResponse id rb)

emptySharedWallet
    :: forall m. MonadUnliftIO m
    => Context
    -> ResourceT m ApiSharedWallet
emptySharedWallet ctx = do
   m15txt <- liftIO $ genMnemonics M15
   m12txt <- liftIO $ genMnemonics M12
   let (Right m15) = mkSomeMnemonic @'[ 15 ] m15txt
   let (Right m12) = mkSomeMnemonic @'[ 12 ] m12txt
   let passphrase = Passphrase $
           BA.convert $ T.encodeUtf8 fixturePassphrase
   let index = 30
   let accXPubDerived =
           sharedAccPubKeyFromMnemonics m15 (Just m12) index passphrase
   let payload = Json [aesonQQ| {
           "name": "Shared Wallet",
           "mnemonic_sentence": #{m15txt},
           "mnemonic_second_factor": #{m12txt},
           "passphrase": #{fixturePassphrase},
           "account_index": "30H",
           "payment_script_template":
               { "cosigners":
                   { "cosigner#0": #{accXPubDerived} },
                 "template":
                     { "all":
                        [ "cosigner#0" ]
                     }
               }
           } |]
   rPost <- postSharedWallet ctx Default payload
   verify (fmap (swapEither . view #wallet) <$> rPost)
       [ expectResponseCode HTTP.status201
       ]
   pure $ getFromResponse Prelude.id rPost

emptySharedWalletDelegating
    :: forall m. MonadUnliftIO m
    => Context
    -> ResourceT m (ApiSharedWallet,ApiSharedWallet)
emptySharedWalletDelegating ctx = do
   --cosigner#0
   m15txtA <- liftIO $ genMnemonics M15
   m12txtA <- liftIO $ genMnemonics M12
   let (Right m15A) = mkSomeMnemonic @'[ 15 ] m15txtA
   let (Right m12A) = mkSomeMnemonic @'[ 12 ] m12txtA
   let passphrase = Passphrase $
           BA.convert $ T.encodeUtf8 fixturePassphrase
   let indexA = 30
   let accXPubDerivedA =
           sharedAccPubKeyFromMnemonics m15A (Just m12A) indexA passphrase
   --cosigner#1
   m15txtB <- liftIO $ genMnemonics M15
   m12txtB <- liftIO $ genMnemonics M12
   let (Right m15B) = mkSomeMnemonic @'[ 15 ] m15txtB
   let (Right m12B) = mkSomeMnemonic @'[ 12 ] m12txtB
   let indexB = 40
   let accXPubDerivedB =
           sharedAccPubKeyFromMnemonics m15B (Just m12B) indexB passphrase

   let payloadA = Json [aesonQQ| {
           "name": "Shared Wallet",
           "mnemonic_sentence": #{m15txtA},
           "mnemonic_second_factor": #{m12txtA},
           "passphrase": #{fixturePassphrase},
           "account_index": "30H",
           "payment_script_template":
               { "cosigners":
                   { "cosigner#0": #{accXPubDerivedA}
                   , "cosigner#1": #{accXPubDerivedB}},
                 "template":
                     { "any":
                        [ "cosigner#0", "cosigner#1" ]
                     }
               },
           "delegation_script_template":
               { "cosigners":
                   { "cosigner#0": #{accXPubDerivedA}
                   , "cosigner#1": #{accXPubDerivedB}},
                 "template":
                     { "any":
                        [ "cosigner#0",
                          "cosigner#1"
                        ]
                     }
               }
           } |]
   rPostA <- postSharedWallet ctx Default payloadA
   verify (fmap (swapEither . view #wallet) <$> rPostA)
       [ expectResponseCode HTTP.status201
       ]

   let payloadB = Json [aesonQQ| {
           "name": "Shared Wallet",
           "mnemonic_sentence": #{m15txtB},
           "mnemonic_second_factor": #{m12txtB},
           "passphrase": #{fixturePassphrase},
           "account_index": "40H",
           "payment_script_template":
               { "cosigners":
                   { "cosigner#0": #{accXPubDerivedA}
                   , "cosigner#1": #{accXPubDerivedB}},
                 "template":
                     { "any":
                        [ "cosigner#0", "cosigner#1" ]
                     }
               },
           "delegation_script_template":
               { "cosigners":
                   { "cosigner#0": #{accXPubDerivedA}
                   , "cosigner#1": #{accXPubDerivedB}},
                 "template":
                     { "any":
                        [ "cosigner#0",
                          "cosigner#1"
                        ]
                     }
               }
           } |]
   rPostB <- postSharedWallet ctx Default payloadB
   verify (fmap (swapEither . view #wallet) <$> rPostB)
       [ expectResponseCode HTTP.status201
       ]

   pure $ (getFromResponse Prelude.id rPostA, getFromResponse Prelude.id rPostB)

fundSharedWallet
    :: forall (n :: NetworkDiscriminant) m.
    ( MonadUnliftIO m
    , DecodeStakeAddress n
    , DecodeAddress n
    , EncodeAddress n )
    => Context
    -> Natural
    -> NonEmpty ApiSharedWallet
    -> ResourceT m ()
fundSharedWallet ctx amt sharedWals = do
   let wal = case NE.head sharedWals of
           ApiSharedWallet (Right wal') -> wal'
           _ -> error
               "funding of shared wallet make sense only for active one"

   rAddr <- request @[ApiAddress n] ctx
       (Link.listAddresses @'Shared wal) Default Empty
   expectResponseCode HTTP.status200 rAddr
   let sharedAddrs = getFromResponse Prelude.id rAddr
   let destination = (sharedAddrs !! 1) ^. #id

   wShelley <- fixtureWallet ctx
   let payloadTx = Json [aesonQQ|{
           "payments": [{
               "address": #{destination},
               "amount": {
                   "quantity": #{amt},
                   "unit": "lovelace"
               }
           }],
           "passphrase": #{fixturePassphrase}
       }|]
   (_, ApiFee (Quantity _) (Quantity feeMax) _ _) <- unsafeRequest ctx
       (Link.getTransactionFeeOld @'Shelley wShelley) payloadTx
   let ep = Link.createTransactionOld @'Shelley
   rTx <- request @(ApiTransaction n) ctx (ep wShelley) Default payloadTx
   expectResponseCode HTTP.status202 rTx
   eventually "wShelley balance is decreased" $ do
       ra <- request @ApiWallet ctx
           (Link.getWallet @'Shelley wShelley) Default Empty
       expectField
           (#balance . #available)
           (`shouldBe` Quantity (faucetAmt - feeMax - amt)) ra

   forM_ sharedWals $ \walShared -> do
       rWal <- getSharedWallet ctx walShared
       verify (fmap (view #wallet) <$> rWal)
           [ expectResponseCode HTTP.status200
           , expectField (traverse . #balance . #available)
               (`shouldBe` Quantity amt)
           ]

fixtureSharedWallet
    :: forall (n :: NetworkDiscriminant) m.
    ( MonadUnliftIO m
    , MonadFail m
    , DecodeStakeAddress n
    , DecodeAddress n
    , EncodeAddress n )
    => Context
    -> ResourceT m ApiActiveSharedWallet
fixtureSharedWallet ctx = do
   walShared@(ApiSharedWallet (Right wal)) <- emptySharedWallet ctx
   fundSharedWallet @n ctx faucetUtxoAmt (NE.fromList [walShared])
   return wal

fixtureSharedWalletDelegating
    :: forall (n :: NetworkDiscriminant) m.
    ( MonadUnliftIO m
    , MonadFail m
    , DecodeStakeAddress n
    , DecodeAddress n
    , EncodeAddress n )
    => Context
    -> ResourceT m (ApiActiveSharedWallet, ApiActiveSharedWallet)
fixtureSharedWalletDelegating ctx = do
   (walSharedA@(ApiSharedWallet (Right walA)), ApiSharedWallet (Right walB)) <-
       emptySharedWalletDelegating ctx
   fundSharedWallet @n ctx faucetUtxoAmt (NE.fromList [walSharedA])
   return (walA, walB)

postSharedWallet
    :: MonadUnliftIO m
    => Context
    -> Headers
    -> Payload
    -> ResourceT m (HTTP.Status, Either RequestException ApiSharedWallet)
postSharedWallet ctx headers payload = snd <$> allocate create (free . snd)
  where
    create =
        request @ApiSharedWallet ctx (Link.postWallet @'Shared) headers payload

    free (Right (ApiSharedWallet (Left w))) = void $ request @Aeson.Value ctx
        (Link.deleteWallet @'Shared w) Default Empty
    free (Right (ApiSharedWallet (Right w))) = void $ request @Aeson.Value ctx
        (Link.deleteWallet @'Shared w) Default Empty
    free (Left _) = return ()

deleteSharedWallet
    :: forall m. MonadUnliftIO m
    => Context
    -> ApiSharedWallet
    -> m (HTTP.Status, Either RequestException Value)
deleteSharedWallet ctx = \case
      ApiSharedWallet (Left wal') -> r wal'
      ApiSharedWallet (Right wal') -> r wal'
  where
      r :: forall w. HasType (ApiT WalletId) w => w -> m (HTTP.Status, Either RequestException Value)
      r w = request @Aeson.Value ctx (Link.deleteWallet @'Shared w) Default Empty

getSharedWallet
    :: forall m. MonadUnliftIO m
    => Context
    -> ApiSharedWallet
    -> m (HTTP.Status, Either RequestException ApiSharedWallet)
getSharedWallet ctx = \case
    ApiSharedWallet (Left wal') -> r wal'
    ApiSharedWallet (Right wal') -> r wal'
  where
      r :: forall w. HasType (ApiT WalletId) w => w -> m (HTTP.Status, Either RequestException ApiSharedWallet)
      r w = request @ApiSharedWallet ctx (Link.getWallet @'Shared w) Default Empty

getSharedWalletKey
    :: forall m. MonadUnliftIO m
    => Context
    -> ApiSharedWallet
    -> Role
    -> DerivationIndex
    -> Maybe Bool
    -> m (HTTP.Status, Either RequestException ApiVerificationKeyShared)
getSharedWalletKey ctx wal role ix isHashed =
    case wal of
        ApiSharedWallet (Left wal') -> r wal'
        ApiSharedWallet (Right wal') -> r wal'
  where
      r :: forall w. HasType (ApiT WalletId) w => w -> m (HTTP.Status, Either RequestException ApiVerificationKeyShared)
      r w = request @ApiVerificationKeyShared ctx (Link.getWalletKey @'Shared w role ix isHashed) Default Empty

postAccountKeyShared
    :: forall m. MonadUnliftIO m
    => Context
    -> ApiSharedWallet
    -> DerivationIndex
    -> Headers
    -> Payload
    -> m (HTTP.Status, Either RequestException ApiAccountKeyShared)
postAccountKeyShared ctx wal ix headers payload =
    case wal of
        ApiSharedWallet (Left wal') -> r wal'
        ApiSharedWallet (Right wal') -> r wal'
  where
      r :: forall w. HasType (ApiT WalletId) w => w -> m (HTTP.Status, Either RequestException ApiAccountKeyShared)
      r w = request @ApiAccountKeyShared ctx (Link.postAccountKey @'Shared w ix) headers payload

getAccountKeyShared
    :: forall m. MonadUnliftIO m
    => Context
    -> ApiSharedWallet
    -> Maybe KeyFormat
    -> m (HTTP.Status, Either RequestException ApiAccountKeyShared)
getAccountKeyShared ctx wal isHashed =
    case wal of
        ApiSharedWallet (Left wal') -> r wal'
        ApiSharedWallet (Right wal') -> r wal'
  where
      r :: forall w. HasType (ApiT WalletId) w => w -> m (HTTP.Status, Either RequestException ApiAccountKeyShared)
      r w = request @ApiAccountKeyShared ctx (Link.getAccountKey @'Shared w isHashed) Default Empty

getSomeVerificationKey
    :: forall m. MonadUnliftIO m
    => Context
    -> ApiWallet
    -> m (ApiVerificationKeyShelley, ApiT (Hash "VerificationKey"))
getSomeVerificationKey ctx w = do
    let link = Link.getWalletKey @'Shelley w UtxoExternal (DerivationIndex 0) Nothing
    (_, vk@(ApiVerificationKeyShelley (bytes, _) _)) <-
        unsafeRequest @ApiVerificationKeyShelley ctx link Empty
    pure (vk, ApiT $ Hash $ blake2b224 @ByteString bytes)

patchEndpointEnding :: CredentialType -> Text
patchEndpointEnding = \case
    Payment -> "payment-script-template"
    Delegation -> "delegation-script-template"

patchSharedWallet
    :: forall m. MonadUnliftIO m
    => Context
    -> ApiSharedWallet
    -> CredentialType
    -> Payload
    -> m (HTTP.Status, Either RequestException ApiSharedWallet)
patchSharedWallet ctx wal cred payload =
    case wal of
        ApiSharedWallet (Left wal') -> r wal'
        ApiSharedWallet (Right wal') -> r wal'
  where
      r :: forall w. HasType (ApiT WalletId) w => w -> m (HTTP.Status, Either RequestException ApiSharedWallet)
      r w =
          let endpoint = "v2/shared-wallets" </> w ^. walletId </> patchEndpointEnding cred
          in request @ApiSharedWallet ctx ("PATCH", endpoint) Default payload

fixtureRawTx
    :: Context
    -> (Address, Natural)
    -> IO BL.ByteString
fixtureRawTx ctx (addr, amt) =
    nextTxBuilder (_faucet ctx) >>= \build ->
        BL.fromStrict <$> build (addr, Coin $ fromIntegral amt)

-- | Default passphrase used for fixture wallets
fixturePassphrase :: Text
fixturePassphrase =
    "cardano-wallet"

-- | fixturePassphrase encrypted by Scrypt function
fixturePassphraseEncrypted :: Text
fixturePassphraseEncrypted =
    "31347c387c317c2b6a6f747446495a6a566d586f43374c6c54425a576c\
    \597a425834515177666475467578436b4d485569733d7c78324d646738\
    \49554a3232507235676531393575445a76583646552b7757395a6a6a2f\
    \51303054356c654751794279732f7662753367526d726c316c657a7150\
    \43676d364e6758476d4d2f4b6438343265304b4945773d3d"


-- | Restore a faucet and wait until funds are available.
--
-- Note: @ResourceT@ is used to allow automatic garbage collection of unused
-- wallets through small blocks of @runResourceT@ (e.g. once per test). It
-- doesn't return @ReleaseKey@ since manual releasing is not needed.
fixtureWallet
    :: MonadIO m
    => Context
    -> ResourceT m ApiWallet
fixtureWallet = fmap fst . fixtureWalletWithMnemonics (Proxy @"shelley")

fixtureWalletWithMnemonics
    :: forall m scheme. (MonadIO m, NextWallet scheme)
    => Proxy scheme
    -> Context
    -> ResourceT m (ApiWallet, [Text])
fixtureWalletWithMnemonics _ ctx = snd <$> allocate create (free . fst)
  where
    create = do
        mnemonics <- mnemonicToText <$> nextWallet @scheme (_faucet ctx)
        let payload = Json [aesonQQ| {
                "name": "Faucet Wallet",
                "mnemonic_sentence": #{mnemonics},
                "passphrase": #{fixturePassphrase}
                } |]
        r <- request @ApiWallet ctx
            (Link.postWallet @'Shelley) Default payload
        expectResponseCode HTTP.status201 r
        let w = getFromResponse id r
        race (threadDelay sixtySeconds) (checkBalance w) >>= \case
            Left _ -> expectationFailure'
                "fixtureWallet: waited too long for initial transaction"
            Right a -> return (a, mnemonics)

    free w = void $ request @Aeson.Value ctx
        (Link.deleteWallet @'Shelley w) Default Empty
    sixtySeconds = 60*oneSecond
    checkBalance w = do
        r <- request @ApiWallet ctx
            (Link.getWallet @'Shelley w) Default Empty
        if getFromResponse (#balance . #available) r > Quantity 0
            then return (getFromResponse id r)
            else threadDelay oneSecond *> checkBalance w

-- | Restore a faucet Random wallet and wait until funds are available.
fixtureRandomWalletMws
    :: MonadUnliftIO m
    => Context
    -> ResourceT m (ApiByronWallet, Mnemonic 12)
fixtureRandomWalletMws ctx = do
    mnemonics <- liftIO $ nextWallet @"random" (_faucet ctx)
    (,mnemonics) <$> fixtureLegacyWallet ctx "random" (mnemonicToText mnemonics)

fixtureRandomWallet
    :: MonadUnliftIO m
    => Context
    -> ResourceT m ApiByronWallet
fixtureRandomWallet = fmap fst . fixtureRandomWalletMws

fixtureRandomWalletAddrs
    :: forall (n :: NetworkDiscriminant) m.
        ( PaymentAddress n ByronKey 'CredFromKeyK
        , MonadIO m
        , MonadUnliftIO m
        )
    => Context
    -> ResourceT m (ApiByronWallet, [Address])
fixtureRandomWalletAddrs =
    fmap (second (randomAddresses @n)) . fixtureRandomWalletMws

-- | Restore a wallet with the given UTxO distribution. Note that there's a
-- limitation to what can be done here. We only have 10 UTxO available in each
-- faucet and they "only" have 'faucetUtxoAmt = 100_000 Ada' in each.
--
-- This function makes no attempt at ensuring the request is valid, so be
-- careful.
--
-- TODO: Remove duplication between Shelley / Byron fixtures.
fixtureRandomWalletWith
    :: forall (n :: NetworkDiscriminant) m.
        ( EncodeAddress n
        , DecodeAddress n
        , DecodeStakeAddress n
        , PaymentAddress n ByronKey 'CredFromKeyK
        , MonadIO m
        , MonadUnliftIO m
        )
    => Context
    -> [Natural]
    -> ResourceT m ApiByronWallet
fixtureRandomWalletWith ctx coins0 = do
    src  <- fixtureRandomWallet ctx
    mws  <- liftIO $ entropyToMnemonic <$> genEntropy
    dest <- emptyByronWalletWith ctx "random"
        ("Random Wallet", mnemonicToText @12 mws, fixturePassphrase)
    let addrs = randomAddresses @n mws
    liftIO $ mapM_ (moveByronCoins @n ctx src (dest, addrs)) (groupsOf 10 coins0)
    void $ request @() ctx
        (Link.deleteWallet @'Byron src) Default Empty
    r <- request @ApiByronWallet ctx
        (Link.getWallet @'Byron dest) Default Empty
    expectResponseCode HTTP.status200 r
    pure (getFromResponse id r)

-- | Restore a faucet Icarus wallet and wait until funds are available.
fixtureIcarusWalletMws
    :: MonadUnliftIO m
    => Context
    -> ResourceT m (ApiByronWallet, Mnemonic 15)
fixtureIcarusWalletMws ctx = do
    mnemonics <- liftIO $ nextWallet @"icarus" (_faucet ctx)
    (,mnemonics) <$> fixtureLegacyWallet ctx "icarus" (mnemonicToText mnemonics)

fixtureIcarusWallet
    :: MonadUnliftIO m
    => Context
    -> ResourceT m ApiByronWallet
fixtureIcarusWallet = fmap fst . fixtureIcarusWalletMws

fixtureIcarusWalletAddrs
    :: forall (n :: NetworkDiscriminant) m.
        ( PaymentAddress n IcarusKey 'CredFromKeyK
        , MonadIO m
        , MonadUnliftIO m
        )
    => Context
    -> ResourceT m (ApiByronWallet, [Address])
fixtureIcarusWalletAddrs =
    fmap (second (icarusAddresses @n)) . fixtureIcarusWalletMws

-- | Restore a wallet with the given UTxO distribution. Note that there's a
-- limitation to what can be done here. We only have 10 UTxO available in each
-- faucet and they "only" have 'faucetUtxoAmt = 100_000 Ada' in each.
--
-- This function makes no attempt at ensuring the request is valid, so be
-- careful.
--
-- TODO: Remove duplication between Shelley / Byron fixtures.
fixtureIcarusWalletWith
    :: forall (n :: NetworkDiscriminant) m.
        ( EncodeAddress n
        , DecodeAddress n
        , DecodeStakeAddress n
        , PaymentAddress n IcarusKey 'CredFromKeyK
        , MonadIO m
        , MonadUnliftIO m
        )
    => Context
    -> [Natural]
    -> ResourceT m ApiByronWallet
fixtureIcarusWalletWith ctx coins0 = do
    src  <- fixtureIcarusWallet ctx
    mws  <- liftIO $ entropyToMnemonic <$> genEntropy
    dest <- emptyByronWalletWith ctx "icarus"
        ("Icarus Wallet", mnemonicToText @15 mws, fixturePassphrase)
    let addrs = icarusAddresses @n mws
    liftIO $ mapM_ (moveByronCoins @n ctx src (dest, addrs)) (groupsOf 10 coins0)
    void $ request @() ctx
        (Link.deleteWallet @'Byron src) Default Empty
    r <- request @ApiByronWallet ctx
        (Link.getWallet @'Byron dest) Default Empty
    expectResponseCode HTTP.status200 r
    pure (getFromResponse id r)


-- | Restore a legacy wallet (Byron or Icarus)
fixtureLegacyWallet
    :: forall m. MonadUnliftIO m
    => Context
    -> String
    -> [Text]
    -> ResourceT m ApiByronWallet
fixtureLegacyWallet ctx style mnemonics = snd <$> allocate create free
  where
    create = do
        let payload = Json [aesonQQ| {
                "name": "Faucet Byron Wallet",
                "mnemonic_sentence": #{mnemonics},
                "passphrase": #{fixturePassphrase},
                "style": #{style}
                } |]
        r <- request @ApiByronWallet ctx (Link.postWallet @'Byron) Default payload
        expectResponseCode HTTP.status201 r
        let w = getFromResponse id r
        liftIO $ race (threadDelay sixtySeconds) (checkBalance w) >>= \case
            Left _ ->
                expectationFailure'
                    "fixtureByronWallet: waited too long for initial transaction"
            Right a ->
                return a
    free w = do
        void $ request @() ctx
            (Link.deleteWallet @'Byron w) Default Empty

    sixtySeconds = 60*oneSecond
    checkBalance w = do
        r <- request @ApiByronWallet ctx
            (Link.getWallet @'Byron w) Default Empty
        if getFromResponse (#balance . #available) r > Quantity 0
            then return (getFromResponse id r)
            else threadDelay oneSecond *> checkBalance w

-- | Restore a wallet with the given UTxO distribution. Note that there's a
-- limitation to what can be done here. We only have 10 UTxO available in each
-- faucet and they "only" have 'faucetUtxoAmt = 100_000 Ada' in each.
--
-- This function makes no attempt at ensuring the request is valid, so be
-- careful.
fixtureWalletWith
    :: forall n m.
        ( EncodeAddress n
        , DecodeAddress n
        , DecodeStakeAddress n
        , MonadIO m
        , MonadUnliftIO m
        )
    => Context
    -> [Natural]
    -> ResourceT m ApiWallet
fixtureWalletWith ctx coins0 = do
    src <- fixtureWallet ctx
    dest <- emptyWallet ctx
    liftIO $ mapM_ (moveCoins src dest) (groupsOf 10 coins0)
    liftIO $ void $ request @() ctx
        (Link.deleteWallet @'Shelley src) Default Empty

    waitForTxImmutability ctx

    r <- request @ApiWallet ctx
        (Link.getWallet @'Shelley dest) Default Empty
    expectResponseCode HTTP.status200 r
    pure (getFromResponse id r)
  where
    -- | Move coins from a wallet to another
    moveCoins
        :: ApiWallet
            -- ^ Source Wallet
        -> ApiWallet
            -- ^ Destination wallet
        -> [Natural]
            -- ^ Coins to move
        -> IO ()
    moveCoins src dest coins = do
        balance <- getFromResponse (#balance . #available . #getQuantity)
            <$> request @ApiWallet ctx
                    (Link.getWallet @'Shelley dest) Default Empty
        addrs <- fmap (view #id) . getFromResponse id
            <$> request @[ApiAddress n] ctx
                    (Link.listAddresses @'Shelley dest) Default Empty
        let payments = for (zip coins addrs) $ \(amt, addr) -> [aesonQQ|{
                "address": #{addr},
                "amount": {
                    "quantity": #{amt},
                    "unit": "lovelace"
                }
            }|]
        let payload = Json [aesonQQ|{
                "payments": #{payments :: [Value]},
                "passphrase": #{fixturePassphrase}
            }|]
        request @(ApiTransaction n) ctx
            (Link.createTransactionOld @'Shelley src) Default payload
            >>= expectResponseCode HTTP.status202
        eventually "balance available = balance total" $ do
            rb <- request @ApiWallet ctx
                (Link.getWallet @'Shelley dest) Default Empty
            expectField
                (#balance . #available)
                (`shouldBe` Quantity (sum (balance:coins))) rb
            ra <- request @ApiWallet ctx
                (Link.getWallet @'Shelley src) Default Empty

            getFromResponse (#balance . #available) ra
                `shouldBe`
                    getFromResponse (#balance . #total) ra

-- | Create a fixture from the same mnemonic every time.
-- Don't wait for it to restore before returning.
constFixtureWalletNoWait :: MonadIO m => Context -> ResourceT m ApiWallet
constFixtureWalletNoWait ctx = snd <$> allocate create free
  where
    payload = Json [aesonQQ| {
            "name": "Fixed empty spec wallet",
            "mnemonic_sentence": #{mnemonicToText (head seqMnemonics)},
            "passphrase": #{fixturePassphrase}
        } |]
    create = do
        r <- request @ApiWallet ctx (Link.postWallet @'Shelley) Default payload
        expectResponseCode HTTP.status201 r
        pure $ getFromResponse id r
    free w = void $ request @Aeson.Value ctx
        (Link.deleteWallet @'Shelley w) Default Empty

-- | Move coins from a wallet to another
moveByronCoins
    :: forall (n :: NetworkDiscriminant).
        ( EncodeAddress n
        , DecodeAddress n
        , DecodeStakeAddress n
        )
    => Context
        -- ^ Api context
    -> ApiByronWallet
        -- ^ Source Wallet
    -> (ApiByronWallet, [Address])
        -- ^ Destination wallet
    -> [Natural]
        -- ^ Coins to move
    -> IO ()
moveByronCoins ctx src (dest, addrs) coins = do
    balance <- getFromResponse (#balance . #available . #getQuantity)
        <$> request @ApiByronWallet ctx (Link.getWallet @'Byron dest) Default Empty
    let payments = for (zip coins addrs) $ \(amt, addr) ->
            let addrStr = encodeAddress @n addr
            in [aesonQQ|{
                "address": #{addrStr},
                "amount": {
                    "quantity": #{amt},
                    "unit": "lovelace"
                }
            }|]
    let payload = Json [aesonQQ|{
            "payments": #{payments :: [Value]},
            "passphrase": #{fixturePassphrase}
        }|]
    request @(ApiTransaction n) ctx
        (Link.createTransactionOld @'Byron src) Default payload
        >>= expectResponseCode HTTP.status202
    eventually "balance available = balance total" $ do
        rb <- request @ApiByronWallet ctx
            (Link.getWallet @'Byron dest) Default Empty
        expectField (#balance . #available)
            (`shouldBe` Quantity (sum (balance:coins))) rb
        ra <- request @ApiByronWallet ctx
            (Link.getWallet @'Byron src) Default Empty
        getFromResponse (#balance . #available) ra
            `shouldBe` getFromResponse (#balance . #total) ra


-- | Total amount on each faucet wallet
faucetAmt :: Natural
faucetAmt = 10 * faucetUtxoAmt

-- | Each faucet wallet is composed of 10 times a single faucet UTxO of 100_000
-- Ada.
faucetUtxoAmt :: Natural
faucetUtxoAmt = ada 100_000
  where
    ada = (*) (1_000_000)

getFromResponse
    :: HasCallStack
    => Lens' s a
    -> (HTTP.Status, Either RequestException s)
    -> a
getFromResponse getter (_, res) = case res of
    Left _  -> error "getFromResponse failed to get item"
    Right s -> view getter s

getFromResponseList
    :: Int
    -> Lens' s a
    -> (HTTP.Status, Either RequestException [s])
    -> a
getFromResponseList i getter (_, res) = case res of
    Left _ -> error "getFromResponseList failed to get item"
    Right xs
        | length xs > i -> view getter (xs !! i)
        | otherwise -> error $
            "getFromResponseList: trying to access the #" <> show i <>
            " element from a list but there's none! "

json :: QuasiQuoter
json = aesonQQ

arbitraryStake :: Maybe Coin
arbitraryStake = Just $ ada 10_000_000_000
  where ada = Coin . (1_000_000 *)

joinStakePool
    :: forall n w m.
        ( HasType (ApiT WalletId) w
        , DecodeAddress n
        , DecodeStakeAddress n
        , MonadUnliftIO m
        )
    => Context
    -> ApiPoolSpecifier
    -> (w, Text)
    -> m (HTTP.Status, Either RequestException (ApiTransaction n))
joinStakePool ctx p (w, pass) = do
    let payload = Json [aesonQQ| { "passphrase": #{pass} } |]
    request @(ApiTransaction n) ctx
        (Link.joinStakePool (Identity p) w) Default payload

joinStakePoolUnsigned
    :: forall n style w.
        ( HasType (ApiT WalletId) w
        , DecodeAddress n
        , DecodeStakeAddress n
        , Link.Discriminate style
        )
    => Context
    -> w
    -> ApiT PoolId
    -> IO (HTTP.Status, Either RequestException (ApiCoinSelection n))
joinStakePoolUnsigned ctx w pid = do
    let payload = Json [aesonQQ| {
            "delegation_action": { "action": "join", "pool": #{pid} }
        } |]
    request @(ApiCoinSelection n) ctx
        (Link.selectCoins @style w) Default payload

quitStakePool
    :: forall n w m.
        ( HasType (ApiT WalletId) w
        , DecodeAddress n
        , DecodeStakeAddress n
        , MonadUnliftIO m
        )
    => Context
    -> (w, Text)
    -> m (HTTP.Status, Either RequestException (ApiTransaction n))
quitStakePool ctx (w, pass) = do
    let payload = Json [aesonQQ|{ "passphrase": #{pass} }|]
    request @(ApiTransaction n) ctx (Link.quitStakePool w) Default payload

quitStakePoolUnsigned
    :: forall n style w m.
        ( HasType (ApiT WalletId) w
        , DecodeAddress n
        , DecodeStakeAddress n
        , MonadIO m
        , Link.Discriminate style
        )
    => Context
    -> w
    -> m (HTTP.Status, Either RequestException (ApiCoinSelection n))
quitStakePoolUnsigned ctx w = liftIO $ do
    let payload = Json [aesonQQ|{ "delegation_action": { "action": "quit" } }|]
    request @(ApiCoinSelection n) ctx
        (Link.selectCoins @style w) Default payload

selectCoins
    :: forall n style w m.
        ( HasType (ApiT WalletId) w
        , DecodeAddress n
        , DecodeStakeAddress n
        , EncodeAddress n
        , Link.Discriminate style
        , MonadUnliftIO m
        )
    => Context
    -> w
    -> NonEmpty (AddressAmount (ApiT Address, Proxy n))
    -> m (HTTP.Status, Either RequestException (ApiCoinSelection n))
selectCoins ctx w payments =
    selectCoinsWith @n @style @w ctx w payments id

selectCoinsWith
    :: forall n style w m.
        ( HasType (ApiT WalletId) w
        , DecodeAddress n
        , DecodeStakeAddress n
        , EncodeAddress n
        , Link.Discriminate style
        , MonadUnliftIO m
        )
    => Context
    -> w
    -> NonEmpty (AddressAmount (ApiT Address, Proxy n))
    -> (Payload -> Payload)
    -> m (HTTP.Status, Either RequestException (ApiCoinSelection n))
selectCoinsWith ctx w payments transform = do
    let payload = Json [aesonQQ| {
            "payments": #{payments}
        } |]
    request @(ApiCoinSelection n) ctx
        (Link.selectCoins @style w) Default (transform payload)

delegationFee
    :: forall w m
     . (HasType (ApiT WalletId) w, MonadUnliftIO m)
    => Context
    -> w
    -> m (HTTP.Status, Either RequestException ApiFee)
delegationFee ctx w = do
    request @ApiFee ctx (Link.getDelegationFee w) Default Empty

-- | Generate an infinite list of addresses for random wallets.
--
-- To be typically used as:
--
-- >>> take 1 (randomAddresses @n)
-- [addr]
randomAddresses
    :: forall (n :: NetworkDiscriminant)
     . PaymentAddress n ByronKey 'CredFromKeyK
    => Mnemonic 12
    -> [Address]
randomAddresses mw =
    let
        (seed, pwd) =
            (SomeMnemonic mw, mempty)
        rootXPrv =
            Byron.generateKeyFromSeed seed pwd
        accXPrv =
            Byron.deriveAccountPrivateKey pwd rootXPrv minBound
        addrXPrv =
            Byron.deriveAddressPrivateKey pwd accXPrv
    in
        [ paymentAddress @n (publicKey $ addrXPrv ix)
        | ix <- [minBound..maxBound]
        ]

-- | Generate an infinite list of addresses for icarus wallets
--
-- To be typically used as:
--
-- >>> take 1 (icarusAddresses @n)
-- [addr]
icarusAddresses
    :: forall (n :: NetworkDiscriminant)
     . PaymentAddress n IcarusKey 'CredFromKeyK
    => Mnemonic 15
    -> [Address]
icarusAddresses mw =
    let
        (seed, pwd) =
            (SomeMnemonic mw, mempty)
        rootXPrv =
            Icarus.generateKeyFromSeed seed pwd
        accXPrv =
            deriveAccountPrivateKey pwd rootXPrv minBound
        addrXPrv =
            deriveAddressPrivateKey pwd accXPrv UtxoExternal
    in
        [ paymentAddress @n (publicKey $ addrXPrv ix)
        | ix <- [minBound..maxBound]
        ]

-- | Generate an infinite list of addresses for shelley wallets.
--
-- To be typically used as:
--
-- >>> take 1 (shelleyAddresses @n)
-- [addr]
shelleyAddresses
    :: forall (n :: NetworkDiscriminant)
     . PaymentAddress n ShelleyKey 'CredFromKeyK
    => Mnemonic 15
    -> [Address]
shelleyAddresses mw =
    let
        (seed, pwd) =
            (SomeMnemonic mw, mempty)
        rootXPrv =
            Shelley.generateKeyFromSeed (seed, Nothing) pwd
        accXPrv =
            deriveAccountPrivateKey pwd rootXPrv minBound
        addrXPrv =
            deriveAddressPrivateKey pwd accXPrv UtxoExternal
    in
        [ paymentAddress @n (publicKey $ addrXPrv ix)
        | ix <- [minBound..maxBound]
        ]

listAddresses
    :: forall n m. (MonadUnliftIO m, DecodeAddress n)
    => Context
    -> ApiWallet
    -> m [ApiAddress n]
listAddresses ctx w = do
    let link = Link.listAddresses @'Shelley w
    r <- request @[ApiAddress n] ctx link Default Empty
    expectResponseCode HTTP.status200 r
    return (getFromResponse id r)

signTx
    :: MonadUnliftIO m
    => Context
    -> ApiWallet
    -> ApiT SealedTx
    -> [(HTTP.Status, Either RequestException ApiSerialisedTransaction) -> m ()]
    -> m ApiSerialisedTransaction
signTx ctx w sealedTx expectations = do
    let toSign = Json [aesonQQ|
                           { "transaction": #{sealedTx}
                           , "passphrase": #{fixturePassphrase}
                           }|]
    let signEndpoint = Link.signTransaction @'Shelley w
    r <- request @ApiSerialisedTransaction ctx signEndpoint Default toSign
    verify r expectations
    pure $ getFromResponse Prelude.id r

signSharedTx
    :: MonadUnliftIO m
    => Context
    -> ApiActiveSharedWallet
    -> ApiT SealedTx
    -> [(HTTP.Status, Either RequestException ApiSerialisedTransaction) -> m ()]
    -> m ApiSerialisedTransaction
signSharedTx ctx w sealedTx expectations = do
    let toSign = Json [aesonQQ|
                           { "transaction": #{sealedTx}
                           , "passphrase": #{fixturePassphrase}
                           }|]
    let signEndpoint = Link.signTransaction @'Shared w
    r <- request @ApiSerialisedTransaction ctx signEndpoint Default toSign
    verify r expectations
    pure $ getFromResponse Prelude.id r

submitTx
    :: MonadUnliftIO m
    => Context
    -> ApiSerialisedTransaction
    -> [(HTTP.Status, Either RequestException ApiTxId) -> m ()]
    -> m ApiTxId
submitTx ctx tx expectations = do
    let bytes = serialisedTx $ getApiT (tx ^. #serialisedTxSealed)
    let submitEndpoint = Link.postExternalTransaction
    let headers = Headers
            [ ("Content-Type", "application/octet-stream")
            , ("Accept", "application/json")
            ]
    r <- request @ApiTxId ctx submitEndpoint headers (NonJson $ BL.fromStrict bytes)
    verify r expectations
    pure $ getFromResponse Prelude.id  r

submitTxWithWid
    :: MonadUnliftIO m
    => Context
    -> ApiWallet
    -> ApiSerialisedTransaction
    -> m (HTTP.Status, Either RequestException ApiTxId)
submitTxWithWid ctx w tx = do
    let submitEndpoint = Link.submitTransaction @'Shelley w
    let payload = Json $ Aeson.toJSON tx
    request @ApiTxId ctx submitEndpoint Default payload

submitSharedTxWithWid
    :: MonadUnliftIO m
    => Context
    -> ApiActiveSharedWallet
    -> ApiSerialisedTransaction
    -> m (HTTP.Status, Either RequestException ApiTxId)
submitSharedTxWithWid ctx w tx = do
    let submitEndpoint = Link.submitTransaction @'Shared w
    let payload = Json $ Aeson.toJSON tx
    request @ApiTxId ctx submitEndpoint Default payload

getWallet
    :: forall w m.
        ( MonadUnliftIO m
        , HasType (ApiT WalletId) w
        )
    => Context
    -> w
    -> m ApiWallet
getWallet ctx w = do
    let link = Link.getWallet @'Shelley w
    r <- request @ApiWallet ctx link Default Empty
    expectResponseCode HTTP.status200 r
    return (getFromResponse id r)

listAllTransactions
    :: forall n w m.
        ( DecodeAddress n
        , DecodeStakeAddress n
        , HasType (ApiT WalletId) w
        , MonadUnliftIO m
        )
    => Context
    -> w
    -> m [ApiTransaction n]
listAllTransactions ctx w =
    listTransactions ctx w Nothing Nothing (Just Descending)

listTransactions
    :: forall n w m.
        ( DecodeAddress n
        , DecodeStakeAddress n
        , HasType (ApiT WalletId) w
        , MonadUnliftIO m
        )
    => Context
    -> w
    -> Maybe UTCTime
    -> Maybe UTCTime
    -> Maybe SortOrder
    -> m [ApiTransaction n]
listTransactions ctx w mStart mEnd mOrder = do
    r <- request @[ApiTransaction n] ctx path Default Empty
    expectResponseCode HTTP.status200 r
    let txs = getFromResponse id r
    return txs
  where
    path = Link.listTransactions' @'Shelley w
        Nothing
        (Iso8601Time <$> mStart)
        (Iso8601Time <$> mEnd)
        mOrder

-- | Delete all wallets
deleteAllWallets :: Context -> IO ()
deleteAllWallets ctx = do
    resp <- request @[ApiWallet] ctx ("GET", "v2/wallets") Default Empty
    forM_ (wallets (snd resp)) $ \wal -> do
        let endpoint = "v2/wallets" </> wal ^. walletId
        d <- request @Value ctx ("DELETE", endpoint) None Empty
        expectResponseCode HTTP.status204 d
    respByron <-
        request @[ApiByronWallet] ctx ("GET", "v2/byron-wallets") Default Empty
    forM_ (wallets (snd respByron)) $ \wal -> do
        let endpoint = "v2/byron-wallets" </> wal ^. walletId
        d <- request @Value ctx ("DELETE", endpoint) None Empty
        expectResponseCode HTTP.status204 d
 where
     wallets :: forall w . Either RequestException [w] -> [w]
     wallets c = case c of
         Left e -> error $ "deleteAllWallets: Cannot return wallets: " <> show e
         Right s -> s

-- | Calls 'GET /wallets' and filters the response. This allows tests to be
-- written for a parallel setting.
listFilteredWallets
    :: MonadUnliftIO m
    => Set Text -- ^ Set of walletIds to include
    -> Context
    -> m (HTTP.Status, Either RequestException [ApiWallet])
listFilteredWallets include ctx = do
    (s, mwallets) <- request @[ApiWallet] ctx
        (Link.listWallets @'Shelley) Default Empty
    return (s, filter (\w -> (w ^. walletId) `Set.member` include) <$> mwallets)

-- | Calls 'GET /byron-wallets' and filters the response. This allows tests to
-- be written for a parallel setting.
listFilteredByronWallets
    :: MonadUnliftIO m
    => Set Text -- ^ Set of walletIds to include
    -> Context
    -> m (HTTP.Status, Either RequestException [ApiByronWallet])
listFilteredByronWallets include ctx = do
    (s, mwallets) <- request @[ApiByronWallet] ctx
        (Link.listWallets @'Byron) Default Empty
    return (s, filter (\w -> (w ^. walletId) `Set.member` include) <$> mwallets)

listFilteredSharedWallets
    :: MonadUnliftIO m
    => Set Text -- ^ Set of walletIds to include
    -> Context
    -> m (HTTP.Status, Either RequestException [ApiSharedWallet])
listFilteredSharedWallets include ctx = do
    (s, mwallets) <- request @[ApiSharedWallet] ctx
        (Link.listWallets @'Shared) Default Empty
    return (s, filter (\w -> (getWalletIdFromSharedWallet w ^. walletId) `Set.member` include) <$> mwallets)

getWalletIdFromSharedWallet :: ApiSharedWallet -> ApiT WalletId
getWalletIdFromSharedWallet (ApiSharedWallet (Right res)) = res ^. #id
getWalletIdFromSharedWallet (ApiSharedWallet (Left res)) = res ^. #id

-- | Wait for a booting wallet server to start. Wait up to 30s or fail.
waitForServer
    :: forall ctx m. (HasType (Port "wallet") ctx, MonadIO m)
    => ctx
    -> m ()
waitForServer ctx = liftIO $ void $ retrying
    (capDelay (30*oneSecond) $ constantDelay oneSecond)
    -- NOTE
    -- We still bind the output and error streams to some custom handles because
    -- if we don't, the library defaults to `stdout` and `stderr` which can get
    -- quite noisy.
    (\_ (e, _ :: Stderr, _ :: Stdout) -> pure $ e == ExitFailure 1)
    (const $ listWalletsViaCLI ctx)

unsafeCreateDigest :: Text -> Digest Blake2b_160
unsafeCreateDigest s = fromMaybe
    (error $ "unsafeCreateDigest failed to create digest from: " <> show s)
    (digestFromByteString $ B8.pack $ T.unpack s)

wantedSuccessButError
    :: (MonadIO m, Show e, HasCallStack)
    => e
    -> m ()
wantedSuccessButError = liftIO
    . expectationFailure
    . ("expected a successful response but got an error: " <>)
    . show

wantedErrorButSuccess
    :: (MonadIO m, Show a, HasCallStack)
    => a
    -> m ()
wantedErrorButSuccess = liftIO
    .  expectationFailure
    . ("expected an error but got a successful response: " <>)
    . show

-- | Applies the value 'a' to all assertions in the given sequence.
--
-- If any of the assertions fail, 'a' is shown as the counter-example text.
verify :: (Show a, MonadUnliftIO m) => a -> [a -> m ()] -> m ()
verify a = counterexample msg . mapM_ (a &)
  where
    msg = "While verifying value:\n"+|indentF 2 (pShowBuilder a)|+""

-- | Applies the value 'a' to all assertions in the given sequence.
--
-- Like 'verify', but the counterexample shows a description of what conditions
-- were being checked.
verifyMsg :: (Show a, MonadUnliftIO m) => String -> a -> [a -> m ()] -> m ()
verifyMsg desc a = counterexample msg . mapM_ (a &)
  where
    msg = "Verifying "+|desc|+" for value:\n"+|indentF 2 (pShowBuilder a)|+""

--
-- Manipulating endpoints
--

-- | Override the method of a particular endpoint, mostly to exercise invalid
-- endpoints from existing ones.
withMethod :: Method -> (Method, Text) -> (Method, Text)
withMethod method (_, path) = (method, path)

-- | Modifies the value of a path parameter at position 'n' (indexed from 0)
-- with the given update function. Throws if the given endpoint has no path
-- parameter in the given position.
--
-- >>> Link.getWallet @Shelley myWallet
-- ( "GET", "v2/wallets/2512a00e9653fe49a44a5886202e24d77eeb998f" )
--
-- >>> withPathParam 0 (<> "suffix") $ Link.getWallet @Shelley myWallet
-- ( "GET", "v2/wallets/2512a00e9653fe49a44a5886202e24d77eeb998fsuffix" )
--
-- >>> withPathParam 1 (const "suffix") $ Link.joinStakePool @Shelley myPool myWallet
-- ( "GET", "v2/stake-pools/2512a00e9653fe49a44a5886202e24d77eeb998f/wallets/patate" )
withPathParam :: Int -> (Text -> Text) -> (Method, Text) -> (Method, Text)
withPathParam n0 update (method, path) =
    case T.splitOn "/" path of
        [] -> errInvalidEndpoint

        version:q ->
            (method, T.intercalate "/" (version:findAndModify n0 q))
  where
    findAndModify 0 (k:v:q) = k : update v : q
    findAndModify n (k:v:q) = k : v : findAndModify (n - 1) q
    findAndModify _ _       = errInvalidEndpoint

    errInvalidEndpoint :: a
    errInvalidEndpoint =
        error $ "modifyPathParam: invalid endpoint: " <> show (method, path)

toQueryString :: [(Text, Text)] -> Text
toQueryString kvs = if T.null suffix then mempty else "?" <> suffix
  where
    suffix = T.intercalate "&" $ buildQueryParam <$> kvs
    buildQueryParam (k, v) = k <> "=" <> toQueryParam v

addField :: ToJSON a => Text -> a -> Payload -> Payload
addField fieldName field = \case
    Json (Aeson.Object o) ->
        Json (Aeson.Object (o <> ((Aeson.fromText fieldName) .= field)))
    _ ->
        error "addField called on a non-json payload"

infixr 5 </>
(</>) :: ToHttpApiData a => Text -> a -> Text
base </> thenext = mconcat [base, "/", toQueryParam thenext]

---
--- CLI
---

commandName :: String
commandName = "cardano-wallet"

-- | Run a command using the 'cardano-wallet' executable.
cardanoWalletCLI
    :: forall r m. (CmdResult r, MonadIO m)
    => [String]
    -> m r
cardanoWalletCLI = liftIO . command [EchoStderr False] commandName

generateMnemonicsViaCLI
    :: forall r m. (CmdResult r, MonadIO m)
    => [String]
    -> m r
generateMnemonicsViaCLI args = cardanoWalletCLI
    (["recovery-phrase", "generate"] ++ args)

createWalletViaCLI
    :: forall s m. (HasType (Port "wallet") s, MonadIO m)
    => s
    -> [String]
    -> String
    -> String
    -> String
    -> ResourceT m (ExitCode, String, Text)
createWalletViaCLI ctx args mnemonics secondFactor passphrase =
    snd <$> allocate create free
  where
    create = do
        let portArgs =
                [ "--port", show (ctx ^. typed @(Port "wallet")) ]
        let fullArgs =
                [ "wallet", "create", "from-recovery-phrase" ] ++ portArgs ++ args
        let process = proc' commandName fullArgs
        liftIO $ withCreateProcess process $
            \(Just stdin) (Just stdout) (Just stderr) h -> do
                hPutStr stdin mnemonics
                hPutStr stdin secondFactor
                hPutStr stdin (passphrase ++ "\n")
                hPutStr stdin (passphrase ++ "\n")
                hFlush stdin
                hClose stdin
                c <- waitForProcess h
                out <- TIO.hGetContents stdout
                err <- TIO.hGetContents stderr
                return (c, T.unpack out, err)
    free (ExitFailure _, _, _) = return ()
    free (ExitSuccess, output, _) = do
        w <- expectValidJSON (Proxy @ApiWallet) output
        let wid = T.unpack $ w ^. walletId
        void (try @_ @SomeException (deleteWalletViaCLI @() ctx wid))

createWalletFromPublicKeyViaCLI
    :: forall s m.
        ( HasType (Port "wallet") s

        , MonadIO m
        )
    => s
    -> [String]
        -- ^ NAME, [--address-pool-gap INT], ACCOUNT_PUBLIC_KEY
    -> ResourceT m (Exit, Stdout, Stderr)
createWalletFromPublicKeyViaCLI ctx args = snd <$> allocate create free
  where
    create =
        cardanoWalletCLI $
        [ "wallet", "create", "from-public-key", "--port"
        , show (ctx ^. typed @(Port "wallet"))] ++ args

    free (Exit (ExitFailure _), _, _) = return ()
    free (Exit ExitSuccess, Stdout output, _) = do
        w <- expectValidJSON (Proxy @ApiWallet) output
        let wid = T.unpack $ w ^. walletId
        void (try @_ @SomeException (deleteWalletViaCLI @() ctx wid))

deleteWalletViaCLI
    :: forall r s m.
        ( CmdResult r

        , HasType (Port "wallet") s
        , MonadIO m
        )
    => s
    -> String
    -> m r
deleteWalletViaCLI ctx walId = cardanoWalletCLI
    ["wallet", "delete", "--port", show (ctx ^. typed @(Port "wallet")), walId ]

getWalletViaCLI
    :: forall r s m.
        ( CmdResult r

        , HasType (Port "wallet") s
        , MonadIO m
        )
    => s
    -> String
    -> m r
getWalletViaCLI ctx walId = cardanoWalletCLI
    ["wallet", "get", "--port", show (ctx ^. typed @(Port "wallet")) , walId ]

getWalletUtxoStatisticsViaCLI
    :: forall r s m.
        ( CmdResult r

        , HasType (Port "wallet") s
        , MonadIO m)
    => s
    -> String
    -> m r
getWalletUtxoStatisticsViaCLI ctx walId = cardanoWalletCLI
    ["wallet", "utxo", "--port", show (ctx ^. typed @(Port "wallet")) , walId ]

getWalletUtxoSnapshotViaCLI
    :: forall r s m.
        ( CmdResult r
        , HasType (Port "wallet") s
        , MonadIO m
        )
    => s
    -> String
    -> m r
getWalletUtxoSnapshotViaCLI ctx walId = cardanoWalletCLI
    [ "wallet"
    , "utxo-snapshot"
    , "--port"
    , show (ctx ^. typed @(Port "wallet"))
    , walId
    ]

createAddressViaCLI
    :: forall s m. (HasType (Port "wallet") s, MonadIO m)
    => s
    -> [String]
        -- ^ Args
    -> String
        -- ^ Pass
    -> m (ExitCode, Text, Text)
        -- ^ (ExitCode, StdOut, StdErr)
createAddressViaCLI ctx args pass = do
    let execArgs =
            [ "address", "create"
            , "--port", show (ctx ^. typed @(Port "wallet"))
            ] ++ args
    let process = proc' commandName execArgs
    liftIO $ withCreateProcess process $
        \(Just stdin) (Just stdout) (Just stderr) h -> do
            hPutStr stdin (pass <> "\n")
            hFlush stdin
            hClose stdin
            c <- waitForProcess h
            out <- TIO.hGetContents stdout
            err <- TIO.hGetContents stderr
            pure (c, out, err)

importAddressViaCLI
    :: forall r s m. (CmdResult r, HasType (Port "wallet") s, MonadIO m)
    => s
    -> [String]
        -- ^ Args
    -> m r
        -- ^ (ExitCode, StdOut, StdErr)
importAddressViaCLI ctx args = cardanoWalletCLI $
    ["address", "import", "--port", show (ctx ^. typed @(Port "wallet"))] ++ args

listAddressesViaCLI
    :: forall r s m. (CmdResult r, HasType (Port "wallet") s, MonadIO m)
    => s
    -> [String]
    -> m r
listAddressesViaCLI ctx args = cardanoWalletCLI $
    ["address", "list", "--port", show (ctx ^. typed @(Port "wallet"))] ++ args

listStakePoolsViaCLI
    :: forall r s m. (CmdResult r, HasType (Port "wallet") s, MonadIO m)
    => s
    -> m r
listStakePoolsViaCLI ctx = cardanoWalletCLI
    ["stake-pool", "list", "--port", show (ctx ^. typed @(Port "wallet")) ]

listWalletsViaCLI
    :: forall r s m.
        ( CmdResult r

        , HasType (Port "wallet") s
        , MonadIO m
        )
    => s
    -> m r
listWalletsViaCLI ctx = cardanoWalletCLI
    ["wallet", "list", "--port", show (ctx ^. typed @(Port "wallet")) ]

updateWalletNameViaCLI
    :: forall r s m.
        ( CmdResult r

        , HasType (Port "wallet") s
        , MonadIO m
        )
    => s
    -> [String]
    -> m r
updateWalletNameViaCLI ctx args = cardanoWalletCLI
    (["wallet", "update", "name", "--port", walletPort] ++ args)
  where
    walletPort = show (ctx ^. typed @(Port "wallet"))

updateWalletPassphraseViaCLI
    :: forall s m. (HasType (Port "wallet") s, MonadIO m)
    => s
    -> String
        -- ^ Wallet id
    -> String
        -- ^ Old passphrase
    -> String
        -- ^ New passphrase
    -> String
        -- ^ New passphrase (repeated for confirmation)
    -> m (ExitCode, Text, Text)
updateWalletPassphraseViaCLI ctx wid ppOld ppNew ppNewConfirm = do
    let process = proc' commandName
            [ "wallet", "update", "passphrase"
            , "--port", show (ctx ^. typed @(Port "wallet"))
            , wid
            ]
    liftIO $ withCreateProcess process $
        \(Just stdin) (Just stdout) (Just stderr) h -> do
            hPutStr stdin (ppOld <> "\n")
            hPutStr stdin (ppNew <> "\n")
            hPutStr stdin (ppNewConfirm <> "\n")
            hFlush stdin
            hClose stdin
            c <- waitForProcess h
            out <- TIO.hGetContents stdout
            err <- TIO.hGetContents stderr
            pure (c, out, err)

updateWalletPassphraseWithMnemonicViaCLI
    :: forall s m. (HasType (Port "wallet") s, MonadIO m)
    => s
    -> String
        -- ^ Wallet id
    -> [Text]
        -- ^ Mnemonic
    -> String
        -- ^ New passphrase
    -> String
        -- ^ New passphrase (repeated for confirmation)
    -> m (ExitCode, Text, Text)
updateWalletPassphraseWithMnemonicViaCLI ctx wid mnemonic ppNew ppNewConfirm = do
    let process = proc' commandName
            [ "wallet", "update", "passphrase"
            , "--port", show (ctx ^. typed @(Port "wallet"))
            , "--mnemonic"
            , wid
            ]
    liftIO $ withCreateProcess process $
        \(Just stdin) (Just stdout) (Just stderr) h -> do
            hPutStr stdin (T.unpack (T.unwords mnemonic) <> "\n\n")
            hPutStr stdin (ppNew <> "\n")
            hPutStr stdin (ppNewConfirm <> "\n")
            hClose stdin
            c <- waitForProcess h
            out <- TIO.hGetContents stdout
            err <- TIO.hGetContents stderr
            pure (c, out, err)

postTransactionViaCLI
    :: forall s m. (HasType (Port "wallet") s, MonadIO m)
    => s
    -> String
    -> [String]
    -> m (ExitCode, String, Text)
postTransactionViaCLI ctx passphrase args = do
    let portArgs =
            ["--port", show (ctx ^. typed @(Port "wallet"))]
    let fullArgs =
            ["transaction", "create"] ++ portArgs ++ args
    let process = proc' commandName fullArgs
    liftIO $ withCreateProcess process $
        \(Just stdin) (Just stdout) (Just stderr) h -> do
            hPutStr stdin (passphrase ++ "\n")
            hFlush stdin
            hClose stdin
            out <- TIO.hGetContents stdout
            err <- TIO.hGetContents stderr
            -- For some reason, when
            -- - waitForProcess is called before hGetContents
            -- - os is windows
            -- - postTransactionViaCLI was called with >= 5 outputs
            -- waitForProcess blocks indefinetely. Hence we call waitForProcess
            -- last.
            c <- waitForProcess h
            return (c, T.unpack out, err)

postTransactionFeeViaCLI
    :: forall r s m. (CmdResult r, HasType (Port "wallet") s, MonadIO m)
    => s
    -> [String]
    -> m r
postTransactionFeeViaCLI ctx args = cardanoWalletCLI $ join
        [ ["transaction", "fees"]
        , ["--port", show (ctx ^. typed @(Port "wallet"))]
        , args
        ]

listTransactionsViaCLI
    :: forall r s m. (CmdResult r, HasType (Port "wallet") s, MonadIO m)
    => s
    -> TxMetadataSchema
    -> [String]
    -> m r
listTransactionsViaCLI ctx metadataSchema args = cardanoWalletCLI $ join
    [ ["transaction", "list"]
    , ["--port", show (ctx ^. typed @(Port "wallet"))]
    , ["--simple-metadata" | toSimpleMetadataFlag metadataSchema]
    , args
    ]

postExternalTransactionViaCLI
    :: forall r s m. (CmdResult r, HasType (Port "wallet") s, MonadIO m)
    => s
    -> [String]
    -> m r
postExternalTransactionViaCLI ctx args = cardanoWalletCLI $ join
    [ ["transaction", "submit"]
    , ["--port", show (ctx ^. typed @(Port "wallet"))]
    , args
    ]

deleteTransactionViaCLI
    :: forall r s m. (CmdResult r, HasType (Port "wallet") s, MonadIO m)
    => s
    -> String
    -> String
    -> m r
deleteTransactionViaCLI ctx wid tid = cardanoWalletCLI $ join
    [ ["transaction", "forget"]
    , ["--port", show (ctx ^. typed @(Port "wallet")), wid, tid]
    ]

getTransactionViaCLI
    :: forall r s m. (CmdResult r, HasType (Port "wallet") s, MonadIO m)
    => s
    -> String
    -> String
    -> TxMetadataSchema
    -> m r
getTransactionViaCLI ctx wid tid metadataSchema = cardanoWalletCLI $ join
    [ ["transaction", "get"]
    , ["--port", show (ctx ^. typed @(Port "wallet")), wid, tid]
    , ["--simple-metadata" | toSimpleMetadataFlag metadataSchema]
    ]

proc' :: FilePath -> [String] -> CreateProcess
proc' cmd args = (proc cmd args)
    { std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe }

oneSecond :: Int
oneSecond = 1_000 * oneMillisecond

oneMillisecond :: Int
oneMillisecond = 1_000

-- | Creates group of at most `n` elements. Last group may be smaller if
-- it's not properly divisible.
groupsOf :: Int -> [a] -> [[a]]
groupsOf _ [] = []
groupsOf n xs = take n xs : groupsOf n (drop n xs)

-- | 'map' flipped.
for :: [a] -> (a -> b) -> [b]
for = flip map

--
-- Helper for random wallets from Xprv
--
rootPrvKeyFromMnemonics :: [Text] -> Text -> Text
rootPrvKeyFromMnemonics mnemonics pass =
    T.decodeUtf8 $ hex $ getKey $ Byron.generateKeyFromSeed seed
        (preparePassphrase W.EncryptWithScrypt rawPassd)
 where
     (Right seed) = mkSomeMnemonic @'[12] mnemonics
     rawPassd = Passphrase $ BA.convert $ T.encodeUtf8 pass

--
-- Helper for HWWallets, getting pubKey from mnemonic sentence
--
pubKeyFromMnemonics :: [Text] -> Text
pubKeyFromMnemonics mnemonics =
    T.decodeUtf8 $ serializeXPub $ publicKey
       $ deriveAccountPrivateKey mempty rootXPrv minBound
 where
     seed = either (error . show) id $ mkSomeMnemonic @'[15,24] mnemonics
     rootXPrv = Shelley.generateKeyFromSeed (seed, Nothing) mempty

--
-- Helper for delegation statuses
--
getSlotParams
    :: MonadIO m
    => Context
    -> m (EpochNo, SlottingParameters)
getSlotParams ctx = do
    r1 <- liftIO $ request @ApiNetworkInformation ctx
          Link.getNetworkInfo Default Empty
    let ApiT currentEpoch =
             view (#slotId . #epochNumber)
            $ fromMaybe (error "getSlotParams: tip is Nothing")
            $ getFromResponse #networkTip r1

    let endpoint = ( "GET", "v2/network/parameters" )
    r2 <- liftIO $ request @ApiNetworkParameters ctx endpoint Default Empty
    let (Quantity slotL) = getFromResponse #slotLength r2
    let (Quantity epochL) = getFromResponse #epochLength r2
    let (Quantity coeff) = getFromResponse #activeSlotCoefficient r2
    let (Quantity k) = getFromResponse #securityParameter r2
    let sp = SlottingParameters
            (SlotLength slotL)
            (EpochLength epochL)
            (ActiveSlotCoefficient coeff)
            (Quantity k)

    return (currentEpoch, sp)

-- | Converts a transaction TTL in seconds into a number of slots, using the
-- slot length.
getTTLSlots
    :: MonadIO m
    => Context
    -> NominalDiffTime
    -> m SlotNo
getTTLSlots ctx dt = liftIO $ do
    _slotLenWrong <- unSlotLength . getSlotLength . snd <$> getSlotParams ctx
    let slotLen = 0.2 -- fixme: this is the value from byron genesis
    pure $ SlotNo $ ceiling $ dt / slotLen

-- | Wallet not delegating and not about to join any stake pool.
notDelegating
    :: [(Maybe (ApiT PoolId), EpochInfo)]
    -- ^ Pools to be joined & epoch at which the new delegation will become active
    -> ApiWalletDelegation
notDelegating nexts = ApiWalletDelegation
    { active = ApiWalletDelegationNext NotDelegating Nothing Nothing
    , next = flip map nexts $ \(mpid, epochInfo) -> case mpid of
        Nothing ->
            ApiWalletDelegationNext NotDelegating Nothing (Just epochInfo)
        Just pid ->
            ApiWalletDelegationNext Delegating (Just pid) (Just epochInfo)
    }

delegating
    :: ApiT PoolId
    -- ^ Pool joined
    -> [(Maybe (ApiT PoolId), EpochInfo)]
    -- ^ Pools to be joined & epoch at which the new delegation will become active
    -> ApiWalletDelegation
delegating pidActive nexts = (notDelegating nexts)
    { active = ApiWalletDelegationNext Delegating (Just pidActive) Nothing
    }


getRetirementEpoch :: StakePool -> Maybe EpochNo
getRetirementEpoch = fmap (view #epochNumber) . view #retirement

unsafeResponse :: (HTTP.Status, Either RequestException a) -> a
unsafeResponse = either (error . show) id . snd

-- | Returns the first address, modified to have the same stake key as the
-- second address.
--
-- Only intended to be used with well-known inputs in tests, so throws if
-- anything goes unexpectedly.
replaceStakeKey
    :: forall (n :: NetworkDiscriminant). (DecodeAddress n, EncodeAddress n)
    => (ApiT Address, Proxy n)
    -> (ApiT Address, Proxy n)
    -> (ApiT Address, Proxy n)
replaceStakeKey addr1 addr2 =
    let
        (hrp1, (tag1, pay1, _stake1)) = decodeAddr addr1
        (hrp2, (tag2, _pay2, stake2)) = decodeAddr addr2
    in
       if tag1 == tag2 && hrp1 == hrp2
       then encodeAddr (hrp1, (tag1, pay1, stake2))
       else error
        "replaceStakeKey: hrp or tag mismatch between addresses"

  where
    decodeAddr
        :: (ApiT Address, Proxy n)
        -> (Bech32.HumanReadablePart, (ByteString, ByteString, ByteString))
    decodeAddr =
        either
            (error . show)
            (second (splitAddr . fromJust . Bech32.dataPartToBytes))
        . Bech32.decodeLenient . encodeAddress @n . getApiT . fst
      where
        splitAddr :: ByteString -> (ByteString, ByteString, ByteString)
        splitAddr whole =
            let
                (tag, rest) = BS.splitAt 1 whole
                (paymentKeyH, stakeKeyH) = BS.splitAt 28 rest
            in
                case (BS.length tag, BS.length paymentKeyH, BS.length stakeKeyH) of
                    (1,28,28) -> (tag, paymentKeyH, stakeKeyH)
                    lengths   -> error $ mconcat
                        [ "replaceStakeKey: unknown lengths "
                        , show lengths
                        ]
    encodeAddr
        :: (Bech32.HumanReadablePart, (ByteString, ByteString, ByteString) )
        -> (ApiT Address, Proxy n)
    encodeAddr (hrp, (tag, pay, stake)) =
        let
            bytes = tag <> pay <> stake
            dp = Bech32.dataPartFromBytes bytes
            addr = either (error . show) id $
                decodeAddress @n $ Bech32.encodeLenient hrp dp
        in
            (ApiT addr, Proxy)
