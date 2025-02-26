{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Wallet.UI.Deposit.API.Payments
where

import Prelude

import Cardano.Wallet.Deposit.Pure
    ( BIP32Path (..)
    , DerivationType (..)
    , Word31
    )
import Cardano.Wallet.Deposit.Pure.API.Address
    ( encodeAddress
    )
import Cardano.Wallet.Deposit.Write
    ( Address
    )
import Cardano.Wallet.UI.Deposit.Types.Payments
    ( Receiver (..)
    )
import Data.Aeson
    ( FromJSON (parseJSON)
    , KeyValue ((.=))
    , ToJSON (toJSON)
    , object
    , withObject
    , withText
    , (.:)
    , (.:?)
    )
import Data.Aeson.Types
    ( Parser
    , parseFail
    )
import Data.Map.Monoidal.Strict
    ( MonoidalMap
    )
import Data.Semigroup
    ( Sum (..)
    )
import Data.Text
    ( Text
    )
import GHC.Generics
    ( Generic
    )
import Numeric.Natural
    ( Natural
    )
import Servant
    ( FromHttpApiData (..)
    , ToHttpApiData
    )
import Servant.API
    ( ToHttpApiData (..)
    )
import Web.FormUrlEncoded
    ( FromForm (..)
    , parseMaybe
    , parseUnique
    )

import qualified Data.Aeson as Aeson
import qualified Data.Map.Monoidal.Strict as MonoidalMap
import Data.Maybe
    ( fromMaybe
    )
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL

newtype NewReceiver = NewReceiver Receiver

data AddReceiverForm = AddReceiverForm
    { newReceiver :: NewReceiver
    , addReceiverState :: State
    }

instance FromForm AddReceiverForm where
    fromForm form = do
        newReceiver <- fromForm form
        addReceiverState <- fromForm form
        pure AddReceiverForm{newReceiver, addReceiverState}

instance FromForm NewReceiver where
    fromForm form = do
        address <- parseUnique "new-receiver-address" form
        amountDouble :: Double <- parseUnique "new-receiver-amount" form
        let amount = round $ amountDouble * 1_000_000
        pure
            $ NewReceiver
            $ Receiver{address, amount}

data NewReceiverValidation
    = NewReceiverValidation
    { addressValidation :: Maybe Text
    , amountValidation :: Maybe Text
    }

instance FromForm NewReceiverValidation where
    fromForm form = do
        addressValidation <- parseMaybe "new-receiver-address" form
        amountValidation <- parseMaybe "new-receiver-amount" form
        pure $ NewReceiverValidation{addressValidation, amountValidation}

data Transaction = Transaction
    { dataType :: !Text
    , description :: !Text
    , cborHex :: !Text
    , bip32Paths :: ![BIP32Path]
    }
    deriving (Eq, Show, Generic)

instance ToJSON Transaction where
    toJSON Transaction{dataType, description, cborHex, bip32Paths} =
        object
            [ "type" .= dataType
            , "description" .= description
            , "cborHex" .= cborHex
            , "bip32Paths" .= bip32Paths
            ]

instance FromJSON Transaction where
    parseJSON = withObject "Transaction" $ \o -> do
        dataType <- o .: "type"
        description <- o .: "description"
        cborHex <- o .: "cborHex"
        bip32Paths <- o .:? "bip32Paths"
        pure
            Transaction
                { dataType
                , description
                , cborHex
                , bip32Paths = fromMaybe [] bip32Paths
                }

-- Orphan instances for BIP32Path
-- TODO: move where they belong, in the module defining BIP32Path
instance ToJSON BIP32Path where
    toJSON = toJSON . encodeBIP32
      where
        encodeBIP32 = \case
            (Segment Root Hardened n) -> T.pack (show n) <> "H"
            (Segment Root Soft n) -> T.pack (show n)
            (Segment p Hardened n) ->
                encodeBIP32 p
                    <> "/"
                    <> T.pack (show n)
                    <> "H"
            (Segment p Soft n) ->
                encodeBIP32 p <> "/" <> T.pack (show n)
            Root -> ""

instance FromJSON BIP32Path where
    parseJSON = withText "BIP32Path" parseBip32
      where
        parseBip32 :: Text -> Parser BIP32Path
        parseBip32 t = case T.splitOn "/" t of
            [""] -> pure Root
            xs -> foldSegments <$> traverse parseSegment xs

        foldSegments :: [(Word31, DerivationType)] -> BIP32Path
        foldSegments = foldl (\p (i, t) -> Segment p t i) Root

        parseSegment :: Text -> Parser (Word31, DerivationType)
        parseSegment t = case T.stripSuffix "H" t of
            Nothing -> do
                s <- parseIndex t
                pure (s, Soft)
            Just t' -> do
                s <- parseIndex t'
                pure (s, Hardened)
          where
            parseIndex :: Text -> Parser Word31
            parseIndex text = case reads $ T.unpack text of
                [(i, "")] -> pure i
                _ -> parseFail "Invalid index"

newtype Password = Password Text

data SignatureForm
    = SignatureForm
        { signatureFormState :: State
        , signaturePassword :: Password
        }
    | ExternalSignatureForm
        { signatureFormState :: State
        , signatureSignedTransaction :: Transaction
        }

instance FromForm SignatureForm where
    fromForm form = do
        signatureFormState <- fromForm form
        let
            signature = do
                signaturePassword <- Password <$> parseUnique "passphrase" form
                pure SignatureForm{signatureFormState, signaturePassword}
            externalSignature = do
                signatureSignedTransaction <- parseUnique "signed-transaction" form
                pure
                    ExternalSignatureForm{signatureFormState, signatureSignedTransaction}
        case signature of
            Left _ -> externalSignature
            Right s -> pure s

instance FromHttpApiData Transaction where
    parseQueryParam :: Text -> Either Text Transaction
    parseQueryParam t = case Aeson.decode $ TL.encodeUtf8 $ TL.fromStrict t of
        Nothing -> Left "Invalid JSON for a Transaction"
        Just tx -> pure tx

data StateA t
    = NoState
    | Unsigned t
    | Signed Transaction t
    | Submitted Transaction t
    deriving (Eq, Show, Generic, Functor, Foldable, Traversable)

type State = StateA Transaction

instance ToJSON State
instance FromJSON State

instance FromHttpApiData State where
    parseQueryParam :: Text -> Either Text State
    parseQueryParam t = case Aeson.decode $ TL.encodeUtf8 $ TL.fromStrict t of
        Nothing -> Left "Invalid JSON for a State"
        Just tx -> pure tx

instance FromForm State where
    fromForm form = do
        r <- parseMaybe "payment-state" form
        case r of
            Nothing -> pure NoState
            Just tx -> pure tx

data Signal
    = AddReceiver Receiver
    | DeleteReceiver Address
    | Sign Password
    | ExternallySign Transaction
    | Unsign
    | Submit
    | Reset

type Receivers = MonoidalMap Address (Sum Natural)

data Payment m = Payment
    { unsigned :: Receivers -> m Transaction
    , sign :: Transaction -> Password -> m Transaction
    , submit :: Transaction -> m ()
    , receivers :: Transaction -> m Receivers
    }

onReceivers
    :: Monad m
    => Payment m
    -> Transaction
    -> (Receivers -> Receivers)
    -> m Receivers
onReceivers Payment{receivers} tx f = do
    rs <- receivers tx
    pure $ f rs

deleteReceiver
    :: Monad m => Payment m -> Transaction -> Address -> m State
deleteReceiver c tx a = do
    rs' <- onReceivers c tx $ \r ->
        MonoidalMap.filter (> 0)
            $ MonoidalMap.delete a r
    if null rs'
        then pure NoState
        else Unsigned <$> unsigned c rs'

addReceiver
    :: Monad m => Payment m -> Transaction -> Receiver -> m State
addReceiver c tx r = do
    rs' <- onReceivers c tx $ \rs -> rs <> singleReceivers r
    Unsigned <$> unsigned c rs'

singleReceivers :: Receiver -> Receivers
singleReceivers Receiver{address, amount} =
    MonoidalMap.singleton address (Sum amount)

step :: Monad m => Payment m -> State -> Signal -> m (Maybe State)
step _ _ Reset = pure $ Just NoState
step c NoState (AddReceiver receiver) = do
    tx <- unsigned c (singleReceivers receiver)
    pure $ Just $ Unsigned tx
step c (Unsigned utx) (AddReceiver receiver) = do
    Just <$> addReceiver c utx receiver
step c (Unsigned utx) (DeleteReceiver addr) = do
    Just <$> deleteReceiver c utx addr
step c (Unsigned utx) (Sign pwd) = do
    stx <- sign c utx pwd
    pure $ Just $ Signed utx stx
step _ (Unsigned utx) (ExternallySign stx) = do
    pure $ Just $ Signed utx stx
step c (Signed utx _) (AddReceiver receiver) = do
    Just <$> addReceiver c utx receiver
step c (Signed utx _) (DeleteReceiver addr) = do
    Just <$> deleteReceiver c utx addr
step c (Signed utx stx) Submit = do
    submit c stx
    pure $ Just $ Submitted utx stx
step _ _ _ = pure Nothing

instance ToHttpApiData Address where
    toUrlPiece = encodeAddress
