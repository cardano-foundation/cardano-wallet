{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Integration.Scenario.API.Miscellaneous
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Api.Types
    ( ApiT (..)
    , DecodeAddress
    , DecodeStakeAddress
    , EncodeAddress
    , WalletStyle (..)
    )
import Cardano.Wallet.Primitive.Types.TokenQuantity
    ( TokenQuantity (..) )
import Control.Monad.IO.Class
    ( MonadIO (..) )
import Control.Monad.Trans.Resource
    ( runResourceT )
import Data.Aeson
    ( ToJSON (..), Value (..) )
import Data.Either
    ( isLeft, isRight )
import Data.Generics.Internal.VL.Lens
    ( view )
import Data.Text
    ( Text )
import Data.Text.Class
    ( FromText (..), ToText (..) )
import Network.Wai.Middleware.BigInt
    ( traverseJSON )
import Test.Hspec
    ( SpecWith, describe )
import Test.Hspec.Expectations.Lifted
    ( shouldSatisfy )
import Test.Hspec.Extra
    ( it )
import Test.Integration.Framework.DSL
    ( Context (..)
    , Headers (..)
    , Payload (..)
    , fixtureWallet
    , listAddresses
    , request
    , waitForTxImmutability
    )

import qualified Cardano.Wallet.Api.Link as Link
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

spec :: forall n.
    ( DecodeAddress n
    , DecodeStakeAddress n
    , EncodeAddress n
    ) => SpecWith Context
spec = describe "MISCELLANEOUS" $ do
    it "MISC_01 - API can fallback to string for big int when asked" $ \ctx ->
      runResourceT $ do
        src <- fixtureWallet ctx
        addr:_ <- fmap (getApiT . fst . view #id) <$> listAddresses @n ctx src

        -- In some languages like JavaScript, large numbers can't be parsed
        -- easily from JSON (because it represents all numbers using `Double`).
        --
        -- The strategy is therefore usually to represent them as JSON strings,
        -- and use a dedicated library afterwards to handle big integers.
        --
        -- In JavaScript, the max safe integer is 2^53 - 1
        let safeInteger = 2 ^ (53 :: Integer) - 1

        let moreThanJsCanTake = 2 * safeInteger
        let quantity = TokenQuantity $ fromIntegral moreThanJsCanTake
        liftIO $ _mintSeaHorseAssets ctx quantity 1 [addr]
        waitForTxImmutability ctx

        -- With default headers, large integers are returned as integers.
        (_, Right r1) <- request @Value ctx (Link.getWallet @'Shelley src) Default Empty
        r1 `shouldUseIntegerForIntegerLargerThan` safeInteger

        -- When 'X-Max-Safe-Integer' is set, there's no integers in the JSON
        -- response bigger than the given value. Instead, values have been
        -- turned to string.
        let headers = Headers [("X-Max-Safe-Integer", T.encodeUtf8 (toText safeInteger))]
        (_, Right r2) <- request @Value ctx (Link.getWallet @'Shelley src) headers Empty
        r2 `shouldUseStringForIntegerLargerThan` safeInteger

--
-- Helpers
--

shouldUseStringForIntegerLargerThan
    :: MonadIO m => Value -> Integer -> m ()
shouldUseStringForIntegerLargerThan json iMax = do
    traverseJSON (matchNoIntegerBigger iMax) matchAll json `shouldSatisfy` isRight
    traverseJSON matchAll (matchNoTextBigger iMax) json `shouldSatisfy` isLeft

shouldUseIntegerForIntegerLargerThan
    :: MonadIO m => Value -> Integer -> m ()
shouldUseIntegerForIntegerLargerThan json iMax = do
    traverseJSON (matchNoIntegerBigger iMax) matchAll json `shouldSatisfy` isLeft
    traverseJSON matchAll (matchNoTextBigger iMax) json `shouldSatisfy` isRight

matchAll :: (ToJSON a, Applicative f) => a -> f Value
matchAll = pure . toJSON

matchNoIntegerBigger :: Integer -> Integer -> Either String Value
matchNoIntegerBigger iMax i
    | i > iMax  = Left  $ "Found an integer bigger than target: " <> show i
    | otherwise = Right $ toJSON i

matchNoTextBigger :: Integer -> Text -> Either String Value
matchNoTextBigger iMax s =
    case fromText s of
        Right i | i > iMax ->
            Left $ "Found a string bigger than target: " <> T.unpack s
        _ ->
            Right $ toJSON s
