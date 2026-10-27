{-# LANGUAGE LambdaCase #-}

module Cardano.Wallet.Network.LocalStateQuery.DRepsSpec
    ( spec
    ) where

import Cardano.Wallet.Network.LocalStateQuery.DReps
    ( credToDRepID
    , drepIDToCred
    )
import Cardano.Wallet.Primitive.Ledger.Convert
    ( toLedgerDRep
    )
import Cardano.Wallet.Primitive.Types.DRep
    ( DRep (..)
    , DRepID (..)
    , DRepKeyHash (..)
    , DRepScriptHash (..)
    )
import Data.ByteString
    ( ByteString
    )
import Test.Hspec
    ( Spec
    , describe
    , it
    )
import Test.QuickCheck
    ( Gen
    , Property
    , arbitrary
    , checkCoverage
    , cover
    , forAll
    , oneof
    , vectorOf
    , (===)
    )
import Prelude

import qualified Cardano.Ledger.DRep as Ledger
import qualified Data.ByteString as BS

spec :: Spec
spec = describe "drepIDToCred" $ do
    it
        "is the credential that a vote delegation certificate delegates to"
        $ prop_drepIDToCred
        $ \drep ->
            Ledger.DRepCredential (drepIDToCred drep)
                === toLedgerDRep (FromDRepID drep)
    it "is inverted by credToDRepID"
        $ prop_drepIDToCred
        $ \drep -> credToDRepID (drepIDToCred drep) === Just drep

prop_drepIDToCred :: (DRepID -> Property) -> Property
prop_drepIDToCred prop =
    forAll genDRepID $ \drep ->
        checkCoverage
            $ cover 30 (isKeyHash drep) "a key-hash DRep"
            $ cover 30 (not $ isKeyHash drep) "a script-hash DRep"
            $ prop drep
  where
    isKeyHash = \case
        DRepFromKeyHash _ -> True
        DRepFromScriptHash _ -> False

genDRepID :: Gen DRepID
genDRepID =
    oneof
        [ DRepFromKeyHash . DRepKeyHash <$> genHash
        , DRepFromScriptHash . DRepScriptHash <$> genHash
        ]

genHash :: Gen ByteString
genHash = BS.pack <$> vectorOf 28 arbitrary
