{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Wallet.DB.Store.Submissions.New.StoreSpec ( spec ) where

import Prelude

import Cardano.DB.Sqlite
    ( ForeignKeysSetting (ForeignKeysDisabled) )
import Cardano.Wallet.DB.Arbitrary
    ()
import Cardano.Wallet.DB.Fixtures
    ( WalletProperty, logScale, withDBInMemory, withInitializedWalletProp )
import Cardano.Wallet.DB.Sqlite.Types
    ( TxId (..) )
import Cardano.Wallet.DB.Store.Submissions.New.Operations
    ( DeltaTxSubmissions, mkStoreSubmissions )
import Cardano.Wallet.Primitive.Types
    ( SlotNo (..) )
import Cardano.Wallet.Primitive.Types.Tx
    ( SealedTx (..), mockSealedTx )
import Cardano.Wallet.Submissions.OperationsSpec
    ( genOperationsDelta )
import Cardano.Wallet.Submissions.Submissions
    ( Submissions (..) )
import Control.Monad
    ( replicateM, void )
import Fmt
    ( Buildable (..) )
import System.Random
    ( Random )
import Test.DBVar
    ( prop_StoreUpdates )
import Test.Hspec
    ( Spec, around, describe, it )
import Test.QuickCheck
    ( Arbitrary (..), property )

import qualified Data.ByteString as BS

spec :: Spec
spec = do
    around (withDBInMemory ForeignKeysDisabled) $ do
        describe "submissions via API for a single wallet store" $ do
            it "respects store laws"
                $ property . prop_SingleWalletStoreLawsOperations

instance Buildable DeltaTxSubmissions where
    build = build . show

deriving instance Random SlotNo

prop_SingleWalletStoreLawsOperations :: WalletProperty
prop_SingleWalletStoreLawsOperations = withInitializedWalletProp
    $ \wid runQ -> do
        void $ prop_StoreUpdates
            runQ
            (mkStoreSubmissions wid)
            (pure $ Submissions mempty 0 0)
            (logScale . genOperationsDelta)

{-------------------------------------------------------------------------------
    Arbitrary instances
-------------------------------------------------------------------------------}
instance Arbitrary TxId where
    arbitrary = TxId <$> arbitrary

instance Arbitrary SealedTx where
    arbitrary = mockSealedTx . BS.pack <$> replicateM 16 arbitrary
