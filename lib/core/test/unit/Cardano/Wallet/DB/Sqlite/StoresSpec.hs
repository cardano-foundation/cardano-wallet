{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Cardano.Wallet.DB.Sqlite.StoresSpec
    ( spec
    ) where

import Prelude

import Cardano.DB.Sqlite
    ( SqliteContext (runQuery), newInMemorySqliteContext )
import Cardano.Wallet.DB.Arbitrary
    ()
import Cardano.Wallet.DB.Sqlite.AddressBook
    ( AddressBookIso (..), Prologue )
import Cardano.Wallet.DB.Sqlite.Stores
    ( PersistAddressBook (..) )
import Cardano.Wallet.DB.Sqlite.TH
    ( Wallet (..), migrateAll )
import Cardano.Wallet.DB.Sqlite.Types
    ( BlockId (..) )
import Cardano.Wallet.Primitive.AddressDerivation
    ( NetworkDiscriminant (Mainnet) )
import Cardano.Wallet.Primitive.AddressDerivation.Shared
    ( SharedKey )
import Cardano.Wallet.Primitive.AddressDerivation.Shelley
    ( ShelleyKey )
import Cardano.Wallet.Primitive.AddressDiscovery.Random
    ( RndState )
import Cardano.Wallet.Primitive.AddressDiscovery.Sequential
    ( SeqState )
import Cardano.Wallet.Primitive.AddressDiscovery.Shared
    ( Readiness (Pending), SharedState (..) )
import Cardano.Wallet.Primitive.Types
    ( WalletId (..) )
import Cardano.Wallet.Primitive.Types.Hash
    ( Hash (..) )
import Cardano.Wallet.Unsafe
    ( unsafeFromHex )
import Control.Tracer
    ( nullTracer )
import Data.Generics.Internal.VL
    ( withIso )
import Data.Time.Clock
    ( UTCTime )
import Data.Time.Clock.POSIX
    ( posixSecondsToUTCTime )
import Database.Persist.Sql
    ( Filter, deleteWhere, insert_ )
import Database.Persist.Sqlite
    ( SqlPersistT )
import Fmt
    ( Buildable, pretty )
import Test.Hspec
    ( Spec, around, describe, it )
import Test.QuickCheck
    ( Property, counterexample, property )
import Test.QuickCheck.Monadic
    ( PropertyM, assert, monadicIO, monitor, run )
import UnliftIO.Exception
    ( bracket )

import qualified Cardano.Wallet.DB.Sqlite.TH as TH

spec :: Spec
spec = around withDBInMemory $ do
    describe "Writing and loading from store" $ do
        it "loadPrologue . insertPrologue = id  for SeqState" $
            property . prop_prologue_load_write @(SeqState 'Mainnet ShelleyKey) id

        it "loadPrologue . insertPrologue = id  for RndState" $
            property . prop_prologue_load_write @(RndState 'Mainnet) id
        
        it "loadPrologue . insertPrologue = id  for SharedState" $
            property . prop_prologue_load_write @(SharedState 'Mainnet SharedKey)
                (\s -> s { ready = Pending })

{-------------------------------------------------------------------------------
    Properties
-------------------------------------------------------------------------------}
-- | Check that writing and loading the 'Prologue' works.
prop_prologue_load_write
    :: forall s.
    ( PersistAddressBook s
    , Buildable (Prologue s)
    , Eq (Prologue s)
    )
    => (s -> s) -> SqliteContext -> (WalletId, s) -> Property
prop_prologue_load_write preprocess db (wid, s) =
    prop_loadAfterWrite insertPrologue loadPrologue db (wid, pro)
  where
    (pro, _) = withIso addressIso $ \from _to -> from (preprocess s)
    -- FIXME during ADP-1043: See note at 'multisigPoolAbsent'

-- | Checks that loading a value after writing it to a database table
-- is successful.
prop_loadAfterWrite
    :: ( Buildable (f a) , Eq (f a), Applicative f )
    => (  WalletId
       -> a
       -> SqlPersistT IO ()
       ) -- ^ Write Operation
    -> (  WalletId
       -> SqlPersistT IO (f a)
       ) -- ^ Load Operation
    -> SqliteContext
    -> (WalletId, a)
        -- ^ Property arguments
    -> Property
prop_loadAfterWrite writeOp loadOp db (wid, a) =
    monadicIO (setup >> prop)
  where
    setup = do
        run $ runQuery db $ do
            cleanDB
            -- Add a wallet to ensure that FOREIGN PRIMARY constraints are satisfied
            insert_ $ Wallet
                { walId = wid
                , walName = "Stores"
                , walCreationTime = dummyUTCTime
                , walPassphraseLastUpdatedAt = Nothing
                , walPassphraseScheme = Nothing
                , walGenesisHash = BlockId dummyHash
                , walGenesisStart = dummyUTCTime
                }
    prop = do
        run $ runQuery db $ writeOp wid a
        res <- run $ runQuery db $ loadOp wid
        let fa = pure a
        monitor $ counterexample $ "\nInserted\n" <> pretty fa
        monitor $ counterexample $ "\nRead\n" <> pretty res
        assertWith "Inserted == Read" (res == fa)

-- | Like 'assert', but allow giving a label / title before running a assertion
assertWith :: String -> Bool -> PropertyM IO ()
assertWith lbl condition = do
    let flag = if condition then "✓" else "✗"
    monitor (counterexample $ lbl <> " " <> flag)
    assert condition

{-------------------------------------------------------------------------------
    DB setup
-------------------------------------------------------------------------------}
withDBInMemory :: (SqliteContext -> IO a) -> IO a
withDBInMemory action = bracket newDBInMemory fst (action . snd)

newDBInMemory :: IO (IO (), SqliteContext)
newDBInMemory = newInMemorySqliteContext nullTracer [] migrateAll

-- | Remove all tables in the datase.
cleanDB :: SqlPersistT IO ()
cleanDB = do
    deleteWhere ([] :: [Filter TH.Wallet])
    deleteWhere ([] :: [Filter TH.PrivateKey])
    deleteWhere ([] :: [Filter TH.TxMeta])
    deleteWhere ([] :: [Filter TH.TxIn])
    deleteWhere ([] :: [Filter TH.TxCollateral])
    deleteWhere ([] :: [Filter TH.TxOut])
    deleteWhere ([] :: [Filter TH.TxOutToken])
    deleteWhere ([] :: [Filter TH.TxWithdrawal])
    deleteWhere ([] :: [Filter TH.LocalTxSubmission])
    deleteWhere ([] :: [Filter TH.Checkpoint])
    deleteWhere ([] :: [Filter TH.ProtocolParameters])
    deleteWhere ([] :: [Filter TH.StakeKeyCertificate])
    deleteWhere ([] :: [Filter TH.DelegationCertificate])
    deleteWhere ([] :: [Filter TH.DelegationReward])
    deleteWhere ([] :: [Filter TH.UTxO])
    deleteWhere ([] :: [Filter TH.UTxOToken])
    deleteWhere ([] :: [Filter TH.SeqState])
    deleteWhere ([] :: [Filter TH.SeqStateAddress])
    deleteWhere ([] :: [Filter TH.SeqStatePendingIx])
    deleteWhere ([] :: [Filter TH.RndState])
    deleteWhere ([] :: [Filter TH.RndStateAddress])
    deleteWhere ([] :: [Filter TH.RndStatePendingAddress])
    deleteWhere ([] :: [Filter TH.SharedState])
    deleteWhere ([] :: [Filter TH.CosignerKey])

{-------------------------------------------------------------------------------
    Arbitrary
-------------------------------------------------------------------------------}
dummyUTCTime :: UTCTime
dummyUTCTime = posixSecondsToUTCTime 1506203091

dummyHash :: Hash "BlockHeader"
dummyHash = Hash $ unsafeFromHex
    "5f20df933584822601f9e3f8c024eb5eb252fe8cefb24d1317dc3d432e940ebb"
