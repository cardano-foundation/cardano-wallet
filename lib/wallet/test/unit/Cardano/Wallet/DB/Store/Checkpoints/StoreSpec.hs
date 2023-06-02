{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Cardano.Wallet.DB.Store.Checkpoints.StoreSpec
    ( spec
    ) where

import Prelude

import Cardano.DB.Sqlite
    ( ForeignKeysSetting (..), SqliteContext (runQuery) )
import Cardano.Wallet.Address.Book
    ( AddressBookIso (..), Prologue, getPrologue )
import Cardano.Wallet.Address.Derivation.Shared
    ( SharedKey )
import Cardano.Wallet.Address.Derivation.Shelley
    ( ShelleyKey )
import Cardano.Wallet.Address.Discovery.Random
    ( RndState )
import Cardano.Wallet.Address.Discovery.Sequential
    ( SeqState )
import Cardano.Wallet.Address.Discovery.Shared
    ( Readiness (Pending), SharedState (..) )
import Cardano.Wallet.DB.Arbitrary
    ()
import Cardano.Wallet.DB.Fixtures
    ( initializeWallet, withDBInMemory )
import Cardano.Wallet.DB.Store.Checkpoints.Store
    ( PersistAddressBook (..) )
import Cardano.Wallet.Primitive.Types
    ( WalletId )
import Cardano.Wallet.Read.NetworkId
    ( NetworkDiscriminant (..) )
import Fmt
    ( Buildable (..), pretty )
import Test.Hspec
    ( Spec, around, describe, it )
import Test.QuickCheck
    ( Property, counterexample, property )
import Test.QuickCheck.Monadic
    ( PropertyM, assert, monadicIO, monitor, run )

spec :: Spec
spec = do
    around (withDBInMemory ForeignKeysEnabled) $ do
        describe "Writing and loading" $ do
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
    )
    => (s -> s) -> SqliteContext -> (WalletId, s) -> Property
prop_prologue_load_write preprocess db (wid, s) =
    monadicIO $ run (toIO setup) >> prop
  where
    toIO = runQuery db
    setup = initializeWallet wid
    prop = prop_loadAfterWrite toIO (insertPrologue wid) (loadPrologue wid) pro
    pro = getPrologue $ preprocess s
    -- FIXME during ADP-1043: See note at 'multisigPoolAbsent'

-- | Checks that loading a value after writing it to a database table
-- is successful.
prop_loadAfterWrite
    :: ( Monad m, Buildable (f a) , Eq (f a), Applicative f )
    => (forall b. m b -> IO b)
    -- ^ Function to embed the monad in 'IO'
    -> (a -> m ())
    -- ^ Write operation
    -> (m (f a))
    -- ^ Load operation
    -> a
    -- ^ Property arguments
    -> PropertyM IO ()
prop_loadAfterWrite toIO writeOp loadOp a = do
    res <- run . toIO $ writeOp a >> loadOp
    let fa = pure a
    monitor $ counterexample $ "\nInserted\n" <> pretty fa
    monitor $ counterexample $ "\nRead\n" <> pretty res
    assertWith "Inserted == Read" (res == fa)

{-------------------------------------------------------------------------------
    QuickCheck utilities
-------------------------------------------------------------------------------}
-- | Like 'assert', but allow giving a label / title before running a assertion
assertWith :: String -> Bool -> PropertyM IO ()
assertWith lbl condition = do
    let flag = if condition then "✓" else "✗"
    monitor (counterexample $ lbl <> " " <> flag)
    assert condition
