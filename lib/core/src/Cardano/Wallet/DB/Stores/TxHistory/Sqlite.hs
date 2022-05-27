{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.Wallet.DB.Stores.TxHistory.Sqlite where

import Cardano.Wallet.DB.Stores.TxHistory.Model
    ( DeltaTxHistory (DeltaTxHistory), TxHistory (TxHistory) )
import Control.Exception
    ( SomeException, throw )
import Data.DBVar
    ( Store (Store, loadS, updateS, writeS) )
import Prelude
import Database.Persist.Sqlite (SqlPersistT)
import Cardano.Wallet.Primitive.Types (WalletId)
import Data.Functor.Identity (Identity(Identity))
import Cardano.Wallet.DB.Sqlite.Lib (selectWallet, mkTxHistory)
import Cardano.Wallet.DB (ErrNoSuchWallet(ErrNoSuchWallet))
import Cardano.Wallet.DB.Stores.TxHistory.Update (putTxs)

mkStoreTransactions :: WalletId -> Store (SqlPersistT IO) DeltaTxHistory
mkStoreTransactions wid = Store
    {loadS = load wid, writeS = write, updateS = update wid}

update :: WalletId -> TxHistory -> DeltaTxHistory -> SqlPersistT IO ()
update wid _ (DeltaTxHistory (TxHistory txs)) =
    selectWallet wid >>= \case
        Nothing -> throw $ ErrNoSuchWallet wid
        Just _ -> do
            let Identity
                    ( txMetas
                    , txIns
                    , txCollateralIns
                    , txOuts
                    , txOutTokens
                    , txCollateralOuts
                    , txCollateralOutTokens
                    , txWithdrawals
                    ) = Identity $ mkTxHistory wid txs
            putTxs
                txMetas
                txIns
                txCollateralIns
                txOuts
                txOutTokens
                txCollateralOuts
                txCollateralOutTokens
                txWithdrawals
            pure ()


write :: TxHistory -> SqlPersistT IO ()
write = error "not implemented"

load :: WalletId -> SqlPersistT IO (Either SomeException TxHistory)
load = error "not implemented"
-- data Store m da = Store
--     { loadS   :: m (Either SomeException (Base da))
--     , writeS  :: Base da -> m ()
--     , updateS
--         :: Base da -- old value
--         -> da -- delta to new value
--         -> m () -- write new value
--     }
-- | A delta can be optionally applied.
