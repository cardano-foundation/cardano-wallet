{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.Submissions.Properties.Common
    ( Step (..)
    , deltaState, oldState, newState
    , that, verify, forAllIn
    ) where

import Prelude

import Cardano.Wallet.Submissions.Submissions
    ( Submissions )
import Cardano.Wallet.Submissions.TxStatus
    ( HasTxId )
import Control.Lens
    ( makeLenses )
import Control.Monad.Trans.Writer
    ( Writer, execWriter, tell )
import Data.Foldable
    ( toList )
import Data.Set
    ( Set )
import Test.QuickCheck
    ( Property, Testable (..), conjoin, counterexample )

forAllIn
    :: Show a
    => Set a
    -> (a -> Property)
    -> Property
forAllIn db f = conjoin $ fmap g (toList db)
    where g i = counterexample (show i) $ f i

type Prop t a = Writer [t] a

that :: Testable t => String -> t -> Prop Property ()
that s = tell . pure . counterexample s

verify :: Testable t => Prop t a -> Property
verify = conjoin . execWriter

-- | Encode a change of the store, for inspection
data Step delta slot tx = Step
    { _oldState :: Submissions slot tx
    , _newState :: Submissions slot tx
    , _deltaState :: delta slot tx
    }

deriving instance (Show slot, HasTxId tx, Show tx,  Show (delta slot tx))
    => Show (Step delta slot tx)

makeLenses ''Step
