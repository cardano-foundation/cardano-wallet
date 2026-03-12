module Cardano.Wallet.Primitive.Slotting.TimeTranslation
    ( toTimeTranslation
    , toTimeTranslationPure
    ) where

import Cardano.Balance.Tx.TimeTranslation
    ( TimeTranslation
    , timeTranslationFromEpochInfo
    )
import Cardano.Slotting.EpochInfo.API
    ( hoistEpochInfo
    )
import Cardano.Wallet.Primitive.Slotting
    ( PastHorizonException
    , TimeInterpreter
    , getSystemStart
    , toEpochInfo
    )
import Control.Monad.Trans.Except
    ( ExceptT (..)
    , runExcept
    , runExceptT
    )
import Data.Functor.Identity
    ( Identity (runIdentity)
    )
import UnliftIO.Exception
    ( throwIO
    )
import Prelude

toTimeTranslation
    :: TimeInterpreter (ExceptT PastHorizonException IO)
    -> IO TimeTranslation
toTimeTranslation timeInterpreter = do
    info <-
        runExceptT (toEpochInfo timeInterpreter)
            >>= either throwIO (pure . hoistEpochInfo runExcept)
    pure
        $ timeTranslationFromEpochInfo (getSystemStart timeInterpreter) info

toTimeTranslationPure
    :: TimeInterpreter Identity
    -> TimeTranslation
toTimeTranslationPure ti =
    timeTranslationFromEpochInfo
        (getSystemStart ti)
        (hoistEpochInfo runExcept $ runIdentity $ toEpochInfo ti)
