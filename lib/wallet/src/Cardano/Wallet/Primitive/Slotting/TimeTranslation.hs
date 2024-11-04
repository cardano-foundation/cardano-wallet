module Cardano.Wallet.Primitive.Slotting.TimeTranslation
    ( toTimeTranslation
    , toTimeTranslationPure
    ) where

import Prelude

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
import Internal.Cardano.Write.Tx.TimeTranslation
    ( TimeTranslation
    , timeTranslationFromEpochInfo
    )
import UnliftIO.Exception
    ( throwIO
    )

toTimeTranslation
    :: TimeInterpreter (ExceptT PastHorizonException IO)
    -> IO TimeTranslation
toTimeTranslation timeInterpreter = do
    info <-
        runExceptT (toEpochInfo timeInterpreter)
            >>= either throwIO (pure . hoistEpochInfo runExcept)
    pure $ timeTranslationFromEpochInfo (getSystemStart timeInterpreter) info

toTimeTranslationPure
    :: TimeInterpreter Identity
    -> TimeTranslation
toTimeTranslationPure ti =
    timeTranslationFromEpochInfo
        (getSystemStart ti)
        (hoistEpochInfo runExcept $ runIdentity $ toEpochInfo ti)
