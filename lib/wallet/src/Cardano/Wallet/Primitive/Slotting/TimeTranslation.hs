module Cardano.Wallet.Primitive.Slotting.TimeTranslation
    (  toTimeTranslation
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
