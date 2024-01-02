{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Cardano.Wallet.Spec.Effect.Trace where

import qualified Data.Sequence as Seq

import Cardano.Wallet.Spec.Interpreters.Config
    ( TraceConfiguration (..)
    )
import Data.Sequence
    ( (|>)
    )
import Data.Tagged
    ( Tagged (..)
    )
import Effectful
    ( Eff
    , Effect
    )
import Effectful.Dispatch.Dynamic
    ( reinterpret
    )
import Effectful.State.Static.Local
    ( modify
    , runState
    )
import Effectful.TH
    ( makeEffect
    )
import Path
    ( parseRelFile
    , toFilePath
    , (</>)
    )
import Path.IO
    ( ensureDir
    )
import Prelude hiding
    ( modify
    , runState
    , trace
    )

data FxTrace :: Effect where
    Trace :: Text -> FxTrace m ()

$(makeEffect ''FxTrace)

runTracePure :: Eff (FxTrace : es) a -> Eff es (a, Seq Text)
runTracePure = reinterpret (runState Seq.empty) \_ (Trace msg) ->
    modify (|> msg)

recordTraceLog :: TraceConfiguration -> String -> Seq Text -> IO ()
recordTraceLog (TraceConfiguration (Tagged outDir)) storyLabel log = do
    ensureDir outDir
    fileName <-
        parseRelFile
            $ [if c == ' ' then '_' else c | c <- storyLabel] <> ".log"
    let outFile = outDir </> fileName
    writeFile (toFilePath outFile) $ toString $ unlines $ toList log
