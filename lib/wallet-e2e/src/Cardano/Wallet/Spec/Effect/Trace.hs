{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Cardano.Wallet.Spec.Effect.Trace where

import qualified Data.Sequence as Seq

import Cardano.Wallet.Launch.Cluster.FileOf
    ( DirOf (..)
    , toFilePath
    )
import Cardano.Wallet.Spec.Interpreters.Config
    ( TraceConfiguration (..)
    )
import Data.Sequence
    ( (|>)
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
import Prelude hiding
    ( modify
    , runState
    , trace
    )
import System.Path
    ( relFile
    , (<.>)
    , (</>)
    )
import System.Path.Directory
    ( createDirectoryIfMissing
    )

data FxTrace :: Effect where
    Trace :: Text -> FxTrace m ()

$(makeEffect ''FxTrace)

runTracePure :: Eff (FxTrace : es) a -> Eff es (a, Seq Text)
runTracePure = reinterpret (runState Seq.empty) \_ (Trace msg) ->
    modify (|> msg)

recordTraceLog :: TraceConfiguration -> String -> Seq Text -> IO ()
recordTraceLog (TraceConfiguration (DirOf outDir)) storyLabel log = do
    createDirectoryIfMissing True outDir
    let fileName = relFile
            [if c == ' ' then '_' else c | c <- storyLabel] <.> "log"
    let outFile = outDir </> fileName
    writeFile (toFilePath outFile) $ toString $ unlines $ toList log
