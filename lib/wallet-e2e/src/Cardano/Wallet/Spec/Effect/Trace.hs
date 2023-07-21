{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Cardano.Wallet.Spec.Effect.Trace where

import qualified Data.Sequence as Seq

import Data.Sequence
    ( (|>) )
import Effectful
    ( Eff, Effect )
import Effectful.Dispatch.Dynamic
    ( reinterpret )
import Effectful.State.Static.Local
    ( modify, runState )
import Effectful.TH
    ( makeEffect )
import Path
    ( parseRelFile, reldir, toFilePath, (</>) )
import Path.IO
    ( ensureDir, getCurrentDir )
import Prelude hiding
    ( modify, runState )

data TRACE :: Effect where
    Trace :: Text -> TRACE m ()

$(makeEffect ''TRACE)

runTracePure :: Eff (TRACE : es) a -> Eff es (a, Seq Text)
runTracePure = reinterpret (runState Seq.empty) \_ (Trace msg) ->
    modify (|> msg)

recordTraceLog :: String -> Seq Text -> IO ()
recordTraceLog storyLabel log = do
    cwd <- getCurrentDir
    let outDir = cwd </> [reldir|test-output|]
    ensureDir outDir
    fileName <-
        parseRelFile
            ([if c == ' ' then '_' else c | c <- storyLabel] <> ".log")
    let outFile = outDir </> fileName
    writeFile (toFilePath outFile) $ toString $ unlines $ toList log
