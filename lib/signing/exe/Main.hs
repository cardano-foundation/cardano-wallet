{-# LANGUAGE FlexibleContexts #-}

module Main where

import Prelude

import Main.Utf8
    ( withUtf8
    )

import qualified Command as CLI

main :: IO ()
main = do
    withUtf8 (CLI.setup >> CLI.parse >>= CLI.run)
