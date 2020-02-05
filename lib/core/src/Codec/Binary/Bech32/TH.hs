{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- This module contains Template-Haskell-specific extensions to the Bech32
-- library.

module Codec.Binary.Bech32.TH
    ( humanReadablePart
    ) where

import Prelude

import Codec.Binary.Bech32
    ( HumanReadablePart, humanReadablePartFromText, humanReadablePartToText )
import Language.Haskell.TH.Quote
    ( QuasiQuoter (..) )
import Language.Haskell.TH.Syntax
    ( Lift, lift )

import qualified Data.Text as T

-- | A quasiquoter for Bech32 human-readable prefixes.
--
-- This allows the construction of prefixes to be checked at compile time.
--
humanReadablePart :: QuasiQuoter
humanReadablePart = QuasiQuoter
    { quoteExp  = lift . unsafeHumanReadablePartFromText . T.pack
    , quotePat  = notHandled "patterns"
    , quoteType = notHandled "types"
    , quoteDec  = notHandled "declarations"
    }
  where
    notHandled things =
      error $ things <>
          " are not handled by the Bech32 humanReadablePart quasiquoter."

unsafeHumanReadablePartFromText :: T.Text -> HumanReadablePart
unsafeHumanReadablePartFromText t =
    either throwError id $ humanReadablePartFromText t
  where
    throwError e = error $ mconcat
        [ show e
        , " when processing text "
        , show t
        , "."
        ]

instance Lift HumanReadablePart where
    lift t =
        [|unsafeHumanReadablePartFromText $ T.pack
            $(lift $ T.unpack $ humanReadablePartToText t)|]

