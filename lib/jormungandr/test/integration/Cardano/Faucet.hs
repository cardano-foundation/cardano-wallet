{-# LANGUAGE DataKinds #-}

module Cardano.Faucet
    ( initFaucet
    ) where

import Prelude

import Cardano.Wallet.Primitive.Mnemonic
    ( Mnemonic )
import Cardano.Wallet.Unsafe
    ( unsafeMkMnemonic )
import Control.Concurrent.MVar
    ( newMVar )
import Test.Integration.Faucet
    ( Faucet (..) )

-- | Initialize a bunch of faucet wallets and make them available for the
-- integration tests scenarios.
initFaucet :: IO Faucet
initFaucet = do
    Faucet <$> newMVar mnemonics

mnemonics :: [Mnemonic 15]
mnemonics = unsafeMkMnemonic <$>
    [ [ "dignity", "acoustic", "learn", "exist", "motion"
      , "welcome", "bullet", "bus", "emotion", "kitten"
      , "fringe", "debate", "term", "age", "mention"
      ]
    , [ "knife", "glance", "genre", "glare", "vacuum"
      , "tower", "depend", "bomb", "ghost", "pelican"
      , "nasty", "awesome", "turn", "destroy", "purse"
      ]
    , [ "glass", "discover", "load", "load", "puppy"
      , "derive", "lumber", "address", "head", "denial"
      , "dish", "purse", "team", "wolf", "focus"
      ]
    , [ "code", "frost", "tennis", "ugly", "spider"
      , "cluster", "task", "runway", "young", "grant"
      , "menu", "obey", "remove", "guide", "police"
      ]
    , [ "stumble", "destroy", "vibrant", "goat", "delay"
      , "bachelor", "bag", "shallow", "famous", "depend"
      , "soldier", "select", "insect", "damage", "exit"
      ]
    , [ "case", "vivid", "peace", "punch", "lounge", "library"
      , "sudden", "crowd", "pelican", "perfect", "expire"
      , "airport", "story", "dinosaur", "inflict"
      ]
    , [ "narrow", "sad", "stay", "name", "fetch"
      , "excess", "later", "flag", "wing", "solve"
      , "wife", "faculty", "stuff", "pass", "material"
      ]
    , [ "border", "dice", "silly", "slice", "upset", "foster"
      , "bean", "bid", "west", "visual", "sort"
      , "clerk", "section", "edge", "select"
      ]
    , [ "general", "actress", "captain", "helmet", "throw"
      , "spatial", "kit", "orange", "ability", "impact"
      , "hint", "horror", "project", "ice", "cup"
      ]
    , [ "mushroom", "load", "glad", "card", "love", "seed"
      , "royal", "humor", "margin", "elbow", "slab"
      , "square", "mail", "eager", "bless"
      ]
    ]
{-
import Prelude

import Cardano.Wallet.Jormungandr.Compatibility
    ( Jormungandr, Network (..) )
import Cardano.Wallet.Primitive.AddressDerivation
    ( ChangeChain (..)
    , KeyToAddress (..)
    , Passphrase (..)
    , deriveAccountPrivateKey
    , deriveAddressPrivateKey
    , generateKeyFromSeed
    , publicKey
    )
import Cardano.Wallet.Primitive.Mnemonic
    ( Mnemonic
    , entropyToBytes
    , entropyToMnemonic
    , genEntropy
    , mnemonicToEntropy
    , mnemonicToText
    )
import Cardano.Wallet.Primitive.Types
    ( Address (..), encodeAddress )
import Control.Monad
    ( forM_, replicateM )
import Data.Proxy
    ( Proxy (..) )
import Data.Text
    ( Text )

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

-- | Generate faucets addresses and mnemonics, printing everything to stdout
--
-- >>> genFaucets 1
-- "dignity", "acoustic", "learn", "exist", "motion", "welcome", "bullet", "bus", "emotion", "kitten", "fringe", "debate", "term", "age", "mention"
-- ta1sd65hlwrj0u2k5z83fdc8krea2mdp6d2nna2fjlttmdd2g63td77kqwzqc7
-- ta1s0076l7c9jjm05gum9y390u9ekt8xnkjkkqaca4vjrcsx0lfhycp2qyrygk
-- ta1s0pym76rycvl52rupuatyqdha57ysqefrlzne9jgr7eg5yrqww3z64ej0r9
-- ta1sws7ewvwgla2d2e8w5e8dhn8kepehqg84w9l8acrgk7yaedeazgxjhm4v8q
-- ta1s0ptddxce45lwu5cvk3h7ej2ls7g348xjww0mw6pqganv5l9wlanytfftuf
-- ta1s0qnse6kazerck2rdqxek9yw3rf4gj8ccfle90vfuq5f953q55r4gdayqng
-- ta1sva90xx87l962tdagudz4utm4k0x3r27httj08fpx3qyqydul3yrjw4wcx2
-- ta1svkn3ywl2620je3tltp4g2e0ygnhs5txt933vsvhksettvpchvlcwx3e7lq
-- ta1swgj8q58unau52gfwzwl9z5g2twg8nsj76q69589yhl3y2t4zs5qu0eslsc
-- ta1svuw4z8qj6tg0vly6x7zes58gsh2cf60dk7d4gtfjperextk7chscash9yc
genFaucets :: Int -> IO ()
genFaucets n = do
    mnemonics <- replicateM n (entropyToMnemonic <$> genEntropy)
    forM_ [ (m, take 10 (addresses m)) | m <- mnemonics ] $ \(m, addrs) -> do
        TIO.putStrLn $ T.intercalate ", " $ surroundedBy '"' <$> mnemonicToText @15 m
        forM_ addrs (TIO.putStrLn . encodeAddress (Proxy @(Jormungandr 'Testnet)))
  where
    surroundedBy :: Char -> Text -> Text
    surroundedBy c txt = T.singleton c <> txt <> T.singleton c

addresses :: Mnemonic n -> [Address]
addresses mw =
    let
        (seed, pwd) =
            (Passphrase $ entropyToBytes $ mnemonicToEntropy mw, mempty)
        rootXPrv =
            generateKeyFromSeed (seed, mempty) pwd
        accXPrv =
            deriveAccountPrivateKey pwd rootXPrv minBound
        addrXPrv =
            deriveAddressPrivateKey pwd accXPrv ExternalChain
    in
        [ keyToAddress @(Jormungandr 'Testnet) (publicKey $ addrXPrv ix)
        | ix <- [minBound..maxBound]
        ]
-}
