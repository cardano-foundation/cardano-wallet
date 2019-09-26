{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Faucet
    ( initFaucet
    , getBlock0H
    , getBlock0HText
    ) where

import Prelude

import Cardano.Wallet.Jormungandr.Binary
    ( getBlockId, runGet )
import Cardano.Wallet.Primitive.Mnemonic
    ( Mnemonic )
import Cardano.Wallet.Primitive.Types
    ( Hash (..) )
import Cardano.Wallet.Unsafe
    ( unsafeFromHex, unsafeMkMnemonic )
import Control.Concurrent.MVar
    ( newMVar )
import Data.ByteArray.Encoding
    ( Base (..), convertToBase )
import Data.Text
    ( Text )
import Test.Integration.Faucet
    ( Faucet (..) )

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Encoding as T

-- | Initialize a bunch of faucet wallets and make them available for the
-- integration tests scenarios.
initFaucet :: IO Faucet
initFaucet =
    Faucet <$> newMVar mnemonics

getBlock0H :: IO (Hash "Genesis")
getBlock0H =
    Hash . unsafeFromHex . T.encodeUtf8 <$> getBlock0HText

getBlock0HText :: IO Text
getBlock0HText =
    toHex . extractId <$> BL.readFile block0
  where
    block0 = "test/data/jormungandr/block0.bin"
    extractId = getHash . runGet getBlockId
    toHex :: BS.ByteString -> Text
    toHex = T.decodeUtf8
        . convertToBase @BS.ByteString @BS.ByteString Base16

mnemonics :: [Mnemonic 15]
mnemonics = unsafeMkMnemonic <$>
    [ [ "pass", "proud", "clarify", "cargo", "control"
      , "fancy", "question", "option", "bring", "recall"
      , "dolphin", "meat", "comic", "version", "pitch"
      ]
    , [ "behave", "worth", "later", "banana", "buzz"
      , "advance", "local", "owner", "bulb", "swear"
      , "theory", "border", "elephant", "armor", "chest"
      ]
    , [ "index", "cover", "candy", "upper", "orient"
      , "unit", "shaft", "peace", "size", "lava"
      , "wood", "blush", "huge", "sunset", "grid"
      ]
    , [ "liberty", "powder", "spoil", "health", "cable"
      , "problem", "dice", "alpha", "solution", "wagon"
      , "ask", "siege", "uncle", "brick", "life"
      ]
    , [ "shed", "comic", "subject", "stage", "dirt"
      , "yard", "access", "good", "weasel", "surround"
      , "since", "climb", "menu", "blossom", "joy"
      ]
    , [ "ridge", "garlic", "slush", "attack", "immune"
      , "town", "enforce", "drastic", "burden", "hybrid"
      , "surround", "stereo", "symptom", "remember", "will"
      ]
    , [ "health", "guard", "peace", "rally", "wear"
      , "topple", "ecology", "flush", "flower", "era"
      , "slender", "alpha", "awkward", "relax", "sustain"
      ]
    , [ "raven", "hurt", "hover", "ordinary", "three"
      , "half", "surprise", "sting", "only", "mouse"
      , "dismiss", "try", "exotic", "gossip", "view"
      ]
    , [ "click", "very", "carbon", "patch", "fly"
      , "acoustic", "bus", "drum", "furnace", "flush"
      , "doctor", "thank", "obey", "afraid", "match"
      ]
    , [ "doctor", "dove", "comfort", "doll", "knife"
      , "weather", "water", "flock", "place", "detect"
      , "idea", "wheel", "conduct", "laundry", "float"
      ]
    , [ "any", "group", "paddle", "fever", "hockey"
      , "obscure", "cricket", "nasty", "dose", "chronic"
      , "universe", "avocado", "super", "isolate", "sand"
      ]
    , [ "turn", "require", "basic", "horse", "glory"
      , "ship", "equip", "swear", "pigeon", "hover"
      , "donkey", "sign", "expand", "picture", "enforce"
      ]
    , [ "connect", "similar", "angry", "kid", "soon"
      , "news", "wife", "bright", "claim", "today"
      , "useless", "vacant", "desk", "resource", "narrow"
      ]
    , [ "first", "pair", "ginger", "garden", "siren"
      , "actor", "cotton", "beauty", "habit", "magnet"
      , "luxury", "discover", "success", "supply", "during"
      ]
    , [ "submit", "alone", "course", "safe", "laundry"
      , "what", "attack", "rude", "gaze", "imitate"
      , "merry", "usual", "sheriff", "host", "welcome"
      ]
    , [ "foot", "harsh", "suggest", "weapon", "margin"
      , "fog", "slight", "neutral", "spoon", "clip"
      , "budget", "drop", "alpha", "barrel", "fat"
      ]
    , [ "hedgehog", "hungry", "company", "knife", "student"
      , "object", "meadow", "yellow", "endless", "tool"
      , "rice", "sphere", "sail", "permit", "marriage"
      ]
    , [ "crush", "business", "energy", "era", "room"
      , "parrot", "paper", "joke", "still", "unhappy"
      , "answer", "grab", "shaft", "manage", "pass"
      ]
    , [ "crucial", "unfold", "swarm", "hundred", "outer"
      , "evolve", "unhappy", "crew", "foil", "cliff"
      , "above", "caution", "mansion", "beauty", "repair"
      ]
    , [ "thought", "laundry", "jacket", "bamboo", "birth"
      , "copper", "slide", "sell", "honey", "fragile"
      , "inquiry", "carry", "marriage", "venue", "virus"
      ]
    , [ "route", "urban", "hotel", "avoid", "film"
      , "void", "decade", "small", "steak", "lake"
      , "saddle", "base", "chimney", "hazard", "guard"
      ]
    , [ "whale", "face", "demise", "puppy", "genius"
      , "music", "silent", "build", "token", "art"
      , "current", "medal", "very", "like", "peanut"
      ]
    , [ "print", "era", "refuse", "mimic", "argue"
      , "year", "erosion", "dry", "portion", "symptom"
      , "crumble", "three", "feed", "waste", "save"
      ]
    , [ "rain", "sick", "matter", "lottery", "crime"
      , "universe", "drastic", "fragile", "service", "bean"
      , "wild", "call", "beyond", "cute", "property"
      ]
    , [ "off", "announce", "famous", "output", "awesome"
      , "negative", "obvious", "column", "impact", "make"
      , "history", "spare", "hold", "afraid", "motion"
      ]
    , [ "injury", "badge", "milk", "yellow", "silver"
      , "debate", "pole", "mother", "rate", "piece"
      , "black", "expose", "hurry", "toss", "consider"
      ]
    , [ "pipe", "high", "crawl", "breeze", "swift"
      , "prepare", "visa", "drop", "canoe", "segment"
      , "royal", "engage", "social", "scan", "pistol"
      ]
    , [ "pave", "giant", "program", "guitar", "enact"
      , "garage", "husband", "wreck", "win", "used"
      , "acid", "seed", "casual", "bridge", "gallery"
      ]
    , [ "mistake", "razor", "clean", "hover", "kingdom"
      , "flag", "hard", "depart", "shadow", "flat"
      , "riot", "trip", "bean", "cave", "paddle"
      ]
    , [ "breeze", "call", "surprise", "skirt", "hour"
      , "ice", "palace", "seed", "sister", "start"
      , "blade", "ocean", "size", "elbow", "cake"
      ]
    , [ "elegant", "embark", "puzzle", "canal", "breeze"
      , "bind", "bid", "angle", "plug", "dumb"
      , "hub", "dirt", "current", "warrior", "session"
      ]
    , [ "lawn", "together", "rival", "flame", "neither"
      , "fancy", "dentist", "pelican", "hungry", "lake"
      , "melt", "motor", "eternal", "summer", "region"
      ]
    , [ "pelican", "loop", "crush", "crane", "boost"
      , "joy", "they", "trash", "toast", "machine"
      , "jar", "sport", "mansion", "mystery", "segment"
      ]
    , [ "remember", "kiwi", "photo", "sauce", "supreme"
      , "pony", "page", "stove", "seed", "hold"
      , "oval", "inch", "quick", "decrease", "stadium"
      ]
    , [ "mango", "now", "empower", "post", "little"
      , "insane", "chapter", "real", "shell", "owner"
      , "yellow", "debris", "hub", "weasel", "execute"
      ]
    , [ "tackle", "vendor", "artwork", "achieve", "silk"
      , "where", "vendor", "actual", "pair", "main"
      , "blue", "denial", "evil", "hard", "crumble"
      ]
    , [ "pumpkin", "genius", "injury", "tip", "toast"
      , "three", "orient", "broom", "false", "mandate"
      , "enter", "process", "raise", "nest", "amount"
      ]
    , [ "ring", "member", "another", "narrow", "auction"
      , "come", "perfect", "link", "cook", "egg"
      , "universe", "miss", "input", "trap", "airport"
      ]
    , [ "catch", "history", "lyrics", "now", "easy"
      , "pride", "sting", "wild", "sunny", "tuition"
      , "shove", "chapter", "country", "mango", "pitch"
      ]
    , [ "note", "slim", "phone", "key", "echo"
      , "pass", "rail", "bus", "aim", "maple"
      , "kidney", "dove", "purchase", "mandate", "net"
      ]
    , [ "veteran", "lunch", "ill", "symbol", "error"
      , "spoon", "burger", "boring", "advice", "maple"
      , "left", "humor", "elephant", "taste", "insect"
      ]
    , [ "same", "video", "ski", "wrong", "foil"
      , "feature", "office", "clean", "vacuum", "sunset"
      , "peanut", "robust", "crisp", "champion", "elephant"
      ]
    , [ "unfold", "pink", "risk", "fabric", "lemon"
      , "offer", "width", "sun", "zoo", "coil"
      , "sight", "photo", "metal", "offer", "clock"
      ]
    , [ "census", "outer", "other", "absurd", "immune"
      , "alpha", "rather", "april", "total", "kidney"
      , "barrel", "replace", "today", "tank", "exit"
      ]
    , [ "gift", "van", "cost", "illness", "exchange"
      , "damage", "nut", "still", "very", "glass"
      , "giant", "frown", "odor", "various", "wrong"
      ]
    , [ "hurt", "enforce", "urban", "earth", "bulb"
      , "chief", "noble", "trumpet", "million", "leopard"
      , "label", "fancy", "twin", "bone", "window"
      ]
    , [ "fold", "rug", "vintage", "motion", "soap"
      , "spice", "sunny", "bulk", "rabbit", "acquire"
      , "smile", "bottom", "search", "curve", "choose"
      ]
    , [ "just", "guitar", "marine", "peanut", "kit"
      , "group", "sibling", "depend", "wool", "milk"
      , "away", "mistake", "soap", "view", "shoot"
      ]
    , [ "song", "olympic", "angle", "food", "inject"
      , "silver", "lawn", "symbol", "olympic", "segment"
      , "symbol", "when", "veteran", "blade", "estate"
      ]
    , [ "ride", "walk", "wrist", "quick", "oyster"
      , "correct", "swift", "worth", "approve", "orange"
      , "cereal", "sword", "rent", "smooth", "sauce"
      ]
    , [ "opera", "escape", "argue", "stay", "alley"
      , "rapid", "member", "sadness", "famous", "equip"
      , "found", "deny", "denial", "enjoy", "length"
      ]
    , [ "expect", "term", "orbit", "decline", "gospel"
      , "soda", "hamster", "before", "memory", "regret"
      , "man", "carpet", "old", "ball", "kiwi"
      ]
    , [ "recycle", "sell", "waste", "three", "climb"
      , "invest", "topple", "gossip", "humor", "sense"
      , "outside", "youth", "assault", "crazy", "accident"
      ]
    , [ "seed", "allow", "become", "hire", "account"
      , "torch", "fresh", "street", "october", "obey"
      , "mom", "frown", "addict", "huge", "life"
      ]
    , [ "slush", "bonus", "squeeze", "sibling", "toss"
      , "clay", "bid", "carpet", "tragic", "valve"
      , "invite", "culture", "loan", "reason", "final"
      ]
    , [ "gather", "law", "appear", "carry", "meat"
      , "segment", "equip", "stamp", "chest", "wage"
      , "royal", "bottom", "fine", "sketch", "cable"
      ]
    , [ "stage", "ordinary", "rack", "monkey", "orphan"
      , "come", "sense", "eyebrow", "erupt", "brother"
      , "wagon", "dawn", "scene", "party", "glue"
      ]
    , [ "noodle", "royal", "often", "wide", "cloth"
      , "explain", "glory", "isolate", "hammer", "hurt"
      , "fat", "host", "ensure", "elegant", "label"
      ]
    , [ "rent", "between", "nuclear", "annual", "vessel"
      , "episode", "seat", "sound", "casino", "medal"
      , "mansion", "sign", "loyal", "outer", "drum"
      ]
    , [ "around", "dune", "educate", "leg", "predict"
      , "convince", "trophy", "lab", "obey", "they"
      , "bird", "urge", "clay", "curtain", "patrol"
      ]
    , [ "exit", "outside", "cream", "rural", "snap"
      , "carpet", "truck", "action", "all", "lens"
      , "render", "belt", "develop", "animal", "gauge"
      ]
    , [ "wagon", "host", "skin", "huge", "few"
      , "oxygen", "soccer", "pave", "fork", "thing"
      , "tuna", "chat", "occur", "rain", "ill"
      ]
    , [ "hockey", "flight", "lesson", "address", "travel"
      , "debris", "tape", "brush", "negative", "ripple"
      , "filter", "dose", "room", "coin", "such"
      ]
    , [ "basic", "uniform", "expand", "program", "actor"
      , "spot", "six", "ocean", "extra", "inside"
      , "learn", "gas", "like", "laundry", "sad"
      ]
    , [ "upper", "puppy", "word", "fence", "scorpion"
      , "angle", "lobster", "market", "chicken", "arrow"
      , "vocal", "near", "robust", "random", "venue"
      ]
    , [ "flat", "soccer", "exclude", "juice", "peace"
      , "evidence", "eye", "core", "fragile", "air"
      , "spice", "crane", "term", "thrive", "gauge"
      ]
    , [ "hockey", "lunch", "portion", "fiscal", "toe"
      , "bone", "dizzy", "onion", "snake", "sample"
      , "size", "stairs", "settle", "eight", "limb"
      ]
    , [ "loan", "lift", "spend", "ensure", "clock"
      , "before", "exclude", "slab", "glide", "usual"
      , "false", "reform", "aunt", "love", "boat"
      ]
    , [ "grain", "puppy", "friend", "cup", "oppose"
      , "soup", "upgrade", "cliff", "visit", "convince"
      , "agent", "timber", "educate", "crystal", "heavy"
      ]
    , [ "income", "rate", "hover", "seek", "voyage"
      , "about", "sorry", "charge", "denial", "invest"
      , "wrap", "disease", "flavor", "error", "despair"
      ]
    , [ "shy", "giraffe", "slight", "vital", "story"
      , "ring", "mystery", "twin", "furnace", "setup"
      , "weapon", "grass", "defense", "ship", "lonely"
      ]
    , [ "mom", "medal", "glare", "tennis", "gauge"
      , "table", "put", "success", "chest", "mercy"
      , "angry", "finger", "frame", "club", "blade"
      ]
    , [ "quiz", "state", "hawk", "traffic", "hurt"
      , "sick", "write", "toe", "holiday", "coyote"
      , "demand", "accident", "unusual", "river", "year"
      ]
    , [ "soup", "exact", "odor", "reopen", "twelve"
      , "task", "divorce", "slight", "code", "elevator"
      , "concert", "illness", "virus", "prize", "analyst"
      ]
    , [ "science", "about", "budget", "fortune", "hollow"
      , "aunt", "legend", "dust", "search", "upgrade"
      , "angry", "fruit", "middle", "office", "there"
      ]
    , [ "include", "foot", "double", "polar", "equal"
      , "mom", "eternal", "weekend", "boy", "degree"
      , "method", "sheriff", "use", "survey", "culture"
      ]
    , [ "poverty", "coin", "earth", "mention", "salon"
      , "card", "century", "brief", "provide", "buffalo"
      , "tattoo", "reject", "canoe", "local", "vapor"
      ]
    , [ "arrow", "ranch", "response", "ghost", "orient"
      , "shove", "gentle", "body", "kitchen", "draft"
      , "ocean", "stamp", "margin", "grant", "pipe"
      ]
    , [ "track", "marriage", "permit", "snap", "autumn"
      , "betray", "circle", "beyond", "wonder", "draw"
      , "culture", "evoke", "inject", "chimney", "task"
      ]
    , [ "obscure", "manual", "pistol", "scene", "wash"
      , "orbit", "surge", "iron", "crunch", "capital"
      , "cruel", "icon", "black", "expire", "toast"
      ]
    , [ "talent", "notice", "omit", "view", "diesel"
      , "galaxy", "oven", "egg", "index", "safe"
      , "final", "purchase", "spider", "style", "inherit"
      ]
    , [ "egg", "scene", "april", "scrap", "load"
      , "possible", "faith", "source", "during", "illness"
      , "smart", "city", "unable", "three", "warm"
      ]
    , [ "cradle", "hire", "abstract", "best", "column"
      , "offer", "lunar", "swallow", "tree", "now"
      , "glue", "boost", "august", "blossom", "pear"
      ]
    , [ "fever", "idle", "health", "position", "series"
      , "people", "chimney", "wet", "section", "census"
      , "brother", "sadness", "craft", "office", "angle"
      ]
    , [ "peanut", "stock", "rule", "ancient", "cliff"
      , "apology", "alcohol", "often", "normal", "enlist"
      , "february", "index", "now", "column", "arrange"
      ]
    , [ "tuna", "keen", "speed", "giant", "melody"
      , "obtain", "chicken", "fashion", "barely", "swamp"
      , "brain", "panel", "exhaust", "name", "jaguar"
      ]
    , [ "clay", "search", "guilt", "immense", "mutual"
      , "garment", "imitate", "attitude", "lucky", "initial"
      , "finish", "office", "cannon", "deputy", "about"
      ]
    , [ "spawn", "deputy", "heavy", "grit", "glimpse"
      , "hawk", "drill", "scene", "pyramid", "employ"
      , "gain", "toilet", "exile", "machine", "burst"
      ]
    , [ "sort", "trigger", "grace", "hint", "october"
      , "price", "oyster", "timber", "gossip", "human"
      , "scene", "stumble", "supply", "dawn", "truly"
      ]
    , [ "monster", "rabbit", "wrap", "lock", "spider"
      , "aim", "toward", "tube", "drip", "outside"
      , "observe", "deny", "faith", "couch", "mail"
      ]
    , [ "cake", "achieve", "term", "dad", "culture"
      , "begin", "switch", "dwarf", "excess", "vital"
      , "state", "arm", "suffer", "wait", "lend"
      ]
    , [ "strategy", "kind", "tuition", "vacuum", "across"
      , "meat", "purchase", "pause", "agree", "clinic"
      , "broken", "sponsor", "trophy", "gospel", "cover"
      ]
    , [ "unlock", "top", "upon", "traffic", "rival"
      , "cluster", "index", "second", "sphere", "shock"
      , "iron", "bomb", "lion", "exist", "combine"
      ]
    , [ "arrest", "ski", "capital", "book", "butter"
      , "spread", "repeat", "rug", "endorse", "silly"
      , "either", "field", "slim", "upper", "area"
      ]
    , [ "twin", "neglect", "faint", "zoo", "valid"
      , "desert", "hidden", "exile", "gorilla", "analyst"
      , "share", "skate", "bulb", "knee", "lake"
      ]
    , [ "range", "genius", "bike", "raw", "citizen"
      , "glue", "endorse", "inspire", "timber", "shove"
      , "jewel", "reopen", "fold", "fault", "good"
      ]
    , [ "kid", "foam", "lady", "science", "top"
      , "ritual", "left", "jewel", "soup", "nation"
      , "absorb", "machine", "pluck", "luggage", "lumber"
      ]
    , [ "object", "tell", "fortune", "list", "dragon"
      , "butter", "pave", "skate", "emerge", "hip"
      , "grit", "tail", "super", "sweet", "list"
      ]
    , [ "vibrant", "orphan", "put", "metal", "wreck"
      , "yellow", "final", "bacon", "matter", "spring"
      , "stage", "enhance", "unaware", "skill", "fiber"
      ]
    , [ "market", "volcano", "deputy", "bottom", "bag"
      , "evidence", "pulp", "unfold", "exercise", "float"
      , "peasant", "sister", "north", "chair", "salute"
      ]
    ]

{-
{-# LANGUAGE TypeApplications #-}

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
    ( entropyToBytes
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
    ms <- replicateM n (entropyToMnemonic <$> genEntropy)
    forM_ [ (m, take 10 (addresses m)) | m <- ms ] $ \(m, addrs) -> do
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
