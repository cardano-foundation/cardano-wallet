{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Faucet
    ( initFaucet
    , getBlock0H
    , getBlock0HText
    ) where

import Prelude

import Cardano.Wallet.Jormungandr.Binary
    ( constructTransactionFragment, getBlockId, runGet )
import Cardano.Wallet.Primitive.Mnemonic
    ( Mnemonic )
import Cardano.Wallet.Primitive.Types
    ( Address (..), Coin (..), Hash (..), TxIn (..), TxOut (..) )
import Cardano.Wallet.Unsafe
    ( unsafeFromHex, unsafeMkMnemonic )
import Control.Concurrent.MVar
    ( newMVar )
import Data.ByteArray.Encoding
    ( Base (..), convertToBase )
import Data.ByteString
    ( ByteString )
import Data.Functor.Identity
    ( Identity (..) )
import Data.Text
    ( Text )
import Paths_cardano_wallet_jormungandr
    ( getDataFileName )
import System.Command
    ( CmdResult, Stdout (..), command )
import System.FilePath
    ( FilePath, (</>) )
import System.IO.Temp
    ( withSystemTempDirectory )
import Test.Integration.Faucet
    ( Faucet (..) )

import qualified Codec.Binary.Bech32 as Bech32
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as TIO


-- | Initialize a bunch of faucet wallets and make them available for the
-- integration tests scenarios.
initFaucet :: IO Faucet
initFaucet = Faucet
    <$> newMVar seqMnemonics
    <*> newMVar rndMnemonics
    <*> newMVar (mkTxBuilder <$> externalAddresses)

getBlock0H :: IO (Hash "Genesis")
getBlock0H = do
    block0 <- getDataFileName "jormungandr/block0.bin"
    extractId <$> BL.readFile block0
  where
    extractId = Hash . getHash . runGet getBlockId

getBlock0HText :: IO Text
getBlock0HText = toHex <$> getBlock0H

toHex :: Hash any -> Text
toHex = T.decodeUtf8 . convertToBase Base16 . getHash

-- | Prepare externally signed Tx for Jormungandr
mkTxBuilder
    :: (TxIn, String)
    -> (Address, Coin)
    -> IO ByteString
mkTxBuilder (TxIn inpTx inpIx, key) (addr, Coin amt) =
    withSystemTempDirectory "cardano-wallet-jormungandr" $ \d -> do
        let txFile = d </> "trans.tx"
        let witnessFile = d </> "witness"
        let keyFile = d </> "key.prv"

        TIO.writeFile keyFile (T.pack key)
        prepareTx txFile
        signTx txFile keyFile witnessFile
        toMessage txFile
  where
    runJcli :: CmdResult r => [String] -> IO r
    runJcli = command [] "jcli"

    runJcli_ :: [String] -> IO ()
    runJcli_ = runJcli

    -- A sink address where all changes from external transaction are sent to.
    sinkAddress :: String
    sinkAddress =
        "sink1sw76ufc5c58mg5cn2dmze70rrpcpy4vxch60lheuzaq5up83ccatse6wns7"

    -- Amount associated with each input in the genesis file, in Lovelace
    inputAmt :: Int
    inputAmt =
        100000000000

    prepareTx :: FilePath -> IO ()
    prepareTx txFile = do
        let hrp = Bech32.unsafeHumanReadablePartFromText "addr"
        let dp  = Bech32.dataPartFromBytes (unAddress addr)
        runJcli_
            [ "transaction"
            , "new"
            , "--staging"
            , txFile
            ]
        runJcli_
            [ "transaction"
            , "add-input"
            , T.unpack . toHex $ inpTx
            , show inpIx
            , show inputAmt
            , "--staging"
            , txFile
            ]
        runJcli_
            [ "transaction"
            , "add-output"
            , T.unpack (Bech32.encodeLenient hrp dp)
            , show amt
            , "--staging"
            , txFile
            ]
        runJcli_
            [ "transaction"
            , "finalize"
            , sinkAddress
            , "--fee-constant", "42"
            , "--fee-coefficient", "0"
            , "--staging"
            , txFile
            ]

    signTx :: FilePath -> FilePath -> FilePath -> IO ()
    signTx txFile keyFile witnessFile = do
        Stdout txId <- runJcli
            [ "transaction"
            , "data-for-witness"
            , "--staging"
            , txFile
            ]
        block0H <- getBlock0HText
        runJcli_
            [ "transaction"
            , "make-witness"
            , T.unpack . T.strip . T.pack $ txId
            , "--genesis-block-hash"
            , T.unpack block0H
            , "--type", "utxo"
            , witnessFile
            , keyFile
            ]
        runJcli_
            [ "transaction"
            , "add-witness"
            , witnessFile
            , "--staging"
            , txFile
            ]

    toMessage :: FilePath -> IO ByteString
    toMessage txFile = do
        runJcli_
            [ "transaction"
            , "seal"
            , "--staging"
            , txFile
            ]
        Stdout bytes <- runJcli
            [ "transaction"
            , "to-message"
            , "--staging"
            , txFile
            ]
        return (unsafeFromHex $ T.encodeUtf8 $ T.strip $ T.pack bytes)

seqMnemonics :: [Mnemonic 15]
seqMnemonics = unsafeMkMnemonic <$>
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

rndMnemonics :: [Mnemonic 12]
rndMnemonics = unsafeMkMnemonic <$>
    [ [ "arctic", "decade", "pink", "easy"
      , "jar", "index", "base", "bright"
      , "vast", "ocean", "hard", "pizza"
      ]
    , [ "finish", "evoke", "alone", "town"
      , "express", "wide", "pair", "story"
      , "west", "safe", "news", "wrap"
      ]
    , [ "fox", "now", "hello", "inmate"
      , "era", "jealous", "cruel", "wreck"
      , "dash", "supply", "book", "attend"
      ]
    , [ "must", "lock", "cereal", "water"
      , "silver", "cake", "circle", "express"
      , "sock", "arm", "chapter", "avoid"
      ]
    , [ "give", "verb", "balcony", "hurdle"
      , "pistol", "flee", "manage", "barely"
      , "pulse", "episode", "speak", "school"
      ]
    , [ "divert", "entire", "urge", "banner"
      , "repair", "mechanic", "muffin", "illness"
      , "genre", "intact", "coin", "boss"
      ]
    , [ "pink", "radio", "various", "frame"
      , "argue", "draft", "sun", "speak"
      , "club", "salute", "thank", "price"
      ]
    , [ "all", "beef", "link", "funny"
      , "swing", "duck", "sweet", "swallow"
      , "slow", "shield", "weekend", "open"
      ]
    , [ "green", "friend", "captain", "entry"
      , "utility", "lake", "blur", "matrix"
      , "will", "prefer", "breeze", "shed"
      ]
    , [ "reveal", "jazz", "equal", "salmon"
      , "first", "decline", "liquid", "wolf"
      , "powder", "account", "elbow", "figure"
      ]
    , [ "olympic", "uncover", "stone", "tiger"
      , "oppose", "icon", "property", "heart"
      , "mean", "interest", "account", "head"
      ]
    , [ "poverty", "hungry", "depart", "shift"
      , "proud", "wrap", "voice", "throw"
      , "spoon", "this", "system", "flee"
      ]
    , [ "tattoo", "crop", "genuine", "impact"
      , "govern", "banana", "hope", "bamboo"
      , "junior", "pride", "best", "skirt"
      ]
    , [ "model", "hundred", "exact", "control"
      , "random", "cross", "burst", "fame"
      , "ladder", "bleak", "car", "virus"
      ]
    , [ "ripple", "lazy", "void", "zoo"
      , "social", "plunge", "badge", "jungle"
      , "similar", "draft", "lawn", "execute"
      ]
    , [ "guide", "penalty", "erupt", "plate"
      , "benefit", "moon", "motion", "sing"
      , "envelope", "range", "midnight", "spell"
      ]
    , [ "bulb", "normal", "curious", "leg"
      , "essence", "chronic", "envelope", "cannon"
      , "comfort", "spare", "private", "uniform"
      ]
    , [ "tongue", "cabin", "enact", "square"
      , "feature", "prevent", "journey", "pigeon"
      , "valid", "unable", "drum", "opera"
      ]
    , [ "assist", "pact", "vessel", "spot"
      , "fine", "fine", "crouch", "body"
      , "gown", "allow", "hair", "universe"
      ]
    , [ "tape", "glue", "rate", "squirrel"
      , "jeans", "canoe", "bicycle", "sausage"
      , "lunar", "pair", "fit", "ice"
      ]
    , [ "chronic", "soda", "history", "famous"
      , "owner", "print", "student", "wool"
      , "pulse", "sound", "melt", "gate"
      ]
    , [ "exist", "arrest", "north", "tunnel"
      , "height", "style", "announce", "real"
      , "uncover", "sphere", "sorry", "sudden"
      ]
    , [ "celery", "slim", "stone", "hand"
      , "inmate", "enrich", "stem", "ice"
      , "glass", "fault", "pig", "island"
      ]
    , [ "ancient", "update", "number", "oil"
      , "degree", "virtual", "stairs", "reunion"
      , "question", "toilet", "disagree", "deliver"
      ]
    , [ "surge", "inherit", "gown", "witness"
      , "true", "fame", "couch", "artwork"
      , "orchard", "tunnel", "toss", "mom"
      ]
    , [ "oblige", "room", "table", "auto"
      , "build", "very", "street", "margin"
      , "faculty", "purpose", "shoe", "prison"
      ]
    , [ "theory", "afraid", "tell", "depth"
      , "issue", "cover", "pass", "vacant"
      , "poet", "fury", "fortune", "cruise"
      ]
    , [ "clay", "mix", "capable", "student"
      , "scissors", "ugly", "prefer", "change"
      , "adjust", "push", "cake", "harsh"
      ]
    , [ "shift", "sunny", "brick", "supreme"
      , "tank", "duck", "garment", "feature"
      , "cloud", "canyon", "harbor", "nut"
      ]
    , [ "delay", "exhibit", "social", "wood"
      , "plate", "donate", "differ", "knock"
      , "dignity", "sport", "cost", "visual"
      ]
    , [ "banner", "expand", "fringe", "kiss"
      , "laugh", "muffin", "maximum", "program"
      , "hurdle", "gorilla", "spray", "prepare"
      ]
    , [ "together", "sorry", "amazing", "loyal"
      , "civil", "rely", "success", "range"
      , "adult", "truly", "trade", "tip"
      ]
    , [ "secret", "like", "type", "honey"
      , "average", "sword", "rookie", "mass"
      , "blade", "myth", "double", "salmon"
      ]
    , [ "buddy", "assault", "armed", "whale"
      , "bid", "unfair", "zone", "minimum"
      , "fat", "employ", "front", "lizard"
      ]
    , [ "verb", "blossom", "kiwi", "butter"
      , "express", "other", "shoulder", "hold"
      , "enter", "beyond", "special", "devote"
      ]
    , [ "exhibit", "install", "act", "craft"
      , "grain", "soap", "coral", "jaguar"
      , "echo", "midnight", "ride", "raise"
      ]
    , [ "credit", "raw", "dinosaur", "target"
      , "sustain", "permit", "regret", "strong"
      , "abandon", "guard", "expand", "science"
      ]
    , [ "timber", "grid", "cement", "resemble"
      , "engage", "sugar", "february", "regular"
      , "print", "timber", "produce", "pizza"
      ]
    , [ "solution", "dice", "symbol", "ignore"
      , "gauge", "exist", "also", "mention"
      , "west", "pet", "rule", "first"
      ]
    , [ "tuition", "cost", "tattoo", "vicious"
      , "vast", "doctor", "prevent", "asthma"
      , "barely", "orphan", "close", "bus"
      ]
    , [ "puppy", "crew", "glide", "feature"
      , "bottom", "stumble", "prefer", "hidden"
      , "extra", "north", "bleak", "shoulder"
      ]
    , [ "innocent", "unfold", "combine", "gas"
      , "custom", "luggage", "cricket", "thing"
      , "speak", "bubble", "pitch", "festival"
      ]
    , [ "gospel", "garlic", "midnight", "enemy"
      , "legal", "speed", "sleep", "discover"
      , "enlist", "camp", "metal", "chunk"
      ]
    , [ "lyrics", "lend", "volume", "cruise"
      , "engage", "relief", "memory", "wine"
      , "board", "scorpion", "educate", "differ"
      ]
    , [ "law", "same", "wrist", "cotton"
      , "outer", "debris", "put", "other"
      , "wife", "father", "collect", "chef"
      ]
    , [ "february", "expand", "decline", "sort"
      , "pull", "silk", "average", "update"
      , "spatial", "betray", "remind", "hero"
      ]
    , [ "security", "hill", "flight", "improve"
      , "rotate", "language", "home", "carbon"
      , "boil", "enhance", "pulse", "pill"
      ]
    , [ "inside", "fancy", "sea", "blouse"
      , "estate", "chest", "early", "office"
      , "woman", "license", "obey", "helmet"
      ]
    , [ "course", "toe", "sentence", "defense"
      , "because", "trip", "hockey", "abandon"
      , "essay", "give", "deputy", "insect"
      ]
    , [ "sister", "slogan", "hour", "build"
      , "squeeze", "favorite", "inject", "smart"
      , "slim", "near", "tired", "blind"
      ]
    , [ "upper", "mouse", "spray", "wrong"
      , "food", "affair", "before", "object"
      , "mention", "then", "ask", "solution"
      ]
    , [ "video", "fall", "run", "engine"
      , "wheat", "baby", "december", "issue"
      , "vehicle", "between", "reopen", "wink"
      ]
    , [ "nuclear", "glide", "invest", "speed"
      , "essence", "friend", "clog", "hamster"
      , "service", "crisp", "weasel", "pigeon"
      ]
    , [ "stumble", "either", "orbit", "bundle"
      , "pepper", "total", "radio", "spatial"
      , "umbrella", "explain", "exercise", "science"
      ]
    , [ "slam", "entry", "nation", "frog"
      , "advice", "process", "cycle", "lawsuit"
      , "scrub", "strategy", "shrimp", "push"
      ]
    , [ "ecology", "female", "item", "crime"
      , "remember", "denial", "swallow", "forward"
      , "call", "vehicle", "glue", "hello"
      ]
    , [ "spin", "dinosaur", "honey", "abuse"
      , "exit", "coffee", "ethics", "denial"
      , "proof", "hour", "number", "annual"
      ]
    , [ "power", "age", "slush", "tube"
      , "island", "void", "old", "option"
      , "lobster", "vendor", "typical", "cushion"
      ]
    , [ "drill", "orphan", "hero", "throw"
      , "stand", "ecology", "hat", "gauge"
      , "antique", "hotel", "pistol", "rice"
      ]
    , [ "present", "trophy", "digital", "salad"
      , "kick", "apart", "airport", "stuff"
      , "prosper", "peace", "drive", "adjust"
      ]
    , [ "fluid", "brave", "disease", "rough"
      , "surge", "city", "ignore", "speed"
      , "borrow", "print", "pause", "smile"
      ]
    , [ "begin", "decorate", "smart", "mesh"
      , "cannon", "gas", "toe", "model"
      , "vacant", "survey", "victory", "cat"
      ]
    , [ "liberty", "sunny", "impact", "source"
      , "foil", "arrive", "inch", "find"
      , "obtain", "wet", "uncover", "huge"
      ]
    , [ "own", "pilot", "advance", "stock"
      , "pizza", "over", "february", "cheese"
      , "invite", "hello", "tell", "distance"
      ]
    , [ "alert", "satoshi", "two", "limit"
      , "bag", "soldier", "hair", "scatter"
      , "zebra", "rural", "dizzy", "cry"
      ]
    , [ "phone", "food", "they", "nose"
      , "cross", "music", "core", "leisure"
      , "menu", "curve", "bike", "rate"
      ]
    , [ "truly", "wagon", "soup", "submit"
      , "tail", "first", "push", "split"
      , "concert", "work", "source", "cart"
      ]
    , [ "symbol", "stage", "umbrella", "high"
      , "sand", "tilt", "slight", "open"
      , "kitten", "oil", "fade", "minor"
      ]
    , [ "tumble", "grit", "dumb", "game"
      , "raccoon", "giggle", "valley", "audit"
      , "army", "mandate", "around", "basket"
      ]
    , [ "owner", "foil", "vivid", "cloth"
      , "bright", "hurry", "nerve", "help"
      , "sister", "jaguar", "teach", "loyal"
      ]
    , [ "slender", "topple", "urban", "axis"
      , "swamp", "guess", "dizzy", "correct"
      , "visit", "valve", "ivory", "citizen"
      ]
    , [ "humble", "song", "wrap", "future"
      , "cinnamon", "accuse", "bright", "speed"
      , "inhale", "alien", "theory", "main"
      ]
    , [ "purity", "latin", "danger", "dutch"
      , "avocado", "endless", "off", "scissors"
      , "junk", "biology", "dial", "glue"
      ]
    , [ "lazy", "aunt", "obvious", "pave"
      , "abuse", "loan", "coral", "orchard"
      , "fat", "tone", "knock", "tired"
      ]
    , [ "fantasy", "kit", "luxury", "combine"
      , "bus", "hospital", "hybrid", "stool"
      , "cousin", "gauge", "grid", "audit"
      ]
    , [ "dentist", "inmate", "sun", "town"
      , "fame", "cable", "sport", "depth"
      , "scissors", "rude", "yard", "harbor"
      ]
    , [ "bright", "item", "flame", "august"
      , "consider", "rifle", "stereo", "end"
      , "very", "bright", "matrix", "mom"
      ]
    , [ "today", "pattern", "bacon", "version"
      , "differ", "pony", "universe", "snack"
      , "weird", "toddler", "belt", "door"
      ]
    , [ "veteran", "omit", "knife", "wrist"
      , "truth", "agree", "rhythm", "world"
      , "dynamic", "duty", "saddle", "dove"
      ]
    , [ "hat", "city", "disease", "patrol"
      , "answer", "select", "vibrant", "tag"
      , "dose", "rebuild", "length", "sting"
      ]
    , [ "liberty", "lens", "entry", "marriage"
      , "bean", "camp", "phone", "charge"
      , "alcohol", "boil", "plate", "banner"
      ]
    , [ "talk", "glory", "minute", "include"
      , "flag", "stuff", "laugh", "auction"
      , "benefit", "escape", "confirm", "task"
      ]
    , [ "joy", "convince", "reunion", "increase"
      , "core", "venue", "palm", "scan"
      , "wish", "vault", "until", "rice"
      ]
    , [ "walk", "hybrid", "game", "vanish"
      , "mushroom", "win", "observe", "crush"
      , "core", "lamp", "mirror", "twenty"
      ]
    , [ "hold", "joy", "grit", "great"
      , "quote", "retreat", "famous", "wreck"
      , "busy", "faint", "wish", "fetch"
      ]
    , [ "future", "obscure", "glow", "valid"
      , "wear", "boy", "exercise", "member"
      , "shoe", "add", "country", "spatial"
      ]
    , [ "tooth", "option", "satisfy", "patrol"
      , "amateur", "height", "above", "air"
      , "struggle", "reform", "speed", "mom"
      ]
    , [ "word", "cruel", "plate", "hedgehog"
      , "flavor", "judge", "device", "tuna"
      , "amateur", "walk", "open", "reduce"
      ]
    , [ "right", "energy", "oxygen", "eager"
      , "more", "direct", "yard", "easy"
      , "luxury", "auto", "knife", "loop"
      ]
    , [ "huge", "race", "host", "involve"
      , "win", "interest", "salad", "box"
      , "fatal", "cherry", "cage", "pioneer"
      ]
    , [ "phrase", "rapid", "fine", "neglect"
      , "already", "nut", "note", "chair"
      , "mushroom", "rack", "ivory", "riot"
      ]
    , [ "ivory", "citizen", "rule", "scare"
      , "angle", "method", "bounce", "caution"
      , "noble", "pottery", "plunge", "resource"
      ]
    , [ "behave", "attitude", "glide", "else"
      , "have", "moon", "settle", "minute"
      , "provide", "trade", "negative", "nothing"
      ]
    , [ "diary", "chunk", "total", "cruise"
      , "they", "curious", "foil", "actress"
      , "wish", "universe", "grape", "kind"
      ]
    , [ "mushroom", "print", "dish", "slim"
      , "agent", "tube", "expand", "actor"
      , "layer", "idea", "example", "quarter"
      ]
    , [ "riot", "sport", "access", "grid"
      , "destroy", "chronic", "evil", "doll"
      , "sibling", "blanket", "seed", "goose"
      ]
    , [ "pyramid", "song", "photo", "filter"
      , "subway", "rich", "broken", "anchor"
      , "blur", "lecture", "liar", "hope"
      ]
    , [ "sort", "crouch", "seven", "exile"
      , "extend", "evoke", "summer", "oppose"
      , "fork", "result", "plate", "goat"
      ]
    , [ "safe", "wrap", "order", "affair"
      , "fiber", "walnut", "skill", "timber"
      , "rookie", "ghost", "spot", "napkin"
      ]
    , [ "jaguar", "bitter", "merry", "destroy"
      , "frozen", "dune", "embody", "pull"
      , "cradle", "peasant", "sail", "whisper"
      ]
    ]

externalAddresses :: [(TxIn, String)]
externalAddresses =
    [ ( unsafeMkTxIn
        "external1s0c3kr37th47lcajtcdcmj6z954ylg537msupdcxxwsdrnmuclxfqr9cqau"
      , "ed25519_sk1qmgkz5d4c0swl2uwfvaxuxyd7zmuq95s22xxwkel30sx9l9m6ffqa4hdxg"
      )
    , ( unsafeMkTxIn
        "external1sdqse0sme69dfrrlwshhkwqwwqtrp9h6g4h7qzdgfe0ws27t746ludqrs69"
      , "ed25519_sk1j7gw3p942jh78ck87f0zknx5ts87cjmw7qrc3paa9wkwhznmajcsm72d5v"
      )

    , ( unsafeMkTxIn
        "external1s0h6kelrx0kukrg80p66z63dkycr200taxa8vscgvt07r3heyet97jztehn"
      , "ed25519_sk1vtvxq5ya43zygslw46n05pjtqp686mph2kq6xlh74qcut7rk0jcsd4h40r"
      )

    , ( unsafeMkTxIn
        "external1swe7gkjfh5us4ndh05qs4l5zzr9326gmx90n4wyyqhhcd9z6d2wu7r4altz"
      , "ed25519_sk1zyqdl00vkkkjdnqcgcmkfqr3m5vr7fg4qm000zsv70kunzzzw5cqjmqhza"
      )

    , ( unsafeMkTxIn
        "external1s0df8z9qrqc7mvv2v6uk0th930km5rgj00arzp8pmhm86z9srncd73dzqer"
      , "ed25519_sk1lkv5g5anapcpaud3ny9wsl6d6smdrjj0v2jx70jnkmtzc07u2vksyyheqw"
      )

    , ( unsafeMkTxIn
        "external1swch34gwdh8c5ly5u9872knkkjekvlhtvkxgg0uryr3qy3arxuhr76ks8ha"
      , "ed25519_sk1lek3s5nzv67kn3dqp7tzz5xreh2954e6npa02pct94fpqg7s34rsccyah8"
      )

    , ( unsafeMkTxIn
        "external1swnfgruhxu2y6986vaq8kmnz4q2aa2j82unc52mjtm9mha79axmrzwua468"
      , "ed25519_sk1lq9ccgjz6cmkc203xu5efz0nyhvngea4uapzt0nqwxq6c349ky2qzvr7wf"
      )

    , ( unsafeMkTxIn
        "external1sd3w9tlg6ln8vn8fc8peu8xtavsehdvpx38h44lw782m7lpap4xzjd9dwsc"
      , "ed25519_sk16yqxp28e4lsj00klyrutnf9eld879s7gww8h58chdnvhngvqgz4sf95z86"
      )

    , ( unsafeMkTxIn
        "external1sdmtjwmdqsz2nl2pypgsdxypvf97f7y0dppw5ypg2vyyrhzfpazcckq7557"
      , "ed25519_sk1y8zzc7dh36p54jw8j6cc6awpymhrfs8flczf2xt2zg798zx2qhrscq7myx"
      )

    , ( unsafeMkTxIn
        "external1s0vn7nhenkn8lfx2twydelw7wlzq5rxtkxqzhcu53anjgz2gjl4tkc438l5"
      , "ed25519_sk19aj3wm4ztwz6mjf33knq3u30am0exhntu76qees7zjryc4hzugpspmjz5w"
      )
    ]
  where
    unsafeMkTxIn out =
        let
            Right (_, dp) = Bech32.decodeLenient out
            Just addr = Address <$> Bech32.dataPartToBytes dp
            f _addr = Identity $ const $ error "no input should require no wit"
            fragmentId outs = fst
                . runIdentity
                $ constructTransactionFragment [] outs f
            sourceId = fragmentId [TxOut addr (Coin 100000000000)]
        in
            TxIn sourceId 0

{-

{-# LANGUAGE TypeApplications #-}

import Cardano.Wallet.Jormungandr.Compatibility
    ( Jormungandr, NetworkDiscriminant (..) )
import Cardano.Wallet.Primitive.AddressDerivation
    ( PaymentAddress (..), Passphrase (..), publicKey )
import Cardano.Wallet.Primitive.AddressDerivation.Byron
    ( deriveAccountPrivateKey, deriveAddressPrivateKey, generateKeyFromSeed )
import Cardano.Wallet.Primitive.Mnemonic
    ( entropyToBytes
    , entropyToMnemonic
    , genEntropy
    , mnemonicToEntropy
    , mnemonicToText
    )
import Cardano.Wallet.Primitive.Types
    ( Address (..) )
import Control.Monad
    ( forM_, replicateM )
import Data.Proxy
    ( Proxy (..) )
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
        TIO.putStrLn $ T.intercalate ", " $ surroundedBy '"' <$> mnemonicToText @12 m
        forM_ addrs (TIO.putStrLn . encodeAddress (Proxy @(Jormungandr 'Mainnet)))
  where
    surroundedBy :: Char -> Text -> Text
    surroundedBy c txt = T.singleton c <> txt <> T.singleton c

addresses :: Mnemonic n -> [Address]
addresses mw =
    let
        (seed, pwd) =
            (Passphrase $ entropyToBytes $ mnemonicToEntropy mw, mempty)
        rootXPrv =
            generateKeyFromSeed seed pwd
        accXPrv =
            deriveAccountPrivateKey pwd rootXPrv minBound
        addrXPrv =
            deriveAddressPrivateKey pwd accXPrv
    in
        [ paymentAddress @(Jormungandr 'Mainnet) (publicKey $ addrXPrv ix)
        | ix <- [minBound..maxBound]
        ]
-}
