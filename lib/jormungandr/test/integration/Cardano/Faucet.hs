{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Faucet
    ( initFaucet

    -- * Internal
    , genFaucets
    ) where

import Prelude hiding
    ( appendFile )

import Cardano.Wallet.Api.Types
    ( encodeAddress )
import Cardano.Wallet.Jormungandr.Binary
    ( MkFragment (..), TxWitnessTag (..), fragmentId, putFragment )
import Cardano.Wallet.Primitive.AddressDerivation
    ( AccountingStyle (..)
    , HardDerivation (..)
    , NetworkDiscriminant (..)
    , PaymentAddress (..)
    , SomeMnemonic (..)
    , publicKey
    )
import Cardano.Wallet.Primitive.AddressDerivation.Icarus
    ( generateKeyFromSeed )
import Cardano.Wallet.Primitive.Fee
    ( FeePolicy (..) )
import Cardano.Wallet.Primitive.Mnemonic
    ( Mnemonic, entropyToMnemonic, genEntropy, mnemonicToText )
import Cardano.Wallet.Primitive.Types
    ( Address (..), Coin (..), Hash (..), TxIn (..), TxOut (..) )
import Cardano.Wallet.Unsafe
    ( unsafeFromHex, unsafeMkMnemonic )
import Control.Concurrent.MVar
    ( newMVar )
import Control.Monad
    ( forM_, replicateM )
import Data.ByteString
    ( ByteString )
import Data.Text
    ( Text )
import GHC.TypeLits
    ( KnownNat )
import System.FilePath
    ( FilePath, (</>) )
import System.IO.Temp
    ( withSystemTempDirectory )
import Test.Integration.Faucet
    ( Faucet (..) )
import Test.Integration.Jcli
    ( argHex, argInt, getBlock0H, jcli, jcli_, sinkAddress )

import qualified Codec.Binary.Bech32 as Bech32
import qualified Codec.Binary.Bech32.TH as Bech32
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as TIO

-- | Initialize a bunch of faucet wallets and make them available for the
-- integration tests scenarios.
initFaucet :: FeePolicy -> IO Faucet
initFaucet policy = Faucet
    <$> newMVar seqMnemonics
    <*> newMVar icaMnemonics
    <*> newMVar rndMnemonics
    <*> newMVar (mkTxBuilder policy <$> externalAddresses)

-- | Prepare externally signed Tx for Jormungandr
mkTxBuilder
    :: FeePolicy
    -> (TxIn, String)
    -> (Address, Coin)
    -> IO ByteString
mkTxBuilder (LinearFee cst coeff _) (TxIn inpTx inpIx, key) (addr, Coin amt) =
    withSystemTempDirectory "cardano-wallet-jormungandr" $ \d -> do
        let txFile = d </> "trans.tx"
        let witnessFile = d </> "witness"
        let keyFile = d </> "key.prv"

        TIO.writeFile keyFile (T.pack key)
        prepareTx txFile
        signTx txFile keyFile witnessFile
        toMessage txFile
  where
    -- Amount associated with each input in the genesis file, in Lovelace
    inputAmt :: Int
    inputAmt =
        100000000000

    prepareTx :: FilePath -> IO ()
    prepareTx txFile = do
        let hrp = [Bech32.humanReadablePart|addr|]
        let dp  = Bech32.dataPartFromBytes (unAddress addr)
        jcli_
            [ "transaction"
            , "new"
            , "--staging"
            , txFile
            ]
        jcli_
            [ "transaction"
            , "add-input"
            , argHex inpTx
            , show inpIx
            , show inputAmt
            , "--staging"
            , txFile
            ]
        jcli_
            [ "transaction"
            , "add-output"
            , T.unpack (Bech32.encodeLenient hrp dp)
            , show amt
            , "--staging"
            , txFile
            ]
        jcli_
            [ "transaction"
            , "finalize"
            , sinkAddress
            , "--fee-constant", argInt cst
            , "--fee-coefficient", argInt coeff
            , "--staging"
            , txFile
            ]

    signTx :: FilePath -> FilePath -> FilePath -> IO ()
    signTx txFile keyFile witnessFile = do
        txId <- jcli
            [ "transaction"
            , "data-for-witness"
            , "--staging"
            , txFile
            ]
        block0H <- argHex <$> getBlock0H
        jcli_
            [ "transaction"
            , "make-witness"
            , T.unpack . T.strip . T.pack $ txId
            , "--genesis-block-hash"
            , block0H
            , "--type", "utxo"
            , witnessFile
            , keyFile
            ]
        jcli_
            [ "transaction"
            , "add-witness"
            , witnessFile
            , "--staging"
            , txFile
            ]

    toMessage :: FilePath -> IO ByteString
    toMessage txFile = do
        jcli_
            [ "transaction"
            , "seal"
            , "--staging"
            , txFile
            ]
        bytes <- jcli
            [ "transaction"
            , "to-message"
            , "--staging"
            , txFile
            ]
        return (unsafeFromHex $ T.encodeUtf8 $ T.strip $ T.pack bytes)

seqMnemonics :: [Mnemonic 15]
seqMnemonics = unsafeMkMnemonic <$>
    [ [ "vintage", "poem", "topic", "machine", "hazard"
      , "cement", "dune", "glimpse", "fix", "brief"
      , "account", "badge", "mass", "silly", "business"
      ]
    , [ "shift", "mistake", "rural", "security", "inspire"
      , "loyal", "wink", "special", "blast", "retreat"
      , "crouch", "noise", "dirt", "around", "drastic"
      ]
    , [ "soldier", "this", "verb", "copper", "immune"
      , "unveil", "engine", "know", "tower", "only"
      , "foot", "riot", "orchard", "member", "guitar"
      ]
    , [ "cupboard", "fringe", "garment", "dawn", "caught"
      , "cream", "alpha", "sorry", "unusual", "federal"
      , "item", "leopard", "lawn", "rescue", "desk"
      ]
    , [ "glad", "hold", "sweet", "tobacco", "topple"
      , "rich", "grab", "bridge", "adjust", "excess"
      , "improve", "job", "lottery", "diary", "dust"
      ]
    , [ "all", "flee", "sugar", "mail", "response"
      , "minimum", "bulk", "stone", "cost", "dynamic"
      , "forget", "embrace", "spray", "ocean", "luggage"
      ]
    , [ "kiwi", "million", "space", "squirrel", "deliver"
      , "galaxy", "cat", "private", "meadow", "canvas"
      , "differ", "rescue", "artist", "laptop", "claim"
      ]
    , [ "length", "alpha", "return", "angle", "siren"
      , "buyer", "reject", "absurd", "piece", "crash"
      , "toilet", "flag", "viable", "brick", "sense"
      ]
    , [ "viable", "become", "talk", "benefit", "start"
      , "shield", "chapter", "skull", "donor", "hip"
      , "place", "aware", "acquire", "mango", "hold"
      ]
    , [ "awkward", "electric", "strong", "early", "rose"
      , "abuse", "mutual", "limit", "ketchup", "child"
      , "limb", "exist", "hurry", "business", "whisper"
      ]
    , [ "blood", "limit", "pumpkin", "fringe", "order"
      , "trick", "answer", "festival", "ethics", "educate"
      , "luggage", "dinner", "record", "fox", "truth"
      ]
    , [ "bridge", "joke", "jeans", "width", "social"
      , "banner", "visit", "enlist", "reason", "hand"
      , "license", "subway", "butter", "render", "absent"
      ]
    , [ "bless", "turkey", "install", "across", "bronze"
      , "check", "true", "icon", "treat", "that"
      , "tuition", "flush", "panther", "powder", "ecology"
      ]
    , [ "trick", "razor", "bicycle", "front", "hollow"
      , "liberty", "swift", "coconut", "pull", "raccoon"
      , "level", "woman", "awful", "sound", "swarm"
      ]
    , [ "task", "invite", "open", "reflect", "guilt"
      , "net", "require", "story", "later", "almost"
      , "wedding", "essence", "divert", "shaft", "stone"
      ]
    , [ "annual", "outer", "ozone", "clever", "major"
      , "carry", "evidence", "punch", "update", "antenna"
      , "drive", "scrub", "artefact", "intact", "drift"
      ]
    , [ "absurd", "seat", "ball", "together", "donate"
      , "bulk", "sustain", "loop", "convince", "capital"
      , "peanut", "mutual", "notice", "improve", "jewel"
      ]
    , [ "hurt", "timber", "clip", "wall", "fox"
      , "tiger", "kangaroo", "cliff", "soul", "muscle"
      , "vacant", "output", "whale", "keep", "avoid"
      ]
    , [ "sand", "album", "coconut", "come", "lamp"
      , "sick", "curtain", "mammal", "ritual", "robust"
      , "spirit", "year", "total", "patrol", "roof"
      ]
    , [ "answer", "burst", "guess", "family", "jealous"
      , "acoustic", "captain", "dog", "south", "brisk"
      , "space", "ability", "copper", "trim", "quick"
      ]
    , [ "snack", "furnace", "coil", "winter", "unhappy"
      , "relax", "blade", "corn", "metal", "casual"
      , "medal", "load", "joke", "since", "violin"
      ]
    , [ "phrase", "lyrics", "park", "ring", "orbit"
      , "walk", "bacon", "balcony", "rare", "unable"
      , "wait", "attend", "rigid", "slice", "reason"
      ]
    , [ "drip", "squirrel", "pulp", "joke", "glow"
      , "novel", "flip", "online", "mention", "security"
      , "silk", "wedding", "ceiling", "gospel", "melt"
      ]
    , [ "crazy", "alert", "crop", "beauty", "normal"
      , "ripple", "material", "return", "advance", "rookie"
      , "abstract", "voyage", "carpet", "approve", "pudding"
      ]
    , [ "absorb", "achieve", "keen", "border", "display"
      , "enrich", "item", "economy", "upgrade", "bamboo"
      , "absurd", "airport", "next", "exotic", "lemon"
      ]
    , [ "life", "manage", "acquire", "push", "baby"
      , "much", "organ", "lottery", "charge", "problem"
      , "divert", "treat", "panic", "door", "blade"
      ]
    , [ "degree", "human", "razor", "tonight", "supply"
      , "neutral", "cake", "match", "orient", "kid"
      , "black", "car", "castle", "turtle", "finish"
      ]
    , [ "that", "replace", "level", "proud", "arrange"
      , "cube", "hope", "plastic", "agent", "hollow"
      , "supply", "town", "goddess", "permit", "suffer"
      ]
    , [ "raven", "pulse", "like", "typical", "radar"
      , "fantasy", "neck", "way", "feel", "mixed"
      , "actress", "wrestle", "outer", "game", "empower"
      ]
    , [ "manual", "pretty", "road", "prevent", "merit"
      , "security", "nurse", "junior", "dice", "program"
      , "pudding", "area", "grant", "deer", "tuna"
      ]
    , [ "fly", "key", "inform", "infant", "stool"
      , "juice", "oil", "scheme", "boat", "kangaroo"
      , "sort", "hobby", "rapid", "hunt", "same"
      ]
    , [ "cute", "flee", "error", "choose", "deny"
      , "poverty", "power", "crawl", "soap", "universe"
      , "pact", "grief", "gospel", "cat", "account"
      ]
    , [ "worry", "fox", "sponsor", "equip", "pulp"
      , "excite", "unveil", "mansion", "nerve", "acoustic"
      , "swear", "lottery", "elbow", "hold", "approve"
      ]
    , [ "easily", "original", "word", "layer", "inch"
      , "portion", "way", "dismiss", "control", "return"
      , "match", "update", "spike", "olympic", "party"
      ]
    , [ "amused", "total", "version", "fiber", "anxiety"
      , "volcano", "drip", "mention", "ripple", "list"
      , "wear", "erupt", "island", "leader", "jacket"
      ]
    , [ "identify", "arrow", "pigeon", "wrestle", "oxygen"
      , "logic", "notice", "once", "rally", "cool"
      , "sausage", "ensure", "caution", "next", "lemon"
      ]
    , [ "fury", "basket", "nice", "survey", "unaware"
      , "girl", "large", "vacuum", "brain", "cross"
      , "man", "whisper", "begin", "check", "joke"
      ]
    , [ "force", "skull", "thrive", "skirt", "hundred"
      , "hope", "cheap", "agree", "involve", "sing"
      , "risk", "coffee", "expect", "wrist", "bless"
      ]
    , [ "love", "normal", "hedgehog", "happy", "float"
      , "treat", "bike", "canoe", "differ", "same"
      , "canoe", "sister", "wreck", "amount", "cruise"
      ]
    , [ "soul", "scare", "caught", "before", "kingdom"
      , "width", "object", "crash", "truck", "senior"
      , "master", "bonus", "evidence", "jewel", "property"
      ]
    , [ "document", "decade", "puppy", "short", "surge"
      , "shed", "seed", "strike", "never", "maple"
      , "extra", "home", "route", "require", "devote"
      ]
    , [ "desert", "utility", "stock", "option", "marble"
      , "build", "type", "zero", "royal", "version"
      , "venture", "chief", "lawsuit", "stool", "boring"
      ]
    , [ "hidden", "romance", "message", "federal", "truly"
      , "daughter", "rabbit", "boy", "subway", "anchor"
      , "glare", "arctic", "chaos", "fish", "wise"
      ]
    , [ "vehicle", "cage", "endorse", "buddy", "train"
      , "leg", "push", "muffin", "consider", "cover"
      , "coach", "rubber", "invite", "purity", "crawl"
      ]
    , [ "pass", "diary", "pumpkin", "cradle", "upon"
      , "shy", "stumble", "kangaroo", "soap", "flush"
      , "glove", "funny", "scrap", "two", "balcony"
      ]
    , [ "lend", "notice", "scare", "feature", "black"
      , "bid", "edit", "brass", "doll", "spring"
      , "slice", "foot", "drum", "clown", "solution"
      ]
    , [ "gold", "stumble", "ability", "dynamic", "couple"
      , "guide", "case", "hard", "vacuum", "immune"
      , "grunt", "daring", "soup", "lawn", "venue"
      ]
    , [ "same", "tumble", "pool", "basket", "loud"
      , "cabin", "become", "vibrant", "bunker", "grocery"
      , "motor", "jeans", "churn", "sleep", "slab"
      ]
    , [ "clog", "gloom", "distance", "shoot", "good"
      , "pave", "chief", "cram", "myself", "invite"
      , "involve", "door", "accuse", "crazy", "elevator"
      ]
    , [ "rally", "crater", "ecology", "vintage", "corn"
      , "ice", "obey", "critic", "reveal", "enlist"
      , "next", "chapter", "leg", "cradle", "outdoor"
      ]
    , [ "digital", "avocado", "brief", "lobster", "ankle"
      , "economy", "room", "measure", "canal", "onion"
      , "wood", "old", "place", "drift", "paper"
      ]
    , [ "century", "response", "there", "sure", "toe"
      , "open", "twenty", "source", "discover", "orphan"
      , "zone", "ancient", "black", "liquid", "pact"
      ]
    , [ "supply", "lemon", "test", "expire", "view"
      , "escape", "ask", "settle", "engage", "view"
      , "virtual", "fine", "home", "escape", "damage"
      ]
    , [ "matrix", "culture", "duck", "foil", "carpet"
      , "try", "grab", "author", "vacant", "fabric"
      , "sunset", "vintage", "census", "again", "disease"
      ]
    , [ "toilet", "gas", "easy", "hood", "match"
      , "obtain", "mistake", "recycle", "biology", "issue"
      , "trim", "guitar", "mandate", "marine", "cereal"
      ]
    , [ "outer", "found", "horse", "spray", "siren"
      , "north", "sheriff", "grace", "party", "remind"
      , "enforce", "vintage", "simple", "wreck", "rice"
      ]
    , [ "arm", "dinosaur", "judge", "sense", "cupboard"
      , "return", "narrow", "title", "brisk", "test"
      , "walnut", "gorilla", "cause", "regret", "churn"
      ]
    , [ "case", "pretty", "mesh", "poet", "paddle"
      , "deny", "mobile", "penalty", "curve", "sweet"
      , "cake", "want", "address", "position", "artwork"
      ]
    , [ "few", "tag", "unlock", "rain", "skirt"
      , "human", "swim", "match", "apart", "position"
      , "online", "fetch", "parent", "duty", "invite"
      ]
    , [ "various", "again", "ginger", "desert", "forward"
      , "bean", "length", "raw", "wall", "door"
      , "nerve", "van", "stuff", "error", "describe"
      ]
    , [ "there", "length", "bar", "vapor", "upper"
      , "kangaroo", "tennis", "hollow", "slice", "case"
      , "win", "food", "wild", "erupt", "alarm"
      ]
    , [ "door", "behind", "siege", "offer", "wedding"
      , "business", "sea", "fame", "enforce", "busy"
      , "floor", "enjoy", "wrong", "excite", "floor"
      ]
    , [ "ethics", "spike", "square", "security", "text"
      , "clip", "future", "voice", "lucky", "shift"
      , "phrase", "teach", "kitten", "mushroom", "kite"
      ]
    , [ "glimpse", "inform", "carpet", "rapid", "pretty"
      , "select", "glare", "borrow", "another", "recall"
      , "aspect", "scheme", "gold", "inherit", "asset"
      ]
    , [ "filter", "dad", "crumble", "vehicle", "tape"
      , "ugly", "wolf", "earth", "coin", "grocery"
      , "weather", "crop", "drastic", "shaft", "hurdle"
      ]
    , [ "bright", "type", "cross", "style", "setup"
      , "flame", "mango", "solve", "group", "hood"
      , "vivid", "maze", "faint", "enable", "into"
      ]
    , [ "main", "judge", "tongue", "gather", "gain"
      , "position", "caught", "neutral", "major", "buzz"
      , "hospital", "guide", "page", "chase", "rather"
      ]
    , [ "wink", "enjoy", "vessel", "settle", "sport"
      , "turkey", "main", "palm", "prefer", "awesome"
      , "label", "soup", "fish", "crop", "hen"
      ]
    , [ "luggage", "system", "category", "grain", "aware"
      , "rapid", "burger", "ability", "weapon", "analyst"
      , "seed", "place", "youth", "net", "company"
      ]
    , [ "name", "predict", "water", "toe", "steel"
      , "embark", "clown", "female", "scale", "bomb"
      , "genre", "sign", "rifle", "wash", "cost"
      ]
    , [ "purpose", "elephant", "stove", "renew", "divide"
      , "dress", "brother", "deny", "fan", "exercise"
      , "monkey", "iron", "filter", "negative", "enforce"
      ]
    , [ "mistake", "ecology", "sunset", "panic", "flag"
      , "holiday", "photo", "day", "sentence", "unfair"
      , "duty", "program", "wreck", "sleep", "slot"
      ]
    , [ "zebra", "dizzy", "body", "cheese", "gown"
      , "mouse", "under", "gift", "nice", "detect"
      , "select", "proud", "fold", "actual", "minimum"
      ]
    , [ "glove", "chat", "click", "hazard", "anger"
      , "advice", "empower", "purity", "spatial", "gallery"
      , "slush", "build", "physical", "main", "electric"
      ]
    , [ "say", "luxury", "turtle", "fly", "debris"
      , "physical", "fox", "forward", "advice", "any"
      , "drill", "tongue", "fluid", "frost", "party"
      ]
    , [ "high", "talent", "network", "you", "pledge"
      , "equal", "barrel", "illness", "subway", "shine"
      , "advance", "grab", "spy", "minute", "face"
      ]
    , [ "crouch", "team", "blind", "three", "twist"
      , "glad", "super", "keep", "engage", "perfect"
      , "shop", "gown", "illness", "edge", "script"
      ]
    , [ "hero", "reduce", "law", "vacant", "local"
      , "common", "result", "title", "delay", "flee"
      , "park", "wrestle", "upon", "always", "token"
      ]
    , [ "predict", "come", "exist", "affair", "supply"
      , "joke", "eternal", "border", "area", "all"
      , "puppy", "grow", "expose", "act", "hair"
      ]
    , [ "shift", "dial", "steak", "cross", "leisure"
      , "amused", "story", "gossip", "choice", "summer"
      , "wall", "exit", "distance", "kiss", "sport"
      ]
    , [ "extend", "merge", "staff", "unfold", "soul"
      , "caught", "purse", "silk", "sister", "puzzle"
      , "blush", "gadget", "garment", "local", "used"
      ]
    , [ "pitch", "group", "expand", "permit", "orange"
      , "wide", "roof", "organ", "radio", "bonus"
      , "adapt", "bonus", "twice", "recycle", "tunnel"
      ]
    , [ "cruise", "vessel", "bird", "wing", "goat"
      , "grab", "tide", "artwork", "barrel", "second"
      , "laugh", "ill", "nut", "around", "mouse"
      ]
    , [ "garment", "sail", "west", "salute", "economy"
      , "leave", "tissue", "interest", "vehicle", "globe"
      , "strategy", "else", "skirt", "purpose", "eyebrow"
      ]
    , [ "hurt", "veteran", "wife", "upset", "napkin"
      , "ladder", "lift", "zebra", "nurse", "turn"
      , "story", "fossil", "gesture", "odor", "satoshi"
      ]
    , [ "bonus", "mind", "cement", "two", "predict"
      , "surge", "vocal", "alien", "high", "any"
      , "dune", "keep", "chimney", "until", "sad"
      ]
    , [ "bless", "palm", "room", "afraid", "again"
      , "speak", "indicate", "cry", "blue", "meadow"
      , "orient", "engage", "observe", "flat", "reflect"
      ]
    , [ "outer", "few", "company", "shield", "rather"
      , "cushion", "finger", "easily", "hidden", "sketch"
      , "cluster", "organ", "anger", "expect", "submit"
      ]
    , [ "need", "blood", "actress", "beef", "universe"
      , "belt", "hope", "saddle", "merry", "multiply"
      , "rely", "piece", "circle", "section", "eye"
      ]
    , [ "slot", "laugh", "stumble", "chase", "buddy"
      , "yellow", "camp", "breeze", "stamp", "dentist"
      , "favorite", "trophy", "legend", "margin", "extra"
      ]
    , [ "daring", "trouble", "adapt", "december", "follow"
      , "gap", "ahead", "human", "airport", "surge"
      , "puppy", "popular", "start", "jelly", "buyer"
      ]
    , [ "tilt", "garlic", "amateur", "gain", "various"
      , "royal", "enter", "divide", "verify", "ribbon"
      , "define", "stomach", "lunch", "short", "dentist"
      ]
    , [ "grant", "weird", "gauge", "vessel", "mushroom"
      , "hire", "caught", "glad", "bless", "firm"
      , "mistake", "save", "impose", "electric", "obvious"
      ]
    , [ "scatter", "pool", "boost", "cabin", "poverty"
      , "chair", "one", "movie", "solid", "rain"
      , "emerge", "tree", "bulb", "lava", "tent"
      ]
    , [ "fresh", "fit", "attack", "sentence", "indicate"
      , "fee", "change", "ivory", "mountain", "label"
      , "screen", "crisp", "same", "absorb", "dry"
      ]
    , [ "digital", "bleak", "steak", "marble", "bulb"
      , "angle", "link", "peasant", "mad", "erupt"
      , "spin", "animal", "mule", "foil", "notable"
      ]
    , [ "robot", "release", "absorb", "toss", "van"
      , "duty", "hen", "ready", "resemble", "wild"
      , "grunt", "door", "fortune", "illegal", "often"
      ]
    , [ "lamp", "blur", "fan", "pear", "crucial"
      , "angle", "prize", "wool", "resist", "involve"
      , "swallow", "guard", "used", "dash", "exit"
      ]
    , [ "depend", "scheme", "strike", "gain", "index"
      , "focus", "flower", "donor", "pony", "cement"
      , "leisure", "cereal", "buddy", "acid", "design"
      ]
    , [ "movie", "cigar", "domain", "maze", "they"
      , "lens", "area", "own", "feed", "oppose"
      , "proof", "bench", "act", "tragic", "human"
      ]
    ]

icaMnemonics :: [Mnemonic 15]
icaMnemonics = unsafeMkMnemonic <$>
    [ [ "public", "wild", "salad", "cereal", "when"
      , "zone", "ship", "circle", "other", "second"
      , "time", "priority", "select", "apart", "social"
      ]
    , [ "report", "weird", "border", "gesture", "since"
      , "earn", "motor", "elbow", "huge", "pilot"
      , "cool", "civil", "duty", "outer", "exhaust"
      ]
    , [ "illegal", "uncover", "fruit", "april", "snap"
      , "army", "brown", "sister", "situate", "lunch"
      , "they", "fog", "isolate", "earn", "vocal"
      ]
    , [ "knife", "satisfy", "measure", "around", "time"
      , "thought", "cigar", "boss", "truck", "bar"
      , "mushroom", "hold", "raccoon", "asset", "canvas"
      ]
    , [ "amazing", "pole", "kiss", "expose", "whip"
      , "unfair", "example", "slice", "great", "they"
      , "element", "claw", "photo", "dwarf", "green"
      ]
    , [ "round", "trend", "rescue", "flight", "awkward"
      , "enemy", "luggage", "range", "eagle", "shaft"
      , "giggle", "double", "pencil", "jazz", "home"
      ]
    , [ "talent", "example", "renew", "true", "amused"
      , "alcohol", "immune", "exclude", "cat", "ceiling"
      , "squeeze", "cover", "slender", "pond", "turkey"
      ]
    , [ "box", "elegant", "raccoon", "brick", "uphold"
      , "behind", "blame", "marble", "tip", "move"
      , "gift", "juice", "crystal", "circle", "sound"
      ]
    , [ "mango", "street", "flush", "universe", "clap"
      , "system", "talk", "steel", "tray", "target"
      , "forum", "dust", "brisk", "expose", "prevent"
      ]
    , [ "behind", "rib", "say", "absorb", "enroll"
      , "pyramid", "balance", "strategy", "response", "evolve"
      , "pipe", "dolphin", "shift", "flag", "history"
      ]
    , [ "pipe", "weekend", "master", "nice", "museum"
      , "endless", "cancel", "animal", "end", "aware"
      , "unaware", "submit", "mind", "alert", "oblige"
      ]
    , [ "surge", "fan", "diary", "forget", "lobster"
      , "south", "auto", "slim", "display", "yellow"
      , "caution", "victory", "wreck", "silver", "direct"
      ]
    , [ "mean", "slide", "heavy", "science", "south"
      , "delay", "divorce", "design", "example", "swim"
      , "dog", "neck", "disorder", "drip", "wet"
      ]
    , [ "crumble", "dog", "ordinary", "always", "mention"
      , "lunch", "corn", "key", "color", "veteran"
      , "item", "chapter", "winter", "flee", "shoe"
      ]
    , [ "toddler", "print", "pave", "venue", "mind"
      , "program", "ocean", "purchase", "embody", "super"
      , "exchange", "ignore", "artist", "party", "economy"
      ]
    , [ "survey", "slim", "girl", "raccoon", "valley"
      , "clever", "wide", "assault", "blood", "copy"
      , "uncover", "bachelor", "face", "cart", "style"
      ]
    , [ "vast", "toast", "supreme", "grid", "sniff"
      , "ecology", "eternal", "agent", "cable", "mind"
      , "step", "gravity", "gloom", "process", "couch"
      ]
    , [ "civil", "near", "wrist", "stuff", "draw"
      , "nature", "second", "supreme", "minute", "kit"
      , "document", "dolphin", "same", "extend", "rookie"
      ]
    , [ "account", "spend", "broom", "soon", "swim"
      , "maid", "bring", "trust", "turtle", "hire"
      , "room", "clutch", "copper", "mixture", "early"
      ]
    , [ "neck", "soup", "learn", "tag", "skull"
      , "seek", "face", "vessel", "leopard", "rebel"
      , "engine", "expand", "hat", "magic", "liquid"
      ]
    , [ "media", "tackle", "govern", "play", "snap"
      , "cheap", "fit", "uniform", "welcome", "record"
      , "measure", "lottery", "spoil", "group", "host"
      ]
    , [ "enhance", "luxury", "left", "inch", "together"
      , "strike", "hotel", "fossil", "exhibit", "above"
      , "unusual", "mutual", "hint", "shield", "moral"
      ]
    , [ "moral", "abandon", "depend", "tip", "soap"
      , "mushroom", "grab", "worry", "royal", "strike"
      , "scrub", "walnut", "summer", "that", "poet"
      ]
    , [ "fuel", "twice", "camera", "control", "custom"
      , "oil", "puppy", "scissors", "will", "comic"
      , "general", "cry", "assault", "debate", "whale"
      ]
    , [ "lizard", "burden", "tortoise", "ring", "monkey"
      , "senior", "little", "endless", "increase", "quote"
      , "flat", "repair", "snake", "asset", "brand"
      ]
    , [ "border", "receive", "task", "october", "weird"
      , "palm", "gather", "claw", "either", "matter"
      , "avocado", "pioneer", "borrow", "that", "cash"
      ]
    , [ "camp", "hawk", "gift", "swear", "steel"
      , "evolve", "coconut", "sight", "sustain", "tube"
      , "diagram", "treat", "reason", "tiny", "wear"
      ]
    , [ "water", "wood", "forest", "also", "exile"
      , "settle", "city", "dance", "shine", "maze"
      , "fish", "attract", "verb", "laptop", "hair"
      ]
    , [ "body", "soldier", "pony", "flame", "cave"
      , "brave", "earth", "team", "symptom", "library"
      , "write", "warrior", "certain", "gentle", "cool"
      ]
    , [ "project", "document", "thought", "dentist", "card"
      , "main", "afraid", "cliff", "crucial", "debate"
      , "raise", "anger", "hundred", "lunar", "final"
      ]
    , [ "sun", "someone", "emotion", "replace", "error"
      , "surge", "mobile", "depth", "soft", "anger"
      , "clog", "sing", "fabric", "decline", "output"
      ]
    , [ "behind", "jump", "convince", "spread", "lobster"
      , "six", "tooth", "feel", "error", "jar"
      , "reopen", "save", "rotate", "jealous", "dutch"
      ]
    , [ "title", "primary", "loan", "peasant", "reduce"
      , "tooth", "warm", "way", "daring", "accident"
      , "replace", "pact", "yellow", "mammal", "strategy"
      ]
    , [ "focus", "fortune", "foster", "cattle", "split"
      , "just", "sorry", "phone", "field", "proud"
      , "patch", "expose", "toward", "garment", "now"
      ]
    , [ "dust", "wheel", "blossom", "describe", "spray"
      , "pipe", "broom", "corn", "vintage", "switch"
      , "isolate", "disagree", "over", "ski", "truth"
      ]
    , [ "toast", "claim", "assault", "sword", "scorpion"
      , "emotion", "weasel", "detect", "bounce", "mixture"
      , "various", "warrior", "believe", "wing", "celery"
      ]
    , [ "donor", "casual", "mention", "media", "erosion"
      , "tail", "pass", "camp", "raven", "accident"
      , "nothing", "option", "lobster", "accident", "magnet"
      ]
    , [ "remove", "limit", "okay", "garment", "cat"
      , "long", "steak", "since", "follow", "caution"
      , "forward", "april", "nest", "grab", "height"
      ]
    , [ "nature", "weekend", "medal", "neither", "upgrade"
      , "urban", "book", "swear", "ketchup", "enable"
      , "enter", "oblige", "sport", "cat", "drink"
      ]
    , [ "wheel", "pair", "used", "radar", "rate"
      , "mail", "execute", "february", "decline", "weasel"
      , "exchange", "visit", "slam", "trap", "globe"
      ]
    , [ "session", "outside", "dash", "whisper", "prize"
      , "frost", "used", "dune", "dust", "diamond"
      , "expose", "hamster", "object", "home", "web"
      ]
    , [ "visa", "furnace", "shy", "fun", "quarter"
      , "buffalo", "rough", "october", "cry", "push"
      , "marriage", "around", "pony", "spike", "struggle"
      ]
    , [ "river", "verb", "deny", "tobacco", "release"
      , "game", "culture", "trash", "essay", "excess"
      , "citizen", "ignore", "home", "hawk", "purse"
      ]
    , [ "erode", "father", "violin", "afraid", "satisfy"
      , "supreme", "tag", "flip", "tuition", "satoshi"
      , "two", "wagon", "embody", "area", "good"
      ]
    , [ "repair", "resemble", "appear", "clown", "coconut"
      , "truck", "trade", "ship", "fly", "hat"
      , "layer", "gift", "camera", "else", "spawn"
      ]
    , [ "vast", "garment", "debate", "industry", "tennis"
      , "private", "else", "lazy", "thumb", "arm"
      , "wrong", "mesh", "mushroom", "diet", "feature"
      ]
    , [ "enjoy", "brave", "away", "fold", "denial"
      , "unique", "garage", "blouse", "shuffle", "across"
      , "core", "rich", "cash", "day", "large"
      ]
    , [ "clean", "riot", "orbit", "scheme", "supreme"
      , "copy", "farm", "fetch", "filter", "saddle"
      , "grain", "destroy", "pyramid", "false", "jewel"
      ]
    , [ "flavor", "any", "wish", "cry", "lion"
      , "asset", "easily", "tired", "brass", "language"
      , "multiply", "obvious", "cradle", "disorder", "green"
      ]
    , [ "mobile", "boost", "husband", "between", "open"
      , "illegal", "kitten", "evil", "gallery", "sheriff"
      , "excess", "october", "hope", "example", "artwork"
      ]
    , [ "nothing", "garlic", "length", "vacant", "beyond"
      , "eagle", "odor", "verify", "fire", "ignore"
      , "woman", "march", "plastic", "smart", "exact"
      ]
    , [ "dial", "athlete", "script", "fee", "reduce"
      , "identify", "deer", "grab", "raw", "patrol"
      , "cheese", "stock", "prepare", "wolf", "urban"
      ]
    , [ "cross", "shed", "mountain", "okay", "copper"
      , "long", "bus", "offer", "dawn", "decide"
      , "maze", "swing", "basket", "wine", "change"
      ]
    , [ "bachelor", "sniff", "mixed", "chunk", "convince"
      , "base", "agent", "pretty", "proud", "name"
      , "mind", "magnet", "swap", "rookie", "moon"
      ]
    , [ "lazy", "gorilla", "famous", "lunch", "summer"
      , "share", "sketch", "width", "section", "bundle"
      , "problem", "expect", "pulp", "vintage", "tray"
      ]
    , [ "just", "symbol", "fragile", "saddle", "easy"
      , "proud", "imitate", "system", "comic", "avocado"
      , "trash", "ketchup", "hen", "idea", "solve"
      ]
    , [ "midnight", "light", "axis", "green", "frog"
      , "catch", "dice", "small", "knife", "lunch"
      , "tennis", "love", "path", "happy", "squirrel"
      ]
    , [ "roast", "hint", "fresh", "fork", "floor"
      , "afford", "deputy", "negative", "armor", "evidence"
      , "ice", "arena", "flock", "moral", "relief"
      ]
    , [ "obey", "wage", "truly", "weird", "sense"
      , "mimic", "expect", "ten", "random", "engine"
      , "creek", "ivory", "example", "mixed", "pigeon"
      ]
    , [ "usual", "purity", "order", "make", "diamond"
      , "jealous", "gap", "illness", "cliff", "wonder"
      , "nature", "normal", "high", "hood", "balcony"
      ]
    , [ "brush", "busy", "steel", "pride", "vendor"
      , "hurt", "lava", "salute", "season", "unknown"
      , "announce", "area", "begin", "fashion", "section"
      ]
    , [ "shadow", "online", "parrot", "rough", "among"
      , "decide", "spare", "cupboard", "actor", "pumpkin"
      , "caught", "fit", "planet", "bleak", "trick"
      ]
    , [ "gym", "ability", "silent", "pipe", "tragic"
      , "slice", "poet", "stairs", "swarm", "party"
      , "cruise", "waste", "prefer", "trash", "boy"
      ]
    , [ "leader", "bitter", "era", "crawl", "tiger"
      , "destroy", "sword", "enrich", "angry", "pull"
      , "kitchen", "hold", "sea", "sock", "giraffe"
      ]
    , [ "defense", "brush", "fiscal", "cactus", "rotate"
      , "trouble", "mean", "quantum", "shrug", "slight"
      , "dignity", "corn", "immense", "first", "citizen"
      ]
    , [ "wedding", "size", "surprise", "split", "circle"
      , "angry", "silver", "flame", "usage", "light"
      , "stock", "innocent", "novel", "modify", "mushroom"
      ]
    , [ "multiply", "affair", "bargain", "response", "shop"
      , "behave", "name", "box", "piano", "isolate"
      , "play", "perfect", "shoe", "often", "depart"
      ]
    , [ "like", "hedgehog", "theme", "letter", "first"
      , "output", "special", "that", "boost", "pupil"
      , "coil", "indicate", "arctic", "swing", "bonus"
      ]
    , [ "bus", "enlist", "leaf", "spider", "fun"
      , "joke", "step", "main", "abstract", "frequent"
      , "flash", "erosion", "forward", "infant", "whisper"
      ]
    , [ "cheese", "volume", "image", "misery", "dragon"
      , "border", "garage", "occur", "minute", "zero"
      , "forget", "outer", "sport", "salt", "same"
      ]
    , [ "access", "prison", "immense", "olympic", "fall"
      , "manual", "soccer", "nasty", "object", "attract"
      , "tail", "decade", "index", "play", "risk"
      ]
    , [ "top", "fashion", "salt", "gown", "dilemma"
      , "price", "permit", "isolate", "hedgehog", "december"
      , "attack", "identify", "august", "naive", "effort"
      ]
    , [ "ticket", "pudding", "crane", "kangaroo", "nice"
      , "security", "patient", "arrest", "pass", "motion"
      , "bring", "cabin", "visual", "hospital", "half"
      ]
    , [ "isolate", "base", "oak", "bronze", "wish"
      , "alarm", "height", "olive", "clog", "balcony"
      , "rhythm", "spell", "refuse", "various", "fire"
      ]
    , [ "flag", "super", "pet", "impact", "impose"
      , "anger", "cook", "verb", "laundry", "embrace"
      , "uncover", "mercy", "orbit", "fall", "cycle"
      ]
    , [ "occur", "surprise", "world", "boy", "mouse"
      , "pilot", "sibling", "float", "clump", "matrix"
      , "field", "sauce", "umbrella", "exchange", "sponsor"
      ]
    , [ "announce", "employ", "holiday", "easy", "van"
      , "risk", "cause", "exist", "absorb", "object"
      , "bus", "rigid", "deny", "slot", "ginger"
      ]
    , [ "maximum", "annual", "target", "vague", "patch"
      , "humble", "canvas", "bone", "robust", "try"
      , "puzzle", "clerk", "lunar", "theory", "black"
      ]
    , [ "soldier", "seat", "waste", "symptom", "token"
      , "fiber", "fury", "wear", "nut", "wood"
      , "tackle", "clog", "will", "dynamic", "depend"
      ]
    , [ "large", "convince", "pear", "tube", "view"
      , "rely", "prepare", "joy", "gadget", "mail"
      , "chaos", "zebra", "reject", "example", "taste"
      ]
    , [ "detect", "glare", "pass", "virtual", "rigid"
      , "someone", "transfer", "proud", "feel", "melt"
      , "fever", "travel", "uniform", "lemon", "crop"
      ]
    , [ "join", "fever", "gossip", "someone", "state"
      , "wheel", "galaxy", "season", "action", "patient"
      , "install", "client", "chapter", "ethics", "lunar"
      ]
    , [ "deer", "mushroom", "law", "below", "mimic"
      , "miracle", "tobacco", "frost", "response", "ivory"
      , "captain", "moment", "digital", "car", "wide"
      ]
    , [ "sugar", "maximum", "custom", "entire", "minor"
      , "act", "real", "fire", "balance", "that"
      , "slow", "shuffle", "angry", "gentle", "tattoo"
      ]
    , [ "dad", "citizen", "merge", "bunker", "organ"
      , "chicken", "stable", "tiger", "judge", "also"
      , "marble", "corn", "tuna", "stay", "slush"
      ]
    , [ "globe", "air", "indicate", "dry", "latin"
      , "gospel", "book", "grit", "wrap", "toward"
      , "begin", "pretty", "fade", "adjust", "drill"
      ]
    , [ "among", "garbage", "survey", "unfair", "between"
      , "advice", "dismiss", "tree", "buddy", "climb"
      , "early", "venture", "later", "mule", "season"
      ]
    , [ "squeeze", "expire", "meat", "mixture", "whisper"
      , "retreat", "siege", "beef", "absent", "double"
      , "rotate", "citizen", "neither", "stereo", "accuse"
      ]
    , [ "improve", "make", "wrong", "tiger", "ten"
      , "panther", "duty", "ring", "pull", "exotic"
      , "milk", "chimney", "source", "present", "panther"
      ]
    , [ "busy", "devote", "dirt", "timber", "tumble"
      , "away", "famous", "spatial", "economy", "hub"
      , "near", "spike", "sock", "fee", "head"
      ]
    , [ "cost", "giant", "matter", "divide", "yard"
      , "pluck", "distance", "once", "life", "wool"
      , "ritual", "stage", "banner", "notable", "deposit"
      ]
    , [ "notable", "drill", "dust", "jump", "task"
      , "immense", "very", "tide", "humor", "north"
      , "cream", "behind", "upgrade", "gaze", "about"
      ]
    , [ "relax", "faculty", "bundle", "replace", "mercy"
      , "find", "walk", "remove", "clinic", "glove"
      , "session", "truly", "guess", "range", "skirt"
      ]
    , [ "wheel", "unlock", "spice", "monster", "swarm"
      , "lion", "parrot", "pause", "figure", "rude"
      , "jewel", "borrow", "law", "curve", "sport"
      ]
    , [ "merry", "battle", "blind", "analyst", "milk"
      , "owner", "business", "decide", "glue", "wagon"
      , "perfect", "expire", "razor", "list", "catalog"
      ]
    , [ "dust", "inner", "time", "daring", "donate"
      , "script", "small", "race", "chase", "crawl"
      , "asthma", "captain", "hawk", "subject", "culture"
      ]
    , [ "grass", "sail", "visit", "merry", "raven"
      , "fault", "soda", "isolate", "echo", "tortoise"
      , "pride", "game", "person", "project", "apple"
      ]
    , [ "idle", "absent", "exile", "youth", "magic"
      , "reopen", "tilt", "panther", "human", "citizen"
      , "bubble", "solution", "amused", "gauge", "piece"
      ]
    , [ "cost", "link", "fatal", "puppy", "direct"
      , "under", "fitness", "wrestle", "egg", "token"
      , "yard", "later", "net", "swap", "day"
      ]
    , [ "grain", "left", "kitchen", "attend", "merry"
      , "slim", "wait", "sudden", "gas", "close"
      , "drink", "deputy", "family", "crash", "virus"
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
            sourceId = fragmentId $ putFragment
                (Hash $ BS.replicate 32 0)
                []
                [TxOut addr (Coin 100000000000)]
                (MkFragmentSimpleTransaction TxWitnessUTxO)
        in
            TxIn sourceId 0

-- | Generate faucets addresses and mnemonics, printing everything to stdout
--
-- >>> genFaucets 1
genFaucets :: FilePath -> Int -> IO ()
genFaucets file n = do
    ms <- replicateM n (entropyToMnemonic <$> genEntropy)
    TIO.writeFile file ""
    forM_ [ (m, take 10 (addresses m)) | m <- ms ] $ \(m, addrs) -> do
        appendFile $ T.intercalate ", " $ surroundedBy '"' <$> mnemonicToText @15 m
        forM_ addrs (appendFile . encodeAddress @'Mainnet)
  where
    surroundedBy :: Char -> Text -> Text
    surroundedBy c txt = T.singleton c <> txt <> T.singleton c

    appendFile :: Text -> IO ()
    appendFile txt = TIO.appendFile file (txt <> "\n")

    addresses :: KnownNat n => Mnemonic n -> [Address]
    addresses mw =
        let
            (seed, pwd) =
                (SomeMnemonic mw, mempty)
            rootXPrv =
                generateKeyFromSeed seed pwd
            accXPrv =
                deriveAccountPrivateKey pwd rootXPrv minBound
            addrXPrv =
                deriveAddressPrivateKey pwd accXPrv UTxOExternal
        in
            [ paymentAddress @'Mainnet (publicKey $ addrXPrv ix)
            | ix <- [minBound..maxBound]
            ]
