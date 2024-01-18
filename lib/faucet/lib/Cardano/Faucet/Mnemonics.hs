{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.Faucet.Mnemonics
    ( random
    , mir
    , hardwareLedger
    , MnemonicLength (..)
    , generate
    , generateSome
    , unsafeMnemonic
    ) where

import Prelude

import Cardano.Mnemonic
    ( ConsistentEntropy
    , EntropySize
    , Mnemonic
    , MnemonicWords
    , SomeMnemonic (..)
    , entropyToMnemonic
    , genEntropy
    , mkMnemonic
    )
import Control.Monad.IO.Class
    ( MonadIO
    , liftIO
    )
import Data.Text
    ( Text
    )
import GHC.Generics
    ( Generic
    )
import GHC.Stack
    ( HasCallStack
    )

random :: [Mnemonic 12]
random =
    unsafeMnemonic
        <$> [ ["arctic", "decade", "pink", "easy", "jar", "index", "base", "bright", "vast", "ocean", "hard", "pizza"]
            , ["finish", "evoke", "alone", "town", "express", "wide", "pair", "story", "west", "safe", "news", "wrap"]
            , ["fox", "now", "hello", "inmate", "era", "jealous", "cruel", "wreck", "dash", "supply", "book", "attend"]
            , ["must", "lock", "cereal", "water", "silver", "cake", "circle", "express", "sock", "arm", "chapter", "avoid"]
            , ["give", "verb", "balcony", "hurdle", "pistol", "flee", "manage", "barely", "pulse", "episode", "speak", "school"]
            , ["divert", "entire", "urge", "banner", "repair", "mechanic", "muffin", "illness", "genre", "intact", "coin", "boss"]
            , ["pink", "radio", "various", "frame", "argue", "draft", "sun", "speak", "club", "salute", "thank", "price"]
            , ["all", "beef", "link", "funny", "swing", "duck", "sweet", "swallow", "slow", "shield", "weekend", "open"]
            , ["green", "friend", "captain", "entry", "utility", "lake", "blur", "matrix", "will", "prefer", "breeze", "shed"]
            , ["reveal", "jazz", "equal", "salmon", "first", "decline", "liquid", "wolf", "powder", "account", "elbow", "figure"]
            , ["olympic", "uncover", "stone", "tiger", "oppose", "icon", "property", "heart", "mean", "interest", "account", "head"]
            , ["poverty", "hungry", "depart", "shift", "proud", "wrap", "voice", "throw", "spoon", "this", "system", "flee"]
            , ["tattoo", "crop", "genuine", "impact", "govern", "banana", "hope", "bamboo", "junior", "pride", "best", "skirt"]
            , ["model", "hundred", "exact", "control", "random", "cross", "burst", "fame", "ladder", "bleak", "car", "virus"]
            , ["ripple", "lazy", "void", "zoo", "social", "plunge", "badge", "jungle", "similar", "draft", "lawn", "execute"]
            , ["guide", "penalty", "erupt", "plate", "benefit", "moon", "motion", "sing", "envelope", "range", "midnight", "spell"]
            , ["bulb", "normal", "curious", "leg", "essence", "chronic", "envelope", "cannon", "comfort", "spare", "private", "uniform"]
            , ["tongue", "cabin", "enact", "square", "feature", "prevent", "journey", "pigeon", "valid", "unable", "drum", "opera"]
            , ["assist", "pact", "vessel", "spot", "fine", "fine", "crouch", "body", "gown", "allow", "hair", "universe"]
            , ["tape", "glue", "rate", "squirrel", "jeans", "canoe", "bicycle", "sausage", "lunar", "pair", "fit", "ice"]
            , ["chronic", "soda", "history", "famous", "owner", "print", "student", "wool", "pulse", "sound", "melt", "gate"]
            , ["exist", "arrest", "north", "tunnel", "height", "style", "announce", "real", "uncover", "sphere", "sorry", "sudden"]
            , ["celery", "slim", "stone", "hand", "inmate", "enrich", "stem", "ice", "glass", "fault", "pig", "island"]
            , ["ancient", "update", "number", "oil", "degree", "virtual", "stairs", "reunion", "question", "toilet", "disagree", "deliver"]
            , ["surge", "inherit", "gown", "witness", "true", "fame", "couch", "artwork", "orchard", "tunnel", "toss", "mom"]
            , ["oblige", "room", "table", "auto", "build", "very", "street", "margin", "faculty", "purpose", "shoe", "prison"]
            , ["theory", "afraid", "tell", "depth", "issue", "cover", "pass", "vacant", "poet", "fury", "fortune", "cruise"]
            , ["clay", "mix", "capable", "student", "scissors", "ugly", "prefer", "change", "adjust", "push", "cake", "harsh"]
            , ["shift", "sunny", "brick", "supreme", "tank", "duck", "garment", "feature", "cloud", "canyon", "harbor", "nut"]
            , ["delay", "exhibit", "social", "wood", "plate", "donate", "differ", "knock", "dignity", "sport", "cost", "visual"]
            , ["banner", "expand", "fringe", "kiss", "laugh", "muffin", "maximum", "program", "hurdle", "gorilla", "spray", "prepare"]
            , ["together", "sorry", "amazing", "loyal", "civil", "rely", "success", "range", "adult", "truly", "trade", "tip"]
            , ["secret", "like", "type", "honey", "average", "sword", "rookie", "mass", "blade", "myth", "double", "salmon"]
            , ["buddy", "assault", "armed", "whale", "bid", "unfair", "zone", "minimum", "fat", "employ", "front", "lizard"]
            , ["verb", "blossom", "kiwi", "butter", "express", "other", "shoulder", "hold", "enter", "beyond", "special", "devote"]
            , ["exhibit", "install", "act", "craft", "grain", "soap", "coral", "jaguar", "echo", "midnight", "ride", "raise"]
            , ["credit", "raw", "dinosaur", "target", "sustain", "permit", "regret", "strong", "abandon", "guard", "expand", "science"]
            , ["timber", "grid", "cement", "resemble", "engage", "sugar", "february", "regular", "print", "timber", "produce", "pizza"]
            , ["solution", "dice", "symbol", "ignore", "gauge", "exist", "also", "mention", "west", "pet", "rule", "first"]
            , ["tuition", "cost", "tattoo", "vicious", "vast", "doctor", "prevent", "asthma", "barely", "orphan", "close", "bus"]
            , ["puppy", "crew", "glide", "feature", "bottom", "stumble", "prefer", "hidden", "extra", "north", "bleak", "shoulder"]
            , ["innocent", "unfold", "combine", "gas", "custom", "luggage", "cricket", "thing", "speak", "bubble", "pitch", "festival"]
            , ["gospel", "garlic", "midnight", "enemy", "legal", "speed", "sleep", "discover", "enlist", "camp", "metal", "chunk"]
            , ["lyrics", "lend", "volume", "cruise", "engage", "relief", "memory", "wine", "board", "scorpion", "educate", "differ"]
            , ["law", "same", "wrist", "cotton", "outer", "debris", "put", "other", "wife", "father", "collect", "chef"]
            , ["february", "expand", "decline", "sort", "pull", "silk", "average", "update", "spatial", "betray", "remind", "hero"]
            , ["security", "hill", "flight", "improve", "rotate", "language", "home", "carbon", "boil", "enhance", "pulse", "pill"]
            , ["inside", "fancy", "sea", "blouse", "estate", "chest", "early", "office", "woman", "license", "obey", "helmet"]
            , ["course", "toe", "sentence", "defense", "because", "trip", "hockey", "abandon", "essay", "give", "deputy", "insect"]
            , ["sister", "slogan", "hour", "build", "squeeze", "favorite", "inject", "smart", "slim", "near", "tired", "blind"]
            , ["upper", "mouse", "spray", "wrong", "food", "affair", "before", "object", "mention", "then", "ask", "solution"]
            , ["video", "fall", "run", "engine", "wheat", "baby", "december", "issue", "vehicle", "between", "reopen", "wink"]
            , ["nuclear", "glide", "invest", "speed", "essence", "friend", "clog", "hamster", "service", "crisp", "weasel", "pigeon"]
            , ["stumble", "either", "orbit", "bundle", "pepper", "total", "radio", "spatial", "umbrella", "explain", "exercise", "science"]
            , ["slam", "entry", "nation", "frog", "advice", "process", "cycle", "lawsuit", "scrub", "strategy", "shrimp", "push"]
            , ["ecology", "female", "item", "crime", "remember", "denial", "swallow", "forward", "call", "vehicle", "glue", "hello"]
            , ["spin", "dinosaur", "honey", "abuse", "exit", "coffee", "ethics", "denial", "proof", "hour", "number", "annual"]
            , ["power", "age", "slush", "tube", "island", "void", "old", "option", "lobster", "vendor", "typical", "cushion"]
            , ["drill", "orphan", "hero", "throw", "stand", "ecology", "hat", "gauge", "antique", "hotel", "pistol", "rice"]
            , ["present", "trophy", "digital", "salad", "kick", "apart", "airport", "stuff", "prosper", "peace", "drive", "adjust"]
            , ["fluid", "brave", "disease", "rough", "surge", "city", "ignore", "speed", "borrow", "print", "pause", "smile"]
            , ["begin", "decorate", "smart", "mesh", "cannon", "gas", "toe", "model", "vacant", "survey", "victory", "cat"]
            , ["liberty", "sunny", "impact", "source", "foil", "arrive", "inch", "find", "obtain", "wet", "uncover", "huge"]
            , ["own", "pilot", "advance", "stock", "pizza", "over", "february", "cheese", "invite", "hello", "tell", "distance"]
            , ["alert", "satoshi", "two", "limit", "bag", "soldier", "hair", "scatter", "zebra", "rural", "dizzy", "cry"]
            , ["phone", "food", "they", "nose", "cross", "music", "core", "leisure", "menu", "curve", "bike", "rate"]
            , ["truly", "wagon", "soup", "submit", "tail", "first", "push", "split", "concert", "work", "source", "cart"]
            , ["symbol", "stage", "umbrella", "high", "sand", "tilt", "slight", "open", "kitten", "oil", "fade", "minor"]
            , ["tumble", "grit", "dumb", "game", "raccoon", "giggle", "valley", "audit", "army", "mandate", "around", "basket"]
            , ["owner", "foil", "vivid", "cloth", "bright", "hurry", "nerve", "help", "sister", "jaguar", "teach", "loyal"]
            , ["slender", "topple", "urban", "axis", "swamp", "guess", "dizzy", "correct", "visit", "valve", "ivory", "citizen"]
            , ["humble", "song", "wrap", "future", "cinnamon", "accuse", "bright", "speed", "inhale", "alien", "theory", "main"]
            , ["purity", "latin", "danger", "dutch", "avocado", "endless", "off", "scissors", "junk", "biology", "dial", "glue"]
            , ["lazy", "aunt", "obvious", "pave", "abuse", "loan", "coral", "orchard", "fat", "tone", "knock", "tired"]
            , ["fantasy", "kit", "luxury", "combine", "bus", "hospital", "hybrid", "stool", "cousin", "gauge", "grid", "audit"]
            , ["dentist", "inmate", "sun", "town", "fame", "cable", "sport", "depth", "scissors", "rude", "yard", "harbor"]
            , ["bright", "item", "flame", "august", "consider", "rifle", "stereo", "end", "very", "bright", "matrix", "mom"]
            , ["today", "pattern", "bacon", "version", "differ", "pony", "universe", "snack", "weird", "toddler", "belt", "door"]
            , ["veteran", "omit", "knife", "wrist", "truth", "agree", "rhythm", "world", "dynamic", "duty", "saddle", "dove"]
            , ["hat", "city", "disease", "patrol", "answer", "select", "vibrant", "tag", "dose", "rebuild", "length", "sting"]
            , ["liberty", "lens", "entry", "marriage", "bean", "camp", "phone", "charge", "alcohol", "boil", "plate", "banner"]
            , ["talk", "glory", "minute", "include", "flag", "stuff", "laugh", "auction", "benefit", "escape", "confirm", "task"]
            , ["joy", "convince", "reunion", "increase", "core", "venue", "palm", "scan", "wish", "vault", "until", "rice"]
            , ["walk", "hybrid", "game", "vanish", "mushroom", "win", "observe", "crush", "core", "lamp", "mirror", "twenty"]
            , ["hold", "joy", "grit", "great", "quote", "retreat", "famous", "wreck", "busy", "faint", "wish", "fetch"]
            , ["future", "obscure", "glow", "valid", "wear", "boy", "exercise", "member", "shoe", "add", "country", "spatial"]
            , ["tooth", "option", "satisfy", "patrol", "amateur", "height", "above", "air", "struggle", "reform", "speed", "mom"]
            , ["word", "cruel", "plate", "hedgehog", "flavor", "judge", "device", "tuna", "amateur", "walk", "open", "reduce"]
            , ["right", "energy", "oxygen", "eager", "more", "direct", "yard", "easy", "luxury", "auto", "knife", "loop"]
            , ["huge", "race", "host", "involve", "win", "interest", "salad", "box", "fatal", "cherry", "cage", "pioneer"]
            , ["phrase", "rapid", "fine", "neglect", "already", "nut", "note", "chair", "mushroom", "rack", "ivory", "riot"]
            , ["ivory", "citizen", "rule", "scare", "angle", "method", "bounce", "caution", "noble", "pottery", "plunge", "resource"]
            , ["behave", "attitude", "glide", "else", "have", "moon", "settle", "minute", "provide", "trade", "negative", "nothing"]
            , ["diary", "chunk", "total", "cruise", "they", "curious", "foil", "actress", "wish", "universe", "grape", "kind"]
            , ["mushroom", "print", "dish", "slim", "agent", "tube", "expand", "actor", "layer", "idea", "example", "quarter"]
            , ["riot", "sport", "access", "grid", "destroy", "chronic", "evil", "doll", "sibling", "blanket", "seed", "goose"]
            , ["pyramid", "song", "photo", "filter", "subway", "rich", "broken", "anchor", "blur", "lecture", "liar", "hope"]
            , ["sort", "crouch", "seven", "exile", "extend", "evoke", "summer", "oppose", "fork", "result", "plate", "goat"]
            , ["safe", "wrap", "order", "affair", "fiber", "walnut", "skill", "timber", "rookie", "ghost", "spot", "napkin"]
            , ["jaguar", "bitter", "merry", "destroy", "frozen", "dune", "embody", "pull", "cradle", "peasant", "sail", "whisper"]
            ]

mir :: [Mnemonic 24]
mir =
    unsafeMnemonic
        <$> [ ["ketchup", "embody", "define", "thing", "few", "tornado", "worry", "few", "wisdom", "people", "sure", "bean", "ring", "impact", "clerk", "mirror", "antenna", "truly", "chief", "truth", "sign", "drip", "sorry", "flush"]
            , ["obscure", "protect", "still", "woman", "rescue", "plunge", "lemon", "warm", "cash", "quote", "wood", "adapt", "erase", "muffin", "blush", "diet", "noodle", "biology", "scrap", "involve", "radar", "filter", "oval", "filter"]
            , ["bird", "toilet", "maid", "mule", "mercy", "album", "powder", "misery", "ozone", "fragile", "concert", "media", "inhale", "lonely", "height", "box", "enforce", "mesh", "budget", "arch", "top", "tenant", "spoil", "drop"]
            , ["gadget", "rate", "fame", "nothing", "onion", "surround", "loan", "panel", "moment", "used", "fruit", "jacket", "pretty", "replace", "pig", "stairs", "guard", "slab", "shadow", "child", "over", "win", "focus", "glue"]
            , ["amount", "become", "cousin", "degree", "practice", "garbage", "fall", "witness", "mushroom", "update", "this", "define", "exile", "fame", "paper", "symptom", "ride", "oil", "plate", "park", "broom", "fine", "six", "coast"]
            , ["nasty", "abstract", "scale", "idle", "benefit", "staff", "normal", "auto", "anchor", "balance", "measure", "action", "crucial", "virtual", "lobster", "wave", "caution", "text", "obey", "enact", "only", "nature", "illness", "gain"]
            , ["beyond", "rare", "pulse", "setup", "story", "side", "envelope", "illness", "warm", "doll", "snake", "turtle", "oak", "host", "horse", "where", "rate", "quantum", "notice", "allow", "monkey", "shallow", "police", "code"]
            , ["brief", "asset", "spell", "behave", "real", "galaxy", "dad", "solar", "animal", "wisdom", "imitate", "arch", "abuse", "parade", "loud", "mention", "volcano", "fall", "awake", "course", "solution", "super", "guitar", "rebel"]
            , ["onion", "secret", "sphere", "horror", "hint", "engine", "denial", "six", "omit", "shove", "quit", "sibling", "code", "shallow", "square", "athlete", "dog", "bleak", "cost", "axis", "alone", "nut", "frozen", "stumble"]
            , ["about", "magnet", "nut", "edit", "awake", "matrix", "bamboo", "casual", "diamond", "joke", "man", "crumble", "staff", "ten", "potato", "laptop", "off", "action", "chuckle", "medal", "bread", "blind", "peanut", "horse"]
            , ["version", "reason", "distance", "cargo", "fancy", "anxiety", "renew", "grace", "jealous", "brother", "live", "wheel", "lava", "exercise", "tragic", "foster", "office", "govern", "title", "inquiry", "fit", "twist", "powder", "subway"]
            , ["dentist", "diagram", "eternal", "tuition", "leave", "library", "coffee", "power", "brief", "syrup", "six", "donkey", "inner", "valley", "carpet", "drop", "labor", "observe", "decade", "okay", "play", "stable", "wagon", "blind"]
            , ["gallery", "approve", "trophy", "side", "lawn", "soldier", "gentle", "wire", "enact", "illegal", "chef", "sentence", "nation", "beach", "glimpse", "term", "unlock", "chalk", "monitor", "panel", "famous", "alert", "matter", "female"]
            , ["reason", "grow", "memory", "spray", "gossip", "middle", "grocery", "lesson", "poem", "cannon", "dilemma", "elegant", "point", "east", "evil", "sauce", "exile", "typical", "cram", "ride", "remove", "phrase", "lecture", "degree"]
            , ["else", "normal", "rotate", "flash", "nose", "east", "weasel", "hammer", "priority", "pig", "seven", "mention", "model", "profit", "oxygen", "tomato", "foot", "age", "glad", "jazz", "retire", "okay", "village", "crater"]
            , ["negative", "purpose", "outdoor", "slush", "beach", "radar", "canoe", "course", "donkey", "earn", "bone", "bar", "frost", "manual", "inhale", "humor", "this", "reflect", "learn", "special", "horse", "course", "start", "debris"]
            , ["wealth", "float", "steak", "oil", "rare", "gift", "put", "stool", "vault", "give", "gorilla", "indicate", "inside", "comfort", "lawn", "assault", "urban", "ancient", "identify", "depth", "injury", "solution", "warrior", "exercise"]
            , ["syrup", "shield", "chef", "child", "dwarf", "frog", "hire", "script", "suit", "jelly", "point", "degree", "brisk", "oak", "minute", "absurd", "refuse", "iron", "forum", "effort", "regret", "kidney", "drama", "still"]
            , ["moral", "stem", "myth", "awesome", "crime", "slush", "try", "wood", "coconut", "erase", "patient", "trigger", "crew", "solve", "element", "million", "nasty", "raven", "innocent", "happy", "behind", "ankle", "trick", "museum"]
            , ["wish", "peasant", "void", "nature", "position", "dial", "grant", "recycle", "raw", "melody", "equal", "stool", "parent", "category", "limb", "apart", "indoor", "six", "float", "happy", "insane", "guide", "burst", "other"]
            , ["fury", "possible", "relax", "eyebrow", "supply", "embrace", "decide", "wolf", "boring", "blossom", "credit", "drill", "theme", "skate", "focus", "trick", "field", "wrist", "update", "hawk", "renew", "motor", "learn", "cook"]
            , ["gas", "woman", "grief", "story", "evidence", "actor", "filter", "lion", "pilot", "illness", "abuse", "palm", "hurry", "mail", "equal", "pen", "element", "nut", "lobster", "enemy", "base", "steel", "aisle", "lamp"]
            ]

hardwareLedger :: [SomeMnemonic]
hardwareLedger =
    [ SomeMnemonic hwLedgerMnemonic12
    , SomeMnemonic hwLedgerMnemonic18
    , SomeMnemonic hwLedgerMnemonic24
    ]
  where
    hwLedgerMnemonic12 :: Mnemonic 12
    hwLedgerMnemonic12 = unsafeMnemonic
        [ "struggle", "section", "scissors", "siren", "garbage", "yellow"
        , "maximum", "finger", "duty", "require", "mule", "earn"
        ]

    hwLedgerMnemonic18 :: Mnemonic 18
    hwLedgerMnemonic18 = unsafeMnemonic
        [ "vague", "wrist", "poet", "crazy", "danger", "dinner", "grace"
        , "home", "naive", "unfold", "april", "exile", "relief", "rifle"
        , "ranch", "tone", "betray", "wrong"
        ]

    hwLedgerMnemonic24 :: Mnemonic 24
    hwLedgerMnemonic24 = unsafeMnemonic
        [ "recall", "grace", "sport", "punch", "exhibit", "mad", "harbor"
        , "stand", "obey", "short", "width", "stem", "awkward", "used"
        , "stairs", "wool", "ugly", "trap", "season", "stove", "worth"
        , "toward", "congress", "jaguar"
        ]

-- | Build 'Mnemonic' from literals
unsafeMnemonic
    :: forall mw n csz
    .  ( ConsistentEntropy n mw csz
       , EntropySize mw ~ n
       , HasCallStack
       )
    => [Text]
    -> Mnemonic mw
unsafeMnemonic m = case mkMnemonic m of
    Left e -> error $ "unsafeMnemonic: " <> show e
    Right a -> a

data MnemonicLength = M9 | M12 | M15 | M18 | M21 | M24
    deriving stock (Eq, Ord, Enum, Bounded, Show, Generic)

generateSome :: MonadIO m => MnemonicLength -> m SomeMnemonic
generateSome = \case
    M9  -> SomeMnemonic <$> generate @9
    M12 -> SomeMnemonic <$> generate @12
    M15 -> SomeMnemonic <$> generate @15
    M18 -> SomeMnemonic <$> generate @18
    M21 -> SomeMnemonic <$> generate @21
    M24 -> SomeMnemonic <$> generate @24

generate :: forall mw ent csz m.
    ( ConsistentEntropy ent mw csz
    , ent ~ EntropySize mw
    , mw ~ MnemonicWords ent
    , MonadIO m
    )
    => m (Mnemonic mw)
generate = liftIO $ entropyToMnemonic @mw <$> genEntropy
