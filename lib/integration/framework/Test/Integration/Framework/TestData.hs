{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}

module Test.Integration.Framework.TestData
    ( -- * Mnemonics
      chineseMnemonics9
    , chineseMnemonics18
    , frenchMnemonics12
    , frenchMnemonics21
    , invalidMnemonics12
    , invalidMnemonics15
    , japaneseMnemonics12
    , japaneseMnemonics15
    , notInDictMnemonics15
    , specMnemonicByron
    , specMnemonicSentence
    , specMnemonicSecondFactor

    -- * Wallets
    , arabicWalletName
    , falseWalletIds
    , kanjiWalletName
    , polishWalletName
    , russianWalletName
    , wildcardsWalletName

    -- * Addresses
    , invalidByronBase58

    -- * Assets
    , steveToken

    -- * Helpers
    , cmdOk
    , versionLine
    , payloadWith
    , payloadWith'
    , simplePayload
    , updateNamePayload
    , updatePassPayload
    , updatePassPayloadMnemonic
    , updatePassPayloadMnemonicAndSndFactor
    , updateEmptyPassPayload
    , txMetadata_ADP_1005

    -- * Error messages
    , errMsg400WalletIdEncoding
    , errMsg403NotAByronWallet
    , errMsg403NotAnIcarusWallet
    , errMsg403WrongPass
    , errMsg403WrongMnemonic
    , errMsg403AlreadyInLedger
    , errMsg403PoolAlreadyJoined
    , errMsg403NotDelegating
    , errMsg403NothingToMigrate
    , errMsg404NoEndpoint
    , errMsg404CannotFindTx
    , errMsg403NoRootKey
    , errMsg404NoWallet
    , errMsg409WalletExists
    , errMsg403TxTooBig
    , errMsg400MalformedTxPayload
    , errMsg400TxMetadataStringTooLong
    , errMsg400WronglyEncodedTxPayload
    , errMsg400ParseError
    , errMsg403ZeroAmtOutput
    , errMsg405
    , errMsg406
    , errMsg415
    , errMsg415OctetStream
    , errMsg500
    , errMsg400NumberOfWords
    , errMsgNotInDictionary
    , errMsg403WithdrawalNotBeneficial
    , errMsg403CouldntIdentifyAddrAsMine
    , errMsg503PastHorizon
    , errMsg403KeyAlreadyPresent
    , errMsg403CreateIllegal
    , errMsg400ScriptWrongCoeffcient
    , errMsg400ScriptIllFormed
    , errMsg400ScriptDuplicateKeys
    , errMsg400ScriptTimelocksContradictory
    , errMsg400ScriptNotUniformRoles
    , errMsg403TemplateInvalidNoCosignerInScript
    , errMsg403TemplateInvalidUnknownCosigner
    , errMsg403TemplateInvalidDuplicateXPub
    , errMsg403TemplateInvalidScript
    ) where

import Prelude

import Cardano.Mnemonic
    ( SomeMnemonic
    )
import Cardano.Mnemonic.Extended
    ( someMnemonicToWords
    )
import Cardano.Wallet.Api.Types
    ( ApiAssetMetadata (ApiAssetMetadata)
    , ApiT (..)
    )
import Cardano.Wallet.Application.Version
    ( gitRevision
    , showFullVersion
    , version
    )
import Cardano.Wallet.Primitive.Types.Tx
    ( TxMetadata (..)
    , TxMetadataValue (..)
    )
import Cardano.Wallet.Unsafe
    ( unsafeFromText
    )
import Data.Text
    ( Text
    , pack
    , unpack
    )
import Data.Word
    ( Word32
    )
import Test.Integration.Framework.DSL
    ( Payload (..)
    , fixturePassphrase
    , json
    )

import qualified Cardano.Wallet.Primitive.Types.TokenMetadata as W
import qualified Data.Map as Map

falseWalletIds :: [(String, String)]
falseWalletIds =
        [ ("40 chars hex", replicate 40 '1')
        , ("40 chars non-hex", replicate 40 'ś')
        , ("39 chars hex", replicate 39 '1')
        , ("41 chars hex", replicate 41 '1')
        ]

invalidMnemonics12 :: [Text]
invalidMnemonics12 = ["word","word","word","word","word","word","word",
        "word","word","word","word","hill"]

invalidMnemonics15 :: [Text]
invalidMnemonics15 = ["word","word","word","word","word","word","word",
    "word","word","word","word","word","word","word","word"]

notInDictMnemonics15 :: [Text]
notInDictMnemonics15 = ["one", "two", "three", "four", "five", "six", "seven",
    "eight", "nine", "diary", "twenty", "coin", "regret", "cry", "thumb"]

specMnemonicSentence :: [Text]
specMnemonicSentence = ["squirrel", "material", "silly", "twice", "direct",
    "slush", "pistol", "razor", "become", "junk", "kingdom", "flee",
    "squirrel", "silly", "twice"]

specMnemonicByron :: [Text]
specMnemonicByron = [ "squirrel", "material", "silly", "twice", "direct", "slush",
    "pistol", "razor", "become", "junk", "kingdom", "junk"]

specMnemonicSecondFactor :: [Text]
specMnemonicSecondFactor = ["squirrel", "material", "silly", "twice",
    "direct", "slush", "pistol", "razor", "become"]

japaneseMnemonics12 :: [Text]
japaneseMnemonics12 = ["そうだん",　"ひよう",　"にもつ",　"やさしい",　"きふく",
    "ねつい",　"だったい",　"けんてい",　"けいろ",　"ざつがく",　"ほうもん",　"すこし"]

japaneseMnemonics15 :: [Text]
japaneseMnemonics15 = ["うめる", "せんく", "えんぎ", "はんぺん", "おくりがな",
    "さんち", "きなが", "といれ", "からい", "らくだ", "うえる", "ふめん", "せびろ",
    "られつ", "なにわ"]

chineseMnemonics9 :: [Text]
chineseMnemonics9 = ["钢", "看", "磁", "塑", "凤", "魏", "世", "腐", "恶" ]

chineseMnemonics18 :: [Text]
chineseMnemonics18 = ["盗", "精", "序", "郎", "赋", "姿", "委", "善", "酵",
    "祥", "赛", "矩", "蜡", "注", "韦", "效", "义", "冻"]

frenchMnemonics12 :: [Text]
frenchMnemonics12 = ["palmarès", "supplier", "visuel", "gardien", "adorer",
    "cordage", "notifier", "réglage", "employer", "abandon", "scénario",
    "proverbe"]

frenchMnemonics21 :: [Text]
frenchMnemonics21 = ["pliage", "exhorter", "brasier", "chausson", "bloquer",
    "besace", "sorcier", "absurde", "neutron", "forgeron", "geyser",
    "moulin", "cynique", "cloche", "baril", "infliger", "rompre", "typique",
    "renifler", "creuser", "matière"]

russianWalletName :: Text
russianWalletName = "АаБбВвГгДдЕеЁёЖжЗз ИиЙйКкЛлМмНнО оПпРрСсТтУуФф ХхЦцЧчШшЩщЪъ ЫыЬьЭэЮюЯяІ ѢѲѴѵѳѣі"

polishWalletName :: Text
polishWalletName = "aąbcćdeęfghijklłmnoóprsś\r\ntuvwyzżźAĄBCĆDEĘFGHIJKLŁMNOP\rRSŚTUVWYZŻŹ"

kanjiWalletName :: Text
kanjiWalletName = "亜哀挨愛曖悪握圧扱宛嵐安案暗以衣位囲医依委威為畏胃尉異移萎偉椅彙意違維慰\
\遺緯域育一壱逸茨芋引印因咽姻員院淫陰飲隠韻右宇羽雨唄鬱畝浦運雲永泳英映栄\n営詠影鋭衛易疫益液駅悦越謁\
\閲円延沿炎怨宴媛援園煙猿遠鉛塩演縁艶汚王凹\r\n央応往押旺欧殴桜翁奥横岡屋億憶臆虞乙俺卸音恩温穏下化火加\
\可仮何花佳価果河苛科架夏家荷華菓貨渦過嫁暇禍靴寡歌箇稼課蚊牙瓦我画芽賀雅餓介回灰会快戒改怪拐悔海界\
\皆械絵開階塊楷解潰壊懐諧貝外劾害崖涯街慨蓋該概骸垣柿各角拡革格核殻郭覚較隔閣確獲嚇穫学岳楽額顎掛潟\
\括活喝渇割葛滑褐轄且株釜鎌刈干刊甘汗缶\r"

arabicWalletName :: Text
arabicWalletName = "ثم نفس سقطت وبالتحديد،, جزيرتي باستخدام أن دنو. إذ هنا؟ الستار وتنصيب كان. أهّل ايطاليا، بريطانيا-فرنسا قد أخذ. سليمان، إتفاقية بين ما, يذكر الحدود أي بعد, معاملة بولندا، الإطلاق عل إيو."

wildcardsWalletName :: Text
wildcardsWalletName = "`~`!@#$%^&*()_+-=<>,./?;':\"\"'{}[]\\|❤️ 💔 💌 💕 💞 \
\💓 💗 💖 💘 💝 💟 💜 💛 💚 💙0️⃣ 1️⃣ 2️⃣ 3️⃣ 4️⃣ 5️⃣ 6️⃣ 7️⃣ 8️⃣ 9️⃣ 🔟🇺🇸🇷🇺🇸 🇦🇫🇦🇲🇸"

---
--- Addresses
---

invalidByronBase58 :: Text
invalidByronBase58 = "DdzFFPUkBXGf2an4Lgygm8tYUKXePj9KT4d3opFmG9nnygXRrDjQ6FQe"

---
--- Assets
---

steveToken :: ApiAssetMetadata
steveToken = ApiAssetMetadata
    "SteveToken" "A sample description" (Just "STV")
    (Just (ApiT (unsafeFromText "https://iohk.io/stevetoken")))
    (Just (ApiT (W.AssetLogo "Almost a logo")))
    (Just (ApiT (W.AssetDecimals 6)))

---
--- Helpers
---

cmdOk :: String
cmdOk = "Ok.\n"

payloadWith :: Text -> SomeMnemonic -> Payload
payloadWith name mnemonics = Json [json| {
     "name": #{name},
     "mnemonic_sentence": #{someMnemonicToWords mnemonics},
     "passphrase": #{fixturePassphrase}
     } |]

payloadWith' :: Text -> SomeMnemonic -> Word32 -> Payload
payloadWith' name mnemonics gap = Json [json| {
     "name": #{name},
     "mnemonic_sentence": #{someMnemonicToWords mnemonics},
     "passphrase": #{fixturePassphrase},
     "address_pool_gap": #{gap}
     } |]

simplePayload :: SomeMnemonic -> Payload
simplePayload mnemonic = Json [json| {
    "name": "Secure Wallet",
    "mnemonic_sentence": #{someMnemonicToWords mnemonic},
    "passphrase": #{fixturePassphrase}
    } |]

updateNamePayload :: Text -> Payload
updateNamePayload name = Json [json| {
     "name": #{name}
     } |]

updatePassPayload :: Text -> Text -> Payload
updatePassPayload oldPass newPass = Json [json| {
    "old_passphrase": #{oldPass},
    "new_passphrase": #{newPass}
      } |]

updatePassPayloadMnemonic :: SomeMnemonic -> Text -> Payload
updatePassPayloadMnemonic mnemonic  newPass = Json [json| {
    "mnemonic_sentence": #{someMnemonicToWords mnemonic},
    "new_passphrase": #{newPass}
      } |]

updatePassPayloadMnemonicAndSndFactor :: SomeMnemonic -> [Text] -> Text -> Payload
updatePassPayloadMnemonicAndSndFactor mnemonic sndFactor newPass = Json [json| {
    "mnemonic_sentence": #{someMnemonicToWords mnemonic},
    "mnemonic_second_factor": #{sndFactor},
    "new_passphrase": #{newPass}
      } |]

updateEmptyPassPayload :: Text -> Payload
updateEmptyPassPayload newPass = Json [json| {
    "new_passphrase": #{newPass}
      } |]

versionLine :: Text
versionLine = "Running as " <> pack (showFullVersion version gitRevision)

---
--- Error messages
---

errMsg409WalletExists :: String -> String
errMsg409WalletExists walId = "This operation would yield a wallet with the following\
     \ id: " ++ walId ++ " However, I already know of a wallet with this id."

errMsg400WalletIdEncoding :: String
errMsg400WalletIdEncoding =
    "wallet id should be a hex-encoded string of 40 characters"

errMsg403NotAByronWallet :: String
errMsg403NotAByronWallet =
    "I cannot derive new address for this wallet type.\
    \ Make sure to use Byron random wallet id."

errMsg403NotAnIcarusWallet :: String
errMsg403NotAnIcarusWallet =
    "I cannot derive new address for this wallet type.\
    \ Make sure to use a sequential wallet style, like Icarus."

errMsg403TxTooBig :: String
errMsg403TxTooBig =
    "I was not able to balance the transaction \
    \without exceeding the maximum transaction size."

errMsg400MalformedTxPayload :: String
errMsg400MalformedTxPayload =
    "I couldn't verify that the payload has the correct binary format. \
    \Therefore I couldn't send it to the node. Please check the format \
    \and try again."

errMsg400WronglyEncodedTxPayload :: String
errMsg400WronglyEncodedTxPayload =
    "Parse error. Expecting hex-encoded format."

errMsg400TxMetadataStringTooLong :: String
errMsg400TxMetadataStringTooLong =
    "Text string metadata value must consist of at most 64 UTF8 bytes"

errMsg400ParseError :: String
errMsg400ParseError = mconcat
    [ "I couldn't understand the content of your message. If your "
    , "message is intended to be in JSON format, please check that "
    , "the JSON is valid."
    ]

errMsg403ZeroAmtOutput :: String
errMsg403ZeroAmtOutput = "I can't validate coin selection because\
    \ at least one output has value 0."

_errMsg403InpsOrOutsExceeded :: (Int, Int) -> String
_errMsg403InpsOrOutsExceeded (maxNumInps, maxNumOuts) =
    "I can't validate coin selection because either the number of inputs is\
    \   more than " ++ show maxNumInps ++ " or the number of outputs\
    \ exceeds " ++ show maxNumOuts ++ "."

errMsg403WrongPass :: String
errMsg403WrongPass = "The given encryption passphrase doesn't match the one\
    \ I use to encrypt the root private key of the given wallet"

errMsg403WrongMnemonic :: String
errMsg403WrongMnemonic = "The given mnemonic doesn't match the one this wallet was created with"

errMsg403NothingToMigrate :: Text -> String
errMsg403NothingToMigrate _wid = mconcat
    [ "I wasn't able to construct a migration plan. This could be "
    , "because your wallet is empty, or it could be because the "
    , "amount of ada in your wallet is insufficient to pay for "
    , "any of the funds to be migrated. Try adding some ada to "
    , "your wallet before trying again."
    ]

errMsg404NoEndpoint :: String
errMsg404NoEndpoint = "I couldn't find the requested endpoint. If the endpoint\
    \ contains path parameters, please ensure they are well-formed, otherwise I\
    \ won't be able to route them correctly."

errMsg403AlreadyInLedger :: Text -> String
errMsg403AlreadyInLedger tid = "The transaction with id: " ++ unpack tid ++
    " cannot be forgotten as it is already in the ledger."

errMsg403PoolAlreadyJoined :: Text -> String
errMsg403PoolAlreadyJoined pid = "I couldn't join a stake pool with the given id: "
    ++ unpack pid ++ ". I have already joined this pool; joining again would "
    ++ "incur an unnecessary fee!"

errMsg403NotDelegating :: String
errMsg403NotDelegating = "It seems that you're trying to retire from \
    \delegation although you're not even delegating, nor won't be in an \
    \immediate future."

errMsg404CannotFindTx :: Text -> String
errMsg404CannotFindTx tid = "I couldn't find a transaction with the given id: "
    ++ unpack tid

errMsg403NoRootKey :: Text -> String
errMsg403NoRootKey wid = "I couldn't find a root private key for the given\
    \ wallet: " ++ unpack wid ++ ". However, this operation requires that I do\
    \ have such a key. Either there's no such wallet, or I don't fully own it."

errMsg404NoWallet :: Text -> String
errMsg404NoWallet wid =
    "I couldn't find a wallet with the given id: " ++ unpack wid

errMsg405 :: String
errMsg405 = "You've reached a known endpoint but I don't know how to handle the\
    \ HTTP method specified. Please double-check both the endpoint and the method:\
    \ one of them is likely to be incorrect (for example: POST instead of PUT, or\
    \ GET instead of POST...)."

errMsg406 :: String
errMsg406 = "It seems as though you don't accept 'application/json', but\
    \ unfortunately I only speak 'application/json'! Please double-check your\
    \ 'Accept' request header and make sure it's set to 'application/json'"

errMsg415 :: String
errMsg415 =
    "I'm really sorry but I only understand 'application/json'.\
    \ I need you to tell me what language you're speaking in order for me to\
    \ understand your message. Please double-check your 'Content-Type' request\
    \ header and make sure it's set to 'application/json'."

errMsg415OctetStream :: String
errMsg415OctetStream =
    "I'm really sorry but I only understand 'application/octet-stream'.\
    \ I need you to tell me what language you're speaking in order for me to\
    \ understand your message. Please double-check your 'Content-Type' request\
    \ header and make sure it's set to 'application/octet-stream'."

errMsg500 :: String
errMsg500 = "That's embarrassing. It looks like I've created an invalid\
    \ transaction that could not be parsed by the node. Here's an error\
    \ message that may help with debugging: Transaction failed verification:\
    \ output with no credited value"

errMsgNotInDictionary :: String
errMsgNotInDictionary = "Found an unknown word not present in the pre-defined\
    \ dictionary."

errMsg400NumberOfWords :: String
errMsg400NumberOfWords = "Invalid number of words:"

errMsg403WithdrawalNotBeneficial :: String
errMsg403WithdrawalNotBeneficial =
    "I've noticed that you're requesting a withdrawal from an account that is \
    \either empty or doesn't have a balance big enough to deserve being \
    \withdrawn. I won't proceed with that request."

errMsg403CouldntIdentifyAddrAsMine :: String
errMsg403CouldntIdentifyAddrAsMine = "I \
    \couldn't identify this address as one of mine. It likely belongs to another wallet and I \
    \will therefore not import it."

errMsg503PastHorizon :: String
errMsg503PastHorizon = "Tried to convert something that is past the horizon"

errMsg403KeyAlreadyPresent :: Text -> String
errMsg403KeyAlreadyPresent cred = mconcat
    [ "It looks like you've tried to add a cosigner key to a shared wallet's "
    ,  unpack cred," template that is already ascribed to another cosigner. "
    , "Please make sure to assign a different key to each cosigner."
    ]

errMsg403CreateIllegal :: String
errMsg403CreateIllegal = mconcat
    [ "It looks like you've tried to create a shared wallet with a template "
    , "script for payment credential that does not pass validation. The problem "
    , "is: The wallet's account key must be always present for the script template."
    ]

errMsg403TemplateInvalidNoCosignerInScript :: String
errMsg403TemplateInvalidNoCosignerInScript = mconcat
    [ "It looks like you've tried to create a shared wallet with a template "
    , "script for payment credential that does not pass validation. "
    , "The problem is: The list inside a script is empty or only contains timelocks (which is not recommended)."
    ]

errMsg403TemplateInvalidUnknownCosigner :: String
errMsg403TemplateInvalidUnknownCosigner = mconcat
    [ "It looks like you've tried to create a shared wallet with a template"
    , " script for payment credential that does not pass validation. The problem is:"
    , " The specified cosigner must be present in the script of the template."
    ]

errMsg403TemplateInvalidDuplicateXPub :: String
errMsg403TemplateInvalidDuplicateXPub = mconcat
    [ "It looks like you've tried to create a shared wallet with a template"
    , " script for payment credential that does not pass validation. The problem is:"
    , " The cosigners in a script template must stand behind an unique extended public key."
    ]

errMsg403TemplateInvalidScript :: String -> String
errMsg403TemplateInvalidScript reason = mconcat
    [ "It looks like you've tried to create a shared wallet with a template"
    , " script for payment credential that does not pass validation. The problem is: "
    , reason
    ]

errMsg400ScriptWrongCoeffcient :: String
errMsg400ScriptWrongCoeffcient =
    "At least's coefficient is 0 (which is not recommended)."

errMsg400ScriptIllFormed :: String
errMsg400ScriptIllFormed =
    "The script is ill-formed and is not going to be accepted by the ledger."

errMsg400ScriptDuplicateKeys :: String
errMsg400ScriptDuplicateKeys =
    "The list inside a script has duplicate keys (which is not recommended)."

errMsg400ScriptTimelocksContradictory :: String
errMsg400ScriptTimelocksContradictory =
    "The timelocks used are contradictory when used with 'all' (which is not recommended)."

errMsg400ScriptNotUniformRoles :: String
errMsg400ScriptNotUniformRoles =
    "All keys of a script must have the same role: either payment or delegation."

--------------------------------------------------------------------------------
-- Transaction metadata
--------------------------------------------------------------------------------

-- | Transaction metadata for ADP-1005.
--
-- See https://cardanofoundation.atlassian.net/browse/ADP-1005
--
txMetadata_ADP_1005 :: TxMetadata
txMetadata_ADP_1005 = TxMetadata $ Map.fromList
    [ ( 61284
      , TxMetaMap
        [ ( TxMetaNumber 1
          , TxMetaBytes "\SUB#f\DC3X\DC3\231\219\130\243\SYN\v\226+Ac\221\247'\US)\128h-!\246\193F\172\190\202b"
          )
        , ( TxMetaNumber 2
          , TxMetaBytes "#\229\194\DC3\211l\GSO\177C\128t~\150\ESCy\145K1GW \166O6\196D\166\219\225\SYN$"
          )
        , ( TxMetaNumber 3
          , TxMetaBytes "\224\208\133\DC2\188\ESCA\nM\219D\141\148\182\253\ETXV\NULF\158D\233\ETXq\228\142\134\SOH5"
          )
        , ( TxMetaNumber 4
          , TxMetaNumber 29562467
          )
        ]
      )
    , ( 61285
      , TxMetaMap
        [ ( TxMetaNumber 1
          , TxMetaBytes ",\SOHv\243R\134\242\ACK}\222\DEL\230\219x\155l]\190\134\162\203>\208\217\132\138\175\225\225\187\229\149\176u?\211AP\235\255\171\211\157\&4\GS\255t\DEL\184m\234\144\nj\153\174\164\155y\137o\155_\b"
          )
        ]
      )
    ]
