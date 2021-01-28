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

    -- * Helpers
    , cmdOk
    , versionLine
    , payloadWith
    , payloadWith'
    , simplePayload
    , updateNamePayload
    , updatePassPayload
    , updateEmptyPassPayload

    -- * Error messages
    , errMsg400WalletIdEncoding
    , errMsg400StartTimeLaterThanEndTime
    , errMsg403Fee
    , errMsg403NotAByronWallet
    , errMsg403NotAnIcarusWallet
    , errMsg403NotEnoughMoney
    , errMsg403WrongPass
    , errMsg403AlreadyInLedger
    , errMsg404NoSuchPool
    , errMsg403PoolAlreadyJoined
    , errMsg403NotDelegating
    , errMsg403NonNullReward
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
    , errMsg400MinWithdrawalWrong
    , errMsg403WithdrawalNotWorth
    , errMsg403NotAShelleyWallet
    , errMsg403MinUTxOValue
    , errMsg403CouldntIdentifyAddrAsMine
    , errMsg503PastHorizon
    ) where

import Prelude

import Cardano.Wallet.Version
    ( gitRevision, showFullVersion, version )
import Data.Text
    ( Text, pack, unpack )
import Data.Word
    ( Word32 )
import Test.Integration.Framework.DSL
    ( Payload (..), fixturePassphrase, json )

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
--- Helpers
---

cmdOk :: String
cmdOk = "Ok.\n"

payloadWith :: Text -> [Text] -> Payload
payloadWith name mnemonics = Json [json| {
     "name": #{name},
     "mnemonic_sentence": #{mnemonics},
     "passphrase": #{fixturePassphrase}
     } |]

payloadWith' :: Text -> [Text] -> Word32 -> Payload
payloadWith' name mnemonics gap = Json [json| {
     "name": #{name},
     "mnemonic_sentence": #{mnemonics},
     "passphrase": #{fixturePassphrase},
     "address_pool_gap": #{gap}
     } |]

simplePayload :: [Text] -> Payload
simplePayload mnemonic = Json [json| {
    "name": "Secure Wallet",
    "mnemonic_sentence": #{mnemonic},
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

updateEmptyPassPayload :: Text -> Payload
updateEmptyPassPayload newPass = Json [json| {
    "new_passphrase": #{newPass}
      } |]

versionLine :: Text
versionLine = "Running as v" <> pack (showFullVersion version gitRevision)

---
--- Error messages
---

errMsg403MinUTxOValue :: String
errMsg403MinUTxOValue =
    "Some outputs have ada values that are too small. There's a \
    \minimum ada value specified by the protocol that each output must satisfy. \
    \I'll handle that minimum value myself when you do not explicitly specify \
    \an ada value for an output. Otherwise, you must specify enough ada."

errMsg409WalletExists :: String -> String
errMsg409WalletExists walId = "This operation would yield a wallet with the following\
     \ id: " ++ walId ++ " However, I already know of a wallet with this id."

errMsg400WalletIdEncoding :: String
errMsg400WalletIdEncoding =
    "wallet id should be a hex-encoded string of 40 characters"

errMsg400StartTimeLaterThanEndTime :: String -> String -> String
errMsg400StartTimeLaterThanEndTime startTime endTime = mconcat
    [ "The specified start time '"
    , startTime
    , "' is later than the specified end time '"
    , endTime
    , "'."
    ]

errMsg403Fee :: String
errMsg403Fee =
    "I am unable to finalize the transaction, as there is not enough ada I can \
    \use to pay for fees, or to satisfy the minimum ada quantities of change \
    \outputs."

errMsg403NotAByronWallet :: String
errMsg403NotAByronWallet =
    "I cannot derive new address for this wallet type.\
    \ Make sure to use Byron random wallet id."

errMsg403NotAnIcarusWallet :: String
errMsg403NotAnIcarusWallet =
    "I cannot derive new address for this wallet type.\
    \ Make sure to use a sequential wallet style, like Icarus."

errMsg403NotEnoughMoney :: String
errMsg403NotEnoughMoney =
    "I can't process this payment as there are not enough funds available in \
    \the wallet."

errMsg403TxTooBig :: String
errMsg403TxTooBig =
    "I am not able to finalize the transaction because I need to select \
    \additional inputs and doing so will make the transaction too big. \
    \Try sending a smaller amount."

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

errMsg400MinWithdrawalWrong :: String
errMsg400MinWithdrawalWrong = "The minimum withdrawal value must be at least \
    \1 Lovelace."

errMsg403NothingToMigrate :: Text -> String
errMsg403NothingToMigrate wid =
    "I can't migrate the wallet with the given id: " ++ unpack wid ++
    ", because it's either empty or full of small coins which wouldn't be \
    \worth migrating."

errMsg404NoEndpoint :: String
errMsg404NoEndpoint = "I couldn't find the requested endpoint. If the endpoint\
    \ contains path parameters, please ensure they are well-formed, otherwise I\
    \ won't be able to route them correctly."

errMsg403AlreadyInLedger :: Text -> String
errMsg403AlreadyInLedger tid = "The transaction with id: " ++ unpack tid ++
    " cannot be forgotten as it is already in the ledger."

errMsg404NoSuchPool :: Text -> String
errMsg404NoSuchPool pid = "I couldn't find any stake pool with the given id: "
    ++ unpack pid

errMsg403PoolAlreadyJoined :: Text -> String
errMsg403PoolAlreadyJoined pid = "I couldn't join a stake pool with the given id: "
    ++ unpack pid ++ ". I have already joined this pool; joining again would "
    ++ "incur an unnecessary fee!"

errMsg403NotDelegating :: String
errMsg403NotDelegating = "It seems that you're trying to retire from \
    \delegation although you're not even delegating, nor won't be in an \
    \immediate future."

errMsg403NonNullReward :: String
errMsg403NonNullReward = "It seems that you're trying to retire from delegation \
    \although you've unspoiled rewards in your rewards account!"

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

errMsg403WithdrawalNotWorth :: String
errMsg403WithdrawalNotWorth =
    "I've noticed that you're requesting a withdrawal from an account that is \
    \either empty or doesn't have a balance big enough to deserve being \
    \withdrawn. I won't proceed with that request."

errMsg403NotAShelleyWallet :: String
errMsg403NotAShelleyWallet =
    "It is regrettable but you've just attempted an operation that is invalid \
    \for this type of wallet. Only new 'Shelley' wallets can do something with \
    \rewards and this one isn't."

errMsg403CouldntIdentifyAddrAsMine :: String
errMsg403CouldntIdentifyAddrAsMine = "I \
    \couldn't identify this address as one of mine. It likely belongs to another wallet and I \
    \will therefore not import it."

errMsg503PastHorizon :: String
errMsg503PastHorizon = "Tried to convert something that is past the horizon"
