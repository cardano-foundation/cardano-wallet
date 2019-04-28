{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}


module Test.Integration.Scenario.Wallets
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Api.Types
    ( ApiWallet )
import Cardano.Wallet.Primitive.Types
    ( WalletDelegation (..)
    , WalletState (..)
    , walletNameMaxLength
    , walletNameMinLength )
import Control.Monad
    ( forM_ )
import Data.Quantity
    ( Quantity (..) )
import Data.Text
    ( Text )
import Test.Hspec
    ( SpecWith, describe, it )
import Test.Integration.Framework.DSL
    ( Context (..)
    , Headers (..)
    , Payload (..)
    , addressPoolGap
    , balanceAvailable
    , balanceTotal
    , delegation
    , expectErrorMessage
    , expectFieldEqual
    , expectFieldNotEqual
    , expectListItemFieldEqual
    , expectListSizeEqual
    , expectResponseCode
    , json
    , passphraseLastUpdate
    , request
    , state
    , verify
    , walletId
    , walletName
    )

import qualified Data.Text as T
import qualified Network.HTTP.Types.Status as HTTP

spec :: SpecWith Context
spec = do
    it "WALLETS_CREATE_01 - Create a wallet" $ \ctx -> do

        let payload = Json [json| {
                "name": "1st Wallet",
                "mnemonic_sentence": #{mnemonics15},
                "mnemonic_second_factor": #{mnemonics12},
                "passphrase": "Secure Passphrase",
                "address_pool_gap": 30
                } |]
        r <- request @ApiWallet ctx ("POST", "v2/wallets") Default payload
        verify r
            [ expectResponseCode @IO HTTP.status202
            , expectFieldEqual walletName "1st Wallet"
            , expectFieldEqual addressPoolGap 30
            , expectFieldEqual balanceAvailable 0
            , expectFieldEqual balanceTotal 0
            , expectFieldEqual state (Restoring (Quantity minBound))
            , expectFieldEqual delegation (NotDelegating)
            , expectFieldEqual walletId "2cf060fe53e4e0593f145f22b858dfc60676d4ab"
            , expectFieldNotEqual passphraseLastUpdate "2019-04-12 07:57:28.439742724 UTC"
            ]

    it "WALLETS_CREATE_01 - Created a wallet can be listed" $ \ctx -> do

        let payload = Json [json| {
                "name": "Wallet to be listed",
                "mnemonic_sentence": #{mnemonics18},
                "mnemonic_second_factor": #{mnemonics9},
                "passphrase": "Secure Passphrase",
                "address_pool_gap": 20
                } |]
        _ <- request @ApiWallet ctx ("POST", "v2/wallets") Default payload
        r2 <- request @[ApiWallet] ctx ("GET", "v2/wallets") Default Empty
        verify r2
            [ expectResponseCode @IO HTTP.status200
            , expectListSizeEqual 1
            , expectListItemFieldEqual 0 walletName "Wallet to be listed"
            , expectListItemFieldEqual 0 addressPoolGap 20
            , expectListItemFieldEqual 0 balanceAvailable 0
            , expectListItemFieldEqual 0 balanceTotal 0
            , expectListItemFieldEqual 0 state (Restoring (Quantity minBound))
            , expectListItemFieldEqual 0 delegation (NotDelegating)
            , expectListItemFieldEqual 0 walletId "dfe87fcf0560fb57937a6468ea51e860672fad79"
            ]

    it "WALLETS_CREATE_03 - Cannot create wallet that exists" $ \ctx -> do
        let payload = Json [json| {
                "name": "Some Wallet",
                "mnemonic_sentence": #{mnemonics21},
                "passphrase": "Secure Passphrase"
                } |]
        r1 <- request @ApiWallet ctx ("POST", "v2/wallets") Default payload
        expectResponseCode @IO HTTP.status202 r1

        r2 <- request @ApiWallet ctx ("POST", "v2/wallets") Default payload
        expectResponseCode @IO HTTP.status409 r2

    describe "WALLETS_CREATE_04 - Wallet name" $ do
        let walNameMax = T.pack (replicate walletNameMaxLength 'ą')
        let matrix = [ ( show walletNameMinLength ++ " char long"
                       , "1"
                       , [ expectResponseCode @IO HTTP.status202
                         , expectFieldEqual walletName "1"
                         ]
                       )
                     , ( show walletNameMaxLength ++ " char long"
                       , walNameMax
                       , [ expectResponseCode @IO HTTP.status202
                         , expectFieldEqual walletName walNameMax
                         ]
                       )
                     , ( show (walletNameMaxLength + 1) ++ " char long"
                       , T.pack (replicate (walletNameMaxLength + 1) 'ę')
                       , [ expectResponseCode @IO HTTP.status400
                         , expectErrorMessage "name is too long: expected at\
                                              \ most 255 chars"
                         ]
                       )
                     , ( "Empty name"
                       , ""
                       , [ expectResponseCode @IO HTTP.status400
                         , expectErrorMessage "name is too short: expected at\
                                              \ least 1 char"
                         ]
                       )
                     , ( "Russian name"
                       , russianWalletName
                       , [ expectResponseCode @IO HTTP.status202
                         , expectFieldEqual walletName russianWalletName
                         ]
                       )
                     , ( "Polish name"
                       , polishWalletName
                       , [ expectResponseCode @IO HTTP.status202
                         , expectFieldEqual walletName polishWalletName
                         ]
                       )
                     , ( "Kanji name"
                       , kanjiWalletName
                       , [ expectResponseCode @IO HTTP.status202
                         , expectFieldEqual walletName kanjiWalletName
                         ]
                       )
                     , ( "Arabic name"
                       , arabicWalletName
                       , [ expectResponseCode @IO HTTP.status202
                         , expectFieldEqual walletName arabicWalletName
                         ]
                       )
                     , ( "Wildcards name"
                       , wildcardsWalletName
                       , [ expectResponseCode @IO HTTP.status202
                         , expectFieldEqual walletName wildcardsWalletName
                         ]
                       )
                     ]
        forM_ matrix $ \(title, walName, expectations) -> it title $ \ctx -> do
            let payload = Json [json| {
                    "name": #{walName},
                    "mnemonic_sentence": #{mnemonics24},
                    "passphrase": "Secure Passphrase"
                    } |]
            r <- request @ApiWallet ctx ("POST", "v2/wallets") Default payload
            verify r expectations

    it "WALLETS_CREATE_04 - Name param missing" $ \ctx -> do

        let payload = Json [json| {
                "mnemonic_sentence": #{mnemonics15},
                "mnemonic_second_factor": #{mnemonics12},
                "passphrase": "Secure Passphrase",
                "address_pool_gap": 30
                } |]
        r <- request @ApiWallet ctx ("POST", "v2/wallets") Default payload
        verify r
            [ expectResponseCode @IO HTTP.status400
            , expectErrorMessage "key \"name\" not present"
            ]

 where
    -- mnemonics6 :: [Text]
    -- mnemonics6 = ["tornado", "canvas", "peasant", "spike", "enrich", "dilemma"]

    mnemonics9 :: [Text]
    mnemonics9 = ["subway", "tourist", "abstract", "roast", "border", "curious",
        "exercise", "work", "narrow"]

    mnemonics12 :: [Text]
    mnemonics12 = ["agent", "siren", "roof", "water", "giant", "pepper",
        "obtain", "oxygen", "treat", "vessel", "hip", "garlic"]

    mnemonics15 :: [Text]
    mnemonics15 = ["network", "empty", "cause", "mean", "expire", "private",
        "finger", "accident", "session", "problem", "absurd", "banner", "stage",
        "void", "what"]

    mnemonics18 :: [Text]
    mnemonics18 = ["whisper", "control", "diary", "solid", "cattle", "salmon",
        "whale", "slender", "spread", "ice", "shock", "solve", "panel",
        "caution", "upon", "scatter", "broken", "tonight"]

    mnemonics21 :: [Text]
    mnemonics21 = ["click", "puzzle", "athlete", "morning", "fold", "retreat",
        "across", "timber", "essay", "drill", "finger", "erase", "galaxy",
        "spoon", "swift", "eye", "awesome", "shrimp", "depend", "zebra", "token"]

    mnemonics24 :: [Text]
    mnemonics24 = ["decade", "distance", "denial", "jelly", "wash", "sword",
        "olive", "perfect", "jewel", "renew", "wrestle", "cupboard", "record",
        "scale", "pattern", "invite", "other", "fruit", "gloom", "west", "oak",
        "deal", "seek", "hand"]

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
