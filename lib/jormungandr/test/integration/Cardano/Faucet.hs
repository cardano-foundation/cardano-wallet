{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Faucet
    ( initFaucet
    ) where

import Prelude

import Cardano.Wallet.Network
    ( NetworkLayer )
import Test.Integration.Faucet
    ( Faucet )


-- | Initialize a bunch of faucet wallets and make them available for the
-- integration tests scenarios.
initFaucet :: NetworkLayer t IO -> IO Faucet
initFaucet _nl = undefined


{--
-- code used for mnemonics generation, deriving the corresponding first address
-- and calculating encoding to bench32

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Faucet
    ( generateMnemonicsAndAdresses
    , generateTriples
    ) where

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
    ( Address (..) )
import Codec.Binary.Bech32
    ( HumanReadablePart
    , dataPartFromBytes
    , encodeLenient
    , humanReadablePartFromText
    )
import Control.Monad
    ( replicateM )
import Data.ByteString
    ( ByteString )
import Data.Text
    ( Text )

generateMnemonicsAndAdresses :: Int -> IO [([Text], ByteString)]
generateMnemonicsAndAdresses pairNum = do
    mnemonics <- replicateM pairNum genMnemonic
    pure $ zip (mnemonicToText @15 <$> mnemonics) (getAddress . firstAddress <$> mnemonics)
      where
          genMnemonic :: IO (Mnemonic 15)
          genMnemonic = entropyToMnemonic <$> genEntropy
          firstAddress :: Mnemonic 15 -> Address
          firstAddress mw =
              let
                  (seed, pwd) =
                      (Passphrase $ entropyToBytes $ mnemonicToEntropy mw, mempty)
                  rootXPrv =
                      generateKeyFromSeed (seed, mempty) pwd
                  accXPrv =
                      deriveAccountPrivateKey pwd rootXPrv minBound
                  addrXPrv =
                      deriveAddressPrivateKey pwd accXPrv ExternalChain minBound
              in
                  keyToAddress @(Jormungandr 'Testnet) (publicKey addrXPrv)

generateTriples :: Int -> IO [([Text], ByteString, Text)]
generateTriples tripleNum = do
    pairs <- generateMnemonicsAndAdresses tripleNum
    let bech32outs = map encodeToBech32 (map snd pairs)
    pure $ zipWith (\(m,bs) b -> (m,bs,b)) pairs bech32outs
    where
        unsafeHumanReadablePart :: Text -> HumanReadablePart
        unsafeHumanReadablePart = either errUnsafe id . humanReadablePartFromText
            where
                errUnsafe _ =
                    error "Invalid bech32 human-readable part"
        encodeToBech32 :: ByteString -> Text
        encodeToBech32 bs =
            let hrp = unsafeHumanReadablePart "ta"
            in encodeLenient hrp (dataPartFromBytes bs)
--}

{--
ghci> generateTriples 10
[(["tattoo","potato","foil","mutual","slab","path","forward","pencil","suit","marble","hill","meat","close","garden","bird"],
"\131\ENQK\186?\203{_$\145\134\ESCn+\139\240\163\249%|\223/\223A\202Z\247\a.w\199\SI:",
"ta1svz5hw3leda47fy3scdku2ut7z3ljftumuha7sw2ttmswtnhcu8n5nm0ynp"),

(["maximum","close","arrest","cheese","sleep","choice","fame","grape","enjoy","monkey","endorse","armed","urge","satoshi","scorpion"],
"\131\147\147\196i\217\199\156^\"~z\242\228\"\136*\142.\188!l\221o\158T\224`\242\&1[\196\DC4",
"ta1swfe83rfm8rech3z0ea09epz3q4gut4uy9kd6mu72nsxpu33t0zpg795k3y"),

(["glide","mule","shift","empower","jacket","desert","smoke","blossom","joke","crew","athlete","fabric","ozone","thought","private"],
"\131\206\155\217\224f}\246\230\174y^\139\ACK\177\154\152=!\157\184\192&\v\175\204\239\215\RS\204\254\195\219",
"ta1s08fhk0qve7lde4w090gkp43n2vr6gvahrqzvza0enhaw8kvlmpakd7swne"),

(["appear","silent","loan","inch","plastic","maze","trouble","give","defy","detect","remember","issue","solution","increase","time"],
"\131\STX\NUL^\t\129\243\223Q={0\155\193\&0\250_Z|\167\244I\ETX\166]\189\165\188\147u\131\218=",
"ta1svpqqhsfs8ea75fa0vcfhsfslf045l9873ys8fjahkjmeym4s0dr69g8lec"),

(["actress","kiwi","night","mimic","minor","local","mask","myself","remind","gesture","spirit","daring","rack","input","zoo"],
"\131\140\164,\SI\STX\170\195\SOH\145\ENQ\206\164\172\186n\194G\228\133\147\251\ENQX3%(\230 ~\209\188%",
"ta1swx2gtq0q24vxqv3qh82ft96dmpy0ey9j0as2kpny55wvgr76x7z2u8kvzf"),

(["identify","black","engage","oil","better","keen","elephant","cross","citizen","law","soon","quiz","biology","march","tape"],
"\131\175\217\146d[\t\133\238>\174s[\168W\193\177\141W\173\246B\255\177r\145\137\191[Q\230\176\187",
"ta1swhanynytvyctm374ee4h2zhcxcc64ad7ep0lvtjjxym7k63u6ctkfjeg2n"),

(["uphold","century","misery","dwarf","arctic","merge","traffic","harbor","fine","latin","state","remain","pink","wrap","actual"],
"\131\FS|T#\209X\a\236>\143S\DC1\169Ok/\181y\176g\182\212\DEL\167p\v\ETX\SOH\195c\t\160",
"ta1svw8c4pr69vq0mp73af3r220dvhm27dsv7mdgla8wq9sxqwrvvy6qhpq5c8"),

(["sudden","repeat","brother","arena","jaguar","polar","bounce","board","review","aware","jaguar","expand","leopard","mobile","gravity"],
"\131#c\211\176a\143T\204 \168\&3y\193fGM\209\DC2\FSgM\250\131I~1\236\197\SUB\148V\209",
"ta1sv3k85asvx84fnpq4qehnstxgaxazysuvaxl4q6f0cc7e3g6j3tdzp2vjac"),

(["bird","wave","record","proof","final","sponsor","parrot","cement","resist","dust","note","bean","spice","stage","prevent"],
"\131\252\229\169j*u.n\159\184\CAN\214\203C2><\a&\191\140\202\150M\176\255\175\DLEFxZ!",
"ta1s07wt2t29f6jum5lhqvddj6rxglrcpexh7xv49jdkrl67yzx0pdzzc3wnmv"),

(["where","circle","abstract","estate","bless","state","stool","future","beauty","episode","patient","rhythm","scare","project","trigger"],
"\131=K\138f\DC3\188x\154\144\173\DEL\241T\166\161\141Y\195\176\156\134\237;\146\173\232\250\&2\215\ACK@\254",
"ta1sv75hznxzw783x5s44llz49x5xx4nsasnjrw6wuj4h505vkhqeq0uq77d56")]
--}
