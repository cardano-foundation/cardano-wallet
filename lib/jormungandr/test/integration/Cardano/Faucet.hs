{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Faucet
    ( initFaucet
    ) where

import Prelude

import Cardano.Wallet.Primitive.Mnemonic
    ( mkMnemonic )
import Control.Concurrent.MVar
    ( newMVar )
import Data.Text
    ( Text )
import Test.Integration.Faucet
    ( Faucet (..) )


-- | Initialize a bunch of faucet wallets and make them available for the
-- integration tests scenarios.
initFaucet :: IO Faucet
initFaucet = do
    let wallets =
            map (either (error "cannot retrieve mnemonics") id . mkMnemonic @15)
            (map fst mnemonicsAdresses)
    Faucet <$> newMVar wallets

mnemonicsAdresses :: [([Text], Text)]
mnemonicsAdresses =
    [
      (["tattoo","potato","foil","mutual","slab","path","forward","pencil","suit","marble","hill","meat","close","garden","bird"],
       "ta1svz5hw3leda47fy3scdku2ut7z3ljftumuha7sw2ttmswtnhcu8n5nm0ynp")
    , (["maximum","close","arrest","cheese","sleep","choice","fame","grape","enjoy","monkey","endorse","armed","urge","satoshi","scorpion"],
       "ta1swfe83rfm8rech3z0ea09epz3q4gut4uy9kd6mu72nsxpu33t0zpg795k3y")
    , (["glide","mule","shift","empower","jacket","desert","smoke","blossom","joke","crew","athlete","fabric","ozone","thought","private"],
       "ta1s08fhk0qve7lde4w090gkp43n2vr6gvahrqzvza0enhaw8kvlmpakd7swne")
    , (["appear","silent","loan","inch","plastic","maze","trouble","give","defy","detect","remember","issue","solution","increase","time"],
       "ta1svpqqhsfs8ea75fa0vcfhsfslf045l9873ys8fjahkjmeym4s0dr69g8lec")
    , (["actress","kiwi","night","mimic","minor","local","mask","myself","remind","gesture","spirit","daring","rack","input","zoo"],
       "ta1swx2gtq0q24vxqv3qh82ft96dmpy0ey9j0as2kpny55wvgr76x7z2u8kvzf")
    , (["identify","black","engage","oil","better","keen","elephant","cross","citizen","law","soon","quiz","biology","march","tape"],
       "ta1swhanynytvyctm374ee4h2zhcxcc64ad7ep0lvtjjxym7k63u6ctkfjeg2n")
    , (["uphold","century","misery","dwarf","arctic","merge","traffic","harbor","fine","latin","state","remain","pink","wrap","actual"],
       "ta1svw8c4pr69vq0mp73af3r220dvhm27dsv7mdgla8wq9sxqwrvvy6qhpq5c8")
    , (["sudden","repeat","brother","arena","jaguar","polar","bounce","board","review","aware","jaguar","expand","leopard","mobile","gravity"],
       "ta1sv3k85asvx84fnpq4qehnstxgaxazysuvaxl4q6f0cc7e3g6j3tdzp2vjac")
    , (["bird","wave","record","proof","final","sponsor","parrot","cement","resist","dust","note","bean","spice","stage","prevent"],
       "ta1s07wt2t29f6jum5lhqvddj6rxglrcpexh7xv49jdkrl67yzx0pdzzc3wnmv")
    , (["where","circle","abstract","estate","bless","state","stool","future","beauty","episode","patient","rhythm","scare","project","trigger"],
       "ta1sv75hznxzw783x5s44llz49x5xx4nsasnjrw6wuj4h505vkhqeq0uq77d56")
    ]

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
ghci> generateTriples 1
[(["tattoo","potato","foil","mutual","slab","path","forward","pencil","suit","marble","hill","meat","close","garden","bird"],
"\131\ENQK\186?\203{_$\145\134\ESCn+\139\240\163\249%|\223/\223A\202Z\247\a.w\199\SI:",
"ta1svz5hw3leda47fy3scdku2ut7z3ljftumuha7sw2ttmswtnhcu8n5nm0ynp"),
--}
