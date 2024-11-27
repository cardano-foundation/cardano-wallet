module Cardano.Wallet.DerivingSpec
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Deriving
    ( DerivedKeys (..)
    , deriveKeys
    , fromHex
    )
import Data.ByteString
    ( ByteString
    )
import Data.Text
    ( Text
    )
import Test.Hspec
    ( Spec
    , describe
    , it
    , shouldBe
    )

import qualified Data.Text as T

spec :: Spec
spec = do
    describe "Deriving keys used as payment credentials in enterprise addresses" $ do

        --- $ cat acct.xsk
        --- acct_xsk1mqqrxjztq35xkjzvx8lw730xnxf8lyrgd8fmtrk588fetal8wfvc37fnv7udef5uuktx7lxw3c3msnjmma20k823vy4z9qmclj4352uzq8r72xw2d77l9jafnzr4zjpl9jk7xkeewwq90nweh4q95rd5cud5269p
        --- $ bech32 < acct.xsk
        --- d80033484b04686b484c31feef45e699927f906869d3b58ed439d395f7e7725988f93367b8dca69ce5966f7cce8e23b84e5bdf54fb1d51612a228378fcab1a2b8201c7e519ca6fbdf2cba9988751483f2cade35b39738057cdd9bd405a0db4c7
        --- $ cardano-address key inspect <<< $(cat acct.xsk)
        --- {
        ---     "chain_code": "8201c7e519ca6fbdf2cba9988751483f2cade35b39738057cdd9bd405a0db4c7",
        ---     "key_type": "private",
        ---     "extended_key": "d80033484b04686b484c31feef45e699927f906869d3b58ed439d395f7e7725988f93367b8dca69ce5966f7cce8e23b84e5bdf54fb1d51612a228378fcab1a2b"
        --- }
        let accXPrvTxt = "d80033484b04686b484c31feef45e699927f906869d3b58ed439d395f7e7725988f93367b8dca69ce5966f7cce8e23b84e5bdf54fb1d51612a228378fcab1a2b8201c7e519ca6fbdf2cba9988751483f2cade35b39738057cdd9bd405a0db4c7"
        let accXPrv = fromHexUnsafe accXPrvTxt

        --- Signing keys for payment credential with path 0/0
        --- $ cardano-address key child 0/0 < acct.xsk > pay0.xsk
        --- $ cat pay0.xsk
        --- addr_xsk1vqxel3ywl8rzfxndsxg8v276mppe5tpdz0xazesc0qpr5phgwfve8cv4q5npegy05spaj8yht4hgggtylsrpf74mawvchjclwyqpupllyuap2w3eq5ggmw6fqyfdfcm5pmz685sxggccs53macwqaz3vzuutmydz
        --- $ bech32 < pay0.xsk
        --- 600d9fc48ef9c6249a6d8190762bdad8439a2c2d13cdd1661878023a06e8725993e19505261ca08fa403d91c975d6e842164fc0614fabbeb998bcb1f71001e07ff273a153a3905108dbb490112d4e3740ec5a3d206423188523bee1c0e8a2c17
        --- $ cardano-address key inspect <<< $(cat pay0.xsk)
        --- {
        ---    "chain_code": "ff273a153a3905108dbb490112d4e3740ec5a3d206423188523bee1c0e8a2c17",
        ---    "key_type": "private",
        ---    "extended_key": "600d9fc48ef9c6249a6d8190762bdad8439a2c2d13cdd1661878023a06e8725993e19505261ca08fa403d91c975d6e842164fc0614fabbeb998bcb1f71001e07"
        --- }
        --- OR
        --- $ cardano-address key private --signing-key --hex < pay0.xsk
        --- 600d9fc48ef9c6249a6d8190762bdad8439a2c2d13cdd1661878023a06e8725993e19505261ca08fa403d91c975d6e842164fc0614fabbeb998bcb1f71001e07
        ---
        --- Verification keys for payment credential
        --- $ cardano-address key public --with-chain-code < pay0.xsk > pay0.xvk
        --- $ cat pay0.xvk
        --- addr_xvk1eswxe9rlgfza99a8l60nf0l7prak7ffjyptuc04cw4t0m099a25l7fe6z5arjpgs3ka5jqgj6n3hgrk950fqvs333pfrhmsup69zc9c0khwpd
        --- $ bech32 < pay0.xvk
        --- cc1c6c947f4245d297a7fe9f34bffe08fb6f25322057cc3eb87556fdbca5eaa9ff273a153a3905108dbb490112d4e3740ec5a3d206423188523bee1c0e8a2c17
        --- $ cabal run cardano-address key inspect < pay0.xvk
        --- {
        ---    "chain_code": "ff273a153a3905108dbb490112d4e3740ec5a3d206423188523bee1c0e8a2c17",
        ---    "extended_key": "cc1c6c947f4245d297a7fe9f34bffe08fb6f25322057cc3eb87556fdbca5eaa9",
        ---    "key_type": "public"
        --- }
        it "golden for 0/0" $ do
            let prv0XskTxt = "600d9fc48ef9c6249a6d8190762bdad8439a2c2d13cdd1661878023a06e8725993e19505261ca08fa403d91c975d6e842164fc0614fabbeb998bcb1f71001e07ff273a153a3905108dbb490112d4e3740ec5a3d206423188523bee1c0e8a2c17"
            let prv0Xsk = fromHexUnsafe prv0XskTxt
            let prv0skTxt = "600d9fc48ef9c6249a6d8190762bdad8439a2c2d13cdd1661878023a06e8725993e19505261ca08fa403d91c975d6e842164fc0614fabbeb998bcb1f71001e07"
            let prv0sk = fromHexUnsafe prv0skTxt
            let pub0XvkTxt = "cc1c6c947f4245d297a7fe9f34bffe08fb6f25322057cc3eb87556fdbca5eaa9ff273a153a3905108dbb490112d4e3740ec5a3d206423188523bee1c0e8a2c17"
            let pub0Xvk = fromHexUnsafe pub0XvkTxt
            let prv0vkTxt = "cc1c6c947f4245d297a7fe9f34bffe08fb6f25322057cc3eb87556fdbca5eaa9"
            let prv0vk = fromHexUnsafe prv0vkTxt
            let expectedKeys = DerivedKeys
                    { extendedPrivate = prv0Xsk
                    , private = prv0sk
                    , extendedPublic = pub0Xvk
                    , public = prv0vk
                    }
            deriveKeys accXPrv 0 `shouldBe` Right expectedKeys

        --- Signing keys for payment credential with path 0/10
        --- $ cardano-address key child 0/10 < acct.xsk > pay10.xsk
        --- $ cat pay10.xsk
        --- addr_xsk16zrfnr3juf3u6m039aqjaz0rzyfxy68ath7h8gzm4tq4kq0gwfvucwkzvr406phcycrghramur6fpvcpqa2jzmm6ldslt6m96q7s873shp85xgnfkdvdpnx9tqhz8k66fr7gnh97yqfqg3p095uqewydvvgy5fs6
        --- $ bech32 < pay10.xsk
        --- d086998e32e263cd6df12f412e89e311126268fd5dfd73a05baac15b01e87259cc3ac260eafd06f826068b8fbbe0f490b3010755216f7afb61f5eb65d03d03fa30b84f432269b358d0ccc5582e23db5a48fc89dcbe201204442f2d380cb88d63
        --- $ cardano-address key inspect <<< $(cat pay10.xsk)
        --- {
        ---    "chain_code": "30b84f432269b358d0ccc5582e23db5a48fc89dcbe201204442f2d380cb88d63",
        ---    "extended_key": "d086998e32e263cd6df12f412e89e311126268fd5dfd73a05baac15b01e87259cc3ac260eafd06f826068b8fbbe0f490b3010755216f7afb61f5eb65d03d03fa",
        ---    "key_type": "private"
        ---}
        ---
        --- Verification keys for payment credential
        --- $ cardano-address key public --with-chain-code < pay10.xsk > pay10.xvk
        --- $ cat pay10.xvk
        --- addr_xvk1qxq75cpyl28p4rv7gt0cxgnunserq5j8dn0wg0kgelf8yjasdysrpwz0gv3xnv6c6rxv2kpwy0d45j8u38wtugqjq3zz7tfcpjug6cc8p62wk
        --- $ bech32 < pay10.xvk
        --- 0181ea6024fa8e1a8d9e42df83227c9c323052476cdee43ec8cfd2724bb0692030b84f432269b358d0ccc5582e23db5a48fc89dcbe201204442f2d380cb88d63
        --- $ cabal run cardano-address key inspect < pay10.xvk
        --- {
        ---     "chain_code": "30b84f432269b358d0ccc5582e23db5a48fc89dcbe201204442f2d380cb88d63",
        ---     "extended_key": "0181ea6024fa8e1a8d9e42df83227c9c323052476cdee43ec8cfd2724bb06920",
        ---     "key_type": "public"
        --- }
        it "golden for 0/10" $ do
            let prv10XskTxt = "d086998e32e263cd6df12f412e89e311126268fd5dfd73a05baac15b01e87259cc3ac260eafd06f826068b8fbbe0f490b3010755216f7afb61f5eb65d03d03fa30b84f432269b358d0ccc5582e23db5a48fc89dcbe201204442f2d380cb88d63"
            let prv10Xsk = fromHexUnsafe prv10XskTxt
            let prv10skTxt = "d086998e32e263cd6df12f412e89e311126268fd5dfd73a05baac15b01e87259cc3ac260eafd06f826068b8fbbe0f490b3010755216f7afb61f5eb65d03d03fa"
            let prv10sk = fromHexUnsafe prv10skTxt
            let pub10XvkTxt = "0181ea6024fa8e1a8d9e42df83227c9c323052476cdee43ec8cfd2724bb0692030b84f432269b358d0ccc5582e23db5a48fc89dcbe201204442f2d380cb88d63"
            let pub10Xvk = fromHexUnsafe pub10XvkTxt
            let prv10vkTxt = "0181ea6024fa8e1a8d9e42df83227c9c323052476cdee43ec8cfd2724bb06920"
            let prv10vk = fromHexUnsafe prv10vkTxt
            let expectedKeys = DerivedKeys
                    { extendedPrivate = prv10Xsk
                    , private = prv10sk
                    , extendedPublic = pub10Xvk
                    , public = prv10vk
                    }
            deriveKeys accXPrv 10 `shouldBe` Right expectedKeys

fromHexUnsafe :: Text -> ByteString
fromHexUnsafe txt = case fromHex txt of
    Just key -> key
    Nothing -> error $ "fromHextUnsafe: wrongly encoded key: " <> T.unpack txt
