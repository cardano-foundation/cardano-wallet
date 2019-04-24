# Compare our tx signing with cardano-sl in ghci




## cardano-wallet

`stack ghci cardano-wallet:test:unit --test`

````
:set -XDataKinds
:set -XFlexibleInstances
import Cardano.Wallet.Primitive.AddressDerivation
import Cardano.Wallet.Primitive.AddressDiscovery
import Cardano.Wallet.Primitive.Types
import Cardano.Wallet.Primitive.Signing
import Cardano.Wallet.Binary
import Codec.CBOR.Encoding
import Data.ByteArray.Encoding (convertToBase, Base (Base16))
import Data.Map (Map)
import Data.Text.Class ( toText )

import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Read as CBOR
import qualified Codec.CBOR.Write as CBOR
import qualified Data.ByteArray as BA
import qualified Data.ByteString.Lazy as BL
import qualified Data.Set as Set
import qualified Data.ByteString as BS
import qualified Cardano.Crypto.Wallet as CC
import qualified Data.Map as Map
import qualified Data.Text.IO as TIO



:{
instance AddressScheme (Map Address (Key 'AddressK XPrv)) where
    keyFrom addr _ m = Map.lookup addr m
    generateChangeOutput = error "unimplemented change output" 
:}


let bs = (BS.pack $ replicate 128 0) -- needs to be 128 bytes long
let seed = Passphrase $ BA.convert bs
let gen = Passphrase mempty
let encrypt = Passphrase mempty
let addrXPrv = unsafeGenerateKeyFromSeed (seed, gen) encrypt
let addr = keyToAddress $ publicKey addrXPrv 
let h = Hash $ "\bztb\249\223n5\189A\239Y&\243tgv\253\223\140\222i\236O\221\&8\205\218>\234\240\238"
let ownedIns = (TxIn h 0, TxOut addr (Coin 1) )
let outs = [TxOut addr (Coin 1)]


toHex = (convertToBase @BS.ByteString @BS.ByteString) Base16

let tx = Tx [TxIn h 0] [TxOut addr (Coin 1)]



let m = (Map.singleton addr addrXPrv) :: Map Address (Key 'AddressK XPrv)

let fromR = either undefined Prelude.id
let stx = fromR $ mkStdTx m (error "no root key", encrypt) [ownedIns] outs

print "Address:"
TIO.putStrLn $ toText addr


BS.writeFile "/tmp/comp/wallet-unsigned.bin" (toByteString $ encodeTx tx)

toByteString $ encodeSignedTx stx

BS.writeFile "/tmp/comp/wallet-tx.bin" (toByteString $ encodeSignedTx stx)
````

## cardano-sl

`stack ghci cardano-wallet --ghc-options=-optl-Wl,-dead_strip_dylibs`, select 2

```
import Cardano.Crypto.Wallet
import Pos.Core.NetworkMagic
import Pos.Crypto.HD
import qualified Data.ByteString as B8
import Pos.Crypto.Hashing
import Pos.Chain.Txp
import Pos.Binary.Class

import qualified Data.List.NonEmpty as NE

let nm = NetworkTestnet 1097911063
let pm = ProtocolMagic (ProtocolMagicId 1097911063) RequiresMagic

let pass = PassPhrase mempty
let gen = mempty :: BS.ByteString 
let seed = (BS.pack $ replicate 128 0)

let xprv = CC.generateNew seed gen pass
esk <- mkEncSecretUnsafe pass xprv
let signer = SafeSigner esk pass 

let shuffle = return

let addrXPub = encToPublic esk
let addr = makePubKeyAddressBoot nm addrXPub

-- Construct tx:
let fromR = either undefined id
let inp = (TxInUtxo (unsafeHash "faucetTx") 0, TxOutAux $ TxOut addr (Coin 1))
r <- mkStdTx pm shuffle (const $ Right signer) (NE.fromList [inp]) (NE.fromList [snd inp]) []
let stx = fromR r

print "Address:"

toLazyByteString $ encode addr


let tx = UnsafeTx (NE.fromList [fst inp]) (NE.fromList [toaOut  $ snd inp]) attr
BL.writeFile "/tmp/comp/sl-unsigned.bin" (toLazyByteString $ encode tx)


toLazyByteString $ encode stx

import qualified Data.ByteString.Lazy as BL
BL.writeFile "/tmp/comp/sl-tx.bin" (toLazyByteString $ encode stx)
```
