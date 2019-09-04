# Bech32 library

The library implements Bech32 which is a segwit address format specified by [BIP-0173](https://github.com/bitcoin/bips/blob/master/bip-0173.mediawiki).
Basically, it is a checksummed base32 format and a standard for native segregated witness output addresses using it.

# How to use it

See the hello world example below:

``` haskell
λ import Codec.Binary.Bech32
λ import Prelude
λ let (Right hrp) = humanReadablePartFromText "ca"
λ hrp
HumanReadablePart "ca"
λ let addr = "835823f15b7085fe1efb6af305e65da62bcf5226779a91dd3130b155cb697782b1" :: Text
λ import Data.Text.Encoding
λ let data = dataPartFromBytes (encodeUtf8 addr)
λ data
DataPart "8qen2wpjxdnrzdtzxucrsdtxv5ck2enzxeskvvesx4jnvdtyvymrycnrvc6nyv3kxumnjcfex9jxgve3xvcxyvf4x43kyd3exumnsvnzxy"
λ let bech32encoded = encodeLenient hrp data
"ca18qen2wpjxdnrzdtzxucrsdtxv5ck2enzxeskvvesx4jnvdtyvymrycnrvc6nyv3kxumnjcfex9jxgve3xvcxyvf4x43kyd3exumnsvnzxys57suf"
λ decodeLenient bech32encoded
Right (HumanReadablePart "ca",DataPart "8qen2wpjxdnrzdtzxucrsdtxv5ck2enzxeskvvesx4jnvdtyvymrycnrvc6nyv3kxumnjcfex9jxgve3xvcxyvf4x43kyd3exumnsvnzxy")
```

For more, look at the test directory.
