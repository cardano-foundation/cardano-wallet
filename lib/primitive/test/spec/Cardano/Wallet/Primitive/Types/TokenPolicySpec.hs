
module Cardano.Wallet.Primitive.Types.TokenPolicySpec
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Primitive.Types.Hash
    ( Hash (..) )
import Cardano.Wallet.Primitive.Types.TokenPolicy
    ( TokenFingerprint (..)
    , TokenName (..)
    , TokenPolicyId (..)
    , mkTokenFingerprint
    )
import Cardano.Wallet.Unsafe
    ( unsafeFromHex )
import Data.ByteString
    ( ByteString )
import Data.Text
    ( Text )
import Test.Hspec
    ( Spec, SpecWith, describe, it, shouldBe )

import qualified Data.Text as T

spec :: Spec
spec = describe "mkAssetFingerprint" $ do
    goldenTestCIP14
        "7eae28af2208be856f7a119668ae52a49b73725e326dc16579dcc373"
        ""
        "asset1rjklcrnsdzqp65wjgrg55sy9723kw09mlgvlc3"

    goldenTestCIP14
        "7eae28af2208be856f7a119668ae52a49b73725e326dc16579dcc37e"
        ""
        "asset1nl0puwxmhas8fawxp8nx4e2q3wekg969n2auw3"

    goldenTestCIP14
        "1e349c9bdea19fd6c147626a5260bc44b71635f398b67c59881df209"
        ""
        "asset1uyuxku60yqe57nusqzjx38aan3f2wq6s93f6ea"

    goldenTestCIP14
        "7eae28af2208be856f7a119668ae52a49b73725e326dc16579dcc373"
        "504154415445"
        "asset13n25uv0yaf5kus35fm2k86cqy60z58d9xmde92"

    goldenTestCIP14
        "1e349c9bdea19fd6c147626a5260bc44b71635f398b67c59881df209"
        "504154415445"
        "asset1hv4p5tv2a837mzqrst04d0dcptdjmluqvdx9k3"

    goldenTestCIP14
        "1e349c9bdea19fd6c147626a5260bc44b71635f398b67c59881df209"
        "7eae28af2208be856f7a119668ae52a49b73725e326dc16579dcc373"
        "asset1aqrdypg669jgazruv5ah07nuyqe0wxjhe2el6f"

    goldenTestCIP14
        "7eae28af2208be856f7a119668ae52a49b73725e326dc16579dcc373"
        "1e349c9bdea19fd6c147626a5260bc44b71635f398b67c59881df209"
        "asset17jd78wukhtrnmjh3fngzasxm8rck0l2r4hhyyt"

    goldenTestCIP14
        "7eae28af2208be856f7a119668ae52a49b73725e326dc16579dcc373"
        "0000000000000000000000000000000000000000000000000000000000000000"
        "asset1pkpwyknlvul7az0xx8czhl60pyel45rpje4z8w"

goldenTestCIP14
    :: ByteString -- Base16-encoded PolicyId
    -> ByteString -- Base16-encoded AssetName
    -> Text -- Bech32-encoded TokenFingerprint
    -> SpecWith ()
goldenTestCIP14 rawPolicyId rawAssetName rawFingerprint =
    it ("golden test CIP-0014 - " <> T.unpack rawFingerprint) $ do
        let policyId = UnsafeTokenPolicyId $ Hash $ unsafeFromHex rawPolicyId
        let assetName = UnsafeTokenName $ unsafeFromHex rawAssetName
        let fingerprint = UnsafeTokenFingerprint rawFingerprint
        mkTokenFingerprint policyId assetName `shouldBe` fingerprint
