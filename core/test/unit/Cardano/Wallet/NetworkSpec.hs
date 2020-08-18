module Cardano.Wallet.NetworkSpec
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Network
    ( ErrCurrentNodeTip (..)
    , ErrGetBlock (..)
    , ErrNetworkUnavailable (..)
    , ErrPostTx (..)
    )
import Cardano.Wallet.Primitive.Types
    ( Hash (..) )
import Test.Hspec
    ( Spec, describe, it )

spec :: Spec
spec = do
    describe "Pointless tests to cover 'Show' instances for errors" $ do
        testShow $ ErrNetworkUnreachable mempty
        testShow $ ErrNetworkInvalid mempty
        testShow $ ErrCurrentNodeTipNetworkUnreachable
            $ ErrNetworkUnreachable mempty
        testShow $ ErrCurrentNodeTipNetworkUnreachable
            $ ErrNetworkInvalid mempty
        testShow ErrCurrentNodeTipNotFound
        testShow $ ErrGetBlockNetworkUnreachable
            $ ErrNetworkUnreachable mempty
        testShow $ ErrGetBlockNetworkUnreachable
            $ ErrNetworkInvalid mempty
        testShow $ ErrGetBlockNotFound $ Hash mempty
        testShow $ ErrPostTxNetworkUnreachable
            $ ErrNetworkUnreachable mempty
        testShow $ ErrPostTxNetworkUnreachable
            $ ErrNetworkInvalid mempty
        testShow $ ErrPostTxBadRequest mempty
        testShow $ ErrPostTxProtocolFailure mempty

testShow :: Show a => a -> Spec
testShow a = it (show a) True
