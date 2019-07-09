module Cardano.Wallet.NetworkSpec
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Network
    ( ErrGetBlock (..)
    , ErrNetworkTip (..)
    , ErrNetworkUnreachable (..)
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
        testShow $ ErrNetworkTipNetworkUnreachable
            $ ErrNetworkUnreachable mempty
        testShow ErrNetworkTipNotFound
        testShow $ ErrGetBlockNetworkUnreachable
            $ ErrNetworkUnreachable mempty
        testShow $ ErrGetBlockNotFound $ Hash mempty
        testShow $ ErrPostTxNetworkUnreachable
            $ ErrNetworkUnreachable mempty
        testShow $ ErrPostTxBadRequest mempty
        testShow $ ErrPostTxProtocolFailure mempty

testShow :: Show a => a -> Spec
testShow a = it (show a) True
