module Cardano.Wallet.Byron.CompatibilitySpec
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Byron.Compatibility
    ( toGenTx )
import Cardano.Wallet.Byron.TransactionSpec
    ( goldenMainnet__1_1
    , goldenMainnet__1_25
    , goldenMainnet__25_1
    , goldenMainnet__2_2
    , goldenTestnet__1_1
    , goldenTestnet__1_25
    , goldenTestnet__25_1
    , goldenTestnet__2_2
    )
import Cardano.Wallet.Primitive.Types
    ( SealedTx (..) )
import Cardano.Wallet.Unsafe
    ( unsafeFromHex )
import Control.Monad
    ( forM_ )
import Test.Hspec
    ( Spec, describe, it )

spec :: Spec
spec = do
    describe "Golden Tests - toGenTx (doesn't throw)" $ do
        forM_
            [ goldenMainnet__1_1
            , goldenMainnet__2_2
            , goldenMainnet__1_25
            , goldenMainnet__25_1
            , goldenTestnet__1_1
            , goldenTestnet__2_2
            , goldenTestnet__1_25
            , goldenTestnet__25_1
            ] $ \bytes ->
            let str = show $ toGenTx $ SealedTx $ unsafeFromHex bytes
            in it (take 23 str) (putStrLn str)
