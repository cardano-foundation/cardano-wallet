{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
module Cardano.Wallet.TokenMetadataSpec where

import Cardano.Wallet.Primitive.Types.Hash
    ( Hash (..) )
import Cardano.Wallet.Primitive.Types.TokenPolicy
    ( TokenPolicyId (..) )
import Cardano.Wallet.TokenMetadata
import Prelude
import System.FilePath
    ( (</>) )
import Test.Hspec
import Test.Utils.Paths
    ( getTestData )

import qualified Data.Map as Map

spec :: Spec
spec = do
    describe "Token Metadata" $ do
        let dir = $(getTestData) </> "Cardano" </> "Wallet" </> "TokenMetadata"
        describe "tokenMetadataServerFromFile" $ do
            -- From https://github.com/input-output-hk/metadata-server/pull/1/files
            -- with manually added wrapping []
            --
            -- TODO: Check relevance.
            it "golden" $ do
                let fp = dir </> "golden1.json"
                let server = tokenMetadataServerFromFile fp
                m <- fetchTokenMeta server []
                m `shouldBe` Map.fromList
                    [ (UnsafeTokenPolicyId
                        ( Hash "\DELq\148\t\NAK\234_\232^\132\SI\132<\146\
                               \\158\186F~o\ENQ\EOTu\186\209\241\v\156'")
                        ,"SteveToken" )
                    ]


