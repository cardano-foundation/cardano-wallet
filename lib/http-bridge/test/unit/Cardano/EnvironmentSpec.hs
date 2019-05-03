{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.EnvironmentSpec where

import Prelude

import Cardano.Environment
    ( ErrMissingOrInvalidEnvVar (..), Network, unsafeLookupEnv )
import Data.Maybe
    ( isNothing )
import Data.Proxy
    ( Proxy (..) )
import Data.Text.Class
    ( TextDecodingError (..) )
import System.Environment
    ( setEnv, unsetEnv )
import Test.Hspec
    ( Spec, describe, it, shouldThrow )
import Test.QuickCheck
    ( Arbitrary (..) )
import Test.QuickCheck.Arbitrary.Generic
    ( genericArbitrary, genericShrink )
import Test.Text.Roundtrip
    ( textRoundtrip )

spec :: Spec
spec = do
    describe "Can perform roundtrip textual encoding & decoding" $ do
        textRoundtrip $ Proxy @Network

    describe "ErrMissingOrInvalidEnvVar (Show / displayException)" $ do
        let errNoAdditionalContext = ErrMissingOrInvalidEnvVar
                { name = "PATATE"
                , command = "my-command"
                , additionalContext = Nothing
                }
        let errWithAdditionalContext = ErrMissingOrInvalidEnvVar
                { name = "PATATE"
                , command = "my-command"
                , additionalContext = Just
                    ("💩"
                    , TextDecodingError
                        { getTextDecodingError = "not a valid value" }
                    )
                }
        it (show errNoAdditionalContext) True
        it (show errWithAdditionalContext) True

    describe "unsafeLookupEnv" $ do
        it "throws with no context when variable isn't present" $ do
            unsetEnv "PATATE" -- Just in case
            let io =
                    unsafeLookupEnv @Network "PATATE" `seq` (return ())
            let selector (ErrMissingOrInvalidEnvVar n _ c) =
                    n == "PATATE" && isNothing c
            io `shouldThrow` selector

        it "throws with extra context when variable is present but invalid" $ do
            setEnv "PATATE" "not-a-network"
            let ctx =
                    ( "not-a-network"
                    , TextDecodingError "not-a-network is neither \"mainnet\",\
                        \ \"testnet\", \"staging\" nor \"local\"."
                    )
            let selector (ErrMissingOrInvalidEnvVar n _ c) =
                    n == "PATATE" && c == Just ctx
            let io =
                    unsafeLookupEnv @Network "PATATE" `seq` (return ())
            io `shouldThrow` selector

{-------------------------------------------------------------------------------
                               Arbitrary Instances
-------------------------------------------------------------------------------}

instance Arbitrary Network where
    arbitrary = genericArbitrary
    shrink = genericShrink
