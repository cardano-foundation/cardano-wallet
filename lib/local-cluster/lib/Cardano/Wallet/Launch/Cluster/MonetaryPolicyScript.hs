{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}

module Cardano.Wallet.Launch.Cluster.MonetaryPolicyScript
    ( genMonetaryPolicyScript
    , writeMonetaryPolicyScriptFile
    )
where

import Prelude

import Cardano.Wallet.Launch.Cluster.CardanoCLI
    ( cli
    , cliLine
    )
import Cardano.Wallet.Launch.Cluster.ClusterM
    ( ClusterM
    )
import Control.Monad.IO.Class
    ( MonadIO (..)
    )
import Data.Aeson
    ( object
    , (.:)
    , (.=)
    )
import Data.Generics.Labels
    ()
import Data.Tagged
    ( Tagged (..)
    , untag
    )
import Data.Text
    ( Text
    )
import System.FilePath
    ( (<.>)
    , (</>)
    )
import UnliftIO.Exception
    ( throwString
    )

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.Text as T

-- | For creating test fixtures. Returns PolicyId, signing key, and verification
-- key hash, all hex-encoded. Files are put in the given directory.
genMonetaryPolicyScript
    :: Tagged "output" FilePath
    -> ClusterM (String, (String, String))
genMonetaryPolicyScript outputDir = do
    let policyPub = untag outputDir </> "policy.pub"
    let policyPrv = untag outputDir </> "policy.prv"

    cli
        [ "address"
        , "key-gen"
        , "--verification-key-file"
        , policyPub
        , "--signing-key-file"
        , policyPrv
        ]
    skey <- liftIO $ T.unpack <$> readKeyFromFile policyPrv
    vkeyHash <-
        cliLine
            [ "address"
            , "key-hash"
            , "--payment-verification-key-file"
            , policyPub
            ]
    script <- liftIO $ writeMonetaryPolicyScriptFile outputDir vkeyHash
    policyId <-
        cliLine
            [ "transaction"
            , "policyid"
            , "--script-file"
            , untag script
            ]

    pure (policyId, (skey, vkeyHash))

writeMonetaryPolicyScriptFile
    :: Tagged "output" FilePath
    -- ^ Destination directory for script file
    -> String
    -- ^ The script verification key hash
    -> IO (Tagged "policy-script" FilePath)
    -- ^ Returns the filename written
writeMonetaryPolicyScriptFile outputDir keyHash = do
    let scriptFile = untag outputDir </> keyHash <.> "script"
    Aeson.encodeFile scriptFile
        $ object
            [ "type" .= Aeson.String "sig"
            , "keyHash" .= keyHash
            ]
    pure $ Tagged scriptFile

-- | Dig in to a @cardano-cli@ TextView key file to get the hex-encoded key.
readKeyFromFile :: FilePath -> IO Text
readKeyFromFile f = do
    textView <- either throwString pure =<< Aeson.eitherDecodeFileStrict' f
    either throwString pure
        $ Aeson.parseEither
            (Aeson.withObject "TextView" (.: "cborHex"))
            textView
