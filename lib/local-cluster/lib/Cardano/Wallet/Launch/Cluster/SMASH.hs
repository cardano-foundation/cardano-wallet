module Cardano.Wallet.Launch.Cluster.SMASH
 ( withSMASH
 )
 where

import Prelude

import Cardano.Wallet.Launch.Cluster.Logging
    ( ClusterLog (..)
    )
import Cardano.Wallet.Launch.Cluster.PoolRecipe
    ( PoolId (getPoolId)
    , PoolRecipe (PoolRecipe, delisted, operatorKeys, poolMetadata)
    , defaultPoolConfigs
    )
import Control.Lens
    ( (<&>)
    )
import Control.Monad
    ( forM_
    )
import Control.Tracer
    ( Tracer (..)
    , traceWith
    )
import Crypto.Hash.Extra
    ( blake2b256
    )
import Data.Aeson
    ( object
    )
import Data.ByteArray.Encoding
    ( Base (..)
    , convertToBase
    )
import Data.ByteString
    ( ByteString
    )
import Data.Generics.Labels
    ()
import Data.Text
    ( Text
    )
import Data.Text.Encoding
    ( decodeUtf8
    )
import System.Directory
    ( createDirectoryIfMissing
    )
import System.FilePath
    ( (</>)
    )
import Test.Utils.StaticServer
    ( withStaticServer
    )

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

toTextPoolId :: PoolId -> Text
toTextPoolId = decodeUtf8 . convertToBase Base16 . getPoolId

-- | Hash a ByteString using blake2b_256 and encode it in base16
blake2b256S :: ByteString -> String
blake2b256S =
    T.unpack
        . T.decodeUtf8
        . convertToBase Base16
        . blake2b256

-- | Run a SMASH stub server, serving some delisted pool IDs.
withSMASH
    :: Tracer IO ClusterLog
    -> FilePath
    -- ^ Parent directory to store static files
    -> (String -> IO a)
    -- ^ Action, taking base URL
    -> IO a
withSMASH tr parentDir action = do
    let staticDir = parentDir </> "smash"
    let baseDir = staticDir </> "api" </> "v1"

    -- write pool metadatas
    forM_ defaultPoolConfigs $ \pool -> do
        let (poolId, _, _, _) = operatorKeys pool
        let metadata = poolMetadata pool

        let bytes = Aeson.encode metadata

        let metadataDir = baseDir </> "metadata"
            poolDir = metadataDir </> T.unpack (toTextPoolId poolId)
            hash = blake2b256S (BL.toStrict bytes)
            hashFile = poolDir </> hash

        traceWith tr
            $ MsgRegisteringPoolMetadataInSMASH
                (T.unpack $ toTextPoolId poolId)
                hash

        createDirectoryIfMissing True poolDir
        BL8.writeFile (poolDir </> hashFile) bytes

    -- Write delisted pools
    let toSmashId = T.pack . B8.unpack . convertToBase Base16 . getPoolId
    let poolId (PoolRecipe _ _ _ _ (pid, _, _, _) _) = toSmashId pid
    let delistedPoolIds = poolId <$> NE.filter delisted defaultPoolConfigs
    BL8.writeFile
        (baseDir </> "delisted")
        ( Aeson.encode
            $ delistedPoolIds
            <&> \p -> object ["poolId" Aeson..= p]
        )

    -- health check
    let health =
            Aeson.encode
                $ object
                    [ "status" Aeson..= ("OK" :: Text)
                    , "version" Aeson..= ("1.2.0" :: Text)
                    ]
    BL8.writeFile (baseDir </> "status") health

    withStaticServer staticDir action
