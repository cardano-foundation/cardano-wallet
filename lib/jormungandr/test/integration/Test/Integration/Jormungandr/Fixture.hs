{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Integration.Jormungandr.Fixture
    ( -- * Test stake pools
      registerStakePool
    , OwnerIdentity(..)
    ) where

import Prelude

import Cardano.Wallet.Primitive.Fee
    ( FeePolicy (..) )
import Cardano.Wallet.Primitive.Types
    ( PoolId (..), PoolOwner (..) )
import Cardano.Wallet.Unsafe
    ( unsafeBech32Decode, unsafeFromHex )
import Control.Concurrent.MVar
    ( MVar, newMVar, putMVar, takeMVar )
import Control.Monad
    ( void )
import Control.Monad.Fail
    ( MonadFail )
import Control.Monad.IO.Class
    ( MonadIO, liftIO )
import System.FilePath
    ( (</>) )
import System.IO
    ( writeFile )
import System.IO.Temp
    ( withSystemTempDirectory )
import System.IO.Unsafe
    ( unsafePerformIO )
import Test.Integration.Framework.Request
    ( Context (..) )
import Test.Integration.Jcli
    ( argHex, argInt, getBlock0H, jcli, jcliStdin, jcliStdin_, jcli_ )

import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T

-- | Pre-defined stake pool owners for pools. One is associated with a public
-- for which there're metadata present in the registry, the other don't.
data OwnerIdentity = WithMetadata | WithoutMetadata

-- | Register a new stake pool on-chain. Owners of type 'WithMetadata' will have
-- metadata in the registry and therefore, should have metadata showing up when
-- listing stake pools. Others should not.
--
-- Note that pools are registered but have no stake. Therefore, they shouldn't
-- really interfer with other test because they shouldn't get elected at all...
registerStakePool
    :: Context t
    -> OwnerIdentity
    -> IO (PoolId, PoolOwner)
registerStakePool ctx owner = withSystemTempDirectory "registration" $ \d -> do
    -- NOTE
    -- We use a brand new faucet to pay for the registration because it is
    -- simpler. Without this, we would have to keep track of the account's
    -- balance and transaction history in order to authenticate the transaction.
    (ownerPrv, ownerPub, ownerAddr) <- nextFaucet owner
    (kesPub, vrfPub) <- createCredentials
    (poolId, registrationCert) <- createCertificate ownerPub (kesPub, vrfPub)
    publishCertificate =<< embedCertificate d (ownerPrv, ownerAddr) registrationCert
    pure (poolId, unsafeMkPoolOwner ownerPub)
  where
    createCredentials = do
        kesPrv <- jcli
            [ "key" , "generate" , "--type", "SumEd25519_12" ]
        vrfPrv <- jcli
            [ "key" , "generate" , "--type", "Curve25519_2HashDH" ]
        kesPub <- jcliStdin kesPrv
            [ "key" , "to-public" ]
        vrfPub <- jcliStdin vrfPrv
            [ "key" , "to-public" ]
        pure (kesPub, vrfPub)

    createCertificate ownerPub (kesPub, vrfPub) = do
        registrationCert <- jcli
            [ "certificate"
            , "new"
            , "stake-pool-registration"
            , "--kes-key", kesPub
            , "--vrf-key", vrfPub
            , "--owner", ownerPub
            , "--start-validity", "0"
            , "--management-threshold", "1"
            ]
        poolId <- jcliStdin registrationCert
            [ "certificate", "get-stake-pool-id" ]
        pure (unsafeMkPoolId poolId, registrationCert)

    embedCertificate d (ownerPrv, ownerAddr) cert = do
        let ownerPrvPath = d </> "owner.prv"
        writeFile ownerPrvPath ownerPrv

        void $ fakePipe
            [ [ "transaction", "new"]
            , [ "transaction", "add-account", ownerAddr, show faucetAmt ]
            , [ "transaction", "add-certificate", cert ]
            , [ "transaction", "finalize", ownerAddr
              , "--fee-constant", argInt feeConstant
              , "--fee-coefficient", argInt feeCoefficient
              , "--fee-pool-registration", argInt feeCertificate
              ]
            ]
        block0H <- argHex <$> getBlock0H

        let witnessPath = d </> "owner.wit"
        [dataForWitness] <- fakePipe
            [ [ "transaction", "data-for-witness" ] ]
        jcli_
            [ "transaction"
            , "make-witness", dataForWitness
            , "--genesis-block-hash", block0H
            , "--type", "account"
            , "--account-spending-counter", "0"
            , witnessPath
            , ownerPrvPath
            ]

        last <$> fakePipe
            [ [ "transaction", "add-witness", witnessPath ]
            , [ "transaction", "seal" ]
            , [ "transaction", "auth", "-k", ownerPrvPath ]
            , [ "transaction", "to-message" ]
            ]
      where
        -- Simplifies reading of commands above by "simulating" a UNIX pipe
        -- passing around the staging file for each command.
        fakePipe  = traverse (\args -> jcli $ args ++ ["--staging", d </> "staging"])
        LinearFee feeConstant feeCoefficient feeCertificate = _feePolicy ctx

    publishCertificate tx = do
        let host = "http://localhost:" <> show (_nodePort ctx) <> "/api"
        jcliStdin_ tx
            [ "rest", "v0", "message", "post"
            , "--host", host
            ]

-- | Pop a faucet from the available faucets. Fails if there's no more.
nextFaucet
    :: (MonadIO m, MonadFail m)
    => OwnerIdentity
    -> m (String, String, String)
nextFaucet owner = liftIO (takeMVar faucet) >>= \case
    [] -> fail "nextFaucet: Awe crap! No more account faucet available!"
    (h:q) -> h <$ liftIO (putMVar faucet q)
  where
    faucet = case owner of
        WithMetadata -> faucetWithMetadata
        WithoutMetadata -> faucetWithoutMetadata

faucetAmt :: Int
faucetAmt = 10000

-- | corresponds with metadata stored in:
-- lib/jormungandr/test/data/jormungandr/stake_pools/registry/test-integration-registry.zip
{-# NOINLINE faucetWithMetadata #-}
faucetWithMetadata :: MVar [(String, String, String)]
faucetWithMetadata = unsafePerformIO $ newMVar
    [  -- ticker: SWIM
      ( "ed25519_sk1qm2de9sa6w5ccvrrh7dh7fgpzeupulsjkz3fdqanm2w7e6h0rgasxex46t"
      , "ed25519_pk1kpudvc46w3nnwjfw5zzsrj7jxqwm4znhltkas8yzx9lcezs5e8cswtvx8t"
      , "account1skc834nzhf6xwd6f96sg2qwt6gcpmw52wlawmkqusgchlry2znylzam4v5a"
      )
      -- ticker: LIVER
    , ( "ed25519_sk103yerx2fq2juzpjda2c0ghz5646cwezd0gvmtxa2jg9qj7hgj0es604myw"
      , "ed25519_pk1577ny3x5etp748l09l594t4lh3tlhxpekalhw0j4fe5ava2jam3shm08mf"
      , "account1sknm6vjy6n9v865lauh7sk4wh77907uc8xmh7ae7248xn4n42thwxlc0c70"
      )
    ]

{-# NOINLINE faucetWithoutMetadata #-}
faucetWithoutMetadata :: MVar [(String, String, String)]
faucetWithoutMetadata = unsafePerformIO $ newMVar
    [ ( "ed25519_sk13q9azfkh99pc05saplefgdj79s0yf8tvhcvefjlj7jqyczxxv2dqk2yvax"
      , "ed25519_pk1y0y0dd3hwhvftkds0j56ywsshkckfpsyf79ywwpl4uyn4dgxsd7sv6mjkq"
      , "account1s53u3a4kxa6a39wekp72ng36zz7mzeyxq38c53ec87hsjw44q6ph6nekre5"
      )
    , ( "ed25519_sk1ll8eg0wfskc5c8qflur3avssg73ety9e6m46ptpsqng8krpcyf7svmnys8"
      , "ed25519_pk1cmh8yeuajjhzzw7lap58mmqkehedvv5tlwhmy70wdayklaufk3gqx2h708"
      , "account1shrwuun8nk22ugfmml5xsl0vzmxl943j30a6lvneaeh5jmlh3x69qfyrr28"
      )
    ]

{-------------------------------------------------------------------------------
                                   Internals
-------------------------------------------------------------------------------}

unsafeMkPoolId :: String -> PoolId
unsafeMkPoolId = PoolId . unsafeFromHex . B8.pack

unsafeMkPoolOwner :: String -> PoolOwner
unsafeMkPoolOwner = PoolOwner . BL.toStrict . unsafeBech32Decode . T.pack
