{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Integration.Scenario.API.Shelley.Restoration
    ( spec
    ) where

import Cardano.Wallet.Api.Types
    ( ApiT (..)
    )
import Cardano.Wallet.Api.Types.BlockHeader
    ( ApiBlockHeader (..)
    )
import Cardano.Wallet.Network.RestorationMode
    ( RestorationMode (..)
    )
import Cardano.Wallet.Primitive.Types.Hash
    ( toRawHeaderHash
    )
import Cardano.Wallet.Unsafe
    ( unsafeFromText
    )
import Data.Generics.Product
    ( HasField
    )
import Data.Quantity
    ( Quantity (..)
    )
import Test.Hspec
    ( SpecWith
    , describe
    )
import Test.Integration.Framework.DSL
    ( Context
    , eventually
    )
import Test.Integration.Framework.DSL.Network
    ( waitSomeEpochs
    )
import Test.Integration.Framework.DSL.TestM
    ( clientError
    , errResponseBody
    , errStatusIs
    , fieldIs
    , itM
    , over
    , request
    , pattern Partial
    )
import Test.Integration.Framework.DSL.Wallet
    ( Patch
    , balanceIs
    , createARandomWallet
    , createARandomWalletWithMnemonics
    , createWalletFromMnemonics
    , createWalletFromXPub
    , deleteWallet
    , fundWallet
    , named
    , waitUntilStateIsReady
    , withApiWallet
    , withRestorationMode
    , xPubOfMnemonics
    )
import Prelude

import qualified Cardano.Wallet.Api.Clients.Network as C
import qualified Cardano.Wallet.Read as Read

spec :: SpecWith Context
spec = describe "restoration of wallets" $ do
    itM "WALLET_RESTORE_0.1 create a wallet restoring from tip" $ do
        Partial w <-
            createARandomWallet $ named "Wallet from tip" . restoringFromTip
        over w $ do
            waitUntilStateIsReady
            withApiWallet $ balanceIs 0

    itM "WALLET_RESTORE_0.2 create a wallet restoring from genesis" $ do
        Partial w <-
            createARandomWallet
                $ named "Wallet from genesis" . restoringFromGenesis
        over w $ do
            waitUntilStateIsReady
            withApiWallet $ balanceIs 0

    itM "WALLET_RESTORE_0.3 create a wallet restoring from a checkpoint" $ do
        Partial cp <- request C.blocksLatestHeader
        Partial w <-
            eventually "Wallet from a block"
                $ createARandomWallet
                $ named "Wallet from a block" . restoringFromCheckpoint cp
        over w $ do
            waitUntilStateIsReady
            withApiWallet $ balanceIs 0

    itM
        "WALLET_RESTORE_0.4 create a wallet from the tip \
        \ignores past transactions"
        $ do
            Partial (genesisClone, mnemonics) <-
                createARandomWalletWithMnemonics
                    $ named "Wallet from genesis" . restoringFromGenesis
            over genesisClone $ do
                waitUntilStateIsReady
                fundWallet 42_000_000
                withApiWallet $ balanceIs 42_000_000
                deleteWallet
            Partial tipClone <-
                createWalletFromMnemonics mnemonics
                    $ named "Wallet from tip" . restoringFromTip
            over tipClone $ do
                waitUntilStateIsReady
                withApiWallet $ balanceIs 0

    itM
        "WALLET_RESTORE_0.5 create a wallet from the genesis \
        \capture the past transactions"
        $ do
            Partial (genesisClone, mnemonics) <-
                createARandomWalletWithMnemonics
                    $ named "Wallet from genesis" . restoringFromTip
            over genesisClone $ do
                waitUntilStateIsReady
                fundWallet 42_000_000
                withApiWallet $ balanceIs 42_000_000
                deleteWallet
            Partial tipClone <-
                createWalletFromMnemonics mnemonics
                    $ named "Wallet from genesis second take" . restoringFromGenesis
            over tipClone $ do
                waitUntilStateIsReady
                withApiWallet $ balanceIs 42_000_000

    itM
        "WALLET_RESTORE_0.6 create a wallet from checkpoint \
        \capture the past transactions"
        $ do
            Partial (genesisClone, mnemonics) <-
                createARandomWalletWithMnemonics
                    $ named "Wallet from genesis" . restoringFromTip
            over genesisClone $ do
                waitUntilStateIsReady
            Partial cp <- request C.blocksLatestHeader
            waitSomeEpochs 2
            over genesisClone $ do
                fundWallet 42_000_000
                withApiWallet $ balanceIs 42_000_000
                deleteWallet
            Partial tipClone <-
                createWalletFromMnemonics mnemonics
                    $ named "Wallet from a block" . restoringFromCheckpoint cp
            over tipClone $ do
                waitUntilStateIsReady
                withApiWallet $ balanceIs 42_000_000

    itM
        "WALLET_RESTORE_0.7 create a wallet from checkpoint \
        \ignores past transactions"
        $ do
            Partial (genesisClone, mnemonics) <-
                createARandomWalletWithMnemonics
                    $ named "Wallet from genesis" . restoringFromTip
            over genesisClone $ do
                waitUntilStateIsReady
                fundWallet 42_000_000
                withApiWallet $ balanceIs 42_000_000
                deleteWallet
            waitSomeEpochs 2
            Partial cp <- request C.blocksLatestHeader
            Partial tipClone <-
                createWalletFromMnemonics mnemonics
                    $ named "Wallet from a block" . restoringFromCheckpoint cp
            over tipClone $ do
                waitUntilStateIsReady
                withApiWallet $ balanceIs 0
    itM
        "WALLET_RESTORE_0.8 fail to create a wallet from an \
        \ invalid checkpoint"
        $ do
            let cp =
                    ApiBlockHeader
                        { slotNo = Quantity 0
                        , blockHeight = Quantity 0
                        , headerHash =
                            unsafeFromText
                                "39d89a1e837e968ba35370be47cdfcbfd193cd992fdeed557b77c49b77ee59cf"
                        }
            w <- createARandomWallet (restoringFromCheckpoint cp)
            clientError w $ do
                errStatusIs 404
                errResponseBody
                    $ "message"
                        `fieldIs` "Restoration from a given block failed. \
                                  \The block at slot number 0 \
                                  \and hash 39d89a1e837e968ba35370be47cdfcbfd193cd992fdeed557b77c49b77ee59cf \
                                  \does not exist."
    itM
        "WALLET_RESTORE_0.9 create an account wallet from the tip \
        \ignores past transactions"
        $ do
            Partial (genesisClone, mnemonics) <-
                createARandomWalletWithMnemonics
                    $ named "Account from genesis" . restoringFromTip
            let xpub = xPubOfMnemonics mnemonics
            over genesisClone $ do
                waitUntilStateIsReady
                fundWallet 42_000_000
                withApiWallet $ balanceIs 42_000_000
                deleteWallet
            Partial tipClone <-
                createWalletFromXPub xpub
                    $ named "Account from tip" . restoringFromTip
            over tipClone $ do
                waitUntilStateIsReady
                withApiWallet $ balanceIs 0
    itM
        "WALLET_RESTORE_0.10 create an account wallet from the genesis \
        \capture the past transactions"
        $ do
            Partial (genesisClone, mnemonics) <-
                createARandomWalletWithMnemonics
                    $ named "Account from genesis" . restoringFromTip
            let xpub = xPubOfMnemonics mnemonics
            over genesisClone $ do
                waitUntilStateIsReady
                fundWallet 42_000_000
                withApiWallet $ balanceIs 42_000_000
                deleteWallet
            Partial tipClone <-
                createWalletFromXPub xpub
                    $ named "Account from genesis second take" . restoringFromGenesis
            over tipClone $ do
                waitUntilStateIsReady
                withApiWallet $ balanceIs 42_000_000
    itM
        "WALLET_RESTORE_0.11 create an account wallet from checkpoint \
        \capture the past transactions"
        $ do
            Partial (genesisClone, mnemonics) <-
                createARandomWalletWithMnemonics
                    $ named "Account from genesis" . restoringFromTip
            let xpub = xPubOfMnemonics mnemonics
            over genesisClone $ do
                waitUntilStateIsReady
            Partial cp <- request C.blocksLatestHeader
            waitSomeEpochs 2
            over genesisClone $ do
                fundWallet 42_000_000
                withApiWallet $ balanceIs 42_000_000
                deleteWallet
            Partial tipClone <-
                createWalletFromXPub xpub
                    $ named "Account from a block" . restoringFromCheckpoint cp
            over tipClone $ do
                waitUntilStateIsReady
                withApiWallet $ balanceIs 42_000_000

restoringFromTip
    :: HasField "restorationMode" a a b (Maybe (ApiT RestorationMode))
    => Patch a
restoringFromTip = withRestorationMode RestoreFromTip

restoringFromGenesis
    :: HasField "restorationMode" a a b (Maybe (ApiT RestorationMode))
    => Patch a
restoringFromGenesis = withRestorationMode RestoreFromGenesis

restoringFromCheckpoint
    :: HasField "restorationMode" a a b (Maybe (ApiT RestorationMode))
    => ApiBlockHeader
    -> Patch a
restoringFromCheckpoint cp =
    withRestorationMode
        $ RestoreFromBlock
            (Read.SlotNo $ fromIntegral $ getQuantity $ slotNo cp)
            (toRawHeaderHash $ headerHash cp)
