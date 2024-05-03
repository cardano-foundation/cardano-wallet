module Cardano.Wallet.Deposit.PureSpec
    ( spec
    ) where

import Prelude

import Cardano.Crypto.Wallet
    ( XPub
    , generate
    , toXPub
    )
import Cardano.Wallet.Primitive.Types.Tx.TxSeq
    ( toTxList
    )
import Cardano.Wallet.Primitive.Types.Tx.TxSeq.Gen
    ( genTxSeq
    , getTxSeq
    )
import Cardano.Wallet.Primitive.Types.UTxO.Gen
    ( genUTxO
    )
import Data.Maybe
    ( isJust
    )
import Test.Hspec
    ( Spec
    , describe
    , it
    )
import Test.QuickCheck
    ( Gen
    , Property
    , checkCoverage
    , cover
    , elements
    , forAll
    , property
    , suchThat
    )

import qualified Cardano.Wallet.Deposit.Pure as Wallet
import qualified Cardano.Wallet.Deposit.Read as Read
import qualified Data.ByteString.Char8 as B8

spec :: Spec
spec = do
    describe "rollForwardOne" $
        it "adds initial UTxO"
            prop_rollForwardOne_UTxO

{-----------------------------------------------------------------------------
    Properties
------------------------------------------------------------------------------}
prop_rollForwardOne_UTxO
    :: Property
prop_rollForwardOne_UTxO =
    checkCoverage
    $ forAll (genBlock genAddress) $ \block ->
        -- The wallet has a nonzero balance most of the time
        -- FIXME: Should have all the time?
        cover 50 (hasFunds $ Wallet.rollForwardOne block w1)
            "has balance" (property True)
  where
    w0 = Wallet.fromXPubAndGenesis xpub 0 (error "no genesis data")
    hasFunds w = mempty /= Wallet.availableBalance w

    (addr1, w1) = Wallet.createAddress 1 w0
    genAddress :: Gen Read.Address
    genAddress = elements
        [ Read.fromRawAddress $ B8.pack "this is not a real address"
        , Read.fromRawAddress $ B8.pack "also not a real address"
        , Read.fromRawAddress $ B8.pack "this is a mock address"
        , addr1
        ]

xpub :: XPub
xpub =
    toXPub
    $ generate (B8.pack "random seed for a testing xpub lala") B8.empty

hasOutputs :: Read.Tx -> Bool
hasOutputs tx =
    if Read.txScriptInvalid tx
        then isJust $ Read.collateralOutput tx
        else not . null $ Read.outputs tx

haveSomeOutputs :: Read.Block -> Bool
haveSomeOutputs = any hasOutputs . Read.transactions

genBlock :: Gen Read.Address -> Gen Read.Block
genBlock genAddress =
    (mkBlock <$> genTxs) `suchThat` haveSomeOutputs
  where
    genTxs = toTxList . getTxSeq <$> genTxSeq genUTxO genAddress
    mkBlock transactions =
        Read.Block
            { Read.blockHeader = Read.dummyBHeader
            , Read.transactions = transactions
            }
