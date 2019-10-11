{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.HttpBridge.Primitive.TypesSpec
    ( spec
    ) where

import Prelude

import Cardano.Wallet.HttpBridge.Primitive.Types
    ( Tx (..) )
import Cardano.Wallet.Primitive.Types
    ( Address (..)
    , Block (..)
    , BlockHeader (..)
    , Coin (..)
    , Hash (..)
    , SlotId (..)
    , TxIn (..)
    , TxOut (..)
    )
import Data.Quantity
    ( Quantity (..) )
import Data.Text
    ( Text )
import Fmt
    ( pretty )
import Test.Hspec
    ( Spec, describe, it, shouldBe )

spec :: Spec
spec = do
    describe "Buildable" $ do
        it "Block" $ do
            let block = Block
                    { header = BlockHeader
                        { slotId = SlotId 14 19
                        , blockHeight = Quantity 42
                        , headerHash = Hash "http-bridge"
                        , parentHeaderHash = Hash "\223\252\&5\ACK\211\129\&6\DC4h7b'\225\201\&2:/\252v\SOH\DC1\ETX\227\"Q$\240\142ii\167;"
                        }
                    , transactions =
                        [ Tx
                            { inputs =
                                [ TxIn
                                    { inputId = Hash "\194\157>\160\221\163\&4\218\149\215\178\161]p\185\246\208\198\ENQ \188\216\242\160\190\236\137\151\DC3\134\"\DC4"
                                    , inputIx = 0
                                    }
                                ]
                            , outputs =
                                [ TxOut
                                    { address = Address "\130\216\CANXB\131X\FS\147\ACKn\246.n\DLE\233Y\166)\207c\v\248\183\235\212\EOTV\243h\192\190T\150'\196\161\SOHX\RSX\FS\202>U<\156c\197&\DC3S\235C\198\245\163\204=\214fa\201\t\205\248\204\226r%\NUL\SUB\174\187\&7\t"
                                    , coin = Coin 3823755953610
                                    }
                                , TxOut
                                    { address = Address "\130\216\CANXB\131X\FS\ACK\218k\189\250\189\129\229A\128>`V\153\144EyN\187T\\\151 \171;\251(\t\161\SOHX\RSX\FS\197\217I\176.##'\217l\226i{\200'\176\&32I\150\166\SI+\143\138\GS\SOH+\NUL\SUB7\206\156`"
                                    , coin = Coin 19999800000
                                    }
                                ]
                            }
                        ]
                    }
            "68747470-[14.19#42]\n\
            \    - ~> 1st c29d3ea0\n\
            \      <~ 3823755953610 @ 82d81858...aebb3709\n\
            \      <~ 19999800000 @ 82d81858...37ce9c60\n"
                `shouldBe` pretty @_ @Text block
