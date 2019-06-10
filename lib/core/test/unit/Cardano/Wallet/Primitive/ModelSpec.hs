{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.Primitive.ModelSpec
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Primitive.AddressDiscovery
    ( IsOurs (..) )
import Cardano.Wallet.Primitive.Model
    ( applyBlock
    , applyBlocks
    , availableBalance
    , currentTip
    , getState
    , initWallet
    , totalBalance
    , totalUTxO
    )
import Cardano.Wallet.Primitive.Types
    ( Address (..)
    , Block (..)
    , BlockHeader (..)
    , Coin (..)
    , Direction (..)
    , Dom (..)
    , Hash (..)
    , ShowFmt (..)
    , SlotId (..)
    , Tx (..)
    , TxId (..)
    , TxId (..)
    , TxIn (..)
    , TxMeta (direction)
    , TxOut (..)
    , UTxO (..)
    , balance
    , excluding
    , invariant
    , restrictedTo
    , txIns
    )
import Control.DeepSeq
    ( NFData (..) )
import Control.Monad
    ( foldM )
import Control.Monad.Trans.State.Strict
    ( State, evalState, runState, state )
import Data.Bifunctor
    ( bimap )
import Data.Maybe
    ( catMaybes )
import Data.Set
    ( Set, (\\) )
import Data.Traversable
    ( for )
import GHC.Generics
    ( Generic )
import Test.Hspec
    ( Spec, describe, it )
import Test.QuickCheck
    ( Arbitrary (..)
    , Property
    , checkCoverage
    , choose
    , cover
    , elements
    , genericShrink
    , listOf
    , property
    , shrinkList
    , (.&&.)
    , (===)
    )

import qualified Data.ByteString.Char8 as B8
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set


spec :: Spec
spec = do
    describe "Buildable instances examples" $ do
        let block = blockchain !! 1
        let utxo = utxoFromTx $ head $ transactions block
        it (show $ ShowFmt utxo) True
        it (show $ ShowFmt block) True

    describe "Compare Wallet impl. with Specification" $ do
        it "Lemma 3.2 - dom u ⋪ updateUTxO b u = new b"
            (checkCoverage prop_3_2)

        it "applyBlock matches the basic model from the specification"
            (checkCoverage prop_applyBlockBasic)

    describe "Extra Properties" $ do
        it "Incoming transactions have output addresses that belong to the wallet"
            (property prop_applyBlockTxHistoryIncoming)

        it "Apply Block move the current tip forward"
            (property prop_applyBlockCurrentTip)


{-------------------------------------------------------------------------------
                                Properties
-------------------------------------------------------------------------------}

prop_3_2
    :: ApplyBlock
    -> Property
prop_3_2 (ApplyBlock s utxo block) =
    cover 75 cond "utxo ≠ ∅ " (property prop)
  where
    cond = utxo /= mempty
    prop =
        ShowFmt (updateUTxO' block utxo `excluding` dom utxo)
            ===
        ShowFmt (new block)
    new b = flip evalState s $ do
        let txs = Set.fromList $ transactions b
        utxo' <- (foldMap utxoFromTx txs `restrictedTo`) <$> state (txOutsOurs txs)
        return $ utxo' `excluding` txIns txs
    updateUTxO' b u = evalState (updateUTxO b u) s


prop_applyBlockBasic
    :: WalletState
    -> Property
prop_applyBlockBasic s =
    cover 90 cond0 "ours ≠ ∅ " $
    cover 90 cond1 "addresses \\ ours ≠ ∅ " $
        property prop
  where
    cond0 = not $ null $ ourAddresses s
    cond1 = not $ null $ (Set.fromList addresses) \\ (ourAddresses s)
    prop =
        let
            cp0 = initWallet @_ @DummyTarget block0 s
            wallet = foldl (\cp b -> snd $ applyBlock b cp) cp0 blockchain
            utxo = totalUTxO wallet
            utxo' = evalState (foldM (flip updateUTxO) mempty blockchain) s
        in
            (ShowFmt utxo === ShowFmt utxo') .&&.
            (availableBalance wallet === balance utxo') .&&.
            (totalBalance wallet === balance utxo')


-- Each transaction must have at least one output belonging to us
prop_applyBlockTxHistoryIncoming :: WalletState -> Property
prop_applyBlockTxHistoryIncoming s =
    property (outs (filter isIncoming txs) `overlaps` ourAddresses s')
  where
    cp0 = initWallet @_ @DummyTarget block0 s
    (txs, s') = bimap Map.elems getState $ applyBlocks blockchain cp0
    isIncoming (_, m) = direction m == Incoming
    outs = Set.fromList . concatMap (map address . outputs . fst)
    overlaps a b
        | a == mempty && b == mempty = True
        | otherwise = not (Set.disjoint a b)

-- | Apply blocks move current tip forward
prop_applyBlockCurrentTip :: ApplyBlock -> Property
prop_applyBlockCurrentTip (ApplyBlock s _ b) =
    property $ currentTip wallet' > currentTip wallet
  where
    wallet = initWallet @_ @DummyTarget block0 s
    wallet' = snd $ applyBlock b wallet

{-------------------------------------------------------------------------------
               Basic Model - See Wallet Specification, section 3

   Our implementation of 'applyBlock' is a bit more complex than the basic
   model. In practice, we do not want to compute intersection and tx id of a
   whole block of transactions, but we only do it for the one that are relevant
   to us.
   Plus, we are tracking more than just the UTxO. However, when it comes to UTxO
   the basic model and our implementation should be "on-par" and therefore,
   given a few blocks, we should be able to control that they are indeed.
-------------------------------------------------------------------------------}

-- Update UTxO as described in the formal specification, Fig 3. The basic model
updateUTxO
    :: IsOurs s
    => Block
    -> UTxO
    -> State s UTxO
updateUTxO !b utxo = do
    let txs = Set.fromList $ transactions b
    utxo' <- (foldMap utxoFromTx txs `restrictedTo`) <$> state (txOutsOurs txs)
    return $ (utxo <> utxo') `excluding` txIns txs

-- | Return all transaction outputs that are ours. This plays well within a
-- 'State' monad.
--
-- @
-- myFunction :: Block -> State s Result
-- myFunction b = do
--    ours <- state $ txOutsOurs (transaction b)
--    return $ someComputation ours
-- @
txOutsOurs
    :: forall s. (IsOurs s)
    => Set Tx
    -> s
    -> (Set TxOut, s)
txOutsOurs txs =
    runState $ Set.fromList <$> forMaybe (foldMap outputs txs) pick
  where
    pick :: TxOut -> State s (Maybe TxOut)
    pick out = do
        predicate <- state $ isOurs (address out)
        return $ if predicate then Just out else Nothing
    forMaybe :: Monad m => [a] -> (a -> m (Maybe b)) -> m [b]
    forMaybe xs = fmap catMaybes . for xs

-- | Construct a UTxO corresponding to a given transaction. It is important for
-- the transaction outputs to be ordered correctly, since they become available
-- inputs for the subsequent blocks.
utxoFromTx :: Tx -> UTxO
utxoFromTx tx@(Tx _ outs) =
    UTxO $ Map.fromList $ zip (TxIn (txId @DummyTarget tx) <$> [0..]) outs


{-------------------------------------------------------------------------------
                                  Test Data

    In practice, we may want to generate arbitrary valid sequences of block.
    This isn't trivial though because we would need to generate _valid_ chains
    for various invariants and preconditions to hold. Work have been done in
    cardano-sl to generate such chains, and we may want to use that at some
    point. For now, a valid chain coming from the testnet will do

-------------------------------------------------------------------------------}

data DummyTarget

instance TxId DummyTarget where
    txId = Hash . B8.pack . show

-- | An arbitrary wallet state that can recognize some hard-coded addresses from
-- our chain. This allows us to control that the UTxO gets updated accordingly
-- for some arbitrary instances of that state.
data WalletState = WalletState
    { _ourAddresses :: Set (ShowFmt Address)
    , _discoveredAddresses :: Set (ShowFmt Address)
    } deriving (Generic, Show)

ourAddresses :: WalletState -> Set Address
ourAddresses =
    Set.map (\(ShowFmt a) -> a) . _ourAddresses

instance NFData WalletState

instance Semigroup WalletState where
    (WalletState ours a) <> (WalletState ours' b) =
        invariant "Semigroup WalletState must be defined on same addresses"
            (WalletState ours (a <> b))
            (\_ -> ours == ours')

instance IsOurs WalletState where
    isOurs addr s@(WalletState ours discovered) =
        if (ShowFmt addr) `elem` ours then
            (True, WalletState ours (Set.insert (ShowFmt addr) discovered))
        else
            (False, s)

instance Arbitrary WalletState where
    shrink = genericShrink
    arbitrary = do
        knownAddresses <- Set.fromList <$> listOf arbitrary
        return $ WalletState knownAddresses mempty

instance Arbitrary (ShowFmt Address) where
    shrink _ = []
    arbitrary = ShowFmt <$> elements addresses

-- | Since it's quite tricky to generate a valid Arbitrary chain and
-- corresponding initial UTxO, instead, we take subset of our small valid
-- blockchain and, reconstruct a valid initial UTxO by applying all the given
-- blocks minus one. Then, we control the property when applying that very block
data ApplyBlock = ApplyBlock WalletState UTxO Block
    deriving Show

instance Arbitrary ApplyBlock where
    shrink (ApplyBlock s (UTxO utxo) b) =
        let utxos = UTxO . Map.fromList <$> shrinkList pure (Map.toList utxo)
        in (\u -> ApplyBlock s u b) <$> utxos
    arbitrary = do
        n <- choose (1, length blockchain)
        s <- arbitrary
        let blocks = NE.fromList (take n blockchain)
        let utxo = evalState (foldM (flip updateUTxO) mempty (NE.init blocks)) s
        let block = NE.last blocks
        return $ ApplyBlock s utxo block


addresses :: [Address]
addresses = map address
    $ concatMap outputs
    $ concatMap transactions
    blockchain

block0 :: BlockHeader
block0 = BlockHeader
    { slotId = SlotId 0 0
    , blockHash = Hash "block0"
    , prevBlockHash = Hash "genesis"
    }

-- A excerpt of mainnet, epoch #14, first 20 blocks; plus a few previous blocks
-- which contains transactions referred to in the former. This is useful to test
-- correct resolution of the tx history.
blockchain :: [Block]
blockchain =
    [ Block
        { header = BlockHeader
            { slotId = SlotId 2 19218
            , blockHash = Hash "R\150\234t\210\165l\ETBs\ETX\190\156IfK\204\&4\184i\136\DC2\EOT\SOH5\205\191\229\178\RS\242\ESC\151"
            , prevBlockHash = Hash "y\130\145\211\146\234S\221\150\GS?\212>\167B\134C\r\160J\230\173\SOHn\188\245\141\151u\DC4\236\154"
            }
        , transactions =
            [ Tx
                { inputs =
                    [ TxIn
                        { inputId = Hash "\199D\198\229\227\196\204\231\178\166m\226\134\211\DC1}\243[\204\DC4\171\213\230\246\SOHy\229\t\167\184\235g"
                        , inputIx = 0
                        }
                    ,TxIn
                        { inputId = Hash "\a\241.\180(\a\148\201u$\229\251\147\224\f\166\159\EOT\166m\US\178dN\242\227\b\254\227G\169\RS"
                        , inputIx = 0
                        }
                    ]
                , outputs =
                    [ TxOut
                        { address = Address "\130\216\CANXB\131X\FS\251\STX\v\235\129\179\243k\185\131Eq\190\239\137\143\ETB\167\&7\GS\131\&1\215R\202!\US\205\161\SOHX\RSX\FSq=\137+\197\151g\151-\158\222\RS\246\190\155\EOTz\242\202H\SUB\237\227\167)\fo\198\NUL\SUBw\218X/"
                        , coin = Coin 21063
                        }
                    , TxOut
                        { address = Address "\130\216\CANXB\131X\FS\132X0\144p\144\ENQ\145\&2\224\&3\149hLk\221\152l\142>O\154\210\133\148\211\152\138\161\SOHX\RSX\FS\202>U<\156c\197o\203\t\188C_\254\205\ETXj\237\193\192\144\210KJyU\DEL\240\NUL\SUB\139\185\251\n"
                        , coin = Coin 3844423800000
                        }
                    ]
                }
            ]
        }
    , Block
        { header = BlockHeader
            { slotId = SlotId 13 20991
            , blockHash = Hash "2\US;{6{\214\SI?\206\205\236\208\219DS\244;\145$`\139^\131\235~nY1\145\208?"
            , prevBlockHash = Hash "m\FS\235\ETB6\151'\250M\SUB\133\235%\172\196B_\176n\164k\215\236\246\152\214cc\214\&9\207\142"
            }
        , transactions =
            [ Tx
                { inputs =
                    [ TxIn
                        { inputId = Hash "+\253\232\DC3\132\"M\NULf\EM\228\bh)\STX\171W\215@#\198\a\228\229Z2]\156_fjg"
                        , inputIx = 0
                        }
                    ]
                , outputs =
                    [ TxOut
                        { address = Address "\130\216\CANXB\131X\FS\211Yn9s*R\243\193x\166T\178\189%i\182X\179!\ESC\tf\t;\CAN8\GS\161\SOHX\RSX\FS\202>U<\156c\197M\234W\ETBC\f\177\235\163\254\194\RS\225\ESC\\\244\b\255\164\CAN\201\NUL\SUB\166\230\137["
                        , coin = Coin 3860802399001
                        }
                    , TxOut
                        { address = Address "\130\216\CANXB\131X\FS\149\244~\254\146>\133\160ic\137LqZ\152|N\185\207\CANun\252*\158\\\ACK\NUL\161\SOHX\RSX\FSR\128\f\225\232\SO\196\204\225Dz\SOH\145\129)t\175k\191\148Am\NAK\156\&4\DC2\166q\NUL\SUB\238\180t\198"
                        , coin = Coin 3351830178
                        }
                    ]
                }
            , Tx
                { inputs =
                    [ TxIn
                        { inputId = Hash "\137\150\&8\141\164l\v\ACK\132\198\SI\GS7\201\&3Dd\177fM,\GS)\EM\DC4\242#\211'3\233\163"
                        , inputIx = 0
                        }
                    ]
                , outputs =
                     [ TxOut
                        { address = Address "\130\216\CANXB\131X\FS)/\216\137\&7\187\235\136\159m[g\DC2\156\193v\EM\169^\GS\176\128\rh\186\234\EM\NUL\161\SOHX\RSX\FS\202>U<\156c\197\SYN!\161_C\135\ACK\210/\193|\STX\158f\138C\234\221\RS\134\231\NUL\SUB\190\&2?C"
                        , coin = Coin 3844424216795
                        }
                    , TxOut
                        { address = Address "\130\216\CANXB\131X\FS\ACK\218k\189\250\189\129\229A\128>`V\153\144EyN\187T\\\151 \171;\251(\t\161\SOHX\RSX\FS\197\217I\176.##'\217l\226i{\200'\176\&32I\150\166\SI+\143\138\GS\SOH+\NUL\SUB7\206\156`"
                        , coin = Coin 19999800000
                        }
                    ]
                }
            ]
        }
    , Block
        { header = BlockHeader
            { slotId = SlotId 13 21458
            , blockHash = Hash "\174i\185\141b\197e@8(\241\251\221\FS\196&\131<7\135\174\163\DEL*\NUL\173\176UZ\231\NUL\220"
            , prevBlockHash = Hash "hA\130\182\129\161\&7u8\CANx\218@S{\131w\166\192Bo\131) 2\190\217\134\&7\223\&2>"
            }
        , transactions =
            [ Tx
                { inputs =
                    [ TxIn
                        { inputId = Hash "(\EM#\f\165\236\169=\227\163>MY\225ts\192\SYN\137=\145\155~\212.\252\130l\166v0\SOH"
                        , inputIx = 0
                        }
                    ]
                , outputs =
                    [ TxOut
                        { address = Address "\130\216\CANXB\131X\FS\ACK\142\129o\164[teM\222\&2`\153\STX'\DC4\190\n\194\156:6\DC3\223\184\150[\249\161\SOHX\RSX\FS\202>U<\156c\197\f\132y\163C>\252]w\f\STXb\GS\150\130\255\215`\140\212\CAN\NUL\SUB\135\214\245\224"
                        , coin = Coin 3844425617319
                        }
                    , TxOut
                        { address = Address "\130\216\CANXB\131X\FS\184&\170\193\237\196\242-9\168)Pg\NUL\217\149\&6\169\158U\177c'/\172\221\148\232\161\SOHX\RSX\FS\202>U<\156c\197{\209\173\167C\204n~C\188\169&\217c\212'\131Nm<\150\NUL\SUB=\147\148z"
                        , coin = Coin 3495800000
                        }
                    ]
                }
            , Tx
                { inputs =
                    [ TxIn
                        { inputId = Hash "\128\168muc\212\EMP\238\\\173w\203\159N\205T:\230V\134\164w\143>\192\134\153\SUB$cD"
                        , inputIx = 0
                        }
                    ]
                , outputs =
                    [ TxOut
                        { address = Address "\130\216\CANXB\131X\FSY\128\ETX4\191\170\EOT\144\195#\f]\ESCy\nSe\216+f\132\210\232x\168\160''\161\SOHX\RSX\FS\202>U<\156c\197E\160\162\181C\f|\SO\223\170\DC4\253.R\248R+'\162\172\166\NUL\SUB\220\192\171)"
                        , coin = Coin 3817943388680
                        }
                    , TxOut
                        { address = Address "\130\216\CANXB\131X\FScS\243q\152\237Vv\197\162\RS\168\238\130}\172\&0_=\142n\170]\198EH@l\161\SOHX\RSX\FS(}\ETB\129*k\253\173\&2\177\131V0`\219\243\212*\153\212\159@\128\149\209s\143(\NUL\SUB\"\175\195<"
                        , coin = Coin 29999800000
                        }
                    ]
                }
            ]
        }
    , Block
        { header = BlockHeader
            { slotId = SlotId 13 21586
            , blockHash = Hash "\248\230\r\SI\199\ETX\186E@\241\&5\n\230[\ACK\179\175\238<\DC1\190>\252\185v\249g\141\188t$y"
            , prevBlockHash = Hash "D\152\178<\174\160\225\230w\158\194-$\221\212:z\DC1\255\239\220\148Q!\220h+\134\220\195e5"
            }
        , transactions =
            [ Tx
                { inputs =
                    [ TxIn
                        { inputId = Hash "\164\254\137\218h\f\DLE\245\141u\SYN\248~\253n;\202\144\150\v\229\177\218\195\238\157\230\158\241O\153\215"
                        , inputIx = 0
                        }
                    ]
                , outputs =
                    [ TxOut
                        { address = Address "\130\216\CANXB\131X\FSJ:Kh-\227hW$\139\165\194\192\249\251f\250\NAKf\207\146\131\193\248\242%\153\180\161\SOHX\RSX\FS\202>U<\156c\197\US\196\DC3\208C*1\176\172\138(\EOTd\b\179\157\135e\171#\136\NUL\SUB)\228M*"
                        , coin = Coin 3844435857860
                        }
                    , TxOut
                        { address = Address "\130\216\CANXB\131X\FS\135:\161F\145\151\189z\134\231\254\143\134\129\227I\251\193\129\&8\161\208\236\US[\203e\211\161\SOHX\RSX\FS\142\&2M\NAK\156,\206r\v\237\129;u\168\&3\215\243Kyd\143\EM0\240\182\DC4dE\NUL\SUB\195\DEL\204\176"
                        , coin = Coin 500000000
                        }
                    ]
                }
            ]
        }
    , Block
        { header = BlockHeader
            { slotId = SlotId 14 0
            , blockHash = Hash "-\EOTs+A\208~E\162\184|\ENQ\136\143\149h\ENQ\249K\DLE\143Y\225\255\&1w\134\n\ETB\194\146\219"
            , prevBlockHash = Hash "9\216\154\RS\131~\150\139\163Sp\190G\205\252\191\209\147\205\153/\222\237U{w\196\155w\238Y\207"
            }
        , transactions = []
        }
    , Block
        { header = BlockHeader
            { slotId = SlotId 14 1
            , blockHash = Hash "\233Zn}\163\205a\233#\227\v\EM\152\177\&5\212\tXA\158AW\169\240]/\SI\EMNM{\186"
            , prevBlockHash = Hash "-\EOTs+A\208~E\162\184|\ENQ\136\143\149h\ENQ\249K\DLE\143Y\225\255\&1w\134\n\ETB\194\146\219"
            }
        , transactions =
            [ Tx
                { inputs =
                    [ TxIn
                        { inputId = Hash "\187\199\161\240\222$\bZ\196\138R\238o\137\209\129QE\132Z\135\DC2TsP\167\228\146\&8Yt\171"
                        , inputIx = 0
                        }
                    ]
                , outputs =
                    [ TxOut
                      { address = Address "\130\216\CANXB\131X\FSUh\206'\198\237\161R3\214L\145\245P'\197\230\&6\206\152\173\EOTI:\152\vX&\161\SOHX\RSX\FS\202>U<\156c\197|\227M\202Cv\136\\\253\176\130\185b9G\188_\179\&4\253Y\NUL\SUB\176\EOT\165s"
                      , coin = Coin 3834435886614
                      }
                    , TxOut
                      { address = Address "\130\216\CANXB\131X\FSq4\137\215\171\175Z\ENQ\242\216^\239\197\244^s\230\170}\183}\136\143\218\150\ENQ\137\255\161\SOHX\RSX\FS\173y\SI\234\169\ETB\\\251\238\175\128\178\191a\128\142?(\FSD\148\182\192\250\221\&5;7\NUL\SUB\241\244w\194"
                      , coin = Coin 9999800000
                      }
                    ]
                }
            ]
        }
    , Block
        { header = BlockHeader
            { slotId = SlotId 14 2
            , blockHash = Hash "\181\217p(Z/\133\&4\233A\EM\205c\CAN\136\194\v:N\192pz\130\USm\245\201fP\254\SOH\221"
            , prevBlockHash = Hash "\233Zn}\163\205a\233#\227\v\EM\152\177\&5\212\tXA\158AW\169\240]/\SI\EMNM{\186"
            }
        , transactions =
            [ Tx
                { inputs =
                    [ TxIn
                        { inputId = Hash "s\165\210\a@\213\DC1\224\DLE\144$\DEL\138\202\144\225\229PVBD\ETB25\161\164u\137\NUL{\158v"
                        , inputIx = 0
                        }
                    ]
                , outputs =
                    [ TxOut
                        { address = Address "\130\216\CANXB\131X\FS\255-+\179k\202\194\212\206\224\248\243\158\b\188 \212\141$\189\194&\252\162\166\162jq\161\SOHX\RSX\FS\202>U<\156c\197QM\140\ACKCk=\238\239\134^w\CAN$\253\FSqL\198\128\200\NUL\SUB\f\219\163/"
                        , coin = Coin 3841151724910
                        }
                    , TxOut
                        { address = Address "\130\216\CANXB\131X\FS\n\aGD6\206\202\&2K\n\203%\180\249\227\229\216n\130\218\&6\147\SYN/\SUBq\231\210\161\SOHX\RSX\FS?\DLE\204\131\217-\176\181^\169#?Jn~\137\153\ENQc0<\225\SOH)\DEL\150\163\136\NUL\SUB\b\215\236\238"
                        , coin = Coin 3273721339
                        }
                    ]
                }
            ]
        }
    , Block
        { header = BlockHeader
            { slotId = SlotId 14 3
            , blockHash = Hash "\203\150\255\146\&7(\166~R\223\173T\223\SOH\252Z \199\170\243\134\"j\ENQd\161\CANZ\249y\140\177"
            , prevBlockHash = Hash "\181\217p(Z/\133\&4\233A\EM\205c\CAN\136\194\v:N\192pz\130\USm\245\201fP\254\SOH\221"
            }
        , transactions =
            [ Tx
                { inputs =
                    [ TxIn
                        { inputId = Hash "\177|\163\210\184\169\145\234F\128\209\235\217\148\n\ETXD\155\ESCba\251\230%\213\202\230Y\151&\234A"
                        , inputIx = 0
                        }
                    ]
                , outputs =
                    [ TxOut
                        { address = Address { getAddress = "\130\216\CANXB\131X\FS!\148\NULDcB\r\237\202\255)\DLEe`\159\a\\-IG\"P\218\136\219i\244\134\161\SOHX\RSX\FS\202>U<\156c\197;\236\EOT\STXC\209\173\138\205B\EOT.\ENQ\ACKG@\174\206\185\ESC\206\NUL\SUB\230\150\192\165" }
                        , coin = Coin 3824424245549
                        }
                    , TxOut
                        { address = Address "\130\216\CANXB\131X\FS\ACK\218k\189\250\189\129\229A\128>`V\153\144EyN\187T\\\151 \171;\251(\t\161\SOHX\RSX\FS\197\217I\176.##'\217l\226i{\200'\176\&32I\150\166\SI+\143\138\GS\SOH+\NUL\SUB7\206\156`"
                        , coin = Coin 19999800000
                        }
                    ]
                }
            ]
        }
    , Block
        { header = BlockHeader
            { slotId = SlotId 14 4
            , blockHash = Hash "c\EOT\n\245\237~\178\148\142,\t\164?\148l\145\213\221.\250\161h\187\197\196\243\233\137\207\195\&7\230"
            , prevBlockHash = Hash "\203\150\255\146\&7(\166~R\223\173T\223\SOH\252Z \199\170\243\134\"j\ENQd\161\CANZ\249y\140\177"
            }
        , transactions =  []
        }
    , Block
        { header = BlockHeader
            { slotId = SlotId 14 5
            , blockHash = Hash "\SUB2\224\EM\149\"\\|\213\DC4\224\254P\135\241\154o\213\151\166\a\SUB\212\173\US\191[ \222\&9g\v"
            , prevBlockHash = Hash "c\EOT\n\245\237~\178\148\142,\t\164?\148l\145\213\221.\250\161h\187\197\196\243\233\137\207\195\&7\230"
            }
        , transactions =
            [ Tx
                { inputs =
                    [ TxIn
                        { inputId = Hash "\195\242\DEL-\232v(c\SI+\172\163\245\142\189\214aiB#4\139\172\166\237\167\ETB9\246\150\185\219"
                        , inputIx = 1
                        }
                    , TxIn
                        { inputId = Hash "8O\137\193\224w\243\252s\198\250\201\&04\169\129E\155{\n\DC3H<\199\208\154\214\237\141\128<+"
                        , inputIx = 1
                        }
                    ]
                , outputs =
                    [ TxOut
                        { address = Address "\130\216\CANXB\131X\FS%\ENQ\163x'\DC3\DC1\222\157\197 4*\200v\219\f\201\215\197\136\188\128\243\216\NAKe\214\161\SOHX\RSX\FS\197\217I\176.##LD\224\179i\142\&3\220\162\250\221:F\227\NAK$\156|\EOTY\228\NUL\SUBr\a\134\146"
                        , coin = Coin 15908
                        }
                    , TxOut
                        { address = Address "\130\216\CANXB\131X\FS\SI\DC4f\168\210\188\164\SUBF\239\212\201,\DLE\238\230<r\187A+w\b\222\155\ETB\226m\161\SOHX\RSX\FS\234\DC4%\204\221d\155\200\136\211o~\SOH\229t\229\178p\146\188\214X\237\151T\183\&4\247\NUL\SUBx\242\186\182"
                        , coin = Coin 12999433909
                        }
                    ]
                }
            ]
        }
    , Block
        { header = BlockHeader
            { slotId = SlotId 14 6
            , blockHash = Hash "xU\192\241\SOH\182v\ESC#@X\231\233\253\EM\251\237\159\238\144\162\STX\204\168\153\218\USl\191)Q\141"
            , prevBlockHash = Hash "\SUB2\224\EM\149\"\\|\213\DC4\224\254P\135\241\154o\213\151\166\a\SUB\212\173\US\191[ \222\&9g\v"
            }
        , transactions =  []
        }
    , Block
        { header = BlockHeader
            { slotId = SlotId 14 7
            , blockHash = Hash "\144\a\224Q;\159\234\132\128\&4\167 ;8\f\219\187\166\133\a;\207\183\216\187yQ0\217.{\232"
            , prevBlockHash = Hash "xU\192\241\SOH\182v\ESC#@X\231\233\253\EM\251\237\159\238\144\162\STX\204\168\153\218\USl\191)Q\141"
            }
        , transactions =  []
        }
    , Block
        { header = BlockHeader
            { slotId = SlotId 14 8
            , blockHash = Hash "\n\248\b%\EOT\245\158\177\183\DC1I\129\183\222\233\NUL\144dc\132 8\"\DC1\DC1\135\&0\180Z\211\133\174"
            , prevBlockHash = Hash "\144\a\224Q;\159\234\132\128\&4\167 ;8\f\219\187\166\133\a;\207\183\216\187yQ0\217.{\232"
            }
        , transactions =  []
        }
    , Block
        { header = BlockHeader
            { slotId = SlotId 14 9
            , blockHash = Hash "\173\200\199\GS,\133\206\227\159\187\&4\205\236m\236\162\164\216\206d\147\214\210\143T-\137\GSU\EOT\252\&8"
            , prevBlockHash = Hash "\n\248\b%\EOT\245\158\177\183\DC1I\129\183\222\233\NUL\144dc\132 8\"\DC1\DC1\135\&0\180Z\211\133\174"
            }
        , transactions =  []
        }
    , Block
        { header = BlockHeader
            { slotId = SlotId 14 10
            , blockHash = Hash "O\223\249\241\215Q\219\165\164\139\194\161Mm\251!p\152\130\161=\173I[\133k\247mZ\223K\209"
            , prevBlockHash = Hash "\173\200\199\GS,\133\206\227\159\187\&4\205\236m\236\162\164\216\206d\147\214\210\143T-\137\GSU\EOT\252\&8"
            }
        , transactions =
            [ Tx
                { inputs =
                    [ TxIn
                        { inputId = Hash "\ETXX\189\235\195q81{D\DC3\DLE\228\237(\251\184`l\226\229\184\FSG\132\217\224\202\222\249\246J"
                        , inputIx = 1
                        }
                    ]
                , outputs =
                    [ TxOut
                        { address = Address "\130\216\CANXB\131X\FS\DC3\136\135t\199V\160\217\173\r\235\229\193\&03q{\178'\f\DLE\137k\222P\180\DC3\224\161\SOHX\RSX\FS\202>U<\156c\197<\211\197>C_\207\225?\146\134\160\ETB\207!X\139\250N\220\ESC\NUL\SUB\186\217]\175"
                        , coin = Coin 3827577253906
                        }
                    , TxOut
                        { address = Address { getAddress = "\130\216\CANXB\131X\FS\167\219!{\ETX\157lP>i~\158\225\DEL\141!.I\248\"\183(\DC13\231\185pU\161\SOHX\RSX\FS\SOH\131\136&\ESC\236\240\200\rw\255.\153\252\&6'\174\159vs\CAN\255\153\USf\155\173\223\NUL\SUB\214\237\RS\248" }
                        , coin = Coin 16837395907
                        }
                    ]
                }
            , Tx
                { inputs =
                    [ TxIn
                        { inputId = Hash "\151\146\133\SYN\187\ENQ\252\226\&4\210n\153\178+.h\200\CANAs\SI\181\189\GS\131[g7O\GS\232\215"
                        , inputIx = 1
                        }
                    ]
                , outputs =
                    [ TxOut
                        { address = Address "\130\216\CANXB\131X\FS!f\151\SYN\189\218\167\236\206\253\&9UW%\CAN\238\139\205<\246\132\&1\SOH\164\SUBR\237\DC4\161\SOHX\RSX\FS\202>U<\156c\197T\188\198\219C5_\246\194@\227\217\151\235\139\216(2p\173\236\NUL\SUB0\147sX"
                        , coin = Coin 3843675297120
                        }
                    , TxOut
                        { address = Address "\130\216\CANXB\131X\FS\ETBb\215X\172)\244\139Tp\DC4b\194\DC3\SUB\157\STXqr\172/\175q\244\153\140\214`\161\SOHX\RSX\FSCGQbc\253u+\vF\192XT\185\233e\150}\173\139\199\CAN\215\134\159\166\GS\216\NUL\SUBA}\137A"
                        , coin = Coin 748331810
                        }
                    ]
                }
            ]
        }
    , Block
        { header = BlockHeader
            { slotId = SlotId 14 11
            , blockHash = Hash "\150\163\SUB|\219A\n\235WV\221\180>\226\221\180\198\130\246\&0\141\179\131\DLE\171T\191\&8\184\157k\r"
            , prevBlockHash = Hash "O\223\249\241\215Q\219\165\164\139\194\161Mm\251!p\152\130\161=\173I[\133k\247mZ\223K\209"
            }
        , transactions =
            [ Tx
                { inputs =
                    [ TxIn
                        { inputId = Hash "_>\240.\159\145\US\NUL1\158r\231\&8\214\241\134\&2\DC4\ETB\160\134\237z\143D\229d\DC4\245\208\DC3?"
                        , inputIx = 0
                        }
                    ]
                , outputs =
                    [ TxOut
                        { address = Address "\130\216\CANXB\131X\FS\233\219\220^Zp\135\EOT\205&#\226S\232\&0\160\252\164\&9\224\&2\152\RS\197F\191\193\223\161\SOHX\RSX\FS\202>U<\156c\197\&5\201\210\140C\v\216\253\150\235\177\189*\211E\241\201;L;t\NUL\SUB||\158\&1"
                        , coin = Coin 3842710635646
                        }
                    , TxOut
                        { address = Address "\130\216\CANXB\131X\FS\197\251\223.\192>\179\168\236}\242\180\188$\173\161\229\165\157#\190\USo{]BO\191\161\SOHX\RSX\FSA\162\195Z4\CANj\174\148\160\&34\USo\ETB\179\a\133Te\ACK\131\182y\248\236\211c\NUL\SUB\225\153\247\212"
                        , coin = Coin 1499800000
                        }
                    ]
                }
            , Tx
                { inputs =
                    [ TxIn
                        { inputId = Hash "\187\177J\189\132K\n\175\130\148\&3[\150\193zL\153\191Qjcl\n\162B\241G)>\151\DC4\225"
                        , inputIx = 0
                        }
                    ]
                , outputs =
                    [ TxOut
                        { address = Address "\130\216\CANXB\131X\FS\203\242{\247\221*[\182a\171/`\151,\130\&4\246\219\245I\t\240\&6\ACK\159wg\186\161\SOHX\RSX\FS\202>U<\156c\197\CAN\250\154\238C \170\214\202\244y\140!\189\SYN]\157\132\ETXt\245\NUL\SUB\155\210\\\173"
                        , coin = Coin 3842940911894
                        }
                    , TxOut
                        { address = Address "\130\216\CANXB\131X\FS~\133V\SYN\DEL\211\165\ACK\239\a\182\131\143'\253On\210d\169kc\145\179\156\142\230\140\161\SOHX\RSX\FS\179@nvQ\155\209\149n\214\226y\166\133\170\207\134\131t\219\&7&\246m_Jv\DC2\NUL\SUB\218\132l\235"
                        , coin = Coin 1345293520
                        }
                    ]
                }
            ]
        }
    , Block
        { header = BlockHeader
            { slotId = SlotId 14 12
            , blockHash = Hash "G\192\140\n\DC1\246j\234\185\NAK\229\205\EM6.\141\165\r\194R>b\155#\vs\236{l\219\238\248"
            , prevBlockHash = Hash "\150\163\SUB|\219A\n\235WV\221\180>\226\221\180\198\130\246\&0\141\179\131\DLE\171T\191\&8\184\157k\r"
            }
        , transactions =  []
        }
    , Block
        { header = BlockHeader
            { slotId = SlotId 14 13
            , blockHash = Hash "\214\215\231\158*%\245>o\183q\238\189\ESC\224Rt\134\DLE\EOT\220b\192;\249M\240?\247\184q\152"
            , prevBlockHash = Hash "G\192\140\n\DC1\246j\234\185\NAK\229\205\EM6.\141\165\r\194R>b\155#\vs\236{l\219\238\248"
            }
        , transactions =  []
        }
    , Block
        { header = BlockHeader
            { slotId = SlotId 14 14
            , blockHash = Hash "d~b\178\158\188\176\236\250\vM\235AR\145=\SUBf\150\DC1\214F\a-/X\152\131[\136\217\&8"
            , prevBlockHash = Hash "\214\215\231\158*%\245>o\183q\238\189\ESC\224Rt\134\DLE\EOT\220b\192;\249M\240?\247\184q\152"
            }
        , transactions =  []
        }
    , Block
        { header = BlockHeader
            { slotId = SlotId 14 15
            , blockHash = Hash "\STX\243\140\229\f\148\153\242Rm\217\197\249\232\137\158e\192\196\ETXD\225O\240\GS\198\195\DC17\151\142\251"
            , prevBlockHash = Hash "d~b\178\158\188\176\236\250\vM\235AR\145=\SUBf\150\DC1\214F\a-/X\152\131[\136\217\&8"
            }
        , transactions =  []
        }
    , Block
        { header = BlockHeader
            { slotId = SlotId 14 16
            , blockHash = Hash "R\132\146\222\215)\202w\167+\GS\133eGB\219\133\223\211\182\142lA\ETB\206<%>>\134am"
            , prevBlockHash = Hash "\STX\243\140\229\f\148\153\242Rm\217\197\249\232\137\158e\192\196\ETXD\225O\240\GS\198\195\DC17\151\142\251"
            }
        , transactions =  []
        }
    , Block
        { header = BlockHeader
            { slotId = SlotId 14 17
            , blockHash = Hash "\244(8D\235x\202oc3\176\a\245\167\&5\215\DC4\153\214\206|\200\SYN\132j\ETX:6xK\210\153"
            , prevBlockHash = Hash "R\132\146\222\215)\202w\167+\GS\133eGB\219\133\223\211\182\142lA\ETB\206<%>>\134am"
            }
        , transactions =  []
        }
    , Block
        { header = BlockHeader
            { slotId = SlotId 14 18
            , blockHash = Hash "\223\252\&5\ACK\211\129\&6\DC4h7b'\225\201\&2:/\252v\SOH\DC1\ETX\227\"Q$\240\142ii\167;"
            , prevBlockHash = Hash "\244(8D\235x\202oc3\176\a\245\167\&5\215\DC4\153\214\206|\200\SYN\132j\ETX:6xK\210\153"
            }
        , transactions =
            [ Tx
                { inputs =
                    [ TxIn
                        { inputId = Hash "\150\225pI\SUB\251n\189W\159\213|v\198\132\242$6\248\204:\145#\151\221\177\201\197\ESC\134\251S"
                        , inputIx = 0
                        }
                    ]
                , outputs =
                    [ TxOut
                        { address = Address "\130\216\CANXB\131X\FS\197\CAN\DELP\160W\144\&8\GSW\189\&7m\b\233Y\216I\176\159\250\144\EM\155|\219\n\231\161\SOHX\RSX\FS\202>U<\156c\197\&6\149=XC\217L\SOH\255\166\228\138\221\157\&0\ACK&]`z\DC2\NUL\SUB\149\157\191\162"
                        , coin = Coin 3832107959251
                        }
                    , TxOut
                        { address = Address { getAddress = "\130\216\CANXB\131X\FSI\SI\165\f\DLE\223\214\209\206\187y\128F\SUB\248.\203\186/\244\143m1]\n\132\234\"\161\SOHX\RSX\FSv\SI\240\133L\130\194\DC2\191}\189;5\141\252t]\132}[\244\ESC&\SI\EOT[{\238\NUL\SUB\159\236eZ" }
                        , coin = Coin 11823271860
                        }
                    ]
                }
              , Tx
                  { inputs =
                      [ TxIn
                          { inputId = Hash "\249\DC2\146\&0\GSK\177\182\224@\206\205\255@0\149\155I\201^}\174\bw\130\221U\139\235\182f\138"
                          , inputIx = 0
                          }
                      ]
                  , outputs =
                      [ TxOut
                          { address = Address "\130\216\CANXB\131X\FSe$;\SO\178g\161\226>1w\159M\NAK\141d\173\210\202\192Bn\250\176C(\DC2\ENQ\161\SOHX\RSX\FS\202>U<\156c\197\SUB\225\157\&1C\209\253\183\USuz\163\193\209\196\217:\155!\167!\NUL\SUB\137\240\187\159"
                          , coin = Coin 3841254542346
                          }
                      , TxOut
                          { address = Address "\130\216\CANXB\131X\FS\161\243^\nQ`\DLE\151\147n\153j\STX\215]\SOr7\136\211\222y\US*\157%\DEL\ETB\161\SOHX\RSX\FS\201\SUB\170\156Oe\155)D\US\143\CAN\237\193\244vKM\160\SOH\166&\161\213\188KD\142\NUL\SUB\144\192\240\146"
                          , coin = Coin 2700667457
                          }
                      ]
                }
            ]
        }
    , Block
        { header = BlockHeader
            { slotId = SlotId 14 19
            , blockHash = Hash "\179\200\DC2\213\215y\f\"\232\181\140\SYN\DLEd\v\230O\156\226\DC4eH\136\144\223\199\215\168\&1\NAK\ACK\194"
            , prevBlockHash = Hash "\223\252\&5\ACK\211\129\&6\DC4h7b'\225\201\&2:/\252v\SOH\DC1\ETX\227\"Q$\240\142ii\167;"
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
    ]
