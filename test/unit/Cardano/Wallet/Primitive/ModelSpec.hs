{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.Primitive.ModelSpec
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Binary
    ( txId )
import Cardano.Wallet.Primitive.Model
    ( applyBlock
    , applyBlocks
    , availableBalance
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
    , IsOurs (..)
    , ShowFmt (..)
    , SlotId (..)
    , Tx (..)
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

import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set


spec :: Spec
spec = do
    describe "Patate Buildable instances examples" $ do
        let block = blockchain !! 1
        let utxo = utxoFromTx $ head $ transactions block
        it (show $ ShowFmt utxo) True
        it (show $ ShowFmt block) True

    describe "Patate Compare Wallet impl. with Specification" $ do
        it "Lemma 3.2 - dom u ⋪ updateUTxO b u = new b"
            (checkCoverage prop_3_2)

        it "applyBlock matches the basic model from the specification"
            (checkCoverage prop_applyBlockBasic)

    describe "Patate Extra Properties" $ do
        it "Incoming transactions have output addresses that belong to the wallet"
            (property prop_applyBlockTxHistoryIncoming)

       --  it "Apply Block move the current tip" $
       --      (property prop_applyBlockCurrentTip)


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
            wallet = foldl (\cp b -> snd $ applyBlock b cp) (initWallet s) blockchain
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
    (txs, s') = bimap Map.elems getState $ applyBlocks blockchain (initWallet s)
    isIncoming (_, m) = direction m == Incoming
    outs = Set.fromList . concatMap (map address . outputs . fst)
    overlaps a b
        | a == mempty && b == mempty = True
        | otherwise = not (Set.disjoint a b)

-- prop_applyBlockCurrentTip :: WalletState -> Property
-- prop_applyBlockCurrentTip s =


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
    UTxO $ Map.fromList $ zip (TxIn (txId tx) <$> [0..]) outs


{-------------------------------------------------------------------------------
                                  Test Data

    In practice, we may want to generate arbitrary valid sequences of block.
    This isn't trivial though because we would need to generate _valid_ chains
    for various invariants and preconditions to hold. Work have been done in
    cardano-sl to generate such chains, and we may want to use that at some
    point. For now, a valid chain coming from the testnet will do

-------------------------------------------------------------------------------}

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

-- A excerpt of mainnet, epoch #14, first 20 blocks.
blockchain :: [Block]
blockchain =
    [ Block
        { header = BlockHeader
            { slotId = SlotId 14 0
            , prevBlockHash = Hash "39d89a1e837e968ba35370be47cdfcbfd193cd992fdeed557b77c49b77ee59cf"
            }
        , transactions = []
        }
    , Block
        { header = BlockHeader
            { slotId = SlotId 14 1
            , prevBlockHash = Hash "2d04732b41d07e45a2b87c05888f956805f94b108f59e1ff3177860a17c292db"
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
            , prevBlockHash = Hash "e95a6e7da3cd61e923e30b1998b135d40958419e4157a9f05d2f0f194e4d7bba"
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
            , prevBlockHash = Hash "b5d970285a2f8534e94119cd631888c20b3a4ec0707a821f6df5c96650fe01dd"
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
              , prevBlockHash = Hash "cb96ff923728a67e52dfad54df01fc5a20c7aaf386226a0564a1185af9798cb1"
              }
        , transactions =  []
        }
    , Block
        { header = BlockHeader
              { slotId = SlotId 14 5
              , prevBlockHash = Hash "63040af5ed7eb2948e2c09a43f946c91d5dd2efaa168bbc5c4f3e989cfc337e6"
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
            , prevBlockHash = Hash "1a32e01995225c7cd514e0fe5087f19a6fd597a6071ad4ad1fbf5b20de39670b"
            }
        , transactions =  []
        }
    , Block
        { header = BlockHeader
            { slotId = SlotId 14 7
            , prevBlockHash = Hash "7855c0f101b6761b234058e7e9fd19fbed9fee90a202cca899da1f6cbf29518d"
            }
        , transactions =  []
        }
    , Block
        { header = BlockHeader
            { slotId = SlotId 14 8
            , prevBlockHash = Hash "9007e0513b9fea848034a7203b380cdbbba685073bcfb7d8bb795130d92e7be8"
            }
        , transactions =  []
        }
    , Block
        { header = BlockHeader
            { slotId = SlotId 14 9
            , prevBlockHash = Hash "0af8082504f59eb1b7114981b7dee9009064638420382211118730b45ad385ae"
            }
        , transactions =  []
        }
    , Block
        { header = BlockHeader
            { slotId = SlotId 14 10
            , prevBlockHash = Hash "adc8c71d2c85cee39fbb34cdec6deca2a4d8ce6493d6d28f542d891d5504fc38"
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
            , prevBlockHash = Hash "4fdff9f1d751dba5a48bc2a14d6dfb21709882a13dad495b856bf76d5adf4bd1"
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
            , prevBlockHash = Hash "96a31a7cdb410aeb5756ddb43ee2ddb4c682f6308db38310ab54bf38b89d6b0d"
            }
        , transactions =  []
        }
    , Block
        { header = BlockHeader
            { slotId = SlotId 14 13
            , prevBlockHash = Hash "47c08c0a11f66aeab915e5cd19362e8da50dc2523e629b230b73ec7b6cdbeef8"
            }
        , transactions =  []
        }
    , Block
        { header = BlockHeader
            { slotId = SlotId 14 14
            , prevBlockHash = Hash "d6d7e79e2a25f53e6fb771eebd1be05274861004dc62c03bf94df03ff7b87198"
            }
        , transactions =  []
        }
    , Block
        { header = BlockHeader
            { slotId = SlotId 14 15
            , prevBlockHash = Hash "647e62b29ebcb0ecfa0b4deb4152913d1a669611d646072d2f5898835b88d938"
            }
        , transactions =  []
        }
    , Block
        { header = BlockHeader
            { slotId = SlotId 14 16
            , prevBlockHash = Hash "02f38ce50c9499f2526dd9c5f9e8899e65c0c40344e14ff01dc6c31137978efb"
            }
        , transactions =  []
        }
    , Block
        { header = BlockHeader
            { slotId = SlotId 14 17
            , prevBlockHash = Hash "528492ded729ca77a72b1d85654742db85dfd3b68e6c4117ce3c253e3e86616d"
            }
        , transactions =  []
        }
    , Block
        { header = BlockHeader
            { slotId = SlotId 14 18
            , prevBlockHash = Hash "f4283844eb78ca6f6333b007f5a735d71499d6ce7cc816846a033a36784bd299"
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
            , prevBlockHash = Hash "dffc3506d381361468376227e1c9323a2ffc76011103e3225124f08e6969a73b"
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
