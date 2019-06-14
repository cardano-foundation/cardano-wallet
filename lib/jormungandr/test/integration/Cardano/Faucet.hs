{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Faucet
    ( initFaucet
    ) where

import Prelude

import Cardano.Wallet.Network
    ( NetworkLayer )
import Test.Integration.Faucet
    ( Faucet )


-- | Initialize a bunch of faucet wallets and make them available for the
-- integration tests scenarios.
initFaucet :: NetworkLayer t IO -> IO Faucet
initFaucet _nl = undefined

{--
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Faucet
    ( generateMnemonicsAndAdresses
    ) where

import Prelude

import Cardano.Wallet.Jormungandr.Compatibility
    ( Jormungandr, Network (..) )
import Cardano.Wallet.Primitive.AddressDerivation
    ( ChangeChain (..)
    , KeyToAddress (..)
    , Passphrase (..)
    , deriveAccountPrivateKey
    , deriveAddressPrivateKey
    , generateKeyFromSeed
    , publicKey
    )
import Cardano.Wallet.Primitive.Mnemonic
    ( Mnemonic
    , entropyToBytes
    , entropyToMnemonic
    , genEntropy
    , mnemonicToEntropy
    , mnemonicToText
    )
import Cardano.Wallet.Primitive.Types
    ( Address )
import Control.Monad
    ( replicateM )
import Data.Text
    ( Text )

generateMnemonicsAndAdresses :: Int -> IO [([Text], Address)]
generateMnemonicsAndAdresses pairNum = do
    mnemonics <- replicateM pairNum genMnemonic
    pure $ zip (mnemonicToText @15 <$> mnemonics) (firstAddress <$> mnemonics)
      where
          genMnemonic :: IO (Mnemonic 15)
          genMnemonic = entropyToMnemonic <$> genEntropy
          firstAddress :: Mnemonic 15 -> Address
          firstAddress mw =
              let
                  (seed, pwd) =
                      (Passphrase $ entropyToBytes $ mnemonicToEntropy mw, mempty)
                  rootXPrv =
                      generateKeyFromSeed (seed, mempty) pwd
                  accXPrv =
                      deriveAccountPrivateKey pwd rootXPrv minBound
                  addrXPrv =
                      deriveAddressPrivateKey pwd accXPrv ExternalChain minBound
              in
                  keyToAddress @(Jormungandr 'Testnet) (publicKey addrXPrv)
--}
{--
λ generateMnemonicsAndAdresses 10
    [
(["valid","agree","faith","tower","present","minute","valid","vague","engine","believe","catch","giggle","lumber","hip","unaware"],
Address {getAddress = "\131Yy\228Z\198:9\180\140\RS\2\45L\159O;\ESC:\171\NUL J\200D\ETB\162\170\225\DC1+\214\248\184Q\200\a\133Y\ESCO)\149$\194U\221\245\200\&2\141\USX\218<TPeFQ6\ACK\180\&7\233j"}),

(["trim","three","adult","wolf","brick","enter","sight","shiver","census","lamp","holiday","anger","life","pipe","venture"],
Address {getAddress = "\131\242\175\EOT\177\241@\179\232Y\137\ff\176?\"\196\146\166:E\206\235\167\216K\247'P\f\226z2_q#m\223\SOH*\bW\164\CAN\SI\tN]g\217,\246\166\161$\"\231\254\230\ETB\214\202jZ\161"}),

(["penalty","differ","gauge","tray","work","post","finger","loan","onion","monkey","question","another","benefit","together","theme"],
Address {getAddress = "\131\192|=\196\191\244L\239\STXg\240L\DC2\143\176\US\193\169](E\197\201\186\195\&7\209\224^PfsV\222\rF\198##\132BP\215\155\181\130\ESC\239\173\128\n\249\&6\233\153\211\188zA7(7g|"}),

(["oak","forget","perfect","panther","ladder","foam","valley","attitude","trade","alter","unaware","brain","body","hub","guilt"],
Address {getAddress = "\131\203\203\217\176ko\178=v\150\NUL\233.}L\211E\136G\238vV\FS\SUBx\STX\135\241\154\162\212k\214\210.\198rGQ\STX\221\243a\238N\232F<A\f\240\&5\129\169\154\182\SYN\240=w\233\186R\143"}),

(["fire","bright","good","shine","trust","sight","home","camp","grid","enemy","face","polar","add","address","can"],
Address {getAddress = "\131JB<J\167\148<xB\216/NjvpK.j\164\159\201\218\&62\SOH\194\242\188\162\233)s\253\DC1\232\&8GW\201e\231N\215\152\252\241\196\230\149\133\202C\DC4S\146\213\219T\135\179\152\v\136\&3"}),

(["burger","raise","news","catch","face","crane","innocent","easy","leader","peanut","market","link","anchor","kangaroo","invite"],
Address {getAddress = "\131\248+\RS\GS\DC4?\146$\254\252\234>\184\239\&1\138\238\167\ESC\DC4\SUB_@\ETX\217v\CANu\244{\232v\140Om\205\DC2\163\&1\157o\241\237\240h2\207#\159x\140&\"\216\SI~D\DC4\228\252\138\NAK\134\195"}),

(["saddle","dune","wealth","later","tide","enrich","alley","flee","average","buddy","youth","clinic","manage","cycle","episode"],
Address {getAddress = "\131\241\231\234\DC3x\227\143\222\210\208\STX\153,\r\158)\225Z\211j\229\200\253\190\250\206\133BJ\138\&8\171\197\227\STXe\DC1\DLE\153\150W\CAN\173i\158\DC2\198\DC3e\148\145\128\130\230\156\166\150\163\167Z\152\238;\219"}),

(["balance","lawsuit","castle","credit","curious","buyer","derive","design","electric","drive","into","hold","subject","leopard","rent"],
Address {getAddress = "\131\236\247\152\242jAC@\ENQ\150\172/\184\nn\250\"\195]\ACK6\DEL\b\v\136\165\187\193\132\226\161,\147\230z}N\DC1\133\"t\171Y\178\201\159\r\\v\b\DC4\139\STX\215\211B\146,Z-+H\DC1\160"}),

(["concert","measure","deal","vital","fortune","flag","doll","hobby","tragic","carpet","drastic","genre","glue","board","envelope"],
Address {getAddress = "\131\164\194\198\177\173\243aF\251g*\238\239/\SOf\NUL\184\183&\bj*\230\DC3\223\SI\STX\188e\152\188{~\191\&7\"M\187\RS\177\138\220\219\DC1\b}~6U\173\179\165\185\231\ne\129x\f.6\239\DC3"}),

(["gadget","valve","bounce","fever","online","hat","elder","fat","glue","tower","oppose","coil","fringe","race","senior"],
Address {getAddress = "\131\DC4\SOH\139j\195G\235\DC4\199 \232a\230\RS\157\225\140=\180G\185\239\ESC\206\247L\204\143+\158\196\137\240\187\194(y,\182,\200A\241\234_\158W.\ENQ\RS\134\135\224n\250\\\164\199\216\148\232t\SI\f"})]
--}
