import Main.Utf8
    ( withUtf8
    )
import Prelude

import qualified Cardano.Faucet.Http.Server as Faucet

main :: IO ()
main = withUtf8 $ do
    Faucet.serve (Faucet.TcpPort 23456)
