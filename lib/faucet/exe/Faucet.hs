import qualified Cardano.Faucet.Http.Server as Faucet

import Prelude

import Main.Utf8
    ( withUtf8
    )

main :: IO ()
main = withUtf8 $ do
    Faucet.serve (Faucet.TcpPort 23456)
