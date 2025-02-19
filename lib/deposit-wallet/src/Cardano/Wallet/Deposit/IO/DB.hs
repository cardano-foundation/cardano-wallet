{-# LANGUAGE CPP #-}

module Cardano.Wallet.Deposit.IO.DB
    (
#ifndef mingw32_HOST_OS
    module Cardano.Wallet.Deposit.IO.DB.Real
#endif
    )

where

#ifdef mingw32_HOST_OS
import Cardano.Wallet.Deposit.IO.DB.Stub
    ()
#else
import Cardano.Wallet.Deposit.IO.DB.Real
#endif
