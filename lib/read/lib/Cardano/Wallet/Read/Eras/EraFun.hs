{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Copyright: © 2020-2022 IOHK
-- License: Apache-2.0
--
-- A datatype that represents a vector of functions covering all known eras
-- The functions are supposed to map values from and to the same era.
--
-- We removed the cached encoding at the price of 'MkEraFun' and 'fromEraFun'
-- during all compositions, we are not 100% it's not relevant for performance
-- If the computed functions after record compositions are the same then we can
-- avoid that layer
--
-- Note:
-- composition is anyway expansive, do not recompose,
-- just cache and reuse the compositions
module Cardano.Wallet.Read.Eras.EraFun
    ( -- * Application.
      applyEraFun
    , applyEraFunValue
    )
where

import Cardano.Wallet.Read.Eras.EraValue
    ( EraValue (..)
    )
import Cardano.Wallet.Read.Eras.KnownEras
    ( IsEra (..)
    )
import Control.Category
    ( Category (..)
    )
import Generics.SOP
    ( K (..)
    , Proxy (..)
    , type (-.->) (..)
    )
import Generics.SOP.NP
    ( cpure_NP
    )
import Generics.SOP.NS
    ( ap_NS
    , collapse_NS
    )
import Prelude hiding
    ( id
    , (.)
    )

-- | Apply an 'EraFun' to an 'EraValue'.
applyEraFun :: (forall era. IsEra era => f era -> g) -> EraValue f -> g
applyEraFun f (EraValue v) =
    collapse_NS $ ap_NS (cpure_NP (Proxy @IsEra) $ Fn (K . f)) v

applyEraFunValue
    :: (forall era. IsEra era => f era -> g era)
    -> EraValue f
    -> EraValue g
applyEraFunValue f (EraValue v) =
    EraValue $ ap_NS (cpure_NP (Proxy @IsEra) $ Fn f) v
