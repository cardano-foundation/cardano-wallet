-- | Export values of Haskell types to 'Value', generated at compile time.
module Export.Haskell.Value.Compiletime
    ( declareInstanceToValue
    , declareToValueFunRecord
    , declareToValueFunUnion
    ) where

import Prelude

import Typ
    ( ConstructorName, FieldName, Typ, TypName )
import Export.Haskell.Language

import qualified Language.Haskell.Exts as Hs

{-----------------------------------------------------------------------------
    Compile time definitions
------------------------------------------------------------------------------}
-- | Declare an @instance ToValue@ for a record type.
declareInstanceToValue
    :: TypName
    -> Hs.Decl Annotation
    -> Hs.Decl Annotation
declareInstanceToValue name toValueDeclaration =
    Hs.InstDecl l Nothing instanceRule (Just instanceDecls)
  where
    instanceRule = Hs.IRule l Nothing Nothing instanceHead
    instanceHead = Hs.IHApp l (Hs.IHCon l className) (hsType name)
    className = runtime "ToValue"
    instanceDecls = [ Hs.InsDecl l toValueDeclaration ]

-- | Declare the funcion `toValue` for a record type.
declareToValueFunRecord
    :: TypName
    -> [(FieldName, Typ)]
    -> Hs.Decl Annotation
declareToValueFunRecord constructor fields =
    Hs.FunBind l
        [ Hs.Match l (Hs.Ident l "toValue") [pat] rhs Nothing ]
  where
    pat = Hs.PApp l (Hs.UnQual l $ Hs.Ident l constructor)
        [ Hs.PVar l (Hs.Ident l $ field <> "_pat")
        | (field,_) <- fields
        ]
    rhs = Hs.UnGuardedRhs l $ productV `app` (Hs.List l arguments)
    arguments =
        [ Hs.Var l (runtime "toValue") `app` var (field <> "_pat")
        | (field,_) <- fields
        ]
    productV = Hs.Con l (runtime "ProductV")

-- | Declare the funcion `toValue` for a sum type.
declareToValueFunUnion
    :: TypName
    -> [(ConstructorName, Typ)]
    -> Hs.Decl Annotation
declareToValueFunUnion name constructors =
    Hs.FunBind l
        [ Hs.Match l
            (Hs.Ident l "toValue")
            [pat constructor]
            (rhs ix)
            Nothing
        | (ix,(constructor,_)) <- zip [0..] constructors
        ]
  where
    pat constructor =
        Hs.PApp l
            (Hs.UnQual l $ Hs.Ident l $ raiseFirstLetter constructor)
            [ Hs.PVar l (Hs.Ident l "x") ]
    rhs ix = Hs.UnGuardedRhs l $
        (sumV `app` int ix)
            `app` (Hs.Var l (runtime "toValue") `app` var "x")
    int ix = Hs.Lit l $ Hs.Int l ix (show ix)
    sumV = Hs.Con l (runtime "SumV")

{-----------------------------------------------------------------------------
    Expression utilities
------------------------------------------------------------------------------}
app :: Hs.Exp Annotation -> Hs.Exp Annotation -> Hs.Exp Annotation
app = Hs.App l

runtime :: String -> Hs.QName Annotation
runtime
    = Hs.Qual l (Hs.ModuleName l "Export.Haskell.Value.Runtime")
    . Hs.Ident l

var :: String -> Hs.Exp Annotation
var = Hs.Var l . Hs.UnQual l . Hs.Ident l
