-- | Utilities concerning the Haskell language.
module Export.Haskell.Language
    ( Annotation
    , l

    , raiseFirstLetter

    , tyApp
    , hsType
    , hsList
    , hsUnit
    , hsPair
    , hsImportQualified
    ) where

import Prelude

import Typ
    ( TypName )

import qualified Data.Char
import qualified Language.Haskell.Exts as Hs

{-----------------------------------------------------------------------------
    Haskell language utilities
------------------------------------------------------------------------------}
type Annotation = ()
l :: Annotation
l = ()

raiseFirstLetter :: String -> String
raiseFirstLetter [] = []
raiseFirstLetter (c:cs) = Data.Char.toUpper c : cs

hsType :: TypName -> Hs.Type Annotation
hsType = Hs.TyCon l . Hs.UnQual l . Hs.Ident l

hsList :: Hs.Type Annotation
hsList = Hs.TyCon l $ Hs.Special l $ Hs.ListCon l

hsPair :: Hs.Type Annotation
hsPair = Hs.TyCon l $ Hs.Special l $ Hs.TupleCon l Hs.Boxed 2

hsUnit :: Hs.Type Annotation
hsUnit = Hs.TyCon l $ Hs.Special l $ Hs.UnitCon l

tyApp :: Hs.Type Annotation -> Hs.Type Annotation -> Hs.Type Annotation
tyApp = Hs.TyApp l

hsImportQualified :: String -> Hs.ImportDecl Annotation
hsImportQualified name =
    Hs.ImportDecl
        { Hs.importAnn = l
        , Hs.importModule = Hs.ModuleName l name
        , Hs.importQualified = True
        , Hs.importSrc = False
        , Hs.importSafe = False
        , Hs.importPkg = Nothing
        , Hs.importAs = Nothing
        , Hs.importSpecs = Nothing
        }
