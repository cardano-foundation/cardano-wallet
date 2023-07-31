module Parser
    ( parseLedgerTypes
    ) where

import Prelude

import Control.Monad
    ( void )
import Data.Void
    ( Void )
import Text.Megaparsec
    ( Parsec
    , between
    , endBy
    , many
    , parseMaybe
    , satisfy
    , sepBy
    , try
    , (<?>)
    , (<|>)
    )
import Typ
    ( ConstructorName
    , Declarations
    , FieldName
    , Module (Module)
    , OpBinary (..)
    , OpUnary (..)
    , Typ (..)
    , TypName
    )

import qualified Control.Monad.Combinators.Expr as Parser.Expr
import qualified Data.Map.Strict as Map
import qualified Text.Megaparsec.Char as Parser.Char
import qualified Text.Megaparsec.Char.Lexer as L

{-----------------------------------------------------------------------------
    Exported functions
------------------------------------------------------------------------------}
-- | Parse a 'String' containing mathematical types,
-- as they appears in the Cardano ledger specification.
parseLedgerTypes :: String -> Maybe Module
parseLedgerTypes = parseMaybe document

{-----------------------------------------------------------------------------
    Parser
------------------------------------------------------------------------------}
{- Note
For the design patterns used when implementing this parser, see
  J. Willis, N. Wu, Design Patterns for Parser Combinators (Functional Pearl)
  https://dl.acm.org/doi/10.1145/3471874.3472984
-}

type Parser = Parsec Void String

document :: Parser Module
document = space *> module'

module' :: Parser Module
module' =
    Module
        <$ symbol "module" <*> moduleName
        <* symbol "where" <*> declarations

declarations :: Parser Declarations
declarations = mconcat <$> (declaration `endBy` symbol ";")

-- | Parse a single declaration
declaration :: Parser Declarations
declaration =
    Map.singleton <$> lhs <* symbol "=" <*> rhs
  where
    lhs = typName
    rhs = try abstract <|> try record <|> try union <|> expr

abstract :: Parser Typ
abstract = Abstract <$ symbol "_"

record :: Parser Typ
record = Record <$> braces (field `sepBy` symbol ",")

field :: Parser (FieldName, Typ)
field = (\a b -> (a,b)) <$> fieldName <* symbol ":" <*> expr

union :: Parser Typ
union = Union <$> plusBrackets (constructor `sepBy` symbol ",")

constructor :: Parser (FieldName, Typ)
constructor = (\a b -> (a,b)) <$> constructorName <* symbol ":" <*> expr

-- | Parse an expression.
expr :: Parser Typ
expr = Parser.Expr.makeExprParser atom tableOfOperators <?> "expression"

atom :: Parser Typ
atom = parens expr <|> (Var <$> (constants <|> typName)) <?> "atom"
  where
    constants = try (symbol "ℕ") <|> try (symbol "ℤ")

tableOfOperators :: [[Parser.Expr.Operator Parser Typ]]
tableOfOperators =
    [ [ postfix "*" (Unary Sequence)
      , postfix "?" (Unary Option)
      ]
    , [ prefix "ℙ" (Unary PowerSet)
      ]
    , [ binaryR "×" (Binary Product)
      ]
    , [ binaryR "⊎" (Binary Sum)
      , binaryR "+" (Binary Sum)
      ]
    , [ binaryR "→∗" (Binary FiniteSupport)
      , binaryR "↦0" (Binary FiniteSupport)
      , binaryR "↦" (Binary PartialFunction)
      ]
    ]

binaryR name f = Parser.Expr.InfixR  (f <$ symbol name)
prefix  name f = Parser.Expr.Prefix  (f <$ symbol name)
postfix name f = Parser.Expr.Postfix (f <$ symbol name)

{-----------------------------------------------------------------------------
    Lexer
------------------------------------------------------------------------------}
lineComment :: Parser ()
lineComment = L.skipLineComment "--"

blockComment :: Parser ()
blockComment = L.skipBlockComment "{-" "-}"

space :: Parser ()
space = L.space Parser.Char.space1 lineComment blockComment

{-
space :: Parser ()
space =
    L.space
        (void $ some $ satisfy (`elem` " \t"))
        lineComment
        empty
-}
symbol :: String -> Parser String
symbol = L.symbol space

moduleName :: Parser String
moduleName = typName

typName :: Parser TypName
typName = L.lexeme space $
    (:)
    <$> Parser.Char.upperChar
    <*> many (Parser.Char.alphaNumChar <|> satisfy (`elem` "_^"))

constructorName :: Parser ConstructorName
constructorName = fieldName

fieldName :: Parser FieldName
fieldName = L.lexeme space $
    (:)
    <$> Parser.Char.lowerChar
    <*> many (Parser.Char.alphaNumChar <|> satisfy (`elem` "_^"))

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

plusBrackets :: Parser a -> Parser a
plusBrackets = between (symbol "[+") (symbol "+]")
