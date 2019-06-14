-- 
module Parser where

import AST
import Control.Monad (void)
import Control.Monad.Combinators.Expr
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

-- list of reserved words
keywords :: [String]
keywords = ["if","then","else","true","false","not","mod","let","def","in"]

sc :: Parser ()
sc = L.space space1 lineCmnt empty
  where
    lineCmnt  = L.skipLineComment "#"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

-- | 'parens' parses something between parenthesis.
parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

-- | 'integer' parses an integer.
integer :: Parser Int
integer = lexeme L.decimal

char :: Parser Char
char = between (char '\'') (char '\'') L.charLiteral

parseWord :: String -> Parser ()
parseWord s = (lexeme . try) (string s *> notFollowedBy alphaNumChar)

identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
  where
    p       = (:) <$> letterChar <*> many alphaNumChar
    check x = if elem x keywords
                then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                else return x

myParser :: Parser Expr
myParser = between sc eof expParser

expParser :: Parser Expr
expParser = makeExprParser Terms Operators

Terms :: Parser Expr
Terms = parens expParser
  <|> (EBoolLit True <$ parseWord "true")
  <|> (EBoolLit False <$ parseWord "false")
  <|> (EIntLit <$> integer)
  <|> (ECharLit <$> char)
  <|> (EVar <$> identifier)

-- calParser :: Parser Expr
-- calParser = makeExprParser calTerm calOperators

-- calTerm :: Parser Expr
-- calTerm = parens calParser
--   <|> EIntLit <$> integer

-- calOperators :: [[Operator Parser Expr]]
-- calOperators =
--   [ [Prefix (ESub (EIntLit 0) <$ symbol "-") ]
--   , [ InfixL (EMul <$ symbol "*")
--     , InfixL (EDiv <$ symbol "/")
--     , InfixL (EMod <$ symbol "/") ]
--   , [ InfixL (EAdd <$ symbol "+")
--     , InfixL (ESub <$ symbol "-") ]
--   ]

-- TestPar :: String -> IO ()
-- TestPar = parseTest myParser