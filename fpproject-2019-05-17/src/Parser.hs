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
keywords = ["if","then","else","true","false","not","mod","let","def","in","bool","int","char"]

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
intLit :: Parser Int
intLit = lexeme L.decimal

charLit :: Parser Char
charLit = lexeme $ between (char '\'') (char '\'') L.charLiteral

parseNfb :: String -> Char -> Parser ()
parseNfb s c = (lexeme . try) (string s *> notFollowedBy (char c))

word :: String -> Parser ()
word s = (lexeme . try) (string s *> notFollowedBy alphaNumChar)

identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
  where
    p       = (:) <$> letterChar <*> many alphaNumChar
    check x = if elem x keywords
                then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                else return x

naiveHsParser :: Parser Expr
naiveHsParser = between sc eof expParser

expParser :: Parser Expr
expParser = makeExprParser expTerms expOperators
  <|> ifExpr
  <|> lambdaExpr
  <|> letExpr
  <|> defExpr
  <|> parens expParser

expTerms :: Parser Expr
expTerms = parens expParser
  <|> (EBoolLit True <$ word "true")
  <|> (EBoolLit False <$ word "false")
  <|> (EIntLit <$> intLit)
  <|> (ECharLit <$> charLit)
  <|> (EVar <$> identifier)

expNegate :: Expr -> Expr
expNegate (EIntLit x) = EIntLit $ negate x
expNegate x = ESub (EIntLit 0) x

expOperators :: [[Operator Parser Expr]]
expOperators = 
  [
    [
      InfixL (EApply <$ symbol "$")
    ],
    [
      Prefix (expNegate <$ symbol "-"),
      Prefix (ENot <$ symbol "!")
    ],
    [
      InfixL (EMul <$ symbol "*"),
      InfixL (EDiv <$ parseNfb "/" '='),
      InfixL (EMod <$ symbol "%"),
      InfixL (EAnd <$ symbol "&&"),
      InfixL (EOr <$ symbol "||")
    ],
    [
      InfixL (EAdd <$ symbol "+"),
      InfixL (ESub <$ symbol "-") 
    ],
    [
      InfixL (EEq <$ symbol "=="),
      InfixL (ENeq <$ symbol "/="),
      InfixL (ELe <$ symbol "<="),
      InfixL (EGe <$ symbol ">="),
      InfixL (ELt <$ symbol "<"),
      InfixL (EGt <$ symbol ">")
    ]
  ]

ifExpr :: Parser Expr
ifExpr = do
  word "if"
  cond <- expParser
  word "then"
  exp1 <- expParser
  word "else"
  exp2 <- expParser
  return (EIf cond exp1 exp2)

lambdaExpr :: Parser Expr
lambdaExpr = do
  symbol "\\"
  name <- identifier
  symbol ":"
  t <- parens typeParser
  symbol "->"
  exp <- expParser
  return (ELambda (name, t) exp)

letExpr :: Parser Expr
letExpr = do
  word "let"
  name <- identifier
  symbol "="
  exp1 <- expParser
  word "in"
  exp2 <- expParser
  return (ELet (name, exp1) exp2)

defExpr :: Parser Expr
defExpr = do
  word "def"
  f <- identifier
  symbol ":"
  ty <- parens typeParser
  symbol "="
  (ELambda (x, tx) e1) <- expParser
  word "in"
  e2 <- expParser
  return (ELetRec f (x, tx) (e1, ty) e2)

typeParser :: Parser Type
typeParser = makeExprParser typeTerms typeOperators
  <|> parens typeParser

typeTerms :: Parser Type
typeTerms = parens typeParser
  <|> (TBool <$ word "bool")
  <|> (TInt <$ word "int")
  <|> (TInt <$ word "char")

typeOperators :: [[Operator Parser Type]]
typeOperators = 
  [
    [
      InfixL (TArrow <$ symbol "->")
    ]
  ]
