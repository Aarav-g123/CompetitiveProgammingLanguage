{-# LANGUAGE OverloadedStrings #-}

module Language.Lexer where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad (void)

type Parser = Parsec Void Text

-- | Space consumer that handles indentation-based syntax
sc :: Parser ()
sc = L.space space1 lineComment blockComment
  where
    lineComment = L.skipLineComment "#"
    blockComment = L.skipBlockComment "'''" "'''"

-- | Space consumer for same-line spaces only
scn :: Parser ()
scn = L.space (void $ some (char ' ' <|> char '\t')) lineComment blockComment
  where
    lineComment = L.skipLineComment "#"
    blockComment = L.skipBlockComment "'''" "'''"

-- | Lexeme wrapper
lexeme :: Parser a -> Parser a
lexeme = L.lexeme scn

-- | Symbol parser
symbol :: Text -> Parser Text
symbol = L.symbol scn

-- | Integer literal
integer :: Parser Integer
integer = lexeme L.decimal

-- | Float literal
float :: Parser Double
float = lexeme L.float

-- | String literal
stringLiteral :: Parser Text
stringLiteral = lexeme $ do
  _ <- char '"'
  content <- manyTill L.charLiteral (char '"')
  return $ T.pack content

-- | Identifier
identifier :: Parser Text
identifier = lexeme $ do
  first <- letterChar <|> char '_'
  rest <- many (alphaNumChar <|> char '_')
  let ident = T.pack (first : rest)
  if ident `elem` reservedWords
    then fail $ "Reserved word: " ++ T.unpack ident
    else return ident

-- | Reserved words
reservedWords :: [Text]
reservedWords =
  [ "def", "return", "if", "elif", "else", "while", "for", "in"
  , "range", "True", "False", "and", "or", "not", "break", "continue"
  , "pass", "print", "read", "int", "float", "string", "bool", "auto"
  , "vector", "set", "map", "pair", "deque", "pq", "lambda"
  ]

-- | Reserved word parser
reserved :: Text -> Parser ()
reserved w = lexeme $ try $ do
  _ <- string w
  notFollowedBy alphaNumChar

-- | Operators
operators :: [(Text, Text)]
operators =
  [ ("==", "=="), ("!=", "!="), ("<=", "<="), (">=", ">=")
  , ("<<", "<<"), (">>", ">>"), ("&&", "&&"), ("||", "||")
  , ("**", "**"), ("+=", "+="), ("-=", "-="), ("*=", "*=")
  , ("/=", "/="), ("%=", "%="), ("->", "->")
  , ("+", "+"), ("-", "-"), ("*", "*"), ("/", "/"), ("%", "%")
  , ("<", "<"), (">", ">"), ("=", "="), ("!", "!")
  , ("&", "&"), ("|", "|"), ("^", "^"), ("~", "~")
  , ("(", "("), (")", ")"), ("[", "["), ("]", "]")
  , ("{", "{"), ("}", "}"), (",", ","), (":", ":"), (".", ".")
  ]

-- | Operator parser
operator :: Text -> Parser Text
operator = symbol

-- | Parse parentheses
parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

-- | Parse brackets
brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

-- | Parse braces
braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

-- | Comma-separated list
commaSep :: Parser a -> Parser [a]
commaSep p = p `sepBy` symbol ","

-- | Comma-separated list (at least one)
commaSep1 :: Parser a -> Parser [a]
commaSep1 p = p `sepBy1` symbol ","