{-# LANGUAGE OverloadedStrings #-}

module Language.Parser
  ( parseCP
  , parseExpr
  , parseStatement
  ) where

import Language.AST
import Language.Lexer

import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad.Combinators.Expr
import Data.Maybe (fromMaybe)

-- | Parse a CP program
parseCP :: Text -> Either String Program
parseCP input = case parse (sc *> many pStatement <* eof) "<input>" input of
  Left err -> Left $ errorBundlePretty err
  Right ast -> Right ast

-- | Parse expression (exported for testing)
parseExpr :: Text -> Either String Expr
parseExpr input = case parse (sc *> pExpr <* eof) "<input>" input of
  Left err -> Left $ errorBundlePretty err
  Right expr -> Right expr

-- | Parse statement (exported for testing)
parseStatement :: Text -> Either String Statement
parseStatement input = case parse (sc *> pStatement <* eof) "<input>" input of
  Left err -> Left $ errorBundlePretty err
  Right stmt -> Right stmt

-- | Statement parser
pStatement ::  Parser Statement
pStatement = choice
  [ try pFuncDef
  , try pVarDecl
  , try pVarAssign
  , try pIf
  , try pWhile
  , try pFor
  , try pReturn
  , try pPrint
  , try pRead
  , try pBreak
  , try pContinue
  , try pPass
  , try pComment
  , pExprStmt
  ] <* optional (some newline)

-- | Variable declaration:  x: int = 5
pVarDecl :: Parser Statement
pVarDecl = do
  name <- identifier
  _ <- symbol ":"
  typ <- pType
  _ <- symbol "="
  expr <- pExpr
  return $ VarDecl name typ expr

-- | Variable assignment: x = 5
pVarAssign ::  Parser Statement
pVarAssign = do
  name <- identifier
  _ <- symbol "="
  expr <- pExpr
  return $ VarAssign name expr

-- | Function definition
pFuncDef :: Parser Statement
pFuncDef = do
  reserved "def"
  name <- identifier
  params <- parens (commaSep pParam)
  retType <- option TVoid (symbol "->" *> pType)
  _ <- symbol ":"
  _ <- some newline
  body <- pBlock
  return $ FuncDef name params retType body

-- | Parse function parameter
pParam :: Parser (Text, Type)
pParam = do
  name <- identifier
  _ <- symbol ":"
  typ <- pType
  return (name, typ)

-- | Parse indented block
pBlock :: Parser [Statement]
pBlock = do
  _ <- some (char ' ' <|> char '\t')
  stmts <- pStatement `sepEndBy1` some newline
  return stmts

-- | If statement
pIf :: Parser Statement
pIf = do
  reserved "if"
  cond <- pExpr
  _ <- symbol ":"
  _ <- some newline
  thenBody <- pBlock
  elifs <- many pElif
  elseBody <- optional pElse
  return $ If cond thenBody elifs elseBody

pElif :: Parser (Expr, [Statement])
pElif = do
  reserved "elif"
  cond <- pExpr
  _ <- symbol ":"
  _ <- some newline
  body <- pBlock
  return (cond, body)

pElse :: Parser [Statement]
pElse = do
  reserved "else"
  _ <- symbol ":"
  _ <- some newline
  pBlock

-- | While loop
pWhile ::  Parser Statement
pWhile = do
  reserved "while"
  cond <- pExpr
  _ <- symbol ":"
  _ <- some newline
  body <- pBlock
  return $ While cond body

-- | For loop
pFor :: Parser Statement
pFor = do
  reserved "for"
  var <- identifier
  reserved "in"
  choice [try pForRange, pForEach var]
  where
    pForRange = do
      reserved "range"
      (start, end, step) <- parens $ do
        args <- commaSep1 pExpr
        case args of
          [e] -> return (IntLit 0, e, Nothing)
          [s, e] -> return (s, e, Nothing)
          [s, e, st] -> return (s, e, Just st)
          _ -> fail "range() takes 1-3 arguments"
      _ <- symbol ":"
      _ <- some newline
      body <- pBlock
      var <- identifier  -- This is wrong, need to fix
      return $ For var start end body
    
    pForEach var = do
      arr <- identifier
      _ <- symbol ":"
      _ <- some newline
      body <- pBlock
      return $ ForEach var arr body

-- | Return statement
pReturn :: Parser Statement
pReturn = do
  reserved "return"
  expr <- optional pExpr
  return $ Return (fromMaybe (IntLit 0) expr)

-- | Print statement
pPrint :: Parser Statement
pPrint = do
  reserved "print"
  args <- parens (commaSep pExpr)
  return $ Print args

-- | Read statement  
pRead :: Parser Statement
pRead = do
  reserved "read"
  vars <- parens (commaSep identifier)
  return $ Read vars

-- | Break statement
pBreak :: Parser Statement
pBreak = reserved "break" >> return Break

-- | Continue statement
pContinue :: Parser Statement
pContinue = reserved "continue" >> return Continue

-- | Pass statement
pPass ::  Parser Statement
pPass = reserved "pass" >> return Pass

-- | Comment
pComment :: Parser Statement
pComment = do
  _ <- char '#'
  content <- takeWhileP Nothing (/= '\n')
  return $ Comment content

-- | Expression statement
pExprStmt :: Parser Statement
pExprStmt = ExprStmt <$> pExpr

-- | Expression parser with operator precedence
pExpr :: Parser Expr
pExpr = makeExprParser pTerm operatorTable

operatorTable :: [[Operator Parser Expr]]
operatorTable =
  [ [ Prefix (UnaryOp Neg <$ symbol "-")
    , Prefix (UnaryOp Not <$ reserved "not")
    , Prefix (UnaryOp BitNot <$ symbol "~")
    ]
  , [ InfixL (BinOp Pow <$ symbol "**") ]
  , [ InfixL (BinOp Mul <$ symbol "*")
    , InfixL (BinOp Div <$ symbol "/")
    , InfixL (BinOp Mod <$ symbol "%")
    ]
  , [ InfixL (BinOp Add <$ symbol "+")
    , InfixL (BinOp Sub <$ symbol "-")
    ]
  , [ InfixL (BinOp Shl <$ symbol "<<")
    , InfixL (BinOp Shr <$ symbol ">>")
    ]
  , [ InfixL (BinOp BitAnd <$ symbol "&") ]
  , [ InfixL (BinOp BitXor <$ symbol "^") ]
  , [ InfixL (BinOp BitOr <$ symbol "|") ]
  , [ InfixN (BinOp Eq <$ symbol "==")
    , InfixN (BinOp NEq <$ symbol "!=")
    , InfixN (BinOp LtE <$ symbol "<=")
    , InfixN (BinOp GtE <$ symbol ">=")
    , InfixN (BinOp Lt <$ symbol "<")
    , InfixN (BinOp Gt <$ symbol ">")
    ]
  , [ InfixL (BinOp And <$ reserved "and") ]
  , [ InfixL (BinOp Or <$ reserved "or") ]
  ]

-- | Term parser
pTerm :: Parser Expr
pTerm = do
  base <- pAtom
  suffixes <- many pSuffix
  return $ foldl applySuffix base suffixes

data Suffix = IndexSuffix Expr | CallSuffix [Expr] | MethodSuffix Text [Expr]

applySuffix ::  Expr -> Suffix -> Expr
applySuffix e (IndexSuffix i) = ArrayAccess e i
applySuffix (Var name) (CallSuffix args) = FuncCall name args
applySuffix e (MethodSuffix m args) = MethodCall e m args
applySuffix e (CallSuffix _) = e  -- Invalid, but handle gracefully

pSuffix :: Parser Suffix
pSuffix = choice
  [ IndexSuffix <$> brackets pExpr
  , try $ do
      _ <- symbol "."
      method <- identifier
      args <- parens (commaSep pExpr)
      return $ MethodSuffix method args
  , CallSuffix <$> parens (commaSep pExpr)
  ]

-- | Atom parser (base expressions)
pAtom :: Parser Expr
pAtom = choice
  [ try $ FloatLit <$> float
  , IntLit <$> integer
  , BoolLit True <$ reserved "True"
  , BoolLit False <$ reserved "False"
  , StringLit <$> stringLiteral
  , ArrayLit <$> brackets (commaSep pExpr)
  , try pTuple
  , parens pExpr
  , Var <$> identifier
  ]

pTuple :: Parser Expr
pTuple = parens $ do
  first <- pExpr
  _ <- symbol ","
  rest <- commaSep pExpr
  return $ Tuple (first : rest)

-- | Type parser
pType ::  Parser Type
pType = choice
  [ TInt <$ reserved "int"
  , TInt64 <$ reserved "int64"
  , TFloat <$ reserved "float"
  , TDouble <$ reserved "double"
  , TString <$ reserved "string"
  , TBool <$ reserved "bool"
  , TAuto <$ reserved "auto"
  , TVoid <$ reserved "void"
  , try $ TVector <$> (reserved "vector" *> symbol "<" *> pType <* symbol ">")
  , try $ TSet <$> (reserved "set" *> symbol "<" *> pType <* symbol ">")
  , try $ TDeque <$> (reserved "deque" *> symbol "<" *> pType <* symbol ">")
  , try $ TPriorityQueue <$> (reserved "pq" *> symbol "<" *> pType <* symbol ">")
  , try $ do
      reserved "map"
      _ <- symbol "<"
      k <- pType
      _ <- symbol ","
      v <- pType
      _ <- symbol ">"
      return $ TMap k v
  , try $ do
      reserved "pair"
      _ <- symbol "<"
      a <- pType
      _ <- symbol ","
      b <- pType
      _ <- symbol ">"
      return $ TPair a b
  , TArray <$> brackets pType
  ]