module Language.IR where

import Data.Text (Text)

type Name = Text

data IRProgram = IRProgram [IRFun]
  deriving (Eq, Show)

data IRFun = IRFun
  { irName :: Name
  , irArgs :: [Name]
  , irBody :: [IRStmt]
  } deriving (Eq, Show)

data IRStmt
  = IRAssign Name IRExpr
  | IRIf IRExpr [IRStmt] [IRStmt]
  | IRWhile IRExpr [IRStmt]
  | IRReturn (Maybe IRExpr)
  deriving (Eq, Show)

data IRExpr
  = IRVar Name
  | IRLitInt Integer
  | IRBin Text IRExpr IRExpr
  | IRCall Name [IRExpr]
  deriving (Eq, Show)