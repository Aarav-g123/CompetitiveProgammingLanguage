module Language.AST where

import Data.Text (Text)

type Ident = Text

data Program = Program [TopDecl]
  deriving (Eq, Show)

data TopDecl
  = TopFun Fun  -- Changed from topFun to TopFun (capitalized)
  deriving (Eq, Show)

data Fun = Fun
  { funName   :: Ident
  , funParams :: [(Ident, Type)]
  , funRet    :: Type
  , funBody   :: [Stmt]
  } deriving (Eq, Show)

data Stmt
  = SLet Ident (Maybe Type) Expr
  | SAssign LValue Expr
  | SIf Expr [Stmt] [Stmt]
  | SWhile Expr [Stmt]
  | SFor Ident Expr Expr [Stmt]
  | SExpr Expr
  | SReturn (Maybe Expr)
  deriving (Eq, Show)

data LValue
  = LVar Ident
  | LIndex Ident Expr
  deriving (Eq, Show)

data Expr
  = EVar Ident
  | ELit Lit
  | ECall Ident [Expr]
  | EBin Op Expr Expr
  | EUn Op Expr
  | EIndex Expr Expr
  | ERange Expr Expr
  | EArray [Expr]
  | EIf Expr Expr Expr
  deriving (Eq, Show)

data Lit
  = LInt Integer
  | LBool Bool
  | LString Text
  deriving (Eq, Show)

data Op = Add | Sub | Mul | Div | Mod | Eq | Ne | Lt | Le | Gt | Ge | And | Or | Not
  deriving (Eq, Show)

data Type
  = TInt
  | TBool
  | TString
  | TArray Type
  | TSlice Type
  | TTuple [Type]
  | TVoid
  deriving (Eq, Show)