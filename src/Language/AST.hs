{-# LANGUAGE DeriveGeneric #-}

module Language.AST where

import Data.Text (Text)

-- | The main program is a list of statements
type Program = [Statement]

-- | Statements in the CP language
data Statement
  = VarDecl Text Type Expr              -- x: int = 5
  | VarAssign Text Expr                 -- x = 5
  | ArrayDecl Text Type Expr            -- arr:  [int] = [1,2,3]
  | FuncDef Text [(Text, Type)] Type [Statement]  -- def func(x: int) -> int: 
  | Return Expr                         -- return expr
  | If Expr [Statement] [(Expr, [Statement])] (Maybe [Statement])  -- if/elif/else
  | While Expr [Statement]              -- while condition:
  | For Text Expr Expr [Statement]      -- for i in range(n):
  | ForEach Text Text [Statement]       -- for x in arr:
  | Print [Expr]                        -- print(expr1, expr2)
  | Read [Text]                         -- read(x, y)
  | ExprStmt Expr                       -- expr as statement
  | Break                               -- break
  | Continue                            -- continue
  | Comment Text                        -- # comment
  | Pass                                -- pass (no-op)
  deriving (Show, Eq)

-- | Expressions
data Expr
  = IntLit Integer                      -- 42
  | FloatLit Double                     -- 3.14
  | StringLit Text                      -- "hello"
  | BoolLit Bool                        -- True, False
  | Var Text                            -- variable reference
  | ArrayLit [Expr]                     -- [1, 2, 3]
  | ArrayAccess Expr Expr               -- arr[i]
  | BinOp BinOperator Expr Expr         -- a + b
  | UnaryOp UnaryOperator Expr          -- -x, not x
  | FuncCall Text [Expr]                -- func(args)
  | MethodCall Expr Text [Expr]         -- obj.method(args)
  | Ternary Expr Expr Expr              -- x if cond else y
  | Lambda [(Text, Type)] Expr          -- lambda x: x + 1
  | Tuple [Expr]                        -- (a, b, c)
  | Range Expr Expr (Maybe Expr)        -- range(start, end, step)
  deriving (Show, Eq)

-- | Binary operators
data BinOperator
  = Add | Sub | Mul | Div | Mod | Pow   -- Arithmetic
  | Eq | NEq | Lt | Gt | LtE | GtE      -- Comparison
  | And | Or                             -- Logical
  | BitAnd | BitOr | BitXor | Shl | Shr -- Bitwise
  deriving (Show, Eq)

-- | Unary operators
data UnaryOperator
  = Neg | Not | BitNot
  deriving (Show, Eq)

-- | Types in CP language
data Type
  = TInt
  | TInt64                              -- long long
  | TFloat
  | TDouble
  | TString
  | TBool
  | TArray Type                         -- [int], [string]
  | TVector Type                        -- vector<int>
  | TSet Type                           -- set<int>
  | TMap Type Type                      -- map<int, string>
  | TPair Type Type                     -- pair<int, int>
  | TDeque Type                         -- deque<int>
  | TPriorityQueue Type                 -- priority_queue<int>
  | TVoid
  | TAuto                               -- auto (type inference)
  deriving (Show, Eq)