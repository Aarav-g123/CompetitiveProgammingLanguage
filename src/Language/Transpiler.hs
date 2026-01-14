{-# LANGUAGE OverloadedStrings #-}

module Language.Transpiler
  ( transpileToCpp
  , transpileExpr
  , transpileStatement
  ) where

import Language.AST
import Language.Builtins

import Data.Text (Text)
import qualified Data.Text as T
import Data.List (intercalate)

-- | Transpile a program to C++ code
transpileToCpp :: Program -> String
transpileToCpp stmts = unlines
  [ cppHeaders
  , cppTypedefs
  , cppMacros
  , ""
  , transpileProgram stmts
  ]

-- | C++ headers for competitive programming
cppHeaders :: String
cppHeaders = unlines
  [ "#include <bits/stdc++.h>"
  , "using namespace std;"
  ]

-- | Useful typedefs for CP
cppTypedefs :: String
cppTypedefs = unlines
  [ "typedef long long ll;"
  , "typedef pair<int, int> pii;"
  , "typedef pair<ll, ll> pll;"
  , "typedef vector<int> vi;"
  , "typedef vector<ll> vll;"
  , "typedef vector<pii> vpii;"
  ]

-- | Useful macros for CP
cppMacros :: String
cppMacros = unlines
  [ "#define pb push_back"
  , "#define mp make_pair"
  , "#define fi first"
  , "#define se second"
  , "#define all(x) (x).begin(), (x).end()"
  , "#define rall(x) (x).rbegin(), (x).rend()"
  , "#define sz(x) (int)(x).size()"
  , "#define FOR(i, a, b) for (int i = (a); i < (b); i++)"
  , "#define FORR(i, a, b) for (int i = (a); i >= (b); i--)"
  , "#define endl '\\n'"
  , ""
  , "const int MOD = 1e9 + 7;"
  , "const int INF = 1e9;"
  , "const ll LINF = 1e18;"
  ]

-- | Transpile program body
transpileProgram ::  Program -> String
transpileProgram stmts =
  let (funcs, mainStmts) = separateFunctions stmts
      funcCode = concatMap (transpileStatement 0) funcs
      mainCode = if null mainStmts
                 then ""
                 else unlines
                   [ "int main() {"
                   , "    ios_base::sync_with_stdio(false);"
                   , "    cin.tie(NULL);"
                   , ""
                   , concatMap (transpileStatement 1) mainStmts
                   , "    return 0;"
                   , "}"
                   ]
  in funcCode ++ mainCode

-- | Separate function definitions from main code
separateFunctions :: Program -> ([Statement], [Statement])
separateFunctions = foldr categorize ([], [])
  where
    categorize s@(FuncDef _ _ _ _) (fs, ms) = (s: fs, ms)
    categorize s (fs, ms) = (fs, s: ms)

-- | Transpile a single statement
transpileStatement :: Int -> Statement -> String
transpileStatement indent stmt = indentStr ++ case stmt of
  VarDecl name typ expr ->
    transpileType typ ++ " " ++ T.unpack name ++ " = " ++ transpileExpr expr ++ ";\n"
  
  VarAssign name expr ->
    T.unpack name ++ " = " ++ transpileExpr expr ++ ";\n"
  
  ArrayDecl name typ expr ->
    transpileType typ ++ " " ++ T.unpack name ++ " = " ++ transpileExpr expr ++ ";\n"
  
  FuncDef name params retType body ->
    transpileType retType ++ " " ++ T.unpack name ++ "(" ++
    intercalate ", " (map transpileParam params) ++ ") {\n" ++
    concatMap (transpileStatement (indent + 1)) body ++
    indentStr ++ "}\n\n"
  
  Return expr ->
    "return " ++ transpileExpr expr ++ ";\n"
  
  If cond thenBody elifs elseBody ->
    "if (" ++ transpileExpr cond ++ ") {\n" ++
    concatMap (transpileStatement (indent + 1)) thenBody ++
    indentStr ++ "}" ++
    concatMap (transpileElif indent) elifs ++
    maybe "" (transpileElse indent) elseBody ++ "\n"
  
  While cond body ->
    "while (" ++ transpileExpr cond ++ ") {\n" ++
    concatMap (transpileStatement (indent + 1)) body ++
    indentStr ++ "}\n"
  
  For var start end body ->
    "for (int " ++ T.unpack var ++ " = " ++ transpileExpr start ++
    "; " ++ T.unpack var ++ " < " ++ transpileExpr end ++
    "; " ++ T.unpack var ++ "++) {\n" ++
    concatMap (transpileStatement (indent + 1)) body ++
    indentStr ++ "}\n"
  
  ForEach var arr body ->
    "for (auto& " ++ T.unpack var ++ " : " ++ T.unpack arr ++ ") {\n" ++
    concatMap (transpileStatement (indent + 1)) body ++
    indentStr ++ "}\n"
  
  Print exprs ->
    "cout << " ++ intercalate " << \" \" << " (map transpileExpr exprs) ++ " << endl;\n"
  
  Read vars ->
    concatMap (\v -> indentStr ++ "cin >> " ++ T.unpack v ++ ";\n") vars
  
  ExprStmt expr ->
    transpileExpr expr ++ ";\n"
  
  Break -> "break;\n"
  
  Continue -> "continue;\n"
  
  Comment text -> "// " ++ T.unpack text ++ "\n"
  
  Pass -> "// pass\n"
  
  where
    indentStr = replicate (indent * 4) ' '

transpileElif :: Int -> (Expr, [Statement]) -> String
transpileElif indent (cond, body) =
  " else if (" ++ transpileExpr cond ++ ") {\n" ++
  concatMap (transpileStatement (indent + 1)) body ++
  replicate (indent * 4) ' ' ++ "}"

transpileElse :: Int -> [Statement] -> String
transpileElse indent body =
  " else {\n" ++
  concatMap (transpileStatement (indent + 1)) body ++
  replicate (indent * 4) ' ' ++ "}"

-- | Transpile parameter
transpileParam :: (Text, Type) -> String
transpileParam (name, typ) = transpileType typ ++ " " ++ T.unpack name

-- | Transpile type to C++
transpileType ::  Type -> String
transpileType TInt = "int"
transpileType TInt64 = "ll"
transpileType TFloat = "float"
transpileType TDouble = "double"
transpileType TString = "string"
transpileType TBool = "bool"
transpileType TVoid = "void"
transpileType TAuto = "auto"
transpileType (TArray t) = "vector<" ++ transpileType t ++ ">"
transpileType (TVector t) = "vector<" ++ transpileType t ++ ">"
transpileType (TSet t) = "set<" ++ transpileType t ++ ">"
transpileType (TDeque t) = "deque<" ++ transpileType t ++ ">"
transpileType (TPriorityQueue t) = "priority_queue<" ++ transpileType t ++ ">"
transpileType (TMap k v) = "map<" ++ transpileType k ++ ", " ++ transpileType v ++ ">"
transpileType (TPair a b) = "pair<" ++ transpileType a ++ ", " ++ transpileType b ++ ">"

-- | Transpile expression
transpileExpr :: Expr -> String
transpileExpr expr = case expr of
  IntLit n -> show n
  FloatLit f -> show f
  StringLit s -> "\"" ++ T.unpack s ++ "\""
  BoolLit True -> "true"
  BoolLit False -> "false"
  Var name -> T.unpack name
  
  ArrayLit exprs -> "{" ++ intercalate ", " (map transpileExpr exprs) ++ "}"
  
  ArrayAccess arr idx -> transpileExpr arr ++ "[" ++ transpileExpr idx ++ "]"
  
  BinOp op e1 e2 -> "(" ++ transpileExpr e1 ++ " " ++ transpileBinOp op ++ " " ++ transpileExpr e2 ++ ")"
  
  UnaryOp op e -> transpileUnaryOp op ++ transpileExpr e
  
  FuncCall name args -> transpileBuiltin name (map transpileExpr args)
  
  MethodCall obj method args ->
    transpileExpr obj ++ "." ++ T.unpack method ++
    "(" ++ intercalate ", " (map transpileExpr args) ++ ")"
  
  Ternary cond t f ->
    "(" ++ transpileExpr cond ++ " ? " ++ transpileExpr t ++ " : " ++ transpileExpr f ++ ")"
  
  Lambda params body ->
    "[&](" ++ intercalate ", " (map transpileParam params) ++ ") { return " ++ transpileExpr body ++ "; }"
  
  Tuple exprs -> "make_tuple(" ++ intercalate ", " (map transpileExpr exprs) ++ ")"
  
  Range start end step ->
    -- This is typically used in for loops, handled specially
    "/* range(" ++ transpileExpr start ++ ", " ++ transpileExpr end ++ ") */"

-- | Transpile binary operator
transpileBinOp :: BinOperator -> String
transpileBinOp Add = "+"
transpileBinOp Sub = "-"
transpileBinOp Mul = "*"
transpileBinOp Div = "/"
transpileBinOp Mod = "%"
transpileBinOp Pow = "/* pow */"  -- Need special handling
transpileBinOp Eq = "=="
transpileBinOp NEq = "!="
transpileBinOp Lt = "<"
transpileBinOp Gt = ">"
transpileBinOp LtE = "<="
transpileBinOp GtE = ">="
transpileBinOp And = "&&"
transpileBinOp Or = "||"
transpileBinOp BitAnd = "&"
transpileBinOp BitOr = "|"
transpileBinOp BitXor = "^"
transpileBinOp Shl = "<<"
transpileBinOp Shr = ">>"

-- | Transpile unary operator
transpileUnaryOp :: UnaryOperator -> String
transpileUnaryOp Neg = "-"
transpileUnaryOp Not = "!"
transpileUnaryOp BitNot = "~"