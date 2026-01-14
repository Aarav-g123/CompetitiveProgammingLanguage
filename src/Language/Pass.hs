module Language.Pass
  ( runPipeline
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Language.Lexer (lexTokens)
import Language.Parser (parseProgram)
import Language.Desugar (desugar)
import Language.AST
import Language.Error (Error(..))
import Language.Options (Options(..))
import Language.IR
import Language.CGen (emitC)
import Language.Typecheck (typecheck)
import Language.Runtime (runtimeC)

runPipeline :: Options -> Text -> Either Error Text
runPipeline _opts src = do
  tokens <- lexTokens src
  ast    <- parseProgram tokens
  _      <- typecheck ast
  let core  = desugar ast
      ir    = lower core
      ccode = emitC ir
  pure $ T.unlines [runtimeC, ccode]
  where
    lower :: Program -> IRProgram
    lower (Program decls) = IRProgram $ map lowerFun decls

    lowerFun :: TopDecl -> IRFun
    lowerFun (TopFun (Fun name params _ret body)) =
      IRFun name (map fst params) (lowerStmts body)

    lowerStmts :: [Stmt] -> [IRStmt]
    lowerStmts = map lowerStmt

    lowerStmt :: Stmt -> IRStmt
    lowerStmt (SReturn (Just (ELit (LInt n)))) = IRReturn (Just (IRLitInt n))
    lowerStmt (SReturn Nothing)                = IRReturn Nothing
    lowerStmt _                                = IRReturn (Just (IRLitInt 0))  -- default