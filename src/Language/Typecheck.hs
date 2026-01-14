module Language.Typecheck
  ( typecheck
  ) where

import Language.AST
import Language.Error (Error(..))

typecheck :: Program -> Either Error Program
typecheck prog@(Program decls) = do
  mapM_ checkFun decls
  Right prog
  where
    checkFun :: TopDecl -> Either Error ()
    checkFun (TopFun fun) = do
      mapM_ (\(_, typ) -> checkType typ) (funParams fun)
      checkType (funRet fun)
      mapM_ checkStmt (funBody fun)
    
    checkStmt :: Stmt -> Either Error ()
    checkStmt (SLet _ (Just typ) _) = checkType typ
    checkStmt (SLet _ Nothing _) = Right ()
    checkStmt (SReturn (Just _)) = Right ()
    checkStmt (SReturn Nothing) = Right ()
    checkStmt _ = Right ()  -- TODO: Proper checking
    
    checkType :: Type -> Either Error ()
    checkType TInt = Right ()
    checkType TBool = Right ()
    checkType TString = Right ()
    checkType (TArray t) = checkType t
    checkType (TSlice t) = checkType t
    checkType (TTuple ts) = mapM_ checkType ts
    checkType TVoid = Right ()