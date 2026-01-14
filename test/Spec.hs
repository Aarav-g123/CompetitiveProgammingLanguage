{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.Hspec
import Lib
import Language.AST
import Language.Parser
import Language.Transpiler

main ::  IO ()
main = hspec $ do
  describe "Parser" $ do
    it "parses integer literals" $ do
      parseExpr "42" `shouldBe` Right (IntLit 42)
    
    it "parses boolean literals" $ do
      parseExpr "True" `shouldBe` Right (BoolLit True)
      parseExpr "False" `shouldBe` Right (BoolLit False)
    
    it "parses string literals" $ do
      parseExpr "\"hello\"" `shouldBe` Right (StringLit "hello")
    
    it "parses binary operations" $ do
      parseExpr "1 + 2" `shouldBe` Right (BinOp Add (IntLit 1) (IntLit 2))
      parseExpr "3 * 4" `shouldBe` Right (BinOp Mul (IntLit 3) (IntLit 4))
    
    it "parses function calls" $ do
      parseExpr "min(1, 2)" `shouldBe` Right (FuncCall "min" [IntLit 1, IntLit 2])
    
    it "parses array literals" $ do
      parseExpr "[1, 2, 3]" `shouldBe` Right (ArrayLit [IntLit 1, IntLit 2, IntLit 3])

  describe "Transpiler" $ do
    it "transpiles print statements" $ do
      let ast = [Print [StringLit "hello"]]
      transpileToCpp ast `shouldContain` "cout << \"hello\" << endl;"
    
    it "transpiles variable declarations" $ do
      let ast = [VarDecl "x" TInt (IntLit 5)]
      transpileToCpp ast `shouldContain` "int x = 5;"
    
    it "transpiles for loops" $ do
      let ast = [For "i" (IntLit 0) (IntLit 10) [Print [Var "i"]]]
      let result = transpileToCpp ast
      result `shouldContain` "for (int i = 0; i < 10; i++)"

  describe "Builtins" $ do
    it "transpiles sort correctly" $ do
      transpileBuiltin "sort" ["arr"] `shouldBe` "sort(all(arr))"
    
    it "transpiles len correctly" $ do
      transpileBuiltin "len" ["arr"] `shouldBe` "sz(arr)"
    
    it "transpiles gcd correctly" $ do
      transpileBuiltin "gcd" ["a", "b"] `shouldBe` "__gcd(a, b)"

  describe "End-to-end" $ do
    it "transpiles a simple program" $ do
      let code = "n:  int = 10\nprint(n)"
      case transpileCode code of
        Left err -> expectationFailure $ "Parse error: " ++ err
        Right result -> do
          result `shouldContain` "#include <bits/stdc++.h>"
          result `shouldContain` "int n = 10;"
          result `shouldContain` "cout << n << endl;"