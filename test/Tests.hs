module Main where

import Parser (parseCP)
import Transpiler (transpileToCpp)
import Test.Hspec

main :: IO ()
main = hspec $ do
    describe "Parser Tests" $ do
        it "Parses non-empty .cp file" $ 
            parseCP "print 42" `shouldBe` Right ["print 42"]

        it "Handles empty .cp file gracefully" $
            parseCP "" `shouldBe` Left "File is empty"

    describe "Transpiler Tests" $ do
        it "Converts simple AST" $
            transpileToCpp ["print 42"] `shouldBe` unlines ["// Transpiled line: print 42"]