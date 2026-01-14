module Language.CLI (run) where

import Language.Options
import Language.Pass
import Language.Error (renderError)
import qualified Data.Text.IO as T

run :: IO ()
run = do
  opts <- parseOptions
  src  <- T.readFile (optInput opts)
  res  <- runPipeline opts src
  case res of
    Left err  -> putStrLn (renderError err)
    Right out -> do
      T.writeFile (optOutput opts) out
      putStrLn $ "Wrote " ++ optOutput opts