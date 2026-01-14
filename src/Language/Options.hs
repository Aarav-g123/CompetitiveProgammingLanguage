module Language.Options
  ( Options(..)
  , parseOptions
  ) where

import Options.Applicative

data Options = Options
  { optInput  :: FilePath
  , optOutput :: FilePath
  , optDebug  :: Bool
  , optUnsafe :: Bool
  } deriving (Show)

parseOptions :: IO Options
parseOptions = execParser parserInfo
  where
    parserInfo = info (helper <*> opts) (fullDesc <> progDesc "Compile cp-lang source to C")
    opts = Options
      <$> strOption (long "input"  <> short 'i' <> metavar "FILE" <> help "Source file")
      <*> strOption (long "output" <> short 'o' <> metavar "FILE" <> value "a.c" <> showDefault <> help "Output C file")
      <*> switch    (long "debug"  <> help "Enable debug checks")
      <*> switch    (long "unsafe" <> help "Disable bounds checks")