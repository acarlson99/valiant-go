module Main where

import Grammar.Tokenize (tokenizers)
import Options.Applicative qualified as Opt

data Options = Options
  { gram :: String,
    tokenizer :: String,
    files :: [String]
  }
  deriving (Show)

options :: Opt.Parser Options
options =
  Options
    <$> Opt.strOption
      ( Opt.long "gram"
          <> Opt.metavar "GRAM"
          <> Opt.help "Path to grammar file"
      )
    <*> Opt.strOption
      ( Opt.long "tokenizer"
          <> Opt.metavar "TOKENIZER"
          <> Opt.help "Tokenizer type"
          <> Opt.value "default"
          <> Opt.showDefault
      )
    <*> Opt.many (Opt.argument Opt.str (Opt.metavar "FILES..."))

main :: IO ()
main = do
  opts <- Opt.execParser optsParser
  handleOptions opts

optsParser :: Opt.ParserInfo Options
optsParser =
  Opt.info
    (options Opt.<**> Opt.helper)
    ( Opt.fullDesc
        <> Opt.progDesc "Parse files with specified grammar and tokenizer"
        <> Opt.header "parse - a simple parser utility"
    )

handleOptions :: Options -> IO ()
handleOptions (Options gram tokenizer files) = do
  content <-
    if null files
      then getContents -- Read from stdin if no files are provided
      else concat <$> mapM readFile files -- Read from provided files
  putStrLn $ "Grammar file: " ++ gram
  gramContent <- readFile gram
  putStrLn gramContent
  putStrLn $ "Tokenizer: " ++ tokenizer
  case ($ gramContent) <$> matchTokenizer tokenizer of
    Just xs -> print xs
    Nothing -> putStrLn $ "Invalid tokenizer: " ++ tokenizer ++ " please select from: " ++ concatMap fst tokenizers
  putStrLn "Files content:"
  putStrLn content

matchTokenizer :: String -> Maybe (String -> [String])
matchTokenizer s =
  snd
    <$> case filter ((== s) . fst) tokenizers of
      (x : _) -> Just x
      [] -> Nothing
