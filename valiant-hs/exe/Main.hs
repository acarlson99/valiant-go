module Main where

import Grammar.ContextFree
import Grammar.ContextFree (CFG)
import Grammar.Convert (bnfGram, convert)
import Grammar.Tokenize (getTokenizer)
import Options.Applicative qualified as Opt
import System.Exit (die)
import Tree
import Valiant (valiantParse, _productionRules)

data Options = Options
  { gram :: String,
    tokenizer :: String,
    files :: [String]
  }
  deriving (Show)

optsParser :: Opt.ParserInfo Options
optsParser =
  Opt.info
    (options Opt.<**> Opt.helper)
    ( Opt.fullDesc
        <> Opt.progDesc "Parse files with specified grammar and tokenizer"
        <> Opt.header "parse - a simple parser utility"
    )
  where
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
  (Options gram tokenizer files) <- Opt.execParser optsParser
  content <-
    if null files
      then sequence [getContents] -- Read from stdin if no files are provided
      else mapM readFile files -- Read from provided files
      -- putStrLn $ "Grammar file: " ++ gram
      -- gramContent <- readFile gram
      -- putStrLn gramContent
      -- TODO: parse grammar char list to CFG
  putStrLn $ "Tokenizer: " ++ tokenizer
  tf <- case getTokenizer tokenizer of
    Left xs -> return xs
    Right msg -> die msg
  putStrLn "Files content:"
  -- let g = convert bnfCFG
  let g = bnfGram
  putStrLn "Parsing with grammar:"
  mapM_ print g
  putStrLn "gonna parse"
  case content of
    (x : _) -> do
      putStrLn $ "content" ++ show x
      putStrLn "parsed to:"
      mapM_ (putStrLn . printVP) $ valiantParse g $ tf x
    _ -> putStrLn "ohhhh"

printVP :: (Show nt, Show t) => Tree (Either nt t) -> String
printVP = printVP'
  where
    showEither (Left a) = show a
    showEither (Right a) = show a
    printVP' (Node a l r) = "(" ++ showEither a ++ concatMap printVP' [l, r] ++ ")"
    printVP' Empty = ""
