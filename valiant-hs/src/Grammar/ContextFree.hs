module Grammar.ContextFree where

import Control.Monad
import Data.List (nub, subsequences)
import Data.Set qualified as S

type Symbol = String

type Production = (Symbol, [Symbol])

data CFG = CFG
  { startSymbol :: Symbol,
    nonTerminals :: [Symbol],
    terminals :: [Symbol],
    productions :: [Production]
  }
  deriving (Show)

instance Eq CFG where
  (CFG sa nta ta pa) == (CFG sb ntb tb pb) =
    and
      [ sa == sb,
        S.fromList nta == S.fromList ntb,
        S.fromList ta == S.fromList tb,
        S.fromList pa == S.fromList pb
      ]

renameNonTerminal :: Symbol -> Symbol -> CFG -> CFG
renameNonTerminal old new cfg@(CFG startSymbol nonTerms terms prods) =
  let replaceNonTerm :: Symbol -> Symbol -> Symbol -> Symbol
      replaceNonTerm old new sym = if sym == old then new else sym

      replaceSymbols :: [Symbol] -> [Symbol]
      replaceSymbols = map (replaceNonTerm old new)

      replaceProduction :: Production -> Production
      replaceProduction (lhs, rhs) = (replaceNonTerm old new lhs, replaceSymbols rhs)

      newProductions = map replaceProduction prods
   in CFG (replaceNonTerm old new startSymbol) (map (replaceNonTerm old new) nonTerms) terms newProductions

-- Function to eliminate the start symbol from right-hand sides and introduce a new start symbol S0
eliminateStartSymbol :: CFG -> CFG
eliminateStartSymbol cfg@(CFG {startSymbol = oldStart, productions = oldProductions}) =
  CFG
    { startSymbol = newStart,
      nonTerminals = newNonTerminals,
      terminals = terminals cfg,
      productions = newProductions
    }
  where
    newStart = "S0" -- TODO: generalize
    newProductions = (newStart, [oldStart]) : oldProductions
    newNonTerminals = nub $ newStart : nonTerminals cfg

replaceProduction :: CFG -> Production -> Production -> CFG
replaceProduction cfg oldProd newProd =
  let newProds = map (replaceIfEqual oldProd newProd) (productions cfg)
      newNonTerms =
        if elem (fst oldProd) (nonTerminals cfg)
          then (fst newProd) : filter (/= fst oldProd) (nonTerminals cfg)
          else nonTerminals cfg
      newStartSymbol = if startSymbol cfg == fst oldProd then fst newProd else startSymbol cfg
   in CFG
        { startSymbol = newStartSymbol,
          nonTerminals = newNonTerms,
          terminals = terminals cfg,
          productions = newProds
        }
  where
    replaceIfEqual :: Production -> Production -> Production -> Production
    replaceIfEqual old new p = if p == old then new else p

-- TODO: `splitProduction` but in a tree shape instead of a list shape
splitProduction :: Production -> [Production]
splitProduction (lhs, rhs) =
  if length rhs <= 2
    then [(lhs, rhs)]
    else
      let newNonTerms = map (\i -> lhs ++ "_" ++ show i) [1 .. length rhs - 2]
          newProductions :: [(Symbol, Symbol)]
          newProductions = zipWith (\nt1 nt2 -> (nt1, nt2)) newNonTerms (tail rhs)
          newProductions' :: [Production]
          newProductions' = zipWith (curry (\((lhs, rhs), s) -> (lhs, [rhs, s]))) newProductions (map fst $ tail newProductions)
          finalProduction = (last newNonTerms, [last (init rhs), last rhs])
       in (lhs, [head rhs, head newNonTerms]) : newProductions' ++ [finalProduction]

-- Function to eliminate right-hand sides with more than 2 non-terminals
-- eliminateMoreThanTwoNonTerminals :: CFG -> CFG
eliminateMoreThanTwoNonTerminals :: CFG -> [Production]
eliminateMoreThanTwoNonTerminals cfg@(CFG {nonTerminals = nonTerms, productions = oldProductions}) =
  let toReplace = filter ((> 2) . length . snd) oldProductions
   in toReplace

-- Function to remove epsilon productions from a CFG
removeEpsilonProductions :: CFG -> CFG
removeEpsilonProductions cfg = cfg {productions = newProductions}
  where
    newProductions = filter (\(lhs, rhs) -> not $ hasEpsilon lhs rhs) updatedProductions
    updatedProductions = productions cfg
    hasEpsilon lhs rhs = lhs `elem` nonTerminals cfg && null rhs

-- Function to determine nullable non-terminals
nullableNonTerminals :: CFG -> [Symbol]
nullableNonTerminals (CFG {productions = prods}) = fixpoint $ filterNullable initialNullable
  where
    fixpoint xs = if null newNullable then xs else fixpoint $ nub (xs ++ newNullable)
    initialNullable = [lhs | (lhs, rhs) <- prods, null rhs] -- Start with non-terminals that directly derive ε
    filterNullable nullable = [lhs | (lhs, rhs) <- prods, all (`elem` nullable) rhs]
    newNullable = filterNullable initialNullable

-- Function to generate all versions of a rule with some nullable symbols omitted
generateVersions :: [Symbol] -> [Symbol] -> [Symbol] -> Production -> [Production]
generateVersions nullableSymbols nonTerms terms (lhs, rhs) =
  filter isValid $ filter (not . isEmpty) $ map (\sub -> (lhs, filter (`notElem` sub) rhs)) subsets
  where
    subsets = tail $ subsequences rhs
    isEmpty (_, []) = True
    isEmpty _ = False
    isValid (_, rhs') = all (`elem` nonTermsAndTerms) rhs'
    nonTermsAndTerms = nonTerms ++ terms

-- Sample main function to test transformations
main :: IO ()
main = do
  -- Sample grammar
  let grammar =
        CFG
          { startSymbol = "S",
            nonTerminals = ["S", "A", "B", "C"],
            terminals = ["a", "b", "c"],
            productions =
              [ ("S", ["A", "b", "B"]),
                ("S", ["C"]),
                ("B", ["A", "A"]),
                ("B", ["A", "C"]),
                ("C", ["b"]),
                ("C", ["c"]),
                ("A", ["a"]),
                ("A", [])
              ]
          }

  putStrLn "Original Grammar:"
  print grammar

  let transformations =
        [ ("eliminateStartSymbol", eliminateStartSymbol),
          -- ("eliminateMoreThanTwoNonTerminals", eliminateMoreThanTwoNonTerminals),
          ("removeEpsilonProductions", removeEpsilonProductions)
        ]

  finalGrammar <- foldM (\g (s, f) -> logAndTransform g s f) grammar transformations

  putStrLn "\nFinal Grammar after applying all transformations:"
  print finalGrammar

logAndTransform :: CFG -> [Char] -> (CFG -> CFG) -> IO CFG
logAndTransform g s f = do
  let transformed = f g
  putStrLn $ "\nGrammar after " ++ s ++ " transformation:"
  print transformed
  return transformed

data Tree a = Branch (Tree a) (Tree a) | Leaf a | Empty deriving (Show)

-- Function to split a string into a maximally balanced tree
splitStringToTree :: String -> Tree Char
splitStringToTree [] = Empty
splitStringToTree [x] = Leaf x
splitStringToTree str =
  let (left, right) = splitAt (length str `div` 2) str
   in Branch (splitStringToTree left) (splitStringToTree right)

-- TODO: make this function to convert a tree into a [Production]
treeToGrams :: Tree Symbol -> [(Symbol, [Symbol])]
treeToGrams (Branch (Leaf xa) (Leaf xb)) = [("NAME", [xa, xb])]
treeToGrams (Branch (Leaf xa) Empty) = [("NAME", [xa])]
treeToGrams (Branch l r) = []
treeToGrams Empty = []
