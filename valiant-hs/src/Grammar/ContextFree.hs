module Grammar.ContextFree where

import Control.Monad
import Data.Bifunctor qualified
import Data.List (nub, subsequences)
import Data.Set qualified as S

type Symbol = String

type Production = (Symbol, [Symbol])

data CFG = CFG
  { startSymbol :: Symbol,
    nonTerminals :: S.Set Symbol,
    terminals :: S.Set Symbol,
    productions :: S.Set Production
  }
  deriving (Show)

-- Add a production to the CFG
addProduction :: CFG -> Production -> CFG
addProduction cfg prod =
  let newProductions = prod `S.insert` productions cfg
      newNonTerms = S.map fst $ newProductions `S.union` productions cfg
   in cfg {productions = newProductions, nonTerminals = newNonTerms}

-- Remove a production from the CFG
removeProduction :: CFG -> Production -> CFG
removeProduction cfg prod =
  let newProductions = prod `S.delete` productions cfg
      newNonTerms = S.map fst newProductions
   in cfg {productions = newProductions, nonTerminals = newNonTerms}

instance Eq CFG where
  (CFG sa nta ta pa) == (CFG sb ntb tb pb) =
    and
      [ sa == sb,
        nta == ntb,
        ta == tb,
        pa == pb
      ]

-- https://en.wikipedia.org/wiki/Chomsky_normal_form#START:_Eliminate_the_start_symbol_from_right-hand_sides
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
    newProductions = (newStart, [oldStart]) `S.insert` oldProductions
    newNonTerminals = newStart `S.insert` nonTerminals cfg

-- TODO: `splitProduction` but in a tree shape instead of a list shape
splitProduction :: Production -> [Production]
splitProduction (lhs, rhs) =
  if length rhs <= 2
    then [(lhs, rhs)]
    else
      let newNonTerms = map (\i -> lhs ++ "_" ++ show i) [1 .. length rhs - 2]
          newProductions :: [(Symbol, Symbol)]
          newProductions = zip newNonTerms (tail rhs)
          newProductions' :: [Production]
          newProductions' = zipWith (curry (\((lhs, rhs), s) -> (lhs, [rhs, s]))) newProductions (map fst $ tail newProductions)
          finalProduction = (last newNonTerms, [last (init rhs), last rhs])
       in (lhs, [head rhs, head newNonTerms]) : newProductions' ++ [finalProduction]

-- https://en.wikipedia.org/wiki/Chomsky_normal_form#BIN:_Eliminate_right-hand_sides_with_more_than_2_nonterminals
eliminateMoreThanTwoNonTerminals :: CFG -> CFG
eliminateMoreThanTwoNonTerminals cfg@(CFG {nonTerminals = nonTerms, productions = oldProductions}) =
  let toReplace :: [Production]
      toReplace = filter ((> 2) . length . snd) $ S.toList oldProductions
      pairs = zip toReplace $ map splitProduction toReplace
      ins :: [Production] -> S.Set Production -> S.Set Production
      ins ps s = foldr S.insert s ps
      x = foldr (\(x, xs) prods -> ins xs $ x `S.delete` prods) oldProductions pairs
   in cfg {nonTerminals = S.map fst x, productions = x}

-- https://en.wikipedia.org/wiki/Chomsky_normal_form#DEL:_Eliminate_%CE%B5-rules
removeEpsilonProductions :: CFG -> CFG
removeEpsilonProductions cfg@(CFG {productions = oldProductions}) =
  let nullableNonTerminals = findNullableNonTerminals cfg
      newProductions = S.fromList . removeEmptyRules $ concatMap (inlineNullableProduction nullableNonTerminals) $ S.toList oldProductions
      removeEmptyRules = filter (not . null . snd)
   in cfg {productions = newProductions}

findNullableNonTerminals :: CFG -> S.Set Symbol
findNullableNonTerminals (CFG {productions = prods}) =
  let nullableFromEpsilon = S.map fst $ S.filter (\(_, rhs) -> null rhs) prods
      closure s =
        let newNullable = S.filter (\(lhs, rhs) -> all (`S.member` s) rhs && lhs `S.notMember` s) prods
            updatedSet = s `S.union` S.map fst newNullable
         in if S.null newNullable then s else closure updatedSet
   in closure nullableFromEpsilon

-- Function to inline a single production to include versions with and without nullable non-terminals
inlineNullableProduction :: S.Set Symbol -> Production -> [Production]
inlineNullableProduction nullable (lhs, rhs) =
  if any (`S.member` nullable) rhs
    then
      let versions = generateVersions nullable rhs
       in map (\v -> (lhs, v)) versions
    else [(lhs, rhs)]

-- Function to generate versions of a production with and without nullable non-terminals
generateVersions :: S.Set Symbol -> [Symbol] -> [[Symbol]]
generateVersions nullable [] = [[]]
generateVersions nullable (x : xs) =
  if x `S.member` nullable
    then
      let restVersions = generateVersions nullable xs
          withoutX = map (x :) restVersions
          withX = map (\v -> if null v then xs else v) restVersions
       in withX ++ withoutX
    else map (x :) $ generateVersions nullable xs

-- optimization

-- inlineUnitRules :: CFG -> CFG
inlineUnitRules cfg@(CFG {productions = oldProductions}) = inlineChildren oldProductions $ findUnitProductions oldProductions

inlineChildren :: S.Set Production -> S.Set Production -> S.Set [Production]
inlineChildren prods targets =
  let replacementPairs = S.map (\(lhs, rhs) -> ((lhs, rhs), concatMap (S.toList . rulesThatUseSym prods) rhs))
   in S.map (uncurry (renameRules . fst)) $ replacementPairs targets

rulesThatUseSym :: S.Set Production -> Symbol -> S.Set Production
rulesThatUseSym oldProductions r = S.filter ((== r) . fst) oldProductions

renameRules :: Symbol -> [Production] -> [Production]
renameRules name = map (Data.Bifunctor.first $ const name)

gfp :: S.Set Production -> CFG
gfp prods =
  CFG
    { startSymbol = "S",
      nonTerminals = nts,
      terminals = (S.fromList $ concatMap (\(x, xs) -> x : xs) prods) S.\\ nts,
      productions = prods
    }
  where
    nts = S.map fst prods

-- Function to find all unit-productions in the grammar
findUnitProductions :: S.Set Production -> S.Set Production
findUnitProductions prods =
  S.filter
    ( \x -> case x of
        (_, [s]) -> s `S.member` nonTerminals
        _ -> False
    )
    prods
  where
    nonTerminals = S.map fst prods

-- Sample main function to test transformations
main :: IO ()
main = do
  -- Sample grammar
  let grammar =
        CFG
          { startSymbol = "S",
            nonTerminals = S.fromList ["S", "A", "B", "C"],
            terminals = S.fromList ["a", "b", "c"],
            productions =
              S.fromList
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
          ("eliminateMoreThanTwoNonTerminals", eliminateMoreThanTwoNonTerminals),
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
