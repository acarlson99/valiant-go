module Grammar.ContextFree where

-- This module is for converting a ContextFree grammar to Chomsky Normal Form

import Control.Monad
import Data.Bifunctor qualified
import Data.Set qualified as S

type Symbol = String

type Production = (Symbol, [Symbol])

-- TODO: rewrite structure to only contain startSymbol and productions
data CFG = CFG
  { startSymbol :: Symbol,
    productions :: S.Set Production
  }
  deriving (Show, Eq)

nonTerminals :: CFG -> S.Set Symbol
nonTerminals (CFG s ps) = S.map fst ps

terminals :: CFG -> S.Set Symbol
terminals cfg = symbols cfg S.\\ nonTerminals cfg

symbols :: CFG -> S.Set Symbol
symbols cfg@(CFG _ ps) = ruleSyms ps
  where
    ruleSyms = S.fold (S.union . S.fromList . snd) (nonTerminals cfg)

-- Add a production to the CFG
addProduction :: CFG -> Production -> CFG
addProduction cfg prod = cfg {productions = prod `S.insert` productions cfg}

-- Remove a production from the CFG
removeProduction :: CFG -> Production -> CFG
removeProduction cfg prod = cfg {productions = prod `S.delete` productions cfg}

-- https://en.wikipedia.org/wiki/Chomsky_normal_form#START:_Eliminate_the_start_symbol_from_right-hand_sides
eliminateStartSymbol :: CFG -> CFG
eliminateStartSymbol cfg@(CFG oldStart oldProductions) =
  CFG
    { startSymbol = newStart,
      productions = newProductions
    }
  where
    newStart = "S0" -- TODO: generalize
    newProductions = (newStart, [oldStart]) `S.insert` oldProductions

-- [("S", ["A", "B", "C", "D"])] -> [("S", ["A", "S_1"]), ("S_1", ["B", "S_2"]), ("S_2", ["C", "D"])]
splitProduction :: Production -> [Production]
splitProduction (lhs, rhs) =
  if length rhs <= 2
    then [(lhs, rhs)]
    else
      let newNonTerms = map (\i -> lhs ++ "_" ++ show i) [1 .. length rhs - 2] -- TODO: this line will cause naming conflicts
          newProductions :: [(Symbol, Symbol)]
          newProductions = zip newNonTerms (tail rhs)
          newProductions' :: [Production]
          newProductions' = zipWith (curry (\((lhs, rhs), s) -> (lhs, [rhs, s]))) newProductions (map fst $ tail newProductions)
          finalProduction = (last newNonTerms, [last (init rhs), last rhs])
       in (lhs, [head rhs, head newNonTerms]) : newProductions' ++ [finalProduction]

closestSquareSmallerThan :: Int -> Int
closestSquareSmallerThan = (2 ^) . floor . subtract 1 . logBase 2 . fromIntegral

-- [("S", ["A", "B", "C", "D"])] -> [("S", ["S_1_L", "S_1_R"]), ("S_1_L", ["A", "B"]), ("S_1_R", ["C", "D"])]
splitProductionBalanced :: Production -> [Production]
splitProductionBalanced (lhs, rhs)
  | length rhs <= 2 = [(lhs, rhs)]
  | otherwise =
      let -- splitPos = length rhs `div` 2
          splitPos = length rhs - closestSquareSmallerThan (length rhs)
          (firstHalf, secondHalf) = splitAt splitPos rhs
          leftNonTerm = lhs ++ "_L"
          rightNonTerm = lhs ++ "_R"
          newProductions = splitProductionBalanced (leftNonTerm, firstHalf) ++ splitProductionBalanced (rightNonTerm, secondHalf)
       in (lhs, [leftNonTerm, rightNonTerm]) : newProductions

-- https://en.wikipedia.org/wiki/Chomsky_normal_form#BIN:_Eliminate_right-hand_sides_with_more_than_2_nonterminals
eliminateMoreThanTwoNonTerminals :: CFG -> CFG
eliminateMoreThanTwoNonTerminals cfg@(CFG {productions = oldProductions}) =
  let toReplace :: [Production]
      toReplace = filter ((> 2) . length . snd) $ S.toList oldProductions
      pairs :: [(Production, [Production])]
      pairs = zipWith (\old n -> (,) old $ newRenamedProductions n old) toReplace [1 :: Int ..]
      updateRule (old, newS) = (flip $ foldr S.insert) newS . S.delete old
      prods = foldr updateRule oldProductions pairs
   in cfg {productions = prods}
  where
    renameFirstRule :: Symbol -> [Production] -> [Production]
    renameFirstRule _ [] = []
    renameFirstRule name (x : xs) = Data.Bifunctor.first (const name) x : xs
    newRenamedProductions n (x, xs) = renameFirstRule x $ splitProductionBalanced (x ++ "_" ++ show n, xs)

-- set of all nonTerminals that contain a rule that could be empty
findNullableNonTerminals :: CFG -> S.Set Symbol
findNullableNonTerminals (CFG _ prods) =
  let nullableFromEpsilon = S.map fst $ S.filter (\(_, rhs) -> null rhs) prods
      closure s =
        let newNullable = S.filter (\(lhs, rhs) -> all (`S.member` s) rhs && lhs `S.notMember` s) prods
            updatedSet = s `S.union` S.map fst newNullable
         in if S.null newNullable then s else closure updatedSet
   in closure nullableFromEpsilon

subsequencesByRemoving :: (Foldable t, Eq a) => t a -> [a] -> [[a]]
subsequencesByRemoving _ [] = [[]]
subsequencesByRemoving nullable (x : xs)
  | x `elem` nullable = subsequencesByRemoving nullable xs ++ map (x :) (subsequencesByRemoving nullable xs)
  | otherwise = map (x :) (subsequencesByRemoving nullable xs)

-- Function to inline a single production to include versions with and without nullable non-terminals
inlineNullableProduction :: S.Set Symbol -> Production -> [Production]
inlineNullableProduction nullable (lhs, rhs) =
  if any (`S.member` nullable) rhs
    then
      let versions = subsequencesByRemoving nullable rhs
       in map (lhs,) versions
    else [(lhs, rhs)]

-- https://en.wikipedia.org/wiki/Chomsky_normal_form#DEL:_Eliminate_%CE%B5-rules
removeEpsilonProductions :: CFG -> CFG
removeEpsilonProductions cfg@(CFG {productions = oldProductions}) =
  let nullableNonTerminals = findNullableNonTerminals cfg
      inlinedProductions :: S.Set Production
      inlinedProductions = S.fromList . concatMap (inlineNullableProduction nullableNonTerminals) $ S.toList oldProductions
      emptyProductionRules :: S.Set Production -> S.Set Production
      emptyProductionRules = S.filter (null . snd)
      nonEmptyProductionRules :: S.Set Production -> S.Set Production
      nonEmptyProductionRules = S.filter (not . null . snd)
      symsToDelete = S.map fst (emptyProductionRules inlinedProductions) S.\\ S.map fst (nonEmptyProductionRules inlinedProductions)
      removeSymbolsWhichCannotBeNonEmpty :: S.Set Symbol -> S.Set Production -> S.Set Production
      removeSymbolsWhichCannotBeNonEmpty syms = S.map (\(lhs, rhs) -> (,) lhs $ concatMap (\s -> ([s | s `notElem` syms])) rhs)
      newProductions = removeSymbolsWhichCannotBeNonEmpty symsToDelete $ inlinedProductions S.\\ emptyProductionRules inlinedProductions
      newCFG = cfg {productions = newProductions}
   in if null nullableNonTerminals
        then
          ( if startSymbol cfg `notElem` nonTerminals newCFG
              -- if `startSymbol` has no associated rules then the language must only accept an empty string
              then addProduction cfg (startSymbol cfg, [])
              else cfg
          )
        else removeEpsilonProductions newCFG

rulesThatUseSym :: S.Set Production -> Symbol -> S.Set Production
rulesThatUseSym oldProductions r = S.filter ((== r) . fst) oldProductions

renameRules :: Symbol -> S.Set Production -> S.Set Production
renameRules name = S.map (Data.Bifunctor.first $ const name)

-- https://en.wikipedia.org/wiki/Chomsky_normal_form#UNIT:_Eliminate_unit_rules
inlineUnitRules :: CFG -> CFG
inlineUnitRules cfg@(CFG {productions = oldProductions}) =
  let inlineChildren :: S.Set Production -> Production -> S.Set Production
      inlineChildren prods target =
        let replacementPairs (lhs, rhs) = S.fromList $ concatMap (S.toList . rulesThatUseSym prods) rhs
            replaceIf :: [a] -> (a -> Bool) -> [a] -> [a]
            replaceIf (x : xs) match inthere = let rest = replaceIf xs match inthere in if match x then inthere ++ rest else rest
            replaceIf [] match inthere = []
            performInline :: Production -> Production -> Production
            performInline parent@(pn, px) child@(cn, cx) = (,) pn $ replaceIf px (== cn) cx
         in renameRules (fst target) $ replacementPairs target
      unitRules = findUnitProductions oldProductions
      newRules = S.map (inlineChildren oldProductions) unitRules
      ps = S.union (S.unions newRules) oldProductions S.\\ unitRules
   in if null unitRules then cfg else inlineUnitRules cfg {productions = ps}

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
          ("removeEpsilonProductions", removeEpsilonProductions),
          ("inlineUnitRules", inlineUnitRules)
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
treeToGrams (Branch _ _) = []
treeToGrams Empty = []
