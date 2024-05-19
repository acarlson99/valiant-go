module Grammar.ContextFree where

-- This module is for converting a ContextFree grammar to Chomsky Normal Form

import Control.Monad
import Data.Bifunctor qualified
import Data.List (intercalate)
import Data.Map qualified as Map
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

-- 1.
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

-- 2.
-- https://en.wikipedia.org/wiki/Chomsky_normal_form#TERM:_Eliminate_rules_with_nonsolitary_terminals
eliminateRulesWithNonsolitaryTerminals :: CFG -> CFG
eliminateRulesWithNonsolitaryTerminals cfg@(CFG start oldProductions) =
  let termMap = Map.fromList [(t, "T_" ++ t) | t <- S.toList $ terminals cfg]
      replaceTerminals :: Map.Map Symbol Symbol -> Production -> [Production]
      replaceTerminals termMap (lhs, rhs) =
        let newRHS = map (\s -> Map.findWithDefault s s termMap) rhs
         in if any (`Map.member` termMap) rhs
              then (lhs, newRHS) : [(termMap Map.! t, [t]) | t <- rhs, t `Map.member` termMap]
              else [(lhs, rhs)]
      newNonTerminals = S.union (nonTerminals cfg) (S.fromList $ Map.elems termMap)
      newProductions = S.fromList $ concatMap (replaceTerminals termMap) (S.toList oldProductions)
   in CFG start newProductions

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
          leftNonTerm = intercalate "_" firstHalf
          rightNonTerm = intercalate "_" secondHalf
       in case secondHalf of
            [x] -> (lhs, [leftNonTerm, x]) : splitProductionBalanced (leftNonTerm, firstHalf)
            _ -> (lhs, [leftNonTerm, rightNonTerm]) : splitProductionBalanced (leftNonTerm, firstHalf) ++ splitProductionBalanced (rightNonTerm, secondHalf)

-- 3.
-- https://en.wikipedia.org/wiki/Chomsky_normal_form#BIN:_Eliminate_right-hand_sides_with_more_than_2_nonterminals
eliminateMoreThanTwoNonTerminals :: CFG -> CFG
eliminateMoreThanTwoNonTerminals cfg@(CFG {productions = oldProductions}) =
  let toReplace :: [Production]
      toReplace = filter ((> 2) . length . snd) $ S.toList oldProductions
      pairs :: [(Production, [Production])]
      pairs = map (\old -> (,) old $ splitProductionBalanced old) toReplace
      updateRule (old, newS) = (flip $ foldr S.insert) newS . S.delete old
      prods = foldr updateRule oldProductions pairs
   in cfg {productions = prods}

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

-- 4.
-- https://en.wikipedia.org/wiki/Chomsky_normal_form#DEL:_Eliminate_%CE%B5-rules
removeEpsilonRules :: CFG -> CFG
removeEpsilonRules cfg@(CFG {productions = oldProductions}) =
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
        else removeEpsilonRules newCFG

-- 5.
-- https://en.wikipedia.org/wiki/Chomsky_normal_form#UNIT:_Eliminate_unit_rules
eliminateUnitRules :: CFG -> CFG
eliminateUnitRules cfg@(CFG {productions = oldProductions}) =
  let unitRules = findUnitProductions oldProductions
      newRules = S.map (inlineChildren oldProductions) unitRules
      ps = S.union (S.unions newRules) oldProductions S.\\ unitRules
   in if null unitRules then cfg else eliminateUnitRules cfg {productions = ps}
  where
    inlineChildren :: S.Set Production -> Production -> S.Set Production
    inlineChildren prods target =
      let rulesThatUseSym :: S.Set Production -> Symbol -> S.Set Production
          rulesThatUseSym oldProductions r = S.filter ((== r) . fst) oldProductions

          replacementPairs (lhs, rhs) = S.fromList $ concatMap (S.toList . rulesThatUseSym prods) rhs

          replaceIf :: (a -> Bool) -> [a] -> [a] -> [a]
          replaceIf cond fill = concatMap (\a -> if cond a then fill else [a])

          performInline :: Production -> Production -> Production
          performInline parent@(pn, px) child@(cn, cx) = (,) pn $ replaceIf (== cn) cx px

          renameRules :: Symbol -> S.Set Production -> S.Set Production
          renameRules name = S.map (Data.Bifunctor.first $ const name)
       in renameRules (fst target) $ replacementPairs target

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

toChomskyReducedForm :: CFG -> CFG
toChomskyReducedForm =
  eliminateUnitRules
    . removeEpsilonRules
    . eliminateMoreThanTwoNonTerminals
    . eliminateRulesWithNonsolitaryTerminals
    . eliminateStartSymbol

-- TODO: add `chomskyReducedForm` to check if grammar is correct
isChomskyReducedForm :: CFG -> Bool
isChomskyReducedForm cfg@(CFG x xs) = all (isReduced cfg) $ S.toList xs

isReduced :: CFG -> Production -> Bool
isReduced cfg ps = case snd ps of
  [sym] -> sym `elem` terminals cfg
  [a, b] -> all (`elem` nonTerminals cfg) [a, b]
  _ -> False

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
          ("eliminateRulesWithNonsolitaryTerminals", eliminateRulesWithNonsolitaryTerminals),
          ("eliminateMoreThanTwoNonTerminals", eliminateMoreThanTwoNonTerminals),
          ("removeEpsilonRules", removeEpsilonRules),
          ("eliminateUnitRules", eliminateUnitRules)
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
