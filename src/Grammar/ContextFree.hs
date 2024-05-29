module Grammar.ContextFree where

-- This module is for converting a ContextFree grammar to Chomsky Normal Form

import Data.Bifunctor qualified
import Data.List (intercalate)
import Data.Map qualified as Map
import Data.Set qualified as S

type Symbol = String

type Production = (Symbol, [Symbol])

data CFG = CFG
  { startSymbol :: Symbol,
    productions :: S.Set Production
  }
  deriving (Show, Eq)

nonTerminals :: CFG -> S.Set Symbol
nonTerminals (CFG _ ps) = S.map fst ps

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
eliminateStartSymbol (CFG oldStart oldProductions) =
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
      replaceTerminals m (lhs, rhs) =
        let newRHS = map (\s -> Map.findWithDefault s s m) rhs
         in if any (`Map.member` m) rhs
              then (lhs, newRHS) : [(m Map.! t, [t]) | t <- rhs, t `Map.member` m]
              else [(lhs, rhs)]
      newProductions = S.fromList $ concatMap (replaceTerminals termMap) (S.toList oldProductions)
   in CFG start newProductions

-- -- [("S", ["A", "B", "C", "D"])] -> [("S", ["A", "S_1"]), ("S_1", ["B", "S_2"]), ("S_2", ["C", "D"])]
-- splitProduction :: Production -> [Production]
-- splitProduction (name, rules) =
--   if length rules <= 2
--     then [(name, rules)]
--     else
--       let newNonTerms = map (\i -> name ++ "_" ++ show i) [1 .. length rules - 2] -- TODO: this line will cause naming conflicts
--           newProductions :: [(Symbol, Symbol)]
--           newProductions = zip newNonTerms (tail rules)
--           newProductions' :: [Production]
--           newProductions' = zipWith (\(lhs, rhs) s -> (lhs, [rhs, s])) newProductions (map fst $ tail newProductions)
--           finalProduction = (last newNonTerms, [last (init rules), last rules])
--        in (name, [head rules, head newNonTerms]) : newProductions' ++ [finalProduction]

closestSquareSmallerThan :: Int -> Int
closestSquareSmallerThan = ((2 ^) :: Int -> Int) . (floor :: Double -> Int) . subtract 1 . logBase 2 . fromIntegral

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
      let replacementPairs (_, rhs) = S.fromList $ concatMap (S.toList . rulesWithName prods) rhs

          renameRules :: Symbol -> S.Set Production -> S.Set Production
          renameRules name = S.map (Data.Bifunctor.first $ const name)
       in renameRules (fst target) $ replacementPairs target

rulesWithName :: S.Set Production -> Symbol -> S.Set Production
rulesWithName oldPs r = S.filter ((== r) . fst) oldPs

-- Function to find all unit-productions in the grammar
findUnitProductions :: S.Set Production -> S.Set Production
findUnitProductions prods =
  S.filter
    ( \x -> case x of
        (_, [s]) -> s `S.member` nonTerms
        _ -> False
    )
    prods
  where
    nonTerms = S.map fst prods

removeUnusedRules :: CFG -> CFG
removeUnusedRules (CFG x xs) =
  let usedSyms = fn $ S.fromList [x]
   in CFG x $ S.unions $ S.map (\a -> S.filter ((== a) . fst) xs) usedSyms
  where
    rulesUsingSyms :: S.Set Symbol -> S.Set Production -> S.Set Production
    rulesUsingSyms syms ps = S.union (S.unions $ S.map (rulesWithName ps) syms) $ S.filter (not . null . (`S.intersection` syms) . S.fromList . snd) $ ps
    fn :: S.Set Symbol -> S.Set Symbol
    fn syms =
      let newSyms = S.fromList . concatMap (\a -> pure (fst a) <> snd a) . S.toList $ rulesUsingSyms syms xs
          newSet = S.union syms newSyms
       in if newSet == syms then syms else fn newSet

toChomskyReducedForm :: CFG -> CFG
toChomskyReducedForm =
  eliminateUnitRules
    . removeEpsilonRules
    . eliminateMoreThanTwoNonTerminals
    . eliminateRulesWithNonsolitaryTerminals
    . eliminateStartSymbol

isChomskyReducedForm :: CFG -> Bool
isChomskyReducedForm cfg@(CFG _ xs) = all (isRuleChomskyReducedForm cfg) $ S.toList xs

isRuleChomskyReducedForm :: CFG -> Production -> Bool
isRuleChomskyReducedForm cfg ps = case snd ps of
  [sym] -> sym `elem` terminals cfg
  [a, b] -> all (`elem` nonTerminals cfg) [a, b]
  _ -> False

bnfCFG :: CFG
bnfCFG =
  CFG
    { startSymbol = "<syntax>",
      productions =
        S.fromList
          [ ("<syntax>", ["<rule>"]),
            ("<syntax>", ["<rule>", "<syntax>"]),
            ("<rule>", ["<opt-whitespace>", "<", "<rule-name>", ">", "<opt-whitespace>", ":", ":", "=", "<opt-whitespace>", "<expression>", "<line-end>"]),
            ("<opt-whitespace>", [" "]),
            ("<opt-whitespace>", [" ", "<opt-whitespace>"]),
            ("<opt-whitespace>", []),
            ("<expression>", ["<list>"]),
            ("<expression>", ["<list>", "<opt-whitespace>", "|", "<opt-whitespace>", "<expression>"]),
            ("<line-end>", ["<opt-whitespace>", "<EOL>"]),
            ("<line-end>", ["<line-end>", "<line-end>"]),
            ("<list>", ["<term>"]),
            ("<list>", ["<term>", "<opt-whitespace>", "<list>"]),
            ("<term>", ["<literal>"]),
            ("<term>", ["<", "<rule-name>", ">"]),
            ("<literal>", ["\"", "<text1>", "\""]),
            ("<literal>", ["'", "<text2>", "'"]),
            ("<text1>", []),
            ("<text1>", ["<character1>", "<text1>"]),
            ("<text2>", []),
            ("<text2>", ["<character2>", "<text2>"]),
            ("<character>", ["<letter>"]),
            ("<character>", ["<digit>"]),
            ("<character>", ["<symbol>"]),
            ("<letter>", ["A"]),
            ("<letter>", ["B"]),
            ("<letter>", ["C"]),
            ("<letter>", ["D"]),
            ("<letter>", ["E"]),
            ("<letter>", ["F"]),
            ("<letter>", ["G"]),
            ("<letter>", ["H"]),
            ("<letter>", ["I"]),
            ("<letter>", ["J"]),
            ("<letter>", ["K"]),
            ("<letter>", ["L"]),
            ("<letter>", ["M"]),
            ("<letter>", ["N"]),
            ("<letter>", ["O"]),
            ("<letter>", ["P"]),
            ("<letter>", ["Q"]),
            ("<letter>", ["R"]),
            ("<letter>", ["S"]),
            ("<letter>", ["T"]),
            ("<letter>", ["U"]),
            ("<letter>", ["V"]),
            ("<letter>", ["W"]),
            ("<letter>", ["X"]),
            ("<letter>", ["Y"]),
            ("<letter>", ["Z"]),
            ("<letter>", ["a"]),
            ("<letter>", ["b"]),
            ("<letter>", ["c"]),
            ("<letter>", ["d"]),
            ("<letter>", ["e"]),
            ("<letter>", ["f"]),
            ("<letter>", ["g"]),
            ("<letter>", ["h"]),
            ("<letter>", ["i"]),
            ("<letter>", ["j"]),
            ("<letter>", ["k"]),
            ("<letter>", ["l"]),
            ("<letter>", ["m"]),
            ("<letter>", ["n"]),
            ("<letter>", ["o"]),
            ("<letter>", ["p"]),
            ("<letter>", ["q"]),
            ("<letter>", ["r"]),
            ("<letter>", ["s"]),
            ("<letter>", ["t"]),
            ("<letter>", ["u"]),
            ("<letter>", ["v"]),
            ("<letter>", ["w"]),
            ("<letter>", ["x"]),
            ("<letter>", ["y"]),
            ("<letter>", ["z"]),
            ("<digit>", ["0"]),
            ("<digit>", ["1"]),
            ("<digit>", ["2"]),
            ("<digit>", ["3"]),
            ("<digit>", ["4"]),
            ("<digit>", ["5"]),
            ("<digit>", ["6"]),
            ("<digit>", ["7"]),
            ("<digit>", ["8"]),
            ("<digit>", ["9"]),
            ("<symbol>", ["|"]),
            ("<symbol>", [" "]),
            ("<symbol>", ["!"]),
            ("<symbol>", ["#"]),
            ("<symbol>", ["$"]),
            ("<symbol>", ["%"]),
            ("<symbol>", ["&"]),
            ("<symbol>", ["("]),
            ("<symbol>", [")"]),
            ("<symbol>", ["*"]),
            ("<symbol>", ["+"]),
            ("<symbol>", [","]),
            ("<symbol>", ["-"]),
            ("<symbol>", ["."]),
            ("<symbol>", ["/"]),
            ("<symbol>", [":"]),
            ("<symbol>", [";"]),
            ("<symbol>", [">"]),
            ("<symbol>", ["="]),
            ("<symbol>", ["<"]),
            ("<symbol>", ["?"]),
            ("<symbol>", ["@"]),
            ("<symbol>", ["["]),
            ("<symbol>", ["\\"]),
            ("<symbol>", ["]"]),
            ("<symbol>", ["^"]),
            ("<symbol>", ["_"]),
            ("<symbol>", ["`"]),
            ("<symbol>", ["{"]),
            ("<symbol>", ["}"]),
            ("<symbol>", ["~"]),
            ("<character1>", ["<character>"]),
            ("<character1>", ["'"]),
            ("<character2>", ["<character>"]),
            ("<character2>", ["\""]),
            ("<rule-name>", ["<letter>"]),
            ("<rule-name>", ["<rule-name>", "<rule-char>"]),
            ("<rule-char>", ["<letter>"]),
            ("<rule-char>", ["<digit>"]),
            ("<rule-char>", ["-"]),
            ("<EOL>", ["\n"])
          ]
    }
