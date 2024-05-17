import Data.List (nub, (\\))
import Data.Map qualified as Map
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import Data.Set qualified as Set

type Symbol = String

type Production = (Symbol, [Symbol])

type CFG = ([Symbol], [Symbol], [Production], Symbol)

productions (_, _, ps, _) = ps

-- Data structure to keep track of transformation history
data TransformHistory = TransformHistory
  { originalProductions :: [Production],
    cnfProductions :: [Production],
    productionMap :: Map.Map Production Production
  }
  deriving (Show)

-- Initialize transformation history
initHistory :: [Production] -> TransformHistory
initHistory prods = TransformHistory prods [] Map.empty

-- Add a new CNF production and track its origin
addCNFProduction :: TransformHistory -> Production -> Production -> TransformHistory
addCNFProduction hist origProd cnfProd =
  hist
    { cnfProductions = cnfProd : cnfProductions hist,
      productionMap = Map.insert cnfProd origProd (productionMap hist)
    }

-- 1. Remove epsilon productions
removeEpsilon :: CFG -> TransformHistory -> (CFG, TransformHistory)
removeEpsilon (nonTerminals, terminals, productions, start) hist =
  ((nonTerminals, terminals, newProductions, newStart), newHist)
  where
    nullable = findNullable productions
    nonNullableProds = filter (\(_, rhs) -> not (null rhs && length rhs == 1)) productions
    newProductions = nub $ concatMap (replaceNullable nullable) nonNullableProds
    newStart = if start `elem` nullable then start ++ "'" else start
    newHist = foldl (\h (orig, new) -> addCNFProduction h orig new) hist [(p, p) | p <- newProductions]

    replaceNullable :: [Symbol] -> Production -> [Production]
    replaceNullable nullable (lhs, rhs) =
      if any (`elem` nullable) rhs
        then [(lhs, rhs') | rhs' <- subsequencesByRemoving rhs nullable]
        else [(lhs, rhs)]

    subsequencesByRemoving :: [Symbol] -> [Symbol] -> [[Symbol]]
    subsequencesByRemoving [] _ = [[]]
    subsequencesByRemoving (x : xs) nullable
      | x `elem` nullable = subsequencesByRemoving xs nullable ++ map (x :) (subsequencesByRemoving xs nullable)
      | otherwise = map (x :) (subsequencesByRemoving xs nullable)

    findNullable :: [Production] -> [Symbol]
    findNullable prods = go [] prods
      where
        go nullable [] = nullable
        go nullable ((lhs, rhs) : rest)
          | all (`elem` nullable) rhs = go (lhs : nullable) rest
          | otherwise = go nullable rest

-- 2. Remove unit productions
removeUnit :: CFG -> TransformHistory -> (CFG, TransformHistory)
removeUnit (nonTerminals, terminals, productions, start) hist =
  ((nonTerminals, terminals, newProductions, start), newHist)
  where
    unitPairs = findUnitPairs productions
    nonUnitProds = filter (not . isUnitProduction) productions
    newProductions = nub $ nonUnitProds ++ [(lhs, rhs) | (lhs, x) <- unitPairs, rhs <- findRHS x productions]
    newHist = foldl (\h (orig, new) -> addCNFProduction h orig new) hist [(p, p) | p <- newProductions]

    isUnitProduction :: Production -> Bool
    isUnitProduction (_, [rhs]) = rhs `elem` nonTerminals
    isUnitProduction _ = False

    findUnitPairs :: [Production] -> [(Symbol, Symbol)]
    findUnitPairs prods = nub $ go prods prods []
      where
        go [] _ acc = acc
        go ((lhs, [rhs]) : rest) allProds acc
          | lhs /= rhs = go rest allProds ((lhs, rhs) : [(lhs, x) | (rhs', x) <- acc, rhs == rhs'])
          | otherwise = go rest allProds acc
        go (_ : rest) allProds acc = go rest allProds acc

    findRHS :: Symbol -> [Production] -> [[Symbol]]
    findRHS x prods = [rhs | (lhs, rhs) <- prods, lhs == x]

-- 3. Remove useless symbols
removeUseless :: CFG -> TransformHistory -> (CFG, TransformHistory)
removeUseless (nonTerminals, terminals, productions, start) hist =
  ((nonTerminals', terminals, newProductions, start), newHist)
  where
    generating = findGenerating nonTerminals productions
    reachable = findReachable start productions
    useful = Set.intersection (Set.fromList generating) (Set.fromList reachable)
    nonTerminals' = filter (`Set.member` useful) nonTerminals
    newProductions = filter (\(lhs, rhs) -> lhs `Set.member` useful && all (`Set.member` useful) rhs) productions
    newHist = foldl (\h (orig, new) -> addCNFProduction h orig new) hist [(p, p) | p <- newProductions]

    findGenerating :: [Symbol] -> [Production] -> [Symbol]
    findGenerating nonTerms prods = go [] prods
      where
        go gen [] = gen
        go gen ((lhs, rhs) : rest)
          | all (`elem` (terminals ++ gen)) rhs = go (lhs : gen) rest
          | otherwise = go gen rest

    findReachable :: Symbol -> [Production] -> [Symbol]
    findReachable start prods = go [start] prods
      where
        go reached [] = reached
        go reached ((lhs, rhs) : rest)
          | lhs `elem` reached = go (reached ++ (rhs \\ reached)) rest
          | otherwise = go reached rest

-- 4. Convert terminals in rules with more than one symbol into non-terminals
convertTerminals :: CFG -> TransformHistory -> (CFG, TransformHistory)
convertTerminals (nonTerminals, terminals, productions, start) hist =
  ((nonTerminals', terminals, newProductions, start), newHist)
  where
    terminalMap = Map.fromList [(t, "T_" ++ t) | t <- terminals]
    nonTerminals' = nonTerminals ++ Map.elems terminalMap
    newProductions = concatMap (replaceTerminals terminalMap) productions
    newHist = foldl (\h (orig, new) -> addCNFProduction h orig new) hist origToNewPairs

    origToNewPairs = [(p, (lhs, newRHS)) | p@(lhs, rhs) <- productions, let newRHS = map (\s -> fromMaybe s (Map.lookup s terminalMap)) rhs]

    replaceTerminals :: Map.Map Symbol Symbol -> Production -> [Production]
    replaceTerminals termMap (lhs, rhs) =
      let newRHS = map (\s -> fromMaybe s (Map.lookup s termMap)) rhs
       in if any (`Map.member` termMap) rhs
            then (lhs, newRHS) : [(termMap Map.! t, [t]) | t <- rhs, t `Map.member` termMap]
            else [(lhs, rhs)]

-- 5. Break down rules with more than two non-terminals on the right-hand side
breakDownLongRules :: CFG -> TransformHistory -> (CFG, TransformHistory)
breakDownLongRules (nonTerminals, terminals, productions, start) hist =
  ((nonTerminals', terminals, newProductions, start), newHist)
  where
    newProductions = concatMap splitProduction productions
    newNonTerminals = nub [lhs ++ "_" ++ show i | (lhs, rhs) <- productions, i <- [1 .. length rhs - 2]]
    nonTerminals' = nonTerminals ++ newNonTerminals
    newHist = foldl (\h (orig, new) -> addCNFProduction h orig new) hist origToNewPairs

    origToNewPairs = concatMap (\p -> zip (repeat p) (splitProduction p)) productions

    splitProduction :: Production -> [Production]
    splitProduction (lhs, rhs)
      | length rhs <= 2 = [(lhs, rhs)]
      | otherwise =
          let (firstHalf, secondHalf) = splitAt 2 rhs
              leftNonTerm = lhs ++ "_L"
              rightNonTerm = lhs ++ "_R"
              newProductions = splitProduction (leftNonTerm, firstHalf) ++ splitProduction (rightNonTerm, secondHalf)
           in (lhs, [leftNonTerm, rightNonTerm]) : newProductions

-- Full CFG to CNF conversion with tracking
convertToCNF :: CFG -> (CFG, TransformHistory)
convertToCNF cfg =
  let hist = initHistory (productions cfg)
      (cfg1, hist1) = removeEpsilon cfg hist
      (cfg2, hist2) = removeUnit cfg1 hist1
      (cfg3, hist3) = removeUseless cfg2 hist2
      (cfg4, hist4) = convertTerminals cfg3 hist3
   in breakDownLongRules cfg4 hist4

-- Example usage
main :: IO ()
main = do
  let nonTerminals = ["S", "A", "B", "C", "D", "E", "F"]
      terminals = ["a", "b", "c", "d", "e", "f"]
      productions =
        [ ("S", ["A", "B", "C", "D", "E", "F"]),
          ("A", ["a"]),
          ("B", ["b"]),
          ("C", ["c"]),
          ("D", ["d"]),
          ("E", ["e"]),
          ("F", ["f"])
        ]
      start = "S"
      cfg = (nonTerminals, terminals, productions, start)
  let (cnfCfg, history) = convertToCNF cfg
  print cnfCfg
  print history

-- Reverse transformation
reverseCNF :: TransformHistory -> [Production] -> [Production]
reverseCNF hist cnfProductions = mapMaybe (`Map.lookup` (productionMap hist)) cnfProductions

-- Example reversal usage
exampleReversal :: IO ()
exampleReversal = do
  let cnfProductions =
        [ ("S", ["A_L", "D_EF"]),
          ("A_L", ["A", "B"]),
          ("D_EF", ["C", "D_E"]),
          ("D_E", ["D", "E"]),
          ("E", ["F"])
        ]
  let history =
        TransformHistory
          { originalProductions =
              [ ("S", ["A", "B", "C", "D", "E", "F"]),
                ("A", ["a"]),
                ("B", ["b"]),
                ("C", ["c"]),
                ("D", ["d"]),
                ("E", ["e"]),
                ("F", ["f"])
              ],
            cnfProductions = cnfProductions,
            productionMap =
              Map.fromList
                [ (("S", ["A_L", "D_EF"]), ("S", ["A", "B", "C", "D", "E", "F"])),
                  (("A_L", ["A", "B"]), ("S", ["A", "B"])),
                  (("D_EF", ["C", "D_E"]), ("S", ["C", "D", "E"])),
                  (("D_E", ["D", "E"]), ("S", ["D", "E"])),
                  (("E", ["F"]), ("S", ["E", "F"]))
                ]
          }
  print $ reverseCNF history cnfProductions
