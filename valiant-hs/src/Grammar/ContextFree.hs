import Data.List (nub, subsequences)

-- Sample type synonyms
type Symbol = String

type Production = (Symbol, [Symbol])

-- Define a data type for representing a context-free grammar
data CFG = CFG
  { startSymbol :: Symbol, -- Start symbol of the grammar
    nonTerminals :: [Symbol], -- List of non-terminal symbols
    terminals :: [Symbol], -- List of terminal symbols
    productions :: [Production] -- List of productions
  }
  deriving (Show)

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
    newStart = "S0"
    newProductions = ("S0", [oldStart]) : oldProductions
    newNonTerminals = nub $ newStart : nonTerminals cfg

-- Function to remove epsilon productions from a CFG
removeEpsilonProductions :: CFG -> CFG
removeEpsilonProductions cfg = cfg {productions = newProductions}
  where
    newProductions = filter (\(lhs, rhs) -> not $ hasEpsilon lhs rhs) updatedProductions
    updatedProductions = productions cfg
    hasEpsilon lhs rhs = lhs `elem` nonTerminals cfg && null rhs

-- Function to eliminate right-hand sides with more than 2 non-terminals
eliminateMoreThanTwoNonTerminals :: CFG -> CFG
eliminateMoreThanTwoNonTerminals cfg@(CFG {nonTerminals = nonTerms, productions = oldProductions}) =
  CFG
    { startSymbol = startSymbol cfg,
      nonTerminals = nub $ newNonTerms ++ nonTerms,
      terminals = terminals cfg,
      productions = newProductions
    }
  where
    newNonTerms = map (\i -> "A" ++ show i) [1 .. length oldProductions] -- Generate new non-terminal symbols A1, A2, ...
    newProductions = concatMap (\(lhs, rhs) -> binTransformation lhs rhs newNonTerms) oldProductions
    binTransformation lhs [] _ = [(lhs, [])]
    binTransformation lhs [x] _ = [(lhs, [x])]
    binTransformation lhs (x : y : xs) (newNonTerm : newNonTerms) = (lhs, [x, newNonTerm]) : binTransformation newNonTerm (y : xs) newNonTerms

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

-- Function to eliminate ε-rules
-- TODO: this is wrong-- do not trust the robots
eliminateEpsilonRules :: CFG -> CFG
eliminateEpsilonRules cfg@(CFG {startSymbol = start, nonTerminals = nonTerms, terminals = terms, productions = prods}) =
  CFG
    { startSymbol = start,
      nonTerminals = nonTerms,
      terminals = terms,
      productions = finalProductions
    }
  where
    nullable = nullableNonTerminals cfg
    versions = concatMap (generateVersions nullable nonTerms terms) prods
    finalProductions = filter (\(lhs, rhs) -> not (null rhs) || lhs == start || lhs `elem` nullable) versions

-- Sample main function to test transformations
main :: IO ()
main = do
  -- Sample grammar
  let grammar =
        CFG
          { startSymbol = "S0",
            nonTerminals = ["S0", "A", "B", "C"],
            terminals = ["a", "b", "c"],
            productions =
              [ ("S0", ["A", "b", "B"]),
                ("S0", ["C"]),
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

  let grammarWithoutEpsilon = removeEpsilonProductions grammar
  putStrLn "\nGrammar after removing ε-productions:"
  print grammarWithoutEpsilon

  let grammarWithoutEpsilonAndNewStart = eliminateStartSymbol grammarWithoutEpsilon
  putStrLn "\nGrammar after eliminating start symbol from right-hand sides and introducing a new start symbol:"
  print grammarWithoutEpsilonAndNewStart

  let grammarAfterBinTransformation = eliminateMoreThanTwoNonTerminals grammarWithoutEpsilonAndNewStart
  putStrLn "\nGrammar after BIN transformation:"
  print grammarAfterBinTransformation

  let finalGrammar = eliminateEpsilonRules grammarAfterBinTransformation
  putStrLn "\nFinal Grammar after DEL transformation:"
  print finalGrammar
