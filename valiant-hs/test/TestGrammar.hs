module TestGrammar where

import Data.Set qualified as S
import Grammar.ContextFree
import Test.HUnit

tests :: [Test]
tests =
  [ testRenameNonTerminal,
    testReplaceProduction,
    testSplitProduction,
    testAddProduction,
    testRemoveProduction,
    testEliminateMoreThanTwoNonTerminals,
    testRemoveEpsilonProductions,
    testFindNullableNonTerminals,
    testIsEpsilonProduction
  ]

testRenameNonTerminal :: Test
testRenameNonTerminal =
  let testCFG = CFG "S" (S.fromList ["A", "B", "C"]) (S.fromList ["a", "b"]) (S.fromList [("S", ["A", "a"]), ("A", ["B", "b"]), ("C", ["a"]), ("B", ["C"])])
   in test
        [ "Renaming a non-terminal that exists in the CFG should modify it"
            ~: renameNonTerminal "A" "X" testCFG
            ~?= CFG "S" (S.fromList ["X", "B", "C"]) (S.fromList ["a", "b"]) (S.fromList [("S", ["X", "a"]), ("X", ["B", "b"]), ("B", ["C"]), ("C", ["a"])]),
          "Renaming a non-terminal that doesn't exist in the CFG"
            ~: renameNonTerminal "D" "X" testCFG
            ~?= CFG "S" (S.fromList ["A", "B", "C"]) (S.fromList ["a", "b"]) (S.fromList [("S", ["A", "a"]), ("A", ["B", "b"]), ("B", ["C"]), ("C", ["a"])]),
          "Renaming the start symbol"
            ~: renameNonTerminal "S" "X" testCFG
            ~?= CFG "X" (S.fromList ["A", "B", "C"]) (S.fromList ["a", "b"]) (S.fromList [("X", ["A", "a"]), ("A", ["B", "b"]), ("B", ["C"]), ("C", ["a"])]),
          "Renaming a non-terminal to the same name"
            ~: renameNonTerminal "A" "A" testCFG
            ~?= testCFG
        ]

testReplaceProduction :: Test
testReplaceProduction =
  test
    [ "Replace single production"
        ~: let cfg = CFG {startSymbol = "S", nonTerminals = S.fromList ["S", "A", "B"], terminals = S.fromList ["a", "b"], productions = S.fromList [("S", ["A"]), ("A", ["a", "B"]), ("B", ["b"])]}
               oldProd = ("A", ["a", "B"])
               newProd = ("A", ["a", "B", "a"])
               expectedCFG = CFG {startSymbol = "S", nonTerminals = S.fromList ["S", "A", "B"], terminals = S.fromList ["a", "b"], productions = S.fromList [("S", ["A"]), ("A", ["a", "B", "a"]), ("B", ["b"])]}
            in replaceProduction cfg oldProd newProd ~?= expectedCFG,
      "Replace production not in nonTerminals should have no effect"
        ~: let cfg = CFG {startSymbol = "S", nonTerminals = S.fromList ["S", "A", "B"], terminals = S.fromList ["a", "b"], productions = S.fromList [("S", ["A"]), ("A", ["a", "B"]), ("B", ["b"])]}
               oldProd = ("C", ["a", "b"])
               newProd = ("C", ["a", "a"])
               expectedCFG = CFG {startSymbol = "S", nonTerminals = S.fromList ["S", "A", "B"], terminals = S.fromList ["a", "b"], productions = S.fromList [("S", ["A"]), ("A", ["a", "B"]), ("B", ["b"])]}
            in replaceProduction cfg oldProd newProd ~?= expectedCFG,
      "Replace production with start symbol"
        ~: let cfg = CFG {startSymbol = "A", nonTerminals = S.fromList ["A", "B"], terminals = S.fromList ["a", "b"], productions = S.fromList [("A", ["a"]), ("B", ["b"])]}
               oldProd = ("A", ["a"])
               newProd = ("A", ["b"])
               expectedCFG = CFG {startSymbol = "A", nonTerminals = S.fromList ["A", "B"], terminals = S.fromList ["a", "b"], productions = S.fromList [("A", ["b"]), ("B", ["b"])]}
            in replaceProduction cfg oldProd newProd ~?= expectedCFG
    ]

testSplitProduction :: Test
testSplitProduction =
  test
    [ "Split production"
        ~: splitProduction ("S", ["A", "b", "B"])
        ~?= [("S", ["A", "S_1"]), ("S_1", ["b", "B"])],
      "Production with 0 non-terminals"
        ~: splitProduction ("S", [])
        ~?= [("S", [])],
      "Production with 1 non-terminal"
        ~: splitProduction ("S", ["A"])
        ~?= [("S", ["A"])],
      "Production with 2 non-terminals"
        ~: splitProduction ("S", ["A", "B"])
        ~?= [("S", ["A", "B"])],
      "Production with 3 non-terminals"
        ~: splitProduction ("S", ["A", "B", "C"])
        ~?= [("S", ["A", "S_1"]), ("S_1", ["B", "C"])],
      "Production with 4 non-terminals"
        ~: splitProduction ("S", ["A", "B", "C", "D"])
        ~?= [("S", ["A", "S_1"]), ("S_1", ["B", "S_2"]), ("S_2", ["C", "D"])],
      "Production with 5 non-terminals"
        ~: splitProduction ("S", ["A", "B", "C", "D", "E"])
        ~?= [("S", ["A", "S_1"]), ("S_1", ["B", "S_2"]), ("S_2", ["C", "S_3"]), ("S_3", ["D", "E"])]
    ]

-- Add a production to the CFG
testAddProduction :: Test
testAddProduction =
  let cfg = CFG "S" (S.fromList ["S", "A", "B"]) (S.fromList ["a", "b"]) (S.fromList [("S", ["A", "B"]), ("A", ["a"]), ("B", ["b"])])
      newProd = ("A", ["B", "a"])
      updatedCfg = addProduction cfg newProd
      expectedProductions = S.fromList [("S", ["A", "B"]), ("A", ["a"]), ("B", ["b"]), ("A", ["B", "a"])]
      expectedNonTerminals = S.fromList ["S", "A", "B"]
   in TestList
        [ "Adding a production"
            ~: productions updatedCfg
            ~?= expectedProductions,
          "Non-terminals are updated after adding production"
            ~: nonTerminals updatedCfg
            ~?= expectedNonTerminals
        ]

-- Remove a production from the CFG
testRemoveProduction :: Test
testRemoveProduction =
  let cfg = CFG "S" (S.fromList ["S", "A", "B"]) (S.fromList ["a", "b"]) (S.fromList [("S", ["A", "B"]), ("A", ["a"]), ("B", ["b"]), ("A", ["B", "a"])])
      prodToRemove = ("A", ["B", "a"])
      updatedCfg = removeProduction cfg prodToRemove
      expectedProductions = S.fromList [("S", ["A", "B"]), ("A", ["a"]), ("B", ["b"])]
      expectedNonTerminals = S.fromList ["S", "A", "B"]
   in TestList
        [ "Removing a production"
            ~: productions updatedCfg
            ~?= expectedProductions,
          "Non-terminals are updated after removing production"
            ~: nonTerminals updatedCfg
            ~?= expectedNonTerminals
        ]

-- Test cases for eliminateMoreThanTwoNonTerminals function
testEliminateMoreThanTwoNonTerminals :: Test
testEliminateMoreThanTwoNonTerminals =
  TestList
    [ let originalCFG = CFG "S" (S.fromList ["S", "A", "B", "C"]) (S.fromList ["a", "b"]) $ S.fromList [("S", ["A", "B"]), ("A", ["a"]), ("B", ["b"]), ("C", ["A", "B", "C", "D"])]
          expectedCFG =
            CFG "S" (S.fromList ["S", "A", "B", "C", "C_1", "C_2"]) (S.fromList ["a", "b"]) $
              S.fromList [("S", ["A", "B"]), ("A", ["a"]), ("B", ["b"]), ("C", ["A", "C_1"]), ("C_1", ["B", "C_2"]), ("C_2", ["C", "D"])]
       in "Test eliminating more than two non-terminals"
            ~: eliminateMoreThanTwoNonTerminals originalCFG
            ~?= expectedCFG,
      let originalCFG =
            CFG "S" (S.fromList ["S", "A", "B", "C"]) (S.fromList ["a", "b"]) $
              S.fromList
                [ ("S", ["A", "B"]),
                  ("A", ["a"]),
                  ("B", ["b"]),
                  ("C", ["A", "B", "C", "D"]),
                  ("D", ["E", "F", "G", "H", "I", "J"])
                ]
          expectedCFG =
            CFG "S" (S.fromList ["S", "A", "B", "C", "C_1", "C_2", "D", "D_1", "D_2", "D_3", "D_4"]) (S.fromList ["a", "b"]) $
              S.fromList
                [ ("S", ["A", "B"]),
                  ("A", ["a"]),
                  ("B", ["b"]),
                  ("C", ["A", "C_1"]),
                  ("C_1", ["B", "C_2"]),
                  ("C_2", ["C", "D"]),
                  ("D", ["E", "D_1"]),
                  ("D_1", ["F", "D_2"]),
                  ("D_2", ["G", "D_3"]),
                  ("D_3", ["H", "D_4"]),
                  ("D_4", ["I", "J"])
                ]
       in "Test eliminating more than two non-terminals"
            ~: eliminateMoreThanTwoNonTerminals originalCFG
            ~?= expectedCFG
    ]

-- Test cases for removeEpsilonProductions
testRemoveEpsilonProductions :: Test
testRemoveEpsilonProductions =
  TestList
    [ "Test 1"
        ~: removeEpsilonProductions grammarWithEpsilon
        ~?= grammarWithoutEpsilon
        -- Add more test cases here if needed
        -- ...
    ]
  where
    -- Sample grammar with epsilon productions
    grammarWithEpsilon =
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
    -- Expected grammar without epsilon productions
    grammarWithoutEpsilon =
      CFG
        { startSymbol = "S",
          nonTerminals = S.fromList ["S", "A", "B", "C"],
          terminals = S.fromList ["a", "b", "c"],
          productions =
            S.fromList
              [ ("S", ["A", "b", "B"]),
                ("S", ["A", "b"]),
                ("S", ["C"]),
                ("B", ["A", "A"]),
                ("B", ["A", "C"]),
                ("B", ["A"]),
                ("B", ["C"]),
                ("C", ["b"]),
                ("C", ["c"]),
                ("A", ["a"])
              ]
        }

-- Test cases for findNullableNonTerminals
testFindNullableNonTerminals :: Test
testFindNullableNonTerminals =
  TestList
    [ "Test nullable non-terminal parent rules are identified"
        ~: findNullableNonTerminals grammarWithEpsilon
        ~?= S.fromList ["A", "B"]
    ]
  where
    grammarWithEpsilon =
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

-- Test cases for isEpsilonProduction
testIsEpsilonProduction :: Test
testIsEpsilonProduction =
  TestList
    [ "Test non-nullable is not found to be nullable"
        ~: isEpsilonProduction nullableNonTerminals ("A", ["a"])
        ~?= False,
      "Test nullable not found to be unnullable"
        ~: isEpsilonProduction nullableNonTerminals ("A", [])
        ~?= True
    ]
  where
    -- Nullable non-terminals
    nullableNonTerminals = S.fromList ["A"]
