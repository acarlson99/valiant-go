module TestGrammar where

import Data.Set qualified as S
import Grammar.ContextFree
import Test.HUnit

tests =
  [ testSplitProduction,
    testEliminateMoreThanTwoNonTerminals,
    testRemoveEpsilonProductions,
    testInlineUnitRules
  ]

gramFromProds :: [Production] -> CFG
gramFromProds prods =
  CFG
    { startSymbol = fst $ head prods,
      nonTerminals = nts,
      terminals = (S.fromList $ concatMap (\(x, xs) -> x : xs) prods) S.\\ nts,
      productions = S.fromList prods
    }
  where
    nts = S.fromList $ map fst prods

testSplitProduction :: Test
testSplitProduction =
  test
    [ "Split production"
        ~: splitProduction ("S", ["A", "b", "B"])
        ~?= [ ("S", ["A", "S_1"]),
              ("S_1", ["b", "B"])
            ],
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
        ~?= [ ("S", ["A", "S_1"]),
              ("S_1", ["B", "C"])
            ],
      "Production with 4 non-terminals"
        ~: splitProduction ("S", ["A", "B", "C", "D"])
        ~?= [ ("S", ["A", "S_1"]),
              ("S_1", ["B", "S_2"]),
              ("S_2", ["C", "D"])
            ],
      "Production with 5 non-terminals"
        ~: splitProduction ("S", ["A", "B", "C", "D", "E"])
        ~?= [ ("S", ["A", "S_1"]),
              ("S_1", ["B", "S_2"]),
              ("S_2", ["C", "S_3"]),
              ("S_3", ["D", "E"])
            ]
    ]

-- Test cases for eliminateMoreThanTwoNonTerminals function
testEliminateMoreThanTwoNonTerminals :: Test
testEliminateMoreThanTwoNonTerminals =
  TestList
    [ "Test eliminating more than two non-terminals"
        ~: eliminateMoreThanTwoNonTerminals
          ( gramFromProds
              [ ("S", ["A", "B"]),
                ("A", ["a"]),
                ("B", ["b"]),
                ("C", ["A", "B", "C", "D"])
              ]
          )
        ~?= gramFromProds
          [ ("S", ["A", "B"]),
            ("A", ["a"]),
            ("B", ["b"]),
            ("C", ["A", "C_1"]),
            ("C_1", ["B", "C_2"]),
            ("C_2", ["C", "D"])
          ],
      "Test reducing non-terminals of length 6"
        ~: eliminateMoreThanTwoNonTerminals
          ( gramFromProds
              [ ("S", ["A", "B"]),
                ("A", ["a"]),
                ("B", ["b"]),
                ("C", ["A", "B", "C", "D"]),
                ("D", ["E", "F", "G", "H", "I", "J"])
              ]
          )
        ~?= gramFromProds
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
    ]

testRemoveEpsilonProductions :: Test
testRemoveEpsilonProductions =
  TestList
    [ "Test 1"
        ~: removeEpsilonProductions
          ( gramFromProds
              [ ("S", ["A", "b", "B"]),
                ("S", ["C"]),
                ("B", ["A", "A"]),
                ("C", ["b", "c"]),
                ("A", []),
                ("A", ["a"])
              ]
          )
        ~?= gramFromProds
          [ ("S", ["A", "b", "B"]),
            ("S", ["A", "b"]),
            ("S", ["b", "B"]),
            ("S", ["b"]),
            ("S", ["C"]),
            ("B", ["A", "A"]),
            ("B", ["A"]),
            ("C", ["b", "c"]),
            ("A", ["a"])
          ],
      "Test 2"
        ~: removeEpsilonProductions
          ( gramFromProds
              [ ("S", ["A", "b", "B"]),
                ("S", ["C"]),
                ("B", ["A", "A"]),
                ("B", ["A", "C"]),
                ("C", ["b"]),
                ("C", ["c"]),
                ("A", ["a"]),
                ("A", [])
              ]
          )
        ~?= gramFromProds
          [ ("S", ["A", "b", "B"]),
            ("S", ["A", "b"]),
            ("S", ["b", "B"]),
            ("S", ["b"]),
            ("S", ["C"]),
            ("B", ["A", "A"]),
            ("B", ["A", "C"]),
            ("B", ["A"]),
            ("B", ["C"]),
            ("C", ["b"]),
            ("C", ["c"]),
            ("A", ["a"])
          ],
      "test chaining"
        ~: removeEpsilonProductions
          ( gramFromProds
              [ ("S", ["A", "b"]),
                ("A", ["B", "B"]),
                ("B", ["C"]),
                ("C", ["c"]),
                ("C", [])
              ]
          )
        ~?= gramFromProds
          [ ("S", ["A", "b"]),
            ("S", ["b"]),
            ("A", ["B", "B"]),
            ("A", ["B"]),
            ("B", ["C"]),
            ("C", ["c"])
          ]
    ]

testInlineUnitRules =
  test
    [ "Test chains are reduced"
        ~: inlineUnitRules
          ( gramFromProds
              [ ("S", ["A", "b", "B"]),
                ("S", ["b"]),
                ("S", ["C"]),
                ("A", ["a"]),
                ("B", ["A"]),
                ("C", ["b", "c"])
              ]
          )
        ~?= gramFromProds
          [ ("S", ["A", "b", "B"]),
            ("S", ["b"]),
            ("S", ["b", "c"]),
            ("A", ["a"]),
            ("B", ["a"]),
            ("C", ["b", "c"])
          ]
    ]
