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
            ("C", ["C_L", "C_R"]),
            ("C_L", ["A", "B"]),
            ("C_R", ["C", "D"])
          ],
      "Test eliminating two rules with the same name does not conflict"
        ~: eliminateMoreThanTwoNonTerminals
          ( gramFromProds
              [ ("S", ["C"]),
                ("A", ["a"]),
                ("B", ["b"]),
                ("C", ["A", "B", "B", "A"]),
                ("C", ["B", "A", "A", "A", "A", "B"])
              ]
          )
        ~?= gramFromProds
          [ ("S", ["C"]),
            ("A", ["a"]),
            ("B", ["b"]),
            ("C_1", ["C_1_L", "C_1_R"]),
            ("C_1_L", ["A", "B"]),
            ("C_1_R", ["B", "A"]),
            ("C_2", ["C_2_L", "C_2_R"]),
            ("C_2_L", ["C_2_L_L", "C_2_L_R"]),
            ("C_2_L_L", ["B", "A"]),
            ("C_2_L_R", ["A", "A"]),
            ("C_2_R", ["A", "B"])
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
            ("C", ["C_L", "C_R"]),
            ("C_L", ["A", "B"]),
            ("C_R", ["C", "D"]),
            ("D", ["D_L", "D_R"]),
            ("D_L", ["D_L_L", "D_L_R"]),
            ("D_L_L", ["E", "F"]),
            ("D_L_R", ["G", "H"]),
            ("D_R", ["I", "J"])
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
    [ "Test normal reduction"
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
          ],
      "Test chains are fully reduced"
        ~: inlineUnitRules
          ( gramFromProds
              [ ("S", ["A"]),
                ("A", ["B"]),
                ("B", ["C"]),
                ("C", ["c"])
              ]
          )
        ~?= gramFromProds
          [ ("S", ["c"]),
            ("A", ["c"]),
            ("B", ["c"]),
            ("C", ["c"])
          ]
    ]

-- https://en.wikipedia.org/wiki/Chomsky_normal_form#Example
wikiExample :: [Production]
wikiExample =
  [ ("Expr", ["Term"]),
    ("Expr", ["Expr", "AddOp", "Term"]),
    ("Expr", ["AddOp", "Term"]),
    ("Term", ["Factor"]),
    ("Term", ["Term", "MulOp", "Factor"]),
    ("Factor", ["Primary"]),
    ("Factor", ["Factor", "^", "Primary"]),
    ("Primary", ["number", "variable"]),
    ("Primary", ["(", "Expr", ")"]),
    ("AddOp", ["+"]),
    ("AddOp", ["-"]),
    ("MulOp", ["*"]),
    ("MulOp", ["/"])
  ]
