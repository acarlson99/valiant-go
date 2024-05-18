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
        ~: splitProduction
          ("S", ["A", "b", "B"])
        ~?= [ ("S", ["A", "S_1"]),
              ("S_1", ["b", "B"])
            ],
      "Production with 0 non-terminals"
        ~: splitProduction
          ("S", [])
        ~?= [("S", [])],
      "Production with 1 non-terminal"
        ~: splitProduction
          ("S", ["A"])
        ~?= [("S", ["A"])],
      "Production with 2 non-terminals"
        ~: splitProduction
          ("S", ["A", "B"])
        ~?= [("S", ["A", "B"])],
      "Production with 3 non-terminals"
        ~: splitProduction
          ("S", ["A", "B", "C"])
        ~?= [ ("S", ["A", "S_1"]),
              ("S_1", ["B", "C"])
            ],
      "Production with 4 non-terminals"
        ~: splitProduction
          ("S", ["A", "B", "C", "D"])
        ~?= [ ("S", ["A", "S_1"]),
              ("S_1", ["B", "S_2"]),
              ("S_2", ["C", "D"])
            ],
      "Production with 5 non-terminals"
        ~: splitProduction
          ("S", ["A", "B", "C", "D", "E"])
        ~?= [ ("S", ["A", "S_1"]),
              ("S_1", ["B", "S_2"]),
              ("S_2", ["C", "S_3"]),
              ("S_3", ["D", "E"])
            ]
    ]

-- Test cases for eliminateMoreThanTwoNonTerminals function
testEliminateMoreThanTwoNonTerminals :: Test
testEliminateMoreThanTwoNonTerminals =
  let go = eliminateMoreThanTwoNonTerminals . gramFromProds
   in TestList
        [ "TestGeneric"
            ~: go
              [ ("S", ["A", "B", "C", "D"])
              ]
            ~?= gramFromProds
              [ ("S", ["S_1_L", "S_1_R"]),
                ("S_1_L", ["A", "B"]),
                ("S_1_R", ["C", "D"])
              ],
          "Test eliminating more than two non-terminals"
            ~: go
              [ ("S", ["A", "B"]),
                ("A", ["a"]),
                ("B", ["b"]),
                ("C", ["A", "B", "C", "D"])
              ]
            ~?= gramFromProds
              [ ("S", ["A", "B"]),
                ("A", ["a"]),
                ("B", ["b"]),
                ("C", ["C_1_L", "C_1_R"]),
                ("C_1_L", ["A", "B"]),
                ("C_1_R", ["C", "D"])
              ],
          "Test eliminating two rules with the same name does not conflict"
            ~: go
              [ ("S", ["C"]),
                ("A", ["a"]),
                ("B", ["b"]),
                ("C", ["A", "B", "B", "A"]),
                ("C", ["B", "A", "A", "A", "A", "B"])
              ]
            ~?= gramFromProds
              [ ("S", ["C"]),
                ("A", ["a"]),
                ("B", ["b"]),
                ("C", ["C_1_L", "C_1_R"]),
                ("C_1_L", ["A", "B"]),
                ("C_1_R", ["B", "A"]),
                ("C", ["C_2_L", "C_2_R"]),
                ("C_2_L", ["C_2_L_L", "C_2_L_R"]),
                ("C_2_L_L", ["B", "A"]),
                ("C_2_L_R", ["A", "A"]),
                ("C_2_R", ["A", "B"])
              ],
          "Test reducing non-terminals of length 6"
            ~: go
              [ ("S", ["A", "B"]),
                ("A", ["a"]),
                ("B", ["b"]),
                ("C", ["A", "B", "C", "D"]),
                ("D", ["E", "F", "G", "H", "I", "J"])
              ]
            ~?= gramFromProds
              [ ("S", ["A", "B"]),
                ("A", ["a"]),
                ("B", ["b"]),
                ("C", ["C_1_L", "C_1_R"]),
                ("C_1_L", ["A", "B"]),
                ("C_1_R", ["C", "D"]),
                ("D", ["D_2_L", "D_2_R"]),
                ("D_2_L", ["D_2_L_L", "D_2_L_R"]),
                ("D_2_L_L", ["E", "F"]),
                ("D_2_L_R", ["G", "H"]),
                ("D_2_R", ["I", "J"])
              ]
        ]

testRemoveEpsilonProductions :: Test
testRemoveEpsilonProductions =
  let go = removeEpsilonProductions . gramFromProds
   in TestList
        [ "Test remove rule"
            ~: go
              [ ("S", ["A", "B"]),
                ("S", ["C"]),
                ("A", []),
                ("A", ["a"]),
                ("B", ["b"]),
                ("C", ["c"])
              ]
            ~?= gramFromProds
              [ ("S", ["A", "B"]),
                ("S", ["B"]),
                ("S", ["C"]),
                ("A", ["a"]),
                ("B", ["b"]),
                ("C", ["c"])
              ],
          "Test remove rule will all nullable children"
            ~: go
              [ ("S", ["A", "b", "B"]),
                ("S", ["C"]),
                ("A", []),
                ("A", ["a"]),
                ("B", ["A", "A"]),
                ("C", ["b", "c"])
              ]
            ~?= gramFromProds
              [ ("S", ["A", "b", "B"]),
                ("S", ["A", "b"]),
                ("S", ["b", "B"]),
                ("S", ["b"]),
                ("S", ["C"]),
                ("A", ["a"]),
                ("B", ["A", "A"]),
                ("B", ["A"]),
                ("C", ["b", "c"])
              ],
          "Test removes all children and not start symbol when all empty"
            ~: go
              [ ("S", ["A", "B", "C"]),
                ("A", []),
                ("B", []),
                ("C", [])
              ]
            ~?= gramFromProds
              [ ("S", [])
              ],
          "test chaining"
            ~: go
              [ ("S", ["A", "b"]),
                ("A", ["B", "B"]),
                ("B", ["C"]),
                ("C", ["c"]),
                ("C", [])
              ]
            ~?= gramFromProds
              [ ("S", ["A", "b"]),
                ("S", ["b"]),
                ("A", ["B", "B"]),
                ("A", ["B"]),
                ("B", ["C"]),
                ("C", ["c"])
              ],
          "test removes completely empty rules"
            ~: go
              [ ("S", ["A", "B"]),
                ("A", ["a"]),
                ("B", ["C", "C"]),
                ("C", [])
              ]
            ~?= gramFromProds
              [ ("S", ["A"]),
                ("A", ["a"])
              ]
        ]

testInlineUnitRules =
  let go = inlineUnitRules . gramFromProds
   in test
        [ "Test normal reduction"
            ~: go
              [ ("S", ["A", "b", "B"]),
                ("S", ["b"]),
                ("S", ["C"]),
                ("A", ["a"]),
                ("B", ["A"]),
                ("C", ["b", "c"])
              ]
            ~?= gramFromProds
              [ ("S", ["A", "b", "B"]),
                ("S", ["b"]),
                ("S", ["b", "c"]),
                ("A", ["a"]),
                ("B", ["a"]),
                ("C", ["b", "c"])
              ],
          "Test chains are fully reduced"
            ~: go
              [ ("S", ["A"]),
                ("A", ["B"]),
                ("B", ["C"]),
                ("C", ["c"])
              ]
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
