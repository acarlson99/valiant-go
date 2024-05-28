module TestGrammar where

import Data.Set qualified as S
import Grammar.ContextFree
import Test.HUnit

tests :: [Test]
tests =
  [ testEliminateRulesWithNonsolitaryTerminals,
    testEliminateMoreThanTwoNonTerminals,
    testRemoveEpsilonRules,
    testEliminateUnitRules,
    testWikiExample,
    testIsChomskyReduced,
    testRemoveUnusedRules
  ]

gramFromProds :: [Production] -> CFG
gramFromProds prods = case prods of
  ((x, _) : _) ->
    CFG
      { startSymbol = x,
        productions = S.fromList prods
      }
  [] -> error "Invalid grammar"

testEliminateRulesWithNonsolitaryTerminals :: Test
testEliminateRulesWithNonsolitaryTerminals =
  let go = eliminateRulesWithNonsolitaryTerminals . gramFromProds
   in TestList
        [ "Test generic"
            ~: go
              [ ("S", ["a", "S"])
              ]
            ~?= gramFromProds
              [ ("S", ["T_a", "S"]),
                ("T_a", ["a"])
              ]
        ]

testEliminateMoreThanTwoNonTerminals :: Test
testEliminateMoreThanTwoNonTerminals =
  let go = eliminateMoreThanTwoNonTerminals . gramFromProds
   in TestList
        [ "TestGeneric"
            ~: go
              [ ("S", ["A", "B", "C", "D"])
              ]
            ~?= gramFromProds
              [ ("S", ["A_B", "C_D"]),
                ("A_B", ["A", "B"]),
                ("C_D", ["C", "D"])
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
                ("C", ["A_B", "C_D"]),
                ("A_B", ["A", "B"]),
                ("C_D", ["C", "D"])
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
                ("C", ["A_B", "B_A"]),
                ("A_B", ["A", "B"]),
                ("B_A", ["B", "A"]),
                ("C", ["B_A_A_A", "A_B"]),
                ("B_A_A_A", ["B_A", "A_A"]),
                ("B_A", ["B", "A"]),
                ("A_A", ["A", "A"]),
                ("A_B", ["A", "B"])
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
                ("C", ["A_B", "C_D"]),
                ("A_B", ["A", "B"]),
                ("C_D", ["C", "D"]),
                ("D", ["E_F_G_H", "I_J"]),
                ("E_F_G_H", ["E_F", "G_H"]),
                ("E_F", ["E", "F"]),
                ("G_H", ["G", "H"]),
                ("I_J", ["I", "J"])
              ]
        ]

testRemoveEpsilonRules :: Test
testRemoveEpsilonRules =
  let go = removeEpsilonRules . gramFromProds
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

testEliminateUnitRules :: Test
testEliminateUnitRules =
  let go = eliminateUnitRules . gramFromProds
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
              ],
          "Test eliminations do not reduce to level of nonterminals"
            ~: go
              [ ("S", ["A", "B"]),
                ("A", ["C"]),
                ("B", ["b"]),
                ("C", ["c"])
              ]
            ~?= gramFromProds
              [ ("S", ["A", "B"]),
                ("A", ["c"]),
                ("B", ["b"]),
                ("C", ["c"])
              ]
        ]

-- https://en.wikipedia.org/wiki/Chomsky_normal_form#Example
wikiExample :: CFG
wikiExample =
  gramFromProds
    [ ("Expr", ["Term"]),
      ("Expr", ["Expr", "AddOp", "Term"]),
      ("Expr", ["AddOp", "Term"]),
      ("Term", ["Factor"]),
      ("Term", ["Term", "MulOp", "Factor"]),
      ("Factor", ["Primary"]),
      ("Factor", ["Factor", "^", "Primary"]),
      ("Primary", ["number"]),
      ("Primary", ["variable"]),
      ("Primary", ["(", "Expr", ")"]),
      ("AddOp", ["+"]),
      ("AddOp", ["-"]),
      ("MulOp", ["*"]),
      ("MulOp", ["/"])
    ]

testWikiExample :: Test
testWikiExample =
  TestList
    [ "Test START"
        ~: eliminateStartSymbol wikiExample
        ~?= gramFromProds
          [ ("S0", ["Expr"]),
            ("Expr", ["Term"]),
            ("Expr", ["Expr", "AddOp", "Term"]),
            ("Expr", ["AddOp", "Term"]),
            ("Term", ["Factor"]),
            ("Term", ["Term", "MulOp", "Factor"]),
            ("Factor", ["Primary"]),
            ("Factor", ["Factor", "^", "Primary"]),
            ("Primary", ["number"]),
            ("Primary", ["variable"]),
            ("Primary", ["(", "Expr", ")"]),
            ("AddOp", ["+"]),
            ("AddOp", ["-"]),
            ("MulOp", ["*"]),
            ("MulOp", ["/"])
          ],
      "Test TERM"
        ~: (eliminateRulesWithNonsolitaryTerminals . eliminateStartSymbol) wikiExample
        ~?= gramFromProds
          [ ("S0", ["Expr"]),
            ("Expr", ["Term"]),
            ("Expr", ["Expr", "AddOp", "Term"]),
            ("Expr", ["AddOp", "Term"]),
            ("Term", ["Factor"]),
            ("Term", ["Term", "MulOp", "Factor"]),
            ("Factor", ["Primary"]),
            ("Factor", ["Factor", "T_^", "Primary"]),
            ("Primary", ["T_number"]),
            ("T_number", ["number"]),
            ("Primary", ["T_variable"]),
            ("T_variable", ["variable"]),
            ("Primary", ["T_(", "Expr", "T_)"]),
            ("AddOp", ["T_+"]),
            ("T_+", ["+"]),
            ("AddOp", ["T_-"]),
            ("T_-", ["-"]),
            ("MulOp", ["T_*"]),
            ("T_*", ["*"]),
            ("MulOp", ["T_/"]),
            ("T_/", ["/"]),
            ("T_(", ["("]),
            ("T_)", [")"]),
            ("T_^", ["^"])
          ],
      "Test BIN"
        ~: ( eliminateMoreThanTwoNonTerminals
               . eliminateRulesWithNonsolitaryTerminals
               . eliminateStartSymbol
           )
          wikiExample
        ~?= gramFromProds
          [ ("S0", ["Expr"]),
            ("Expr", ["Term"]),
            ("Expr", ["Expr_AddOp", "Term"]),
            ("Expr_AddOp", ["Expr", "AddOp"]),
            ("Expr", ["AddOp", "Term"]),
            ("Term", ["Factor"]),
            ("Term", ["Term_MulOp", "Factor"]),
            ("Term_MulOp", ["Term", "MulOp"]),
            ("Factor", ["Primary"]),
            ("Factor", ["Factor_T_^", "Primary"]),
            ("Factor_T_^", ["Factor", "T_^"]),
            ("Primary", ["T_number"]),
            ("T_number", ["number"]),
            ("Primary", ["T_variable"]),
            ("T_variable", ["variable"]),
            ("Primary", ["T_(_Expr", "T_)"]),
            ("T_(_Expr", ["T_(", "Expr"]),
            ("AddOp", ["T_+"]),
            ("T_+", ["+"]),
            ("AddOp", ["T_-"]),
            ("T_-", ["-"]),
            ("MulOp", ["T_*"]),
            ("T_*", ["*"]),
            ("MulOp", ["T_/"]),
            ("T_/", ["/"]),
            ("T_(", ["("]),
            ("T_)", [")"]),
            ("T_^", ["^"])
          ],
      "Test DEL"
        ~: ( removeEpsilonRules
               . eliminateMoreThanTwoNonTerminals
               . eliminateRulesWithNonsolitaryTerminals
               . eliminateStartSymbol
           )
          wikiExample
        ~?= gramFromProds
          [ ("S0", ["Expr"]),
            ("Expr", ["Term"]),
            ("Expr", ["Expr_AddOp", "Term"]),
            ("Expr_AddOp", ["Expr", "AddOp"]),
            ("Expr", ["AddOp", "Term"]),
            ("Term", ["Factor"]),
            ("Term", ["Term_MulOp", "Factor"]),
            ("Term_MulOp", ["Term", "MulOp"]),
            ("Factor", ["Primary"]),
            ("Factor", ["Factor_T_^", "Primary"]),
            ("Factor_T_^", ["Factor", "T_^"]),
            ("Primary", ["T_number"]),
            ("T_number", ["number"]),
            ("Primary", ["T_variable"]),
            ("T_variable", ["variable"]),
            ("Primary", ["T_(_Expr", "T_)"]),
            ("T_(_Expr", ["T_(", "Expr"]),
            ("AddOp", ["T_+"]),
            ("T_+", ["+"]),
            ("AddOp", ["T_-"]),
            ("T_-", ["-"]),
            ("MulOp", ["T_*"]),
            ("T_*", ["*"]),
            ("MulOp", ["T_/"]),
            ("T_/", ["/"]),
            ("T_(", ["("]),
            ("T_)", [")"]),
            ("T_^", ["^"])
          ],
      "Test UNIT"
        ~: ( eliminateUnitRules
               . removeEpsilonRules
               . eliminateMoreThanTwoNonTerminals
               . eliminateRulesWithNonsolitaryTerminals
               . eliminateStartSymbol
           )
          wikiExample
        ~?= gramFromProds
          [ ("S0", ["AddOp", "Term"]),
            ("S0", ["Expr_AddOp", "Term"]),
            ("S0", ["Factor_T_^", "Primary"]),
            ("S0", ["T_(_Expr", "T_)"]),
            ("S0", ["Term_MulOp", "Factor"]),
            ("S0", ["number"]),
            ("S0", ["variable"]),
            ("Expr", ["AddOp", "Term"]),
            ("Expr", ["Expr_AddOp", "Term"]),
            ("Expr_AddOp", ["Expr", "AddOp"]),
            ("Expr", ["Factor_T_^", "Primary"]),
            ("Expr", ["T_(_Expr", "T_)"]),
            ("Expr", ["Term_MulOp", "Factor"]),
            ("Expr", ["number"]),
            ("Expr", ["variable"]),
            ("Term", ["Factor_T_^", "Primary"]),
            ("Term", ["T_(_Expr", "T_)"]),
            ("Term", ["Term_MulOp", "Factor"]),
            ("Term_MulOp", ["Term", "MulOp"]),
            ("Term", ["number"]),
            ("Term", ["variable"]),
            ("Factor", ["Factor_T_^", "Primary"]),
            ("Factor", ["T_(_Expr", "T_)"]),
            ("Factor", ["number"]),
            ("Factor", ["variable"]),
            ("Factor_T_^", ["Factor", "T_^"]),
            ("Primary", ["number"]),
            ("T_number", ["number"]),
            ("Primary", ["variable"]),
            ("T_variable", ["variable"]),
            ("Primary", ["T_(_Expr", "T_)"]),
            ("T_(_Expr", ["T_(", "Expr"]),
            ("AddOp", ["+"]),
            ("AddOp", ["-"]),
            ("MulOp", ["*"]),
            ("MulOp", ["/"]),
            ("T_(", ["("]),
            ("T_)", [")"]),
            ("T_*", ["*"]),
            ("T_+", ["+"]),
            ("T_-", ["-"]),
            ("T_/", ["/"]),
            ("T_^", ["^"])
          ]
    ]

testIsChomskyReduced :: Test
testIsChomskyReduced =
  let goGram = isChomskyReducedForm . gramFromProds
   in test
        [ "Test 1 terminal passes"
            ~: goGram [("S", ["a"])]
            ~?= True,
          "Test 1 nonterminal fails"
            ~: goGram
              [ ("S", ["A"]),
                ("A", ["a"])
              ]
            ~?= False,
          "Test 2 terminals fails"
            ~: goGram
              [ ("S", ["a", "b"])
              ]
            ~?= False,
          "Test 2 nonterminals passes"
            ~: goGram
              [ ("S", ["A", "B"]),
                ("A", ["a"]),
                ("B", ["b"])
              ]
            ~?= True,
          "Test 3+ nonterminals fails"
            ~: goGram
              [ ("S", ["A", "B", "C"]),
                ("A", ["a"]),
                ("B", ["b"]),
                ("C", ["c"])
              ]
            ~?= False,
          "Test isChomskyReduced"
            ~: (isChomskyReducedForm . toChomskyReducedForm) wikiExample
            ~?= True,
          "Test isChomskyReduced should fail on non-reduced form"
            ~: isChomskyReducedForm wikiExample
            ~?= False
        ]

testRemoveUnusedRules :: Test
testRemoveUnusedRules =
  let go = removeUnusedRules . gramFromProds
   in test
        [ "Test removes unused"
            ~: go [("S", ["a"]), ("A", ["a"])]
            ~?= gramFromProds
              [("S", ["a"])],
          "Test does not remove used"
            ~: go [("S", ["A"]), ("A", ["a"])]
            ~?= gramFromProds
              [("S", ["A"]), ("A", ["a"])],
          "Test removes entire unused trees"
            ~: go [("S", ["A"]), ("A", ["a"]), ("B", ["C"]), ("C", ["D"]), ("D", ["d"])]
            ~?= gramFromProds
              [("S", ["A"]), ("A", ["a"])]
        ]
