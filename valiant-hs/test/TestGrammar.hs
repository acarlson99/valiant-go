module TestGrammar where

import Grammar.ContextFree
import Test.HUnit

tests :: [Test]
tests = [testRenameNonTerminal, testReplaceProduction, testSplitProduction]

testRenameNonTerminal :: Test
testRenameNonTerminal =
  let testCFG = CFG "S" ["A", "B", "C"] ["a", "b"] [("S", ["A", "a"]), ("A", ["B", "b"]), ("C", ["a"]), ("B", ["C"])]
   in test
        [ "Renaming a non-terminal that exists in the CFG should modify it"
            ~: renameNonTerminal "A" "X" testCFG
            ~=? CFG "S" ["X", "B", "C"] ["a", "b"] [("S", ["X", "a"]), ("X", ["B", "b"]), ("B", ["C"]), ("C", ["a"])],
          "Renaming a non-terminal that doesn't exist in the CFG"
            ~: renameNonTerminal "D" "X" testCFG
            ~=? CFG "S" ["A", "B", "C"] ["a", "b"] [("S", ["A", "a"]), ("A", ["B", "b"]), ("B", ["C"]), ("C", ["a"])],
          "Renaming the start symbol"
            ~: renameNonTerminal "S" "X" testCFG
            ~=? CFG "X" ["A", "B", "C"] ["a", "b"] [("X", ["A", "a"]), ("A", ["B", "b"]), ("B", ["C"]), ("C", ["a"])],
          "Renaming a non-terminal to the same name"
            ~: renameNonTerminal "A" "A" testCFG
            ~=? testCFG
        ]

testReplaceProduction :: Test
testReplaceProduction =
  test
    [ "Replace single production"
        ~: let cfg = CFG {startSymbol = "S", nonTerminals = ["S", "A", "B"], terminals = ["a", "b"], productions = [("S", ["A"]), ("A", ["a", "B"]), ("B", ["b"])]}
               oldProd = ("A", ["a", "B"])
               newProd = ("A", ["a", "B", "a"])
               expectedCFG = CFG {startSymbol = "S", nonTerminals = ["S", "A", "B"], terminals = ["a", "b"], productions = [("S", ["A"]), ("A", ["a", "B", "a"]), ("B", ["b"])]}
            in replaceProduction cfg oldProd newProd ~=? expectedCFG,
      "Replace production not in nonTerminals"
        ~: let cfg = CFG {startSymbol = "S", nonTerminals = ["S", "A", "B"], terminals = ["a", "b"], productions = [("S", ["A"]), ("A", ["a", "B"]), ("B", ["b"])]}
               oldProd = ("C", ["a", "b"])
               newProd = ("C", ["a", "a"])
               expectedCFG = CFG {startSymbol = "S", nonTerminals = ["S", "A", "B"], terminals = ["a", "b"], productions = [("S", ["A"]), ("A", ["a", "B"]), ("B", ["b"])]}
            in replaceProduction cfg oldProd newProd ~=? expectedCFG,
      "Replace production with start symbol"
        ~: let cfg = CFG {startSymbol = "A", nonTerminals = ["A", "B"], terminals = ["a", "b"], productions = [("A", ["a"]), ("B", ["b"])]}
               oldProd = ("A", ["a"])
               newProd = ("A", ["b"])
               expectedCFG = CFG {startSymbol = "A", nonTerminals = ["A", "B"], terminals = ["a", "b"], productions = [("A", ["b"]), ("B", ["b"])]}
            in replaceProduction cfg oldProd newProd ~=? expectedCFG
    ]

testSplitProduction :: Test
testSplitProduction =
  test
    [ "Split production"
        ~: splitProduction ("S", ["A", "b", "B"])
        ~=? [("S", ["A", "S_1"]), ("S_1", ["b", "B"])],
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
