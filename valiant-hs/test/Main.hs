module Main where

import Test.HUnit
import TestGrammar qualified as G

main :: IO ()
main = runTestTTAndExit . test $ G.tests
