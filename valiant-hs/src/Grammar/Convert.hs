module Grammar.Convert where

import Data.Set qualified as S
import Grammar.Chomsky qualified as Chomp
import Grammar.ContextFree qualified as CF

convert :: CF.CFG -> Chomp.ProductionRules String String
convert cfg' =
  let cfg@(CF.CFG _ prods) = if CF.isChomskyReducedForm cfg' then cfg' else CF.toChomskyReducedForm cfg'
   in if CF.isChomskyReducedForm cfg
        then map convertRule $ S.toList prods
        else error "for some reason this grammar in not in Chomsky Reduced Form"

convertRule :: CF.Production -> Chomp.ProductionRule String String
convertRule (x, xs) = case xs of
  [a] -> Chomp.Unary x (Chomp.Terminal a)
  [a, b] -> Chomp.Binary x (Chomp.Nonterminal a []) (Chomp.Nonterminal b [])
