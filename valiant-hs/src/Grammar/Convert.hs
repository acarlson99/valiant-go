module Grammar.Convert where

import Grammar.Chomsky qualified as Chomp
import Grammar.ContextFree qualified as CF

convert :: CF.CFG -> Chomp.ProductionRules nt t
convert cfg@(CF.CFG start prods) = undefined