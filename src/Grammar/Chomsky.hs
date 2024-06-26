module Grammar.Chomsky where

import Data.Data

-- Symbol is either a terminal or a nonterminal
-- Nonterminals contain children which should be empty before parsing begins
-- Nonterminal children are filled in during parsing
data Symbol nt t = Nonterminal nt [Symbol nt t] | Terminal t
  deriving (Eq, Ord)

newTerm :: t -> Symbol nt t
newTerm = Terminal

newNonTerm :: nt -> Symbol nt t
newNonTerm = flip Nonterminal []

symNameEq :: (Eq nt, Eq t) => Symbol nt t -> Symbol nt t -> Bool
symNameEq (Nonterminal a _) (Nonterminal b _) = a == b
symNameEq (Terminal a) (Terminal b) = a == b
symNameEq _ _ = False

instance (Show t, Show nt, Typeable t, Typeable nt) => Show (Symbol nt t) where
  show (Nonterminal n _) =
    '<'
      : ( case cast n of
            (Just (s :: String)) -> s
            _ -> show n
        )
      ++ ">"
  show (Terminal a) =
    case cast a of
      (Just (s :: String)) -> show s
      _ -> "\"" ++ show a ++ "\""

data ProductionRule nt t where
  Binary :: nt -> Symbol nt t -> Symbol nt t -> ProductionRule nt t
  Unary :: nt -> Symbol nt t -> ProductionRule nt t

type ProductionRules nt t = [ProductionRule nt t]

instance (Show t, Show nt, Typeable t, Typeable nt) => Show (ProductionRule nt t) where
  show rule = s ++ " -> " ++ ss
    where
      -- ss :: [Symbol nt t]
      -- s :: Symbol nt t
      (s, ss) = case rule of
        (Unary n a) -> (show n, show a)
        (Binary n a b) -> (show n, show a ++ " " ++ show b)

binApp :: (Eq nt, Eq t) => ProductionRule nt t -> Symbol nt t -> Symbol nt t -> Maybe (Symbol nt t)
binApp (Binary n a b) x y = if x `symNameEq` a && b `symNameEq` y then return $ Nonterminal n [x, y] else Nothing
binApp Unary {} _ _ = Nothing

unaryApp :: (Eq nt, Eq t) => ProductionRule nt t -> Symbol nt t -> Maybe (Symbol nt t)
unaryApp (Unary n a) x = if x `symNameEq` a then return $ Nonterminal n [x] else Nothing
unaryApp Binary {} _ = Nothing
