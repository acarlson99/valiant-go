{-# LANGUAGE GADTs #-}

module ContextFree where

data CFG a where
  Rule :: a -> [a] -> CFG a
