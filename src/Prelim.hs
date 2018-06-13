module Prelim (Nat, Rat, increasing) where

type Nat = Integer
type Rat = Rational

increasing :: [Rat] -> [Rat]
increasing qs = scanl (\b a -> max b a) 0 qs
