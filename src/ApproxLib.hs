{-# LANGUAGE ScopedTypeVariables #-}

module ApproxLib (Approx(..), CMetrizable(..)
                 , mkApprox, nthApprox
                 , diagonal, uniqueFail, trunc, modulus
                 , Three(..), collapse, outer, searchOuter) where

import qualified Control.Exception as E

import Prelim


{-------------------------------------------------------------------------------
 - [Note]
 -
 - Contains functionality for approximating elements of a computable metric space.
 -
 -------------------------------------------------------------------------------}


newtype Approx a = Approx { getApprox :: Nat -> a }

mkApprox :: (Nat -> a) -> Approx a  -- fast Cauchy sequence
mkApprox = Approx

nthApprox :: Approx a -> Nat -> a   -- project n-th approx.
nthApprox = getApprox

class CMetrizable a where
    enum :: [a]                     -- countable, dense subset
    metric :: a -> a -> Approx Rat  -- computable metric


{-----------------------------------------------------------
 - Helper
 ------------------------------------------------------------}

-- NOTE(deh): Implementation taken from Control.Monad.Omega
diagonal :: [[a]] -> [a]
diagonal = concat . stripe
    where
    stripe [] = []
    stripe ([]:xss) = stripe xss
    stripe ((x:xs):xss) = [x] : zipCons xs (stripe xss)

    zipCons [] ys = ys
    zipCons xs [] = map (:[]) xs
    zipCons (x:xs) (y:ys) = (x:y) : zipCons xs ys


{-----------------------------------------------------------
 - Modulus of continuity
 ------------------------------------------------------------}

uniqueFail :: a
uniqueFail = error "HAHA ALL YOUR BASE ARE BELONG TO US."

trunc :: Approx a -> Nat -> Approx a
trunc x n = mkApprox (\m -> if m < n then (nthApprox x m) else uniqueFail)

{- Inspired by
 - http://math.andrej.com/2006/03/27/sometimes-all-functions-are-continuous
 -}
modulus :: (Approx a -> Approx b) -> Approx a -> Nat -> IO Nat
modulus f x n = do
  e :: Either E.ErrorCall b <- E.try (E.evaluate $ nthApprox (f x) n)
  case e of
    Left _ -> E.throw E.NonTermination
    Right _ -> search 0
    where
      search :: Nat -> IO Nat
      search acc = do
            e :: Either E.ErrorCall b <- E.try $ E.evaluate $ nthApprox (f (trunc x acc)) n
            case e of
              Left _ -> search (acc + 1)
              Right _ -> return acc    


{-----------------------------------------------------------
 - Searching
 ------------------------------------------------------------}

data Three = In | Out | Unknown

collapse :: [Three] -> Bool
collapse [] = uniqueFail
collapse (In:_) = True
collapse (Out:_) = False
collapse (Unknown:xs) = collapse xs

outer :: [[a]] -> [a]
outer xs = map (\ys -> head ys) xs ++ outer (map (\ys -> tail ys) xs)
              
searchOuter :: [[Bool]] -> [Bool]
searchOuter xs =
    -- Search for finite list of infinite lists
    if all (\x -> x) (map (\ys -> head ys) xs)
    then repeat True
    else False : searchOuter (map (\ys -> tail ys) xs)
