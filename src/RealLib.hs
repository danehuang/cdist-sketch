{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}

module RealLib where

import qualified Control.Exception as E
import Prelude hiding (Real)

import Prelim
import ApproxLib


{-----------------------------------------------------------
 - Types and enumerations
 ------------------------------------------------------------}

type Real = Approx Rat

enumRatPos :: [Rat]
enumRatPos = concat [ [ toRational m / 2^n | m <- [1..2^n * n - 1], isOdd m] ++
                      [ toRational n, toRational (-n)]
                    | n <- [1..]]
  where isOdd m = mod m 2 == 1

enumRat' :: [Rat]
enumRat' = [0] ++ concat [ [ toRational m / 2^n
                           | m <- [-2^n * n..2^n * n], f m n]
                         | n <- [1..]]
  where f m n = mod m 2 == 1 || abs m > 2^n * (n-1)

enumRat :: [Rat]
enumRat = concat [ [ toRational m / 2^n | m <- [1..2^n * n - 1], isOdd m] ++
                   [ toRational (-m) / 2^n | m <- [1..2^n * n - 1], isOdd m] ++
                   [ toRational n, toRational (-n)]
                 | n <- [1..]]
  where isOdd m = mod m 2 == 1


{-----------------------------------------------------------
 - Comparisons
 ------------------------------------------------------------}

(<$) :: Real -> Real -> [Bool]
(<$) x y = [ test n | n <- [0..] ]
    where test n = (nthApprox x n + (2 ^^ (-n))) < (nthApprox y n - (2 ^^ (-n)))

(>$) :: Real -> Real -> [Bool]
(>$) x y = [ test n | n <- [0..] ]
    where test n = (nthApprox x n - (2 ^^ (-n))) > (nthApprox y n + (2 ^^ (-n)))

dec2 :: [Bool] -> [Bool] -> Bool
dec2 (True:_) _ = True
dec2 _ (True:_) = False
dec2 (False:bs) (False:cs) = dec2 bs cs
                             
(<.) :: Real -> Real -> Bool
(<.) x y = search 0
    where search n =
            if (nthApprox x n + (2 ^^ (-n))) < (nthApprox y n - (2 ^^ (-n)))
            then True
            else (if (nthApprox x n - (2 ^^ (-n))) > (nthApprox y n + (2 ^^ (-n)))
                  then False
                  else search (n + 1))

(>.) :: Real -> Real -> Bool
(>.) x y = y <. x


{-----------------------------------------------------------
 - Airthmetic
 ------------------------------------------------------------}

-- TODO(deh): rename a bunch of stuff here

crn :: Nat -> Real
crn n = mkApprox (\_ -> toRational n) 

cr :: Rat -> Real
cr q = mkApprox (\_ -> q)

(+.) :: Real -> Real -> Real
(+.) x y = mkApprox (\n -> nthApprox x (n + 1) + nthApprox y (n + 1))

neg_cr :: Real -> Real
neg_cr x = mkApprox (\n -> - nthApprox x n)
          
(-.) :: Real -> Real -> Real
(-.) x y = x +. (neg_cr y)

(*.) :: Real -> Real -> Real
(*.) x y = mkApprox (\n -> let m1 = search (abs (nthApprox x n)) 0
                               m2 = search (abs (nthApprox y n)) 0
                               m = max m1 m2
                           in
                             nthApprox x (m + n) * nthApprox y (m + n))
    where
      search :: Rat -> Nat -> Nat
      search q m  =
          if (q + 2) < (2 ^^ (m - 1))
          then m
          else search q (m + 1)

inv :: Real -> Real
inv x = mkApprox (\n -> let m = search 0
                        in
                          1 / (nthApprox x (2 * m + n)))
  where search m = if nthApprox x m > (3 * (2 ^^ (-m)))
                   then m
                   else search (m + 1)

fast_inv :: Real -> Real
fast_inv x = mkApprox (\_ -> toRational (1.0 / toFloat (nthApprox x 16)))
                          
(/.) :: Real -> Real -> Real
(/.) x y = x *. (fast_inv y)

min_cr :: Real -> Real -> Real
min_cr x y = mkApprox (\n -> min (nthApprox x n) (nthApprox y n))

max_cr :: Real -> Real -> Real
max_cr x y = (x +. y) -. (min_cr x y)
           
abs_cr :: Real -> Real
abs_cr x = max_cr x (neg_cr x)
                  
pow_cr :: Real -> Nat -> Real
pow_cr _ 0 = cr 1
pow_cr x n = x *. pow_cr x (n - 1)
           
log_lt2 :: Real -> Real
log_lt2 x = mkApprox (\n -> nthApprox (search (cr 0) 1 1 (n + 1)) (n + 1))
    where search (acc :: Real) (sign :: Nat) n m =
              if n <= m
              then let acc' = if sign == 1
                              then acc +. ((pow_cr x n) /. crn n)
                              else acc -. ((pow_cr x n) /. crn n)
                   in
                     search acc' (flip sign) (n + 1) m
              else acc
          flip sign = if sign == 1 then -1 else 1

toFloat :: Rat -> Float
toFloat x = fromRational x
                      
fast_log :: Real -> Real
fast_log x = mkApprox (\_ -> toRational (log (toFloat (nthApprox x 16))))

fast_sqrt :: Real -> Real
fast_sqrt x = mkApprox (\_ -> toRational (sqrt (toFloat (nthApprox x 16))))


{-----------------------------------------------------------
 - Lower/Upper real conversions
 ------------------------------------------------------------}

luToCauchy :: [(Rat, Rat)] -> Real
luToCauchy lu = mkApprox (\n -> [ (l + u) / 2 | (l, u) <- lu, u - l < (1/2^n) ] !! fromInteger n)

cauchyToLu :: Real -> [(Rat, Rat)]
cauchyToLu x = [ (max (nthApprox x n - 1/2^n) 0, nthApprox x n + 1/2^n) | n <- [0..] ]


{-----------------------------------------------------------
 - Type-classes
 ------------------------------------------------------------}

instance Num Real where
    (+) = (+.)
    (*) = (*.)
    (-) = (-.)
    abs = abs_cr
    negate = neg_cr
    signum = E.throw E.NonTermination  -- sign of real undecidable
    fromInteger = crn

instance Fractional Real where
    (/) = (/.)
    fromRational = cr
                  
instance Eq Real where
    (==) = E.throw E.NonTermination  -- Equality is not decidable
    (/=) x y = search 0
        where search n =
                  if abs (nthApprox x n - nthApprox y n) > 2^^(-n + 1)
                  then True
                  else search (n + 1)
            
instance Ord Real where
    compare x y = search 0
        where search n =
                  if (nthApprox x n + (2 ^^ (-n))) < (nthApprox y n - (2 ^^ (-n)))
                  then LT
                  else (if (nthApprox x n - (2 ^^ (-n))) > (nthApprox y n + (2 ^^ (-n)))
                        then GT
                        else search (n + 1))
    (<=) = E.throw E.NonTermination   -- <= undecidable
    (>=) = E.throw E.NonTermination   -- >= undecidable
    (<) = (<.)
    (>) = (>.)
    max = max_cr
    min = min_cr

instance Show Real where
    show x = show (toFloat (nthApprox x 8))

instance CMetrizable Rat where
    enum = concat [ [ toRational m / 2^n | m <- [1..2^n * n - 1], isOdd m] ++
                    [ toRational (-m) / 2^n | m <- [1..2^n * n - 1], isOdd m] ++
                    [ toRational n, toRational (-n)]
                  | n <- [1..]]
        where isOdd m = mod m 2 == 1
    metric x y = mkApprox (\_ -> abs (x - y))
