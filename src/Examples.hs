{-# LANGUAGE FlexibleInstances, FlexibleContexts, ScopedTypeVariables #-}

module Examples where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import qualified Control.Monad.Random as R
import qualified Control.Exception as E
import Data.Ratio
import Prelude hiding (Real)
import qualified System.IO as IO

import Prelim

import ApproxLib
import CompDistLib
import RealLib
import qualified MList as LL



{-----------------------------------------------------------
 - The standard uniform, the basic distribution
 ------------------------------------------------------------}
           
stdUniform :: SampT Real
stdUniform = mkSampT (\u -> mkApprox (\n -> bisect n u 0 1 0))
    where
      bisect n u (left :: Rat) (right :: Rat) m =
          if m < n
          then (if u m
                then bisect n u left (midpt left right) (m+1)
                else bisect n u (midpt left right) right (m+1))
          else midpt left right
      midpt left right = left + (right - left) / 2 


{-----------------------------------------------------------
 - Discrete distributions
 ------------------------------------------------------------}
                         
bernoulli :: Real -> SampT Bool
bernoulli bias = do
  u <- stdUniform
  return (u < bias)

geometric :: Real -> SampT Nat
geometric bias = do
  b <- bernoulli bias
  if b
  then return 1
  else do
    n <- geometric bias
    return (n + 1)

geometricK :: Real -> SampT Nat
geometricK bias = geometricK' bias (\n -> return n)
    where geometricK' :: Real -> (Nat -> SampT Nat) -> SampT Nat
          geometricK' bias k = do
            b <- bernoulli bias
            if b
            then k 1
            else geometricK' bias (\n -> k (n + 1))


{-----------------------------------------------------------
 - Continuous distributions
 ------------------------------------------------------------}

uniform :: Real -> Real -> SampT Real
uniform a b = do
  u <- stdUniform
  return (u * (b - a) + a)

shiftedUniform :: SampT Real
shiftedUniform = uniform (10 ^^ 8) ((10 ^^ 8) + (10 ^^ (-8)))

stdNormal :: SampT Real
stdNormal = do 
  u1 <- uniform (-1) 1
  u2 <- uniform (-1) 1
  s <- return (u1 * u1 + u2 * u2)
  if s < 1
  then return $ u1 * fast_sqrt (- fast_log s / s)
  else stdNormal

normal :: Real -> Real -> SampT Real
normal mu var = do
  x <- stdNormal
  return $ x * fast_sqrt var + mu

gamma1 :: Real -> SampT Real
gamma1 alpha =
    let d = alpha - (1/3)
        c = 1 / fast_sqrt (9 * d)
    in do
      z <- normal 0 1
      u <- stdUniform
      v <- return $ (1 + c * z) * (1 + c * z) * (1 + c * z)
      if z > -1 / c && fast_log u < (0.5 * z * z) + d - (d * v) + (d * (fast_log v))
      then return $ d * v
      else gamma1 alpha

gamma :: Real -> Real -> SampT Real
gamma a b = do
  x <- gamma1 a
  return $ x / b
       
beta :: Real -> Real -> SampT Real
beta a b = do
  x <- gamma a 1
  y <- gamma b 1
  return (x / (x + y))

expon :: Real -> SampT Real
expon l = do
  u <- stdUniform
  return (- fast_log u / l)


{-----------------------------------------------------------
 - Non-parametric distributions
 ------------------------------------------------------------}

type LList = LL.MList SampT
    
-- Urn representation

discId' :: Nat -> Real -> [Real] -> SampT Nat
discId' idx _ [] = return idx
discId' idx acc (w:ws) = do
  u <- stdUniform
  if u < acc
  then return idx
  else  discId' (idx+1) (acc+w) ws

discId :: [Real] -> SampT Nat
discId [] = error "Empty list in disc_id"
discId (w:ws) = discId' 0 w ws

urn' :: Real -> SampT a -> [a] -> SampT a
urn' alpha g0 prev =
    let l = length prev
        n :: Integer = (toInteger l) + 1
        w = 1 / (fromInteger n - 1 + alpha)
        ws = replicate l w ++ [alpha]
        d = discId ws
    in do
      c <- d 
      if c == n - 1
      then g0
      else return $ prev !! (fromInteger c)

urn :: forall a. Real -> SampT a -> SampT (LList a)
urn alpha g0 =
    let f :: ((a, [a]) -> SampT a) = (\x -> return $ fst x)
        g (_, acc) = do
          x <- urn' alpha g0 acc
          return $ (x, acc ++ [x])
    in do
      x0 <- g0
      xs <- LL.map f (LL.iterate g (return (x0, [])))
      LL.tail xs

      
-- Stick breaking construction

mdiscId' :: Nat -> Real -> LList Real -> SampT Nat
mdiscId' idx acc (LL.Cons w mws) = do
  u <- stdUniform
  if u < acc
  then return idx
  else do
    ws <- mws
    mdiscId' (idx + 1) (acc + w) ws
mdiscId' idx _ LL.Nil = return idx
             
mdiscId :: LList Real -> SampT Nat
mdiscId (LL.Cons w mws) = do
  ws <- mws
  mdiscId' 0 w ws
mdiscId LL.Nil = error "Empty MList in mdisc_id"
                  
sticks :: Real -> SampT a -> SampT (LList a)
sticks alpha g0 = do
  xs <- LL.repeat g0
  ws <- weights 1
  LL.repeat (mdiscId ws >>= (\c -> xs LL.!!! fromInteger c))
    where
      weights :: Real -> SampT (LList Real)
      weights left =
          do
            v <- beta 1 alpha
            return $ (v * left) LL.@: (weights (left * (1 - v)))


{-----------------------------------------------------------
 - Singular and other weird distributions
 ------------------------------------------------------------}

cantor :: SampT Real
cantor = mkSampT (\u -> mkApprox (\n -> go u 0 1 0 n))
    where go u (left :: Rat) (right :: Rat) n m =
              let pow = 3 ^^ (-n) in
              if (n <= m)
              then (if u n
                    then go u left (left + pow) (n + 1) m
                    else go u (right - pow) right (n + 1) m)
              else right - (1 / 2) * pow


                
weirdDist :: (Real -> Real) -> Real -> SampT Real
weirdDist f x = do
  n <- geometricK (1 / 2)
  m <- lift $ g n
  if m > n * n
  then uniform (-2) (-1)
  else uniform 1 2
    where
      g :: Nat -> IO Nat
      g = modulus f x

impureDist :: (Real -> Bool) -> SampT Real
impureDist f = do
  u <- uniform 0 1
  b :: Either E.ErrorCall Bool <- lift $ E.try (E.evaluate (f u))
  case b of
    Left _ -> uniform (-2) (-1)
    Right True -> uniform (-1/2) (1/2)
    Right False -> uniform 1 2


{-----------------------------------------------------------
 - Limited distributions on function spaces
 ------------------------------------------------------------}
         
distFun :: CMetrizable (a -> b) => [a -> b] -> SampT (a -> b)
distFun fs = do
  idx <- geometric (1/2)
  return $ (fs !! (fromInteger idx))


{-----------------------------------------------------------
 - Divergence
 ------------------------------------------------------------}

botSamp :: (CMetrizable a) => SampT (Approx a)
botSamp = botSamp

botSampBot :: (CMetrizable a) => SampT (Approx a)
botSampBot = mkSampT (\u -> bot)
    where bot = bot

alwaysDiv :: SampT Real
alwaysDiv = do
  _ <- botSamp :: SampT Real
  stdUniform

neverDiv :: SampT Real
neverDiv = do
  _ <- botSampBot :: SampT Real
  stdUniform


{-----------------------------------------------------------
 - Commutativity
 ------------------------------------------------------------}

myNormal  :: SampT Real
myNormal = do
  x <- normal (-1) 1
  y <- normal (1) 1
  return (x + y)

myNormal' :: SampT Real
myNormal' = do
  y <- normal (1) 1
  x <- normal (-1) 1  
  return (x + y)


{-----------------------------------------------------------
 - Conditioning
 ------------------------------------------------------------}

obs :: SampT a -> (a -> Bool) -> SampT a
obs dist p = do
  x <- dist
  if p x then return x else obs dist p

     
{- Example of paradox
 - X ~ N(0, 1)
 - Y ~ N(0, 1)
 - what is P(Y | Y = X) ? -}
     
dist :: SampT (Real, Real)
dist = do
  x <- normal 0 1
  y <- normal 0 1
  return (x, y)

{- Diverging measure-0 observations -}
         
-- P(Y | Z_1 = 0) where Z_1 = Y - X
pred1 :: (Real, Real) -> Bool
pred1 (x,y) = abs (y - x) == 0

-- P(Y | Z_2 = 0) where Z_2 = Y / X
pred2 :: (Real, Real) -> Bool
pred2 (x,y) = abs (1 - y / x) == 0

-- P(Y | Z_3 = 1) where Z_3 = 1_{Y = X}
pred3 :: (Real, Real) -> Bool
pred3 (x,y) = x == y

              
{- Relaxation of measure-0 observations
 - We have a sequence (eps_n)_{n \in Nat} -> 0
 - The true conditional distribution is then 
 -   P(Y | Z_{eps_n}) -> P(Y | Z) ? in distribution
 - We need to check that it converges in distribution to the correct distribution
 -}           
              
-- P(Y | Z_1 \in (0, eps))
pred1' :: Rat -> (Real, Real) -> Bool
pred1' eps (x,y) = abs (y - x) < cr eps

-- P(Y | Z_2 \in (0, eps))
pred2' :: Rat -> (Real, Real) -> Bool
pred2' eps (x,y) = abs (1 - y / x) < cr eps

-- P(Y | Z_3 \in (0, eps))
pred3' :: Rat -> (Real, Real) -> Bool
pred3' eps (x,y) = abs (y - x) < cr eps
                   

z1 :: SampT Real
z1 = do
  (_, y) <- obs dist (pred1' (1 % 8))
  return y
                   
z2 :: SampT Real
z2 = do
  (_, y) <- obs dist (pred2' (1 % 8))
  return y


{-----------------------------------------------------------
 - Non-computable conditional distributions
 ------------------------------------------------------------}

tmHaltsWithinK :: Nat -> Nat -> Integer
tmHaltsWithinK = error "Insert your favorite non-halting problem here"

nonComp :: SampT (Nat, Real)
nonComp = do
  n <- geometric (1/2)
  c <- bernoulli (1/3)
  u <- uniform 0 1
  v <- uniform 0 1
  x <- return (mkApprox (\k -> select u v c k (tmHaltsWithinK n k)))
  return (n, x)
    where select u v c k m
            | m > k = nthApprox v k
            | m == k = if c then 1 else 0
            | m < k = nthApprox u (k - m - 1)  


{-----------------------------------------------------------
 - Some testing
 ------------------------------------------------------------}
         
sampDiscDist :: SampT Nat -> [Char] -> Int -> IO ()
sampDiscDist d file n = do
  h <- IO.openFile file IO.WriteMode
  replicateM_ n $ f h
  IO.hClose h
    where f h = do
            _ <- R.newStdGen
            (x :: Nat, _) <- runStateT d (\_ -> True)
            IO.hPrint h $ x

sampContDist :: SampT Real -> [Char] -> Int -> Bool -> Nat -> IO ()
sampContDist d file n float prec = do
  h <- IO.openFile file IO.WriteMode
  replicateM_ n $ f h
  IO.hClose h
    where f h = do
            _ <- R.newStdGen
            (x :: Real, _) <- runStateT d (\_ -> True)
            if float
            then IO.hPrint h $ toFloat $ nthApprox x prec
            else IO.hPrint h $ nthApprox x prec
          
sampContDistLs :: SampT (LList Real) -> [Char] -> Int -> IO ()
sampContDistLs d file n = do
  h <- IO.openFile file IO.WriteMode
  replicateM_ n $ f h
  IO.hClose h
    where f h = do
            _ <- R.newStdGen
            (xs :: [Real], _) <- runStateT tmp (\_ -> True)
            IO.hPrint h $ show xs 
          tmp :: SampT [Real] = do
            d' <- d
            LL.takeMList 3 d'
