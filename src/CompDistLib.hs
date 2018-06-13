{-# LANGUAGE ScopedTypeVariables #-}

module CompDistLib ( RandBits, randbitsToReal, Samp(..), mkSamp
                   , SampM, SampT, idealSamp, mkSampT, mkSampM
                   , LRat, Prob, iidCoinFlips
                   , integrate, integrateBndDom, integrateDom, integrateBnd) where

import Control.Monad.Trans.Class
import Control.Monad.Random
import Control.Monad.Trans.State
import Prelude hiding (Real)

import Prelim

import ApproxLib
import CmsLib
import RealLib


{-------------------------------------------------------------------------------
 - [Note]
 -
 - Contains functionality for implementing computable distributions as samplers.
 -
 -------------------------------------------------------------------------------}


{-----------------------------------------------------------
 - Random bits
 ------------------------------------------------------------}
       
type RandBits = Nat -> Bool

randbitsToReal :: RandBits -> Real
randbitsToReal u = mkApprox (\n -> sum [b2i (u m) | m <- [0..n-1]])
    where b2i False = 0
          b2i True = 1


{-----------------------------------------------------------
 - Sampling View
 ------------------------------------------------------------}

newtype Samp a = Samp { getSamp :: RandBits -> a }

mkSamp :: (CMetrizable a) => (RandBits -> Approx a) -> Samp (Approx a)
mkSamp = Samp

instance Functor Samp where
  fmap f u = Samp (\n -> f (getSamp u n))

instance Applicative Samp where
  pure x = Samp (const x)
  (<*>) f s = Samp (\u -> (getSamp f) u (getSamp s u))

instance Monad Samp where
  return x = Samp (const x)
  (>>=) s f = Samp ((uncurry (getSamp . f)) . (pair (getSamp s . fst) snd) . split)
    where pair f g = \x -> (f x, g x)
          split = pair even odd
          even u = \n -> u (2 * n)
          odd u = \n -> u (2 * n + 1)


{-----------------------------------------------------------
 - Get "real random bits"
 ------------------------------------------------------------}

type SampM = State RandBits
type SampT = StateT RandBits IO

idealSamp :: (RandBits -> a) -> SampT a
idealSamp f = StateT (\u -> let (u1, u2) = split u
                            in
                              return (f u1, u2))
    where
      split u = (\n -> u (2 * n), \n -> u (2 * n + 1))

mkSampT :: (RandBits -> a) -> SampT a
mkSampT f = do
  g <- lift newStdGen
  bs :: [Bool] <- return (randoms g)
  return $ f (\n -> bs !! (fromIntegral n))
                
mkSampM :: (RandBits -> a) -> SampM a
mkSampM f = StateT (\u -> let (u1, u2) = split u in
                           return (f u1, u2))
    where
      split u = (\n -> u (2 * n), \n -> u (2 * n + 1))


{-----------------------------------------------------------
 - Computable Distribution View
 ------------------------------------------------------------}

type LRat = [Rat]
type Prob a = [Ball a Rat] -> LRat

iidCoinFlips :: Prob [Bool]
iidCoinFlips bs = map fst (scanl g (0, []) bs)
    where ignore b prefixes = any (\b' -> b' == b) prefixes
          g :: (Rat, [[Bool]]) -> Ball [Bool] Rat -> (Rat, [[Bool]])
          g (prob, pre) b =
              if ignore (ctr b) pre
              then (prob, pre)
              else (prob + (1/2^(length (ctr b))), (ctr b) : pre)


{-----------------------------------------------------------
 - Integration
 ------------------------------------------------------------}

integrateDom :: (CMetrizable a) => Prob a -> (Approx a -> Real) -> [Ball a Rat] -> LRat
integrateDom mu f dom =
    -- Compute increasing
    --   { mu(f^-1(0,1) \cap dom)
    --   , 1/2mu(f^-1(0,1/2) \cap dom) + ... + 1/2mu(f^-1(3/2,2) \cap dom) ... -}
    let intervals = [ [ Ball (m/2^n) (1/2^n) | m <- [1..2^(2 * n) - 1] ]
                          | n <- [1..] ]
        g b = map (\p -> p * 2 * rad b) (mu (cmsInter (preimage f [b]) dom))
        res = diagonal (map (\bs -> diagonal (map g bs)) intervals)
    in
      increasing res

integrate :: (CMetrizable a) => Prob a -> (Approx a -> Real) -> LRat
integrate mu f = integrateDom mu f enumBalls

integrateBndDom :: (CMetrizable a) => Prob a -> (Approx a -> Real) -> Rat -> [Ball a Rat] -> Real
integrateBndDom mu f bnd dom =
    let lower = map (\p -> p - bnd) (integrateDom mu (\x -> f x + fromRational bnd) dom)
        upper = map (\p -> p - bnd) (integrateDom mu (\x -> fromRational bnd - f x) dom)
    in
      luToCauchy (zip lower upper)
                 
integrateBnd :: (CMetrizable a) => Prob a -> (Approx a -> Real) -> Rat -> Real
integrateBnd mu f bnd = integrateBndDom mu f bnd enumBalls
