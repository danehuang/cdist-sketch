{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}

module CondLib where

import Prelude hiding (Real, (<$))
import Control.Monad.Trans.State

import Prelim

import ApproxLib
import CantorLib
import CompDistLib
import CompDistAdLib
import CmsLib
import ProdLib
import RealLib


{-------------------------------------------------------------------------------
 - [Note]
 -
 - Contains functionality for implementing conditioning.
 -
 -------------------------------------------------------------------------------}



{-----------------------------------------------------------
 - Positive probability events
 ------------------------------------------------------------}

newtype ProbEvent a =
    ProbEvent { getEvent :: [Ball a Real] {- finite list -} }

probEvent :: (CMetrizable a) => [Ball a Real] -> ProbEvent a
probEvent b = ProbEvent b


{-----------------------------------------------------------
 - Bounded and computable density
 ------------------------------------------------------------}

newtype BndDens a b =
    BndDens { getBndDens :: (Approx a -> Approx b -> Real, Rat) }

bndDens :: (CMetrizable a, CMetrizable b) =>
           (Approx a -> Approx b -> Real, Rat) -> BndDens a b
bndDens f = BndDens f
                
         
{-----------------------------------------------------------
 - Samplable iff computable
 ------------------------------------------------------------}
                 
binExpandEnc :: (CMetrizable a) => Prob a -> RandBits -> Approx a
binExpandEnc mu u =
    -- Find a.d. balls that u is contained in and return centers
    -- Should wait for fast Cauchy sequence
    let bs = adBasis mu
        inball = filter (\x -> snd x) [ (n, u n) | n <- [0..] ]
    in
      mkApprox (\n -> ctr (bs !! fromInteger (fst (inball !! fromInteger n))))

binExpandDec :: (CMetrizable a) => Prob a -> Approx a -> RandBits
binExpandDec mu x n =
    let b = Ball (nthApprox x n) (1 / 2^n)
        ad = adBasis mu
    in
      collapse $ inOrNotCmsPrv b [ad !! fromInteger n]
                                       
itsCantor :: RandBits -> ProbAD [Bool] -> RandBits
itsCantor u mu_d n = (iter (itsCantor' u mu_d) [] 0) !! fromInteger n
    where
      iter f a0 m | m <= 0 = f a0
      iter f a0 m | m < n = iter f (f a0) (m - 1)
      
      itsCantor' :: RandBits -> ProbAD [Bool] -> [Bool] -> [Bool]
      itsCantor' u mu_d acc =
        if mu_d [Ball acc (2^(length acc))] < randbitsToReal u
        then acc ++ [True]
        else acc ++ [False]
                                              
adIidCoinFlips :: ProbAD [Bool]
adIidCoinFlips = idealValToAd iidCoinFlips


-- | Convert a computable distribution into a samplable distribution                 
compToSamp :: (CMetrizable a) => Prob a -> SampM (Approx a)
compToSamp mu = mkSampM (\u -> binExpandEnc mu (itsCantor u adIidCoinFlips))

-- | Convert a samplable distribution to a computable distribution
sampToComp :: (CMetrizable a) => SampM (Approx a) -> Prob a
sampToComp dist bs = 
    iidCoinFlips (preimage (\u -> fst $ runState dist (aToRb u)) bs)
   

{-----------------------------------------------------------
 - Observation primitives
 ------------------------------------------------------------}

-- | Observe positive probability event
obsPos :: (CMetrizable a) => SampM (Approx a) -> ProbEvent a -> SampM (Approx a)
obsPos dist cond =
    let mu = sampToComp dist
        mu_ad = idealValToAd mu
        nu = \bs -> mu_ad bs / mu_ad (getEvent cond)
    in
      compToSamp (adValToIdeal mu nu)

-- | Observed bounded and computable density
obsDens ::
    forall u v y. (CMetrizable u, CMetrizable v, CMetrizable y) =>
    SampM (Approx (u, v)) -> BndDens u y -> Approx y -> SampM (Approx (u, v))
obsDens dist (BndDens (dens, bnd)) d =
    let f :: Approx (u, v) -> Real = \x -> dens (approxFst x) d
        mu :: Prob (u, v) = sampToComp dist
        nu :: Prob (u, v) = \bs ->
              let num  = integrateBndDom mu f bnd bs
                  denom = integrateBnd mu f bnd 
              in
                map fst (cauchyToLu (num / denom))
    in
      compToSamp nu        


{-----------------------------------------------------------
 - Unpaired version of conditioning
 ------------------------------------------------------------}

pairApprox :: SampM (Approx u, Approx v) -> SampM (Approx (u, v))
pairApprox dist = do
  (x, y) <- dist
  return (mkApprox (\n -> (nthApprox x n, nthApprox y n)))

unpairApprox :: SampM (Approx (u, v)) -> SampM (Approx u, Approx v)
unpairApprox dist = do
  x <- dist
  return (mkApprox (\n -> fst (nthApprox x n)), mkApprox (\n -> snd (nthApprox x n)))
        
obsDens2 ::
    forall u v y. (CMetrizable u, CMetrizable v, CMetrizable y) =>
    SampM (Approx u, Approx v) -> BndDens u y -> Approx y -> SampM (Approx u, Approx v)
obsDens2 dist (BndDens (dens, bnd)) d =
    let f :: Approx (u, v) -> Real = \x -> dens (approxFst x) d
        mu :: Prob (u, v) = sampToComp $ pairApprox dist
        nu :: Prob (u, v) = \bs ->
              let num  = integrateBndDom mu f bnd bs
                  denom = integrateBnd mu f bnd 
              in
                map fst (cauchyToLu (num / denom))
    in
      unpairApprox $ compToSamp nu        


{-----------------------------------------------------------
 - Approximate Bayesian Computation
 ------------------------------------------------------------}

approxBayesComp :: (b -> b -> Real) -> Real -> (a -> SampT b) -> (SampT a) -> b -> (SampT a)
approxBayesComp dist eps like prior ys = do
  theta <- prior
  ys' <- like theta
  if dist ys ys' < eps
  then return theta
  else approxBayesComp dist eps like prior ys
