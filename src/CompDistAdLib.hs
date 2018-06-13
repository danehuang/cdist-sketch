{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}

module CompDistAdLib where

import Prelude hiding (Real, (<$))
import Data.List

import Prelim

import ApproxLib
import CompDistLib
import CmsLib
import RealLib


{-------------------------------------------------------------------------------
 - [Note]
 -
 - Contains functionality for almost-decidable bases used in conditioning.
 -
 -------------------------------------------------------------------------------}


{-----------------------------------------------------------
 - Almost-decidable Basis
 ------------------------------------------------------------}
       
type ProbAD a = [Ball a Real] -> Real
                     

-- bs must be finite list        
adCmsIntComp :: (CMetrizable a) => Prob a -> [Ball a Real] -> [Ball a Real]
adCmsIntComp mu bs =
    let balls = adBasis mu
        -- For fixed ball b, test if provably not in bs
        f = \b -> zip (notInCmsPrv b bs) (repeat b)
        int_comp = filter (\(cond, _) -> cond) (diagonal (map f balls))
    in
      map snd $ int_comp                     

        
adToIdeal :: (CMetrizable a) => [Ball a Real] -> [Ball a Rat]
adToIdeal bs =
    let -- For fixed ball b, test if q < b.rad
        f = \b -> diagonal [ zip (fromRational q <$ rad b)
                                 (repeat (Ball (ctr b)  q)) | q <- enum ]
        -- Test for all balls
        bs' = diagonal (map f bs)
    in
      map snd $ filter (\(cond, _) -> cond) bs'

idealToAd :: (CMetrizable a) => Prob a -> [Ball a Rat] -> [Ball a Real]
idealToAd mu bs =
    let -- For fixed ball b, test if r < b.rad
        f = \b -> diagonal [ zip (r <$ fromRational (rad b))
                                 (repeat (Ball (ctr b) r)) | r <- adRadii mu ]
        -- Test for all balls
        bs' = diagonal (map f bs)
    in
      map snd $ filter (\(cond, _) -> cond) bs'              

                      
-- Hoyrup Lemma 5.1.1
adRadii :: forall a. (CMetrizable a) => Prob a -> [Real]
adRadii mu =
    let bar :: a -> Nat -> [Ball Rat Rat]
        bar s k = diagonal $ diagonal
                  [ [ let t1 = mu (cmsIntComp [Ball s q1]) <$
                               (mu [Ball s q1] +$ (1 / fromInteger k))
                          t2 = mu (cmsIntComp [Ball s q2]) <$
                               (mu [Ball s q2] +$ (1 / fromInteger k))
                          t3 = map (\(c1, c2) -> c1 && c2) (zip t1 t2)
                          t4 = zip t3 (repeat (mkInt q1 q2))
                          t5 = map snd (filter (\(cond, _) -> cond) t4)
                      in
                        t5 | q2 <- enumRatPos, q1 < q2 ] | q1 <- enumRatPos ]
        us = diagonal [ [ bar s k | s <- enum ] | k <- [1..] ]
        vs = diagonal [ [ cmsIntComp [Ball (nthApprox (metric c1 c2) 0) 0]
                          | (c2 :: a) <- enum ] | c1 <- enum ]
        ks :: [Ball Rat Rat] = diagonal [ [ mkInt q1 q2 | q2 <- enumRatPos, q1 /= q2 ] | q1 <- enumRatPos ]
        t1 :: (Int, Ball Rat Rat) -> Ball Rat Rat -> (Int, Ball Rat Rat) = go us vs
        t2 :: [[Ball Rat Rat]] = [ map snd (scanl t1 (0, ks !! n) ks) | n <- [0..] ]
    in
      map (\b -> ballToCauchy b) t2
    where
      (<$) xs ys = map (\(x,y) -> x < y) (zip xs ys)
      (+$) xs q = map (\x -> x + q) xs
      mkInt l r =  let l' = min l r
                       r' = max l r
                   in
                     Ball ((l' + r') / 2) ((r' - l') / 2)
      go :: [[Ball Rat Rat]] -> [[Ball Rat Rat]] -> (Int, Ball Rat Rat) -> Ball Rat Rat -> (Int, Ball Rat Rat)
      go us vs (k, b) a =
          let cover = cmsInter (us !! k) (vs !! k)
              foo = zip (inCmsPrv a (realFinOpenCover b cover))
                    (repeat a)
              foo2 = case find (\(cond, b) -> cond) foo of
                       Just x -> b
                       Nothing -> error "Shouldn't happen"
          in
            (k + 1, foo2)
          
adBasis :: (CMetrizable a) => Prob a -> [Ball a (Approx Rat)]
adBasis mu = diagonal [ [ Ball s r | s <- enum ] | r <- adRadii mu ]

                  
idealValToAd :: (CMetrizable a) => Prob a -> ProbAD a
idealValToAd mu bs =
    let lower = mu (adToIdeal bs)
        upper = mu (adToIdeal (adCmsIntComp mu bs))
    in
      luToCauchy (zip lower upper)
      
adValToIdeal :: (CMetrizable a) => Prob a -> ProbAD a -> Prob a
adValToIdeal mu nu_ad bs =
    let ideal = idealToAd mu bs
        ps = diagonal [ [ max 0 (nthApprox (nu_ad (take n ideal)) m - (1/2^m)) | m <- [0..] ] | n <- [0..] ]
    in
      increasing ps
