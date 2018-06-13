{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}

module CmsLib where

import Prelude hiding (Real, (<$))
import System.IO.Unsafe

import Prelim

import ApproxLib
import RealLib

-- Ball in a computable metric space
data Ball a b = Ball { ctr :: a   -- Ball center
                     , rad :: b   -- Ball radius
                     } deriving Show

enumBalls :: (CMetrizable a) => [Ball a Rat]
enumBalls = diagonal [ [ Ball s q | q <- enumRatPos ] | s <- enum ]


{-----------------------------------------------------------
 - Operations on computable metric spaces
 ------------------------------------------------------------}
            
class CmsRad a where
    toRad :: a -> Real

instance CmsRad Rat where
    toRad x = fromRational x

instance CmsRad Real where
    toRad x = x
              
-- bs must be finite list
inCmsPrv :: (CMetrizable a, CmsRad c) => Ball a c -> [Ball a c] -> [Bool]
inCmsPrv b bs = 
    let balls = map (\b' -> (metric (ctr b) (ctr b') + toRad (rad b'))
                            <$ toRad (rad b)) bs
    in
      searchOuter balls

-- bs must be finite list
notInCmsPrv :: (CMetrizable a, CmsRad c) => Ball a c -> [Ball a c] -> [Bool]
notInCmsPrv b bs =
    let balls = map (\b' -> metric (ctr b) (ctr b') >$
                            (toRad (rad b) + toRad (rad b'))) bs
    in
      searchOuter balls

-- bs must be finite list
inOrNotCmsPrv :: (CMetrizable a, CmsRad c) => Ball a c -> [Ball a c] -> [Three]
inOrNotCmsPrv b bs =
    let prv_in = inCmsPrv b bs
        prv_notin = notInCmsPrv b bs
    in
      map (\(x, y) -> conv x y) (zip prv_in prv_notin)
    where conv True _ = In
          conv _ True = Out
          conv _ _ = Unknown

cmsInter :: (CMetrizable a) => [Ball a Rat] -> [Ball a Rat] -> [Ball a Rat]
cmsInter u1 u2 =
    let contained = diagonal [ [ zip (inCmsPrv b (take n u1 ++ take n u2))
                                 (repeat b) | n <- [1..] ]
                               | b <- enumBalls ]
    in
      map snd (filter (\(cond, _) -> cond) (diagonal contained))
                     
-- bs must be finite list
cmsIntComp :: (CMetrizable a) => [Ball a Rat] -> [Ball a Rat]
cmsIntComp bs =
    let -- For fixed ball b, test if provably not in bs
        f = \b -> zip (notInCmsPrv b bs) (repeat b)
        int_comp = filter (\(cond, _) -> cond) (diagonal (map f enumBalls))
    in
      map snd $ int_comp


{- WARNING: uses unsafePerformIO
 - Is safe because modulus is referentially transparent -}
preimage :: forall a b. (CMetrizable a, CMetrizable b) => (Approx a -> Approx b) -> [Ball b Rat] -> [Ball a Rat]
preimage f bs = 
  let -- modulus of f evaluated at every ideal point
      -- modulus is referentially transparent
      mods :: [(a, b, Nat, Nat)] =
              diagonal [ [ let x = mkApprox (\_ -> s)
                               m = unsafePerformIO (modulus f x n)
                           in
                             (s, nthApprox (f x) m, n, m) 
                                 | s <- enum ] | n <- [0..] ]
      contained =
          diagonal $ map (\(x, fx, n, m) ->
                              zip (inCmsPrv (Ball fx (1/2^m)) bs)
                                      (repeat (Ball x (1/2^n)))) mods
  in
    map snd (filter (\(cond, _) -> cond) contained)


-- Translate open cover to finite open cover
realFinOpenCover :: Ball Rat Rat -> [Ball Rat Rat] -> [Ball Rat Rat]
realFinOpenCover compact cover =
    let l = ctr compact - rad compact
        r = ctr compact + rad compact
    in
      go l r 0 cover
    where conn bs =
              let l = ctr (head bs) - rad (head bs)
                  r = foldl (\r b ->
                                 if ctr b - rad b < r
                                 then ctr b + rad b
                                 else r) (ctr (head bs) + rad (head bs)) (tail bs)
              in
                (l, r)
          go l r n cover =
              let (l', r') = conn (take n cover)                  
              in
                if l' <= l && r' >= r then (take n cover) else go l r (n + 1) cover

    
ballToCauchy :: [Ball Rat Rat] -> Real
ballToCauchy bs = luToCauchy (map (\b -> (ctr b - rad b, ctr b + rad b)) bs)
