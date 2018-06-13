module ProdLib where

import Prelude hiding (Real)

import ApproxLib
import RealLib


{-------------------------------------------------------------------------------
 - [Note]
 -
 - Contains functionality for approximating elements of products of computable
 - metric spaces.
 -
 -------------------------------------------------------------------------------}


approxFst :: Approx (a, b) -> Approx a
approxFst f = mkApprox (\n -> fst $ (getApprox f) n)

approxSnd :: Approx (a, b) -> Approx b
approxSnd f = mkApprox (\n -> snd $ (getApprox f) n)

instance (CMetrizable a, CMetrizable b) => CMetrizable (a, b) where
    enum = diagonal [ [ (x, y) | y <- enum ] | x <- enum ]
    metric x y = max (metric (fst x) (fst y)) (metric (snd x) (snd y))
        where max :: Real -> Real -> Real
              max x y = mkApprox (\n -> let m = n + 2
                                        in
                                          if getApprox x m < getApprox y m
                                          then getApprox x m
                                          else getApprox y m)
