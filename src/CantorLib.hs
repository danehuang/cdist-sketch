{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CantorLib where

import ApproxLib
import CompDistLib


{-------------------------------------------------------------------------------
 - [Note]
 -
 - Cantor space.
 -
 -------------------------------------------------------------------------------}


type Cantor = Approx [Bool]

instance CMetrizable [Bool] where
    enum = enum
    metric x y = metric x y

rbToA :: RandBits -> Cantor
rbToA u = mkApprox (\n -> [u m | m <- [0..n]])
    
aToRb :: Cantor -> RandBits
aToRb x = \n -> (nthApprox x n) !! (fromInteger n - 1)
