{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Exception
import Control.Monad
import qualified Control.Monad.Random as R
import Control.Monad.Trans.State
import Prelude hiding (Real)
import System.Environment
import System.IO.Unsafe

import Prelim

import ApproxLib
import RealLib
import CantorLib
import ProdLib
import CompDistLib
import CondLib

import Examples
import Synthetic

       
iter :: Int
iter = 1

b1 :: [Bool]
b1 = [False | _ <- [0..10]] ++ [True] ++ [False | _ <- [0..]]

b2 :: [Bool]
b2 = [False | _ <- [0..]] 

search :: [Bool] -> [Bool] -> Bool
search x y = go 0
    where go n = any (\x -> x) (take n x) || any (\x -> x) (take n y)

bot :: a
bot = bot
       
two :: Real
two = mkApprox (\n -> 2 - (1/2^n))

three :: Real      
three = mkApprox (\n -> 3 + (1/2^n))

twenty :: Real
twenty = mkApprox (\n -> 20 - (1/2^n))
           
main :: IO ()
main = do
  args <- getArgs
  case args !! 0 of
    "0" -> do
         -- Test standard distributions
         sampContDist stdUniform "data/uniform.txt" iter True 8
         sampContDist (normal 0 1) "data/normal.txt" iter True 8
         sampContDist (gamma 0.5 0.5) "data/gamma.txt" iter True 8
         sampContDist (beta 0.5 0.5) "data/beta.txt" iter True 8
         sampContDist (expon 2.0) "data/expon.txt" iter True 8
    "1" -> do      
         -- Test paradox distributions
         sampContDist z1 "data/paradox1.txt" iter True 8
         sampContDist z2 "data/paradox2.txt" iter True 8
    "2" -> do
         -- Test stick breaking process
         sampContDistLs (sticks 0.1 (uniform 0 1)) "data/sticks.txt" iter
    "3" -> do
         -- Test urn representation
         sampContDistLs (urn 0.1 (uniform 0 1)) "data/urn.txt" iter
    "4" ->
        let b = do
              e :: Either ErrorCall Rat <- try $ evaluate (nthApprox (trunc (cr 1) 4) 2)
              case e of
                Left _ -> return True
                Right _ -> return False
        in do
          print $ unsafePerformIO b
    "7" -> do
         sampDiscDist (geometric (1/2)) "data/geo.txt" iter
    "8" -> do
         _ <- R.newStdGen
         (x :: Real, _) <- runStateT neverDiv (\_ -> True)
         print x
         -- sampDiscDist (alwaysDiv) "data/always-div.txt" iter

    "5" -> do
         b1 <- synJoin T bot
         b2 <- synBigJoin [bot, bot, bot, bot, bot, T, bot]
         print b1
         print b2

    "6" ->
        let xs = [ unsafePerformIO $ modulus (\x -> x * two) three n | n <- [0..]  ]
            xs' = take 12 xs
            xs'' = take 4 xs ++ [error "HELLO"] ++ xs
            ys = [ unsafePerformIO $ modulus (\x -> x * three) twenty n | n <- [0..]  ]
        in do
          print (xs !! 0)
          print (xs !! 1)
          print (xs !! 2)
          print (xs !! 3)
          print (xs !! 4)
          print (xs !! 10)
          print (xs' !! 0)
          print (xs' !! 1)
          print (xs' !! 2)
          print (xs' !! 3)
          print (xs' !! 4)
          print (xs' !! 10)
          print (xs'' !! 0)
          print (xs'' !! 1)
          print (xs'' !! 2)
          print (xs'' !! 3)
          print (xs'' !! 4)
          print (xs'' !! 10)
          print (ys !! 0)
          print (ys !! 1)
          print (ys !! 2)
          print (ys !! 3)
          print (ys !! 4)
          print (ys !! 10)
    _ -> print "SIGH*"

