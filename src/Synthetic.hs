{-# LANGUAGE ScopedTypeVariables #-}

module Synthetic where

import qualified Control.Exception as E
import Control.Monad.Random
import Control.Concurrent

import ApproxLib


{- Source:
 - http://neilmitchell.blogspot.com/2014/06/optimisation-with-continuations.html 
 -}
newOnce :: IO (IO () -> IO ())
newOnce = do
    run <- newMVar True
    return $ \act -> do
        b <- modifyMVar run $ \b -> return (False, b)
        when b act

{- Source:
 - http://neilmitchell.blogspot.com/2014/06/optimisation-with-continuations.html 
 - -}        
parallel :: IO a -> IO a -> IO a
parallel t1 t2 = do
    once <- newOnce
    var <- newEmptyMVar
    forkIO $ t1 >>= once . putMVar var
    forkIO $ t2 >>= once . putMVar var
    readMVar var

        
-- Sierpinski data-type
            
data Sierp = T deriving Show

synMeet :: Sierp -> Sierp -> Sierp
synMeet T T = T 
             
synJoin :: Sierp -> Sierp -> IO Sierp
synJoin x y = parallel (E.evaluate x) (E.evaluate y)

synBigJoin :: [Sierp] -> IO Sierp
synBigJoin (x:xs) = parallel (E.evaluate x) (synBigJoin xs)
synBigJoin [] = synBigJoin []
           
type SOpen a = Approx a -> Sierp
type SOpenM a = Approx a -> IO Sierp
type SClosed a = Approx a -> Sierp
    
synCompO :: SOpen a -> SClosed a
synCompO f = f

synUnion :: SOpen a -> SOpen a -> SOpenM a
synUnion f1 f2 = \x -> synJoin (f1 x) (f2 x)

synInter :: SOpen a -> SOpen a -> SOpen a
synInter f1 f2 = \x -> synMeet (f1 x) (f2 x)

synBigUnion :: [SOpen a] -> SOpenM a
synBigUnion fs = \x -> synBigJoin (map (\f -> f x) fs)

synPreimage :: (Approx a -> Approx b) -> SOpen b -> SOpen a
synPreimage f u = u . f

synMem :: Approx a -> SOpen a -> Sierp
synMem x u = u x
