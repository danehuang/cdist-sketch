{-# LANGUAGE ScopedTypeVariables #-}

module MList where

-- Lazy monad list
data MList m a = Nil | Cons a (m (MList m a))

toMList :: (Monad m) => [a] -> MList m a
toMList [] = Nil
toMList (x:xs) = Cons x $ return $ toMList xs

takeMList :: (Monad m) => Int -> MList m a -> m [a]
takeMList _ Nil = return []
takeMList n (Cons x xs) =
    if n == 0
    then return []
    else do
      xs' <- xs
      xs'' <- takeMList (n - 1) xs'
      return (x : xs'')

head :: (Monad m) => MList m a -> a
head (Cons x _) = x
head Nil = error "Empty MList"

tail :: (Monad m) => MList m a -> m (MList m a)
tail (Cons _ mxs) = mxs
tail Nil = error "Empty MList"

repeat :: (Monad m) => m a -> m (MList m a)
repeat x = do
  x' <- x
  return $ Cons x' (MList.repeat x)

iterate :: (Monad m) => (a -> m a) -> (m a) ->  m (MList m a)
iterate f x = do
  x' <- x
  return $ x' @: MList.iterate f (f x')

map :: (Monad m) => (a -> m b) -> m (MList m a) -> m (MList m b)
map f mxs = do
  xs <- mxs
  case xs of
    Nil -> return Nil
    Cons x mxs' -> do
              x' <- f x
              return (Cons x' (MList.map f mxs'))

-- probably all of these can be lifted?
                     
length :: (Monad m) => MList m a -> m Int
length Nil = return 0
length (Cons _ mxs) = do
  xs <- mxs
  l <- MList.length xs
  return (l + 1)
         
-- Index
(!!!) :: (Monad m) => MList m a -> Int -> m a
Nil !!! _ = error "no more elements"
(Cons x mxs) !!! n
    | n <= 0 = return x
    | n > 0 = mxs >>= (\xs -> xs !!! (n - 1))

-- Cons
(@:) :: (Monad m) => a -> m (MList m a) -> MList m a
(@:) x mxs = Cons x mxs

             
instance Functor m => Functor (MList m) where
    fmap _ Nil = Nil
    fmap f (Cons x xs) = Cons (f x) (fmap (fmap f) xs)
