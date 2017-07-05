----------------------------------------------------------------------------
-- |
-- Module      :  EitherVsCodensityVsCPSEither
-- Copyright   :  (c) Sergey Vinokurov 2015
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  serg.foo@gmail.com
-- Created     :  Thursday, 10 December 2015
-- Stability   :
-- Portability :
--
--
----------------------------------------------------------------------------

{-# LANGUAGE DeriveFoldable        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE UndecidableInstances  #-}

module Main (main) where

import Control.DeepSeq
import Control.Monad.Except
import Control.Monad.State
-- import Data.Functor.Identity

import Criterion.Main

import Utils


newtype EitherChurch e a = EitherChurch { runEitherChurch :: forall r. (e -> r) -> (a -> r) -> r }
  -- deriving (Functor)

instance Functor (EitherChurch e) where
  fmap f (EitherChurch g) = EitherChurch $ \ek sk -> g ek (sk . f)

instance Applicative (EitherChurch e) where
  pure x = EitherChurch $ \_ sk -> sk x
  EitherChurch f <*> EitherChurch g = EitherChurch $
    \ek sk -> f ek (\h -> g ek (\x -> sk $ h x))

instance Monad (EitherChurch e) where
  return = pure
  EitherChurch f >>= m = EitherChurch $ \ek sk ->
    f ek (\x -> runEitherChurch (m x) ek sk)

instance MonadError e (EitherChurch e) where
  throwError e = EitherChurch $ \ek _ -> ek e
  catchError (EitherChurch f) handler = EitherChurch $ \ek sk ->
    f (\e -> runEitherChurch (handler e) ek sk) sk


newtype EitherChurchT e m a = EitherChurchT { runEitherChurchT :: forall r. (e -> m r) -> (a -> m r) -> m r }
  -- deriving (Functor)

instance Functor (EitherChurchT e m) where
  fmap f (EitherChurchT g) = EitherChurchT $ \ek sk -> g ek (sk . f)

instance Applicative (EitherChurchT e m) where
  pure x = EitherChurchT $ \_ sk -> sk x
  EitherChurchT f <*> EitherChurchT g = EitherChurchT $
    \ek sk -> f ek (\h -> g ek (\x -> sk $ h x))

instance Monad (EitherChurchT e m) where
  return = pure
  EitherChurchT f >>= m = EitherChurchT $ \ek sk ->
    f ek (\x -> runEitherChurchT (m x) ek sk)

instance MonadError e (EitherChurchT e m) where
  throwError e = EitherChurchT $ \ek _ -> ek e
  catchError (EitherChurchT f) handler = EitherChurchT $ \ek sk ->
    f (\e -> runEitherChurchT (handler e) ek sk) sk

instance (MonadState s m) => MonadState s (EitherChurchT e m) where
  get = EitherChurchT $ \_ sk -> get >>= sk
  put x = EitherChurchT $ \_ sk -> put x >>= sk


newtype Codensity m a = Codensity { runCodensity :: forall r. (a -> m r) -> m r }
  -- deriving (Functor)

instance Functor (Codensity m) where
  fmap f (Codensity g) = Codensity $ \k -> g (k . f)

instance Applicative (Codensity m) where
  pure x = Codensity $ \k -> k x
  Codensity f <*> Codensity g = Codensity $ \k -> f (\h -> g (\x -> k $ h x))

instance Monad (Codensity m) where
  return = pure
  Codensity f >>= m = Codensity $ \k -> f (\x -> runCodensity (m x) k)

instance MonadError e (Codensity (Either e)) where
  throwError e = Codensity $ \_ -> Left e
  catchError (Codensity f) handler = Codensity $ \k ->
    case f k of
      Left e  -> runCodensity (handler e) k
      Right x -> Right x


newtype EitherK e a = EitherK { runEitherK :: forall r. (a -> Either e r) -> Either e r }

instance Functor (EitherK e) where
  fmap f (EitherK g) = EitherK $ \k -> g (k . f)

instance Applicative (EitherK e) where
  pure x = EitherK $ \k -> k x
  EitherK f <*> EitherK x = EitherK $ \k -> f $ \f' -> x $ \x' -> k $ f' x'

instance Monad (EitherK e) where
  return = pure
  EitherK f >>= m = EitherK $ \k -> f $ \x -> runEitherK (m x) k

instance MonadError e (EitherK e) where
  throwError e = EitherK $ \_ -> Left e
  catchError (EitherK f) handler = EitherK $ \k ->
    case f k of
      Left e  -> runEitherK (handler e) k
      Right x -> Right x

data Tree a =
    Leaf
  | Node a (Tree a) (Tree a)
  deriving (Functor, Foldable, Traversable)

instance (NFData a) => NFData (Tree a) where
  rnf Leaf         = ()
  rnf (Node x l r) = rnf x `seq` rnf l `seq` rnf r

-- Returns fibonacci-shaped tree and maximum index plus one - returns number
-- of allocated nodes in a tree.
fibTree :: Int -> (Tree Int, Int)
fibTree height = runState (mkTree height) 0
  where
    mkTree :: Int -> State Int (Tree Int)
    mkTree 0 = return Leaf
    mkTree 1 = return Leaf
    mkTree n = Node <$> incr <*> mkTree n' <*> mkTree n''
      where
        n'  = n - 1
        n'' = n' - 1
    incr :: State Int Int
    incr = do
      n <- get
      put $! n + 1
      return n

type Result = Tree Int

{-# SPECIALIZE transform :: Tree Int -> Int -> StateT Int (Either String) Result             #-}
{-# SPECIALIZE transform :: Tree Int -> Int -> ExceptT String (State Int) Result             #-}
{-# SPECIALIZE transform :: Tree Int -> Int -> StateT Int (EitherChurch String) Result       #-}
{-# SPECIALIZE transform :: Tree Int -> Int -> EitherChurchT String (State Int) Result       #-}
{-# SPECIALIZE transform :: Tree Int -> Int -> StateT Int (Codensity (Either String)) Result #-}
{-# SPECIALIZE transform :: Tree Int -> Int -> StateT Int (EitherK String) Result            #-}
transform :: forall m. (MonadError String m, MonadState Int m) => Tree Int -> Int -> m Result
transform tree mx =
  -- traverse f tree
  mapM f tree
  where
    f :: Int -> m Int
    f m
      | m <= mx   = do
        k <- get
        let newValue = m + k + 1
        put $! newValue
        return $ newValue
      | otherwise = throwError $ "Value " ++ show m ++ " greater than " ++ show mx

transformEither :: Tree Int -> Int -> StateT Int (Either String) Result
transformEither = transform

runTransformEither :: Tree Int -> Int -> Either String Result
runTransformEither tree n =
  evalStateT (transformEither tree n) 0

transformExceptT :: Tree Int -> Int -> ExceptT String (State Int) Result
transformExceptT = transform

runTransformExceptT :: Tree Int -> Int -> Either String Result
runTransformExceptT tree n =
  evalState (runExceptT (transformExceptT tree n)) 0

transformEitherChurch :: Tree Int -> Int -> StateT Int (EitherChurch String) Result
transformEitherChurch = transform

runTransformEitherChurch :: Tree Int -> Int -> Either String Result
runTransformEitherChurch tree n =
  runEitherChurch (evalStateT (transformEitherChurch tree n) 0) Left Right

transformEitherChurchT :: Tree Int -> Int -> EitherChurchT String (State Int) Result
transformEitherChurchT = transform

runTransformEitherChurchT :: Tree Int -> Int -> Either String Result
runTransformEitherChurchT tree n =
  evalState (runEitherChurchT (transformEitherChurchT tree n) (return . Left) (return . Right)) 0

transformCodensity :: Tree Int -> Int -> StateT Int (Codensity (Either String)) Result
transformCodensity = transform

runTransformCodensity :: Tree Int -> Int -> Either String Result
runTransformCodensity tree n =
  runCodensity (evalStateT (transformCodensity tree n) 0) Right

transformEitherK :: Tree Int -> Int -> StateT Int (EitherK String) Result
transformEitherK = transform

runTransformEitherK :: Tree Int -> Int -> Either String Result
runTransformEitherK tree n =
  runEitherK (evalStateT (transformEitherK tree n) 0) Right


main :: IO ()
main = do
  config <- makeConfig "either-vs-cps.html"
  putStrLn $ "treeSize = " ++ show treeSize
  -- putStrLn "Results:"
  -- let report msg x = putStrLn $ msg ++ ": " ++ show x
  -- report "runTransformEither"        $ runTransformEither tree k
  -- report "runTransformExceptT"       $ runTransformExceptT tree k
  -- report "runTransformEitherChurch"  $ runTransformEitherChurch tree k
  -- report "runTransformEitherChurchT" $ runTransformEitherChurchT tree k
  -- report "runTransformCodensity"     $ runTransformCodensity tree k
  defaultMainWith config
    [ bgroup "No cutoff"
        [ bench "Either"        $ nf (runTransformEither tree) noCutoff
        , bench "ExceptT"       $ nf (runTransformExceptT tree) noCutoff
        , bench "EitherChurch"  $ nf (runTransformEitherChurch tree) noCutoff
        , bench "EitherChurchT" $ nf (runTransformEitherChurchT tree) noCutoff
        , bench "Codensity"     $ nf (runTransformCodensity tree) noCutoff
        , bench "EitherK"       $ nf (runTransformEitherK tree) noCutoff
        ]
    , bgroup "Cutoff at half"
        [ bench "Either"        $ nf (runTransformEither tree) cutoffHalf
        , bench "ExceptT"       $ nf (runTransformExceptT tree) cutoffHalf
        , bench "EitherChurch"  $ nf (runTransformEitherChurch tree) cutoffHalf
        , bench "EitherChurchT" $ nf (runTransformEitherChurchT tree) cutoffHalf
        , bench "Codensity"     $ nf (runTransformCodensity tree) cutoffHalf
        , bench "EitherK"       $ nf (runTransformEitherK tree) cutoffHalf
        ]
    ]
  where
    n                = 25
    (tree, treeSize) = fibTree n
    noCutoff         = treeSize -- 9 * (treeSize `div` 10)
    cutoffHalf       = 1 * (treeSize `div` 2)

