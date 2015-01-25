module Main where

import Control.Applicative
import Control.Monad
import Data.List
import Data.Vector (Vector)
import qualified Data.Vector as V

import Data.Random.Distribution.Uniform (stdUniform)
import Data.Random.Sample (sample)
import Data.Random.Source (MonadRandom)
import Data.Random.Source.IO ()
import Data.Random.Source.PureMT ()

import Criterion.Main

import Utils

randomVec :: (Functor m, MonadRandom m) => Int -> m (Vector Double)
randomVec n = V.fromList <$> replicateM n (sample stdUniform)

force :: a -> a
force x = seq x x
{-# INLINE force #-}

countGreaterThan :: (Ord a) => Vector a -> a -> Int
countGreaterThan vec x =
  V.foldr (\y acc -> if y > x then force $ acc + 1 else acc) 0 vec

main :: IO ()
main = do
  v <- randomVec n
  let sortedV  = force $ V.fromList $ sort $ V.toList v
      sortedV' = seq (sortedV == sortedV) sortedV
  config <- makeConfig "pipeline-test.html"
  defaultMainWith config [
      bench "vanilla" $ whnf (countGreaterThan v) 0.5
    , bench "sorted" $ whnf (countGreaterThan sortedV') 0.5
    ]
  where
    n = 10000

