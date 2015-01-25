{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Control.DeepSeq
import Control.Exception
import Control.Monad
import Data.List

import Criterion.Main

import Utils

addBoxes :: Box -> Box -> Box
addBoxes (Box x) (Box y) = Box $ x + y

zeroBox :: Box
zeroBox = Box 0

data Box = Box Int

instance NFData Box where
  rnf (Box n) = rnf n

addNBoxes :: NBox -> NBox -> NBox
addNBoxes (NBox x) (NBox y) = NBox $ x + y

zeroNBox :: NBox
zeroNBox = NBox 0

newtype NBox = NBox Int deriving (NFData)

main :: IO ()
main = do
  void $ evaluate $ force $ xs
  void $ evaluate $ force $ ys
  void $ evaluate $ force $ zs
  config <- makeConfig "foldl-test.html"
  defaultMainWith config [
      bench "foldr Int" $ whnf (foldr (+) 0) xs
    , bench "foldl Int" $ whnf (foldl (+) 0) xs
    , bench "foldl' Int" $ whnf (foldl' (+) 0) xs
    , bench "foldr data Int" $ whnf (foldr addBoxes zeroBox) ys
    , bench "foldl data Int" $ whnf (foldl addBoxes zeroBox) ys
    , bench "foldl' data Int" $ whnf (foldl' addBoxes zeroBox) ys
    , bench "foldr newtype Int" $ whnf (foldr addNBoxes zeroNBox) zs
    , bench "foldl newtype Int" $ whnf (foldl addNBoxes zeroNBox) zs
    , bench "foldl' newtype Int" $ whnf (foldl' addNBoxes zeroNBox) zs
    ]
  where
    xs :: [Int]
    xs = [1..1000000]
    ys :: [Box]
    ys = map Box xs
    zs :: [NBox]
    zs = map NBox xs
