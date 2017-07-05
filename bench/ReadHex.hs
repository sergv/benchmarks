----------------------------------------------------------------------------
-- |
-- Module      :  ReadHex
-- Copyright   :  (c) Sergey Vinokurov 2017
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  serg.foo@gmail.com
-- Created     :  27 January 2017
-- Stability   :
-- Portability :
--
--
----------------------------------------------------------------------------

{-# LANGUAGE LambdaCase #-}

module ReadHex (main) where

import Control.DeepSeq
import Control.Exception
import Criterion.Main
import Data.Char
import Data.Word

import Utils

readHexChar :: Char -> Maybe Word8
readHexChar = \case
  '0' -> Just 0x0
  '1' -> Just 0x1
  '2' -> Just 0x2
  '3' -> Just 0x3
  '4' -> Just 0x4
  '5' -> Just 0x5
  '6' -> Just 0x6
  '7' -> Just 0x7
  '8' -> Just 0x8
  '9' -> Just 0x9
  'a' -> Just 0xa
  'b' -> Just 0xb
  'c' -> Just 0xc
  'd' -> Just 0xd
  'e' -> Just 0xe
  'f' -> Just 0xf
  'A' -> Just 0xa
  'B' -> Just 0xb
  'C' -> Just 0xc
  'D' -> Just 0xd
  'E' -> Just 0xe
  'F' -> Just 0xf
  _   -> Nothing

testString :: String
testString = "0123456789abcdefABCDEF"

main :: IO ()
main = do
  config <- makeConfig "read-hex.html"
  _ <- evaluate $ force testString
  defaultMainWith config
    [ bench "Data.Char.digitToInt" $ nf (map digitToInt)  testString
    , bench "readHexChar"          $ nf (map readHexChar) testString
    ]
