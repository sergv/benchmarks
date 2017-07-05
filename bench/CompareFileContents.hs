----------------------------------------------------------------------------
-- |
-- Module      :  CompareFileContents
-- Copyright   :  (c) Sergey Vinokurov 2017
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  serg.foo@gmail.com
-- Created     :   5 July 2017
-- Stability   :
-- Portability :
--
--
----------------------------------------------------------------------------

{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module CompareFileContents (main) where

import Criterion.Main
import qualified Data.ByteString.Lazy as BSL
import Data.Foldable
import Data.Int
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TLIO
import System.Directory
import System.IO
import System.IO.Temp (withSystemTempFile)

import Utils

filesEqualCabal :: FilePath -> FilePath -> IO Bool
filesEqualCabal f1 f2 = do
  ex1 <- doesFileExist f1
  ex2 <- doesFileExist f2
  if not (ex1 && ex2) then return False else
    withBinaryFile f1 ReadMode $ \h1 ->
      withBinaryFile f2 ReadMode $ \h2 -> do
        c1 <- BSL.hGetContents h1
        c2 <- BSL.hGetContents h2
        return $! c1 == c2

-- filesEqualString :: FilePath -> FilePath -> IO Bool
-- filesEqualString f1 f2 = do
--   ex1 <- doesFileExist f1
--   ex2 <- doesFileExist f2
--   if not (ex1 && ex2) then return False else
--     withBinaryFile f1 ReadMode $ \h1 ->
--       withBinaryFile f2 ReadMode $ \h2 -> do
--         c1 <- hGetContents h1
--         c2 <- hGetContents h2
--         return $! c1 == c2

filesEqualWithSize :: FilePath -> FilePath -> IO Bool
filesEqualWithSize f1 f2 = do
  ex1 <- doesFileExist f1
  ex2 <- doesFileExist f2
  if not (ex1 && ex2) then return False else
    withBinaryFile f1 ReadMode $ \h1 ->
      withBinaryFile f2 ReadMode $ \h2 -> do
        s1 <- hFileSize h1
        s2 <- hFileSize h2
        if s1 /= s2
          then return False
          else do
            c1 <- BSL.hGetContents h1
            c2 <- BSL.hGetContents h2
            return $! c1 == c2

doComparisons :: FilePath -> FilePath -> [Benchmark]
doComparisons x y =
  [ bench "filesEqualCabal" $ whnfIO $ filesEqualCabal x y
  -- , bench "filesEqualString" $ whnfIO $ filesEqualString x y
  , bench "filesEqualWithSize" $ whnfIO $ filesEqualWithSize x y
  ]

data Entry = Entry
  { eName           :: String
  , eSize           :: Int64
  , eEqFile         :: FilePath
  , eEqFileHandle   :: Handle
  , eDiffFile       :: FilePath
  , eDiffFileHandle :: Handle
  }

main :: IO ()
main = do
  config <- makeConfig "compare-file-contents.html"
  withSystemTempFile "file_10Kb_eq" $ \file10KbEq hFile10KbEq ->
    withSystemTempFile "file_10Kb_diff" $ \file10KbDiff hFile10KbDiff ->
      withSystemTempFile "file_100Kb_eq" $ \file100KbEq hFile100KbEq ->
        withSystemTempFile "file_100Kb_diff" $ \file100KbDiff hFile100KbDiff ->
          withSystemTempFile "file_1Mb_eq" $ \file1MbEq hFile1MbEq ->
            withSystemTempFile "file_1Mb_diff" $ \file1MbDiff hFile1MbDiff -> do
              let entries =
                    [ Entry
                        { eName           = "10Kb"
                        , eSize           = 10 * 1024
                        , eEqFile         = file10KbEq
                        , eEqFileHandle   = hFile10KbEq
                        , eDiffFile       = file10KbDiff
                        , eDiffFileHandle = hFile10KbDiff
                        }
                    , Entry
                        { eName           = "100Kb"
                        , eSize           = 100 * 1024
                        , eEqFile         = file100KbEq
                        , eEqFileHandle   = hFile100KbEq
                        , eDiffFile       = file100KbDiff
                        , eDiffFileHandle = hFile100KbDiff
                        }
                    , Entry
                        { eName           = "1Mb"
                        , eSize           = 1024 * 1024
                        , eEqFile         = file1MbEq
                        , eEqFileHandle   = hFile1MbEq
                        , eDiffFile       = file1MbDiff
                        , eDiffFileHandle = hFile1MbDiff
                        }
                    ]
              for_ entries $ \Entry{eSize, eEqFileHandle, eDiffFileHandle} -> do
                TLIO.hPutStr eEqFileHandle   $ TL.replicate eSize "a"
                TLIO.hPutStr eDiffFileHandle $ TL.replicate eSize "a"
                TLIO.hPutStr eDiffFileHandle $ TL.replicate 1 "b"
                hClose eEqFileHandle
                hClose eDiffFileHandle
              defaultMainWith config $ flip map entries $ \Entry{eName, eEqFile, eDiffFile} ->
                bgroup eName
                  [ bgroup "Equal"     $ doComparisons eEqFile eEqFile
                  , bgroup "Different" $ doComparisons eEqFile eDiffFile
                  ]
