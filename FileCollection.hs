----------------------------------------------------------------------------
-- |
-- Module      :  FileCollection
-- Copyright   :  (c) Sergey Vinokurov 2016
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  serg.foo@gmail.com
-- Created     :  Sunday,  3 January 2016
-- Stability   :
-- Portability :
--
--
----------------------------------------------------------------------------

import Control.Monad
import Criterion.Main
import Data.DList (DList)
import qualified Data.DList as DList
import qualified System.Directory as Directory
import System.FilePath

import Utils

getProperDirContents :: FilePath -> IO [FilePath]
getProperDirContents topdir = do
    names <- Directory.getDirectoryContents topdir
    let properNames = filter (`notElem` [".", ".."]) names
    return $ map ((</>) topdir) properNames

getRecursiveDirContents :: FilePath -> IO [FilePath]
getRecursiveDirContents topdir = do
    paths  <- getProperDirContents topdir
    paths' <- forM paths $ \path -> do
        isDirectory <- Directory.doesDirectoryExist path
        if isDirectory
            then getRecursiveDirContents path
            else return [path]
    return (concat paths')

getRecursiveDirContentsDList :: FilePath -> IO [FilePath]
getRecursiveDirContentsDList = fmap DList.toList . getRecursiveDirContentsDList'

getRecursiveDirContentsDList' :: FilePath -> IO (DList FilePath)
getRecursiveDirContentsDList' topdir = do
    paths  <- getProperDirContents topdir
    paths' <- forM paths $ \path -> do
        isDirectory <- Directory.doesDirectoryExist path
        if isDirectory
            then getRecursiveDirContentsDList' path
            else return $ DList.singleton path
    return (DList.concat paths')



main :: IO ()
main = do
  config <- makeConfig "file-collection.html"
  defaultMainWith config
    [ bench "list"  $ nfIO (getRecursiveDirContents dir)
    , bench "dlist" $ nfIO (getRecursiveDirContentsDList dir)
    ]
  where
    -- dir = "/home/sergey/projects/haskell/packages/all-packages"
    -- dir = "/home/sergey/projects/haskell/projects/fast-tags-testing/latest-packages"


    -- hierarchy of 10000 files built with
    -- mkdir /tmp/degenerate-hierarchy
    -- cd /tmp/degenerate-hierarchy
    -- for d in $(seq 1 100); do for f in $(seq 1 100); do touch "file$f"; done; mkdir "directory$d"; cd "directory$d"; done
    dir = "/tmp/degenerate-hierarchy"
