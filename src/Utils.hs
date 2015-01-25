----------------------------------------------------------------------------
-- |
-- Module      :  Utils
-- Copyright   :  (c) Sergey Vinokurov 2015
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  serg.foo@gmail.com
-- Stability   :
-- Portability :
--
--
----------------------------------------------------------------------------

module Utils where

import System.Directory
import System.FilePath
import Criterion.Main
import Criterion.Types

makeConfig :: FilePath -> IO Config
makeConfig reportOutFile = do
  tmpDir <- getTemporaryDirectory
  return $ defaultConfig { forceGC    = True
                         , reportFile = Just $ tmpDir </> reportOutFile
                         , resamples  = 10000
                         }

