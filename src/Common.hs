module Common where

import Paths_minesweeper (getDataDir)
import System.Directory (getDirectoryContents)
import System.FilePath
import Control.Applicative hiding ((<|>))
import Text.Parsec as P

isRegularFileOrDirectory :: FilePath -> Bool
isRegularFileOrDirectory f = f /= "." && f /= ".."

type Directory = String
getFiles :: Directory -> IO [FilePath]
getFiles dir = do 
  dataDir <- getDataDir
  let directory = dataDir </> dir
  files <- filter isRegularFileOrDirectory <$> getDirectoryContents directory
  return $ map (directory </>) files

solutionFiles :: IO [FilePath]
solutionFiles = getFiles "solutionFiles"

testFiles :: IO [FilePath]
testFiles = getFiles "testFiles"

for = flip map
filtered = flip filter

parseFromFile p fname = do
    input <- readFile fname
    return $ runParser p () fname input
