{-# LANGUAGE NoMonomorphismRestriction, RecordWildCards #-}

module Main where

import Control.Monad
import Text.PrettyPrint as Pr
import Text.Parsec as P
import Data.Char
import Data.List
import Data.List.Split (chunksOf)
import Control.Applicative hiding ((<|>), many)
import System.Environment (getArgs)
import Common

--------------
data Board = Board {
    height :: Int
  , width  :: Int
  , lines  :: [[InputSquare]] } deriving (Show, Read, Eq)

data InputSquare = Bomb | Blank deriving (Show, Read, Eq)
data ResultSquare = Bomb' | AdjacentBombs Int deriving (Show, Read, Eq)
--------------

------------
-- Parser --
------------
readBoard = do
  height <- digitToInt <$> digit
  P.space
  width <- digitToInt <$> digit
  lines <- count height $ newline *> count width readValue
  return Board {..}

readValue = do
  let noBomb = P.char '-' *> return Blank
      bomb   = P.char 'x' *> return Bomb
  noBomb <|> bomb

-- |Read the input from `fname` and return the solution String
minesweep :: FilePath -> IO String
minesweep fname = do
  result <- parseFromFile readBoard fname
  return $ either show solve result

solve :: Board -> String
solve Board{..} = 
  let values :: [(Int, InputSquare)]
      values = zip [0..] $ concat lines 

      areConnected :: Int -> Int -> Bool
      areConnected index1 index2 = abs ((index1 `mod` width) -
                                       (index2 `mod` width)) <= 1
                                  && abs (index1 - index2) <= width+1

      adjacentBombs :: (Int, InputSquare) -> ResultSquare
      adjacentBombs (index, _) = AdjacentBombs . length . filtered values $ \(index2, val2) ->
        val2 == Bomb && index /= index2 && areConnected index index2
   
      results :: [[Doc]]
      results = chunksOf width $ for values $ \(index, value) -> show' $
        if value == Bomb then 
          Bomb'
        else
          adjacentBombs (index, value)
    in

   Pr.render $ Pr.vcat $ map Pr.hsep results
      
---- Makes use of the pretty printing library
show' (AdjacentBombs 0) = Pr.char ' '
show' (AdjacentBombs n) = Pr.int n
show' Bomb' = Pr.char 'B'

--------------------

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> processAllFiles
    _               -> putStrLn $ "No argument expected!"

processAllFiles = do
  files <- sort <$> testFiles
  solutionFiles' <- sort <$> solutionFiles
  results <- mapM minesweep files
  solutions <- forM solutionFiles' $ \file -> do { content <- readFile file; return (file, content)}

  forM_ (zip solutions results) $ \((filename, solution), result) -> do
    putStrLn $ "Filename: " ++ filename
    putStrLn $ "Solution: \n" ++ solution
    putStrLn $ "Result:   \n" ++ result
