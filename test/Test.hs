module Test where

import Control.Monad
import Test.Hspec
import Text.Parsec
import Data.Either
import Main
import Data.List
import Control.Applicative hiding ((<|>))
import Common


test = hspec $ do

  describe "readValue" $ do

    it "parses a bomb correctly" $ 
      parse readValue "" "x" `shouldBe` Right Bomb

    it "parses a blank correctly" $ 
      parse readValue "" "-" `shouldBe` Right Blank

    it "fails when given neither a bomb nor a blank" $ 
      parse readValue "" "i" `shouldSatisfy` isLeft

  describe "readBoard" $ 

    it "parses all solution files" $ do
      files <- testFiles
      result <- sequence <$> mapM (parseFromFile readBoard) files
      result `shouldSatisfy` isRight

  describe "minesweep" $ 

    it "solves all files correctly" $ void $ do
      files <- sort <$> testFiles
      results <- mapM minesweep files
      solutions <- mapM readFile =<< (sort <$> solutionFiles)

      forM (zip solutions results) $ \(solution, result) -> solution `shouldBe` (result ++ "\n")
      
         
