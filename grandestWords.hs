module GrandestWords (getGrandestWords) where

import Data.Char (isLetter, toLower)
import Data.List (nub)
import System.IO (hGetContents, openFile, IOMode(ReadMode))
import System.Environment (getArgs)

main = do
  args <- getArgs
  handle <- openFile (head args) ReadMode
  contents <- hGetContents handle
  print $ getGrandestWordPairs contents

getGrandestWords :: String -> ([String],[ String])
getGrandestWords contents = getGrandestWords' [""] [""] $ words contents

getGrandestWords' :: [String] -> [String] -> [String] -> ([String], [String])
getGrandestWords' grandest secondGrandest [] = (grandest, secondGrandest)
getGrandestWords' grandest secondGrandest [x]
  | getGrandnessValue x > getGrandnessValue (head grandest) = getGrandestWords' [x] grandest []
  | getGrandnessValue x == getGrandnessValue (head grandest) = getGrandestWords' (grandest ++ [x]) secondGrandest []
  | getGrandnessValue x > getGrandnessValue (head secondGrandest) = getGrandestWords' grandest [x] []
  | getGrandnessValue x == getGrandnessValue (head secondGrandest) = getGrandestWords' grandest (secondGrandest ++ [x]) []
  | otherwise = getGrandestWords' grandest secondGrandest []
getGrandestWords' grandest secondGrandest (x:xs)
  | getGrandnessValue x > getGrandnessValue (head grandest) = getGrandestWords' [x] grandest xs
  | getGrandnessValue x == getGrandnessValue (head grandest) = getGrandestWords' (grandest ++ [x]) secondGrandest xs
  | getGrandnessValue x > getGrandnessValue (head secondGrandest) = getGrandestWords' grandest [x] xs
  | getGrandnessValue x == getGrandnessValue (head secondGrandest) = getGrandestWords' grandest (secondGrandest ++ [x]) xs
  | otherwise = getGrandestWords' grandest secondGrandest xs

getGrandnessValue :: String -> Int
getGrandnessValue word =
  length $ nub $ map toLower $ filter isLetter word

