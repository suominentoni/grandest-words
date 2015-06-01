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
  | getGrandness x >  getGrandness (head grandest) = getGrandestWords' [x] grandest []
  | getGrandness x == getGrandness (head grandest) = getGrandestWords' (grandest ++ [x]) secondGrandest []
  | getGrandness x >  getGrandness (head secondGrandest) = getGrandestWords' grandest [x] []
  | getGrandness x == getGrandness (head secondGrandest) = getGrandestWords' grandest (secondGrandest ++ [x]) []
  | otherwise = getGrandestWords' grandest secondGrandest []
getGrandestWords' grandest secondGrandest (x:xs)
  | getGrandness x >  getGrandness (head grandest) = getGrandestWords' [x] grandest xs
  | getGrandness x == getGrandness (head grandest) = getGrandestWords' (grandest ++ [x]) secondGrandest xs
  | getGrandness x >  getGrandness (head secondGrandest) = getGrandestWords' grandest [x] xs
  | getGrandness x == getGrandness (head secondGrandest) = getGrandestWords' grandest (secondGrandest ++ [x]) xs
  | otherwise = getGrandestWords' grandest secondGrandest xs

getGrandness :: String -> Int
getGrandness word =
  length $ nub $ map toLower $ filter isLetter word

