module GrandestWords (getGrandestWords, getGrandestWordPairs) where

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

getGrandestWordPairs :: String -> [(String, String)]
getGrandestWordPairs contents = getGrandestWordPairs' (words contents) (words contents) [("", "")]

getGrandestWordPairs' :: [String] -> [String]-> [(String, String)] -> [(String, String)]
getGrandestWordPairs' [] remaining grandestPairs
  | remaining == [] = grandestPairs
  | otherwise = getGrandestWordPairs' (tail remaining) (tail remaining) grandestPairs
getGrandestWordPairs' [x] remaining grandestPairs = getGrandestWordPairs' [] remaining grandestPairs
getGrandestWordPairs' (x:xs) remaining grandestPairs
  | isNewGrandestPair      = getGrandestWordPairs' ([x] ++ (tail xs)) remaining [(x,(head xs))]
  | isACurrentGrandestPair = getGrandestWordPairs' ([x] ++ (tail xs)) remaining (grandestPairs ++ [(x,(head xs))])
  | otherwise              = getGrandestWordPairs' ([x] ++ (tail xs)) remaining grandestPairs
  where isNewGrandestPair = getGrandness (x ++ (head xs)) >  getGrandness (((fst $ head grandestPairs) ++ (snd $ head grandestPairs)))
        isACurrentGrandestPair = getGrandness (x ++ (head xs)) == getGrandness (((fst $ head grandestPairs) ++ (snd $ head grandestPairs)))



