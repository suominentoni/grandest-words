module GrandestWords (getGrandestPairs, main) where

import Data.Char (isLetter, toLower)
import Data.List (nub, sortBy)
import System.IO (hGetContents, openFile, IOMode(ReadMode))
import System.Environment (getArgs)

main = do
  args <- getArgs
  handle <- openFile (head args) ReadMode
  contents <- hGetContents handle
  print $ nub $ getGrandestPairs contents

getGrandestPairs :: String -> [(String, String)]
getGrandestPairs contents = getGrandestPairs' (sortBy compareGrandness (words contents)) [("", "")]

getGrandestPairs' :: [String] -> [(String, String)] -> [(String, String)]
getGrandestPairs' [] grandestPairs = grandestPairs
getGrandestPairs' (x:xs) grandestPairs
  | sumLessThanGrandestPair x x grandestPairs = grandestPairs
  | otherwise = getGrandestPairs' xs (getGrandestPairsForAWord x xs grandestPairs)

getGrandestPairsForAWord :: String -> [String] -> [(String, String)] -> [(String, String)]
getGrandestPairsForAWord word [] grandestPairs = grandestPairs
getGrandestPairsForAWord word (x:xs) grandestPairs
  | sumLessThanGrandestPair word x grandestPairs = grandestPairs
  | lessThanGrandestPair    word x grandestPairs = getGrandestPairsForAWord word xs grandestPairs
  | greaterThanGrandestPair word x grandestPairs = getGrandestPairsForAWord word xs [(word,x)]
  | equalToGrandestPair     word x grandestPairs = getGrandestPairsForAWord word xs (grandestPairs ++ [(word,x)])
  | otherwise = getGrandestPairsForAWord word xs grandestPairs

sumLessThanGrandestPair :: String -> String -> [(String, String)] -> Bool
sumLessThanGrandestPair word1 word2 grandestPairs = ((getGrandness word1) + (getGrandness word2)) < getPairGrandness grandestPairs

lessThanGrandestPair :: String -> String -> [(String, String)] -> Bool
lessThanGrandestPair word1 word2 grandestPairs = ((getGrandness $ word1 ++ word2)) < getPairGrandness grandestPairs

greaterThanGrandestPair :: String -> String -> [(String, String)] -> Bool
greaterThanGrandestPair word1 word2 grandestPairs = (getGrandness $ word1 ++ word2) > getPairGrandness grandestPairs

equalToGrandestPair :: String -> String -> [(String, String)] -> Bool
equalToGrandestPair word1 word2 grandestPairs = (getGrandness $ word1 ++ word2) == getPairGrandness grandestPairs

getPairGrandness :: [(String, String)] -> Int
getPairGrandness grandestPairs = (getGrandness $ (fst $ head grandestPairs) ++ (snd $ head grandestPairs))

getGrandness :: String -> Int
getGrandness word =
  length $ nub $ map toLower $ filter isLetter word

compareGrandness :: String -> String -> Ordering
compareGrandness a b
  | getGrandness a < getGrandness b = GT
  | getGrandness a > getGrandness b = LT
  | otherwise = EQ

