module GrandestWords (getGrandestPairs) where

import Data.Char (isLetter, toLower)
import Data.List (nub, sortBy)
import System.IO (hGetContents, openFile, IOMode(ReadMode))
import System.Environment (getArgs)

main = do
  args <- getArgs
  handle <- openFile (head args) ReadMode
  contents <- hGetContents handle
  print $ getGrandestPairs contents

getGrandestPairs :: String -> [(String, String)]
getGrandestPairs contents = getGrandestPairs' sortedWords sortedWords [("", "")]
  where sortedWords = (sortBy compareGrandness (words contents))

getGrandestPairs' :: [String] -> [String] -> [(String, String)] -> [(String, String)]
getGrandestPairs' [] _ grandestPairs          = grandestPairs
getGrandestPairs' (_:xs) [] grandestPairs     = getGrandestPairs' xs xs grandestPairs
getGrandestPairs' [x] (y:ys) grandestPairs
  | (getGrandness $ x ++ y) < getPairGrandness grandestPairs = getGrandestPairs' [] ys grandestPairs
  | greaterThanGrandestPair x y grandestPairs = getGrandestPairs' [] ys [(x,y)]
  | equalToGrandestPair     x y grandestPairs = getGrandestPairs' [] ys (grandestPairs ++ [(x,y)])
  | otherwise                                 = getGrandestPairs' [] ys grandestPairs
getGrandestPairs' [x] [y] grandestPairs
  | (getGrandness $ x ++ y) < getPairGrandness grandestPairs = getGrandestPairs' [] [] grandestPairs
  | greaterThanGrandestPair x y grandestPairs = getGrandestPairs' [] [] [(x,y)]
  | equalToGrandestPair     x y grandestPairs = getGrandestPairs' [] [] (grandestPairs ++ [(x,y)])
  | otherwise                                 = getGrandestPairs' [] [] grandestPairs
getGrandestPairs' (x:xs) (y:ys) grandestPairs
  | (getGrandness $ x ++ y) < getPairGrandness grandestPairs = getGrandestPairs' (x:xs) ys grandestPairs
  | greaterThanGrandestPair x y grandestPairs = getGrandestPairs' (x:xs) ys [(x,y)]
  | equalToGrandestPair     x y grandestPairs = getGrandestPairs' (x:xs) ys (grandestPairs ++ [(x,y)])
  | otherwise                                 = getGrandestPairs' (x:xs) ys grandestPairs
getGrandestPairs' (x:xs) [y] grandestPairs
  | (getGrandness $ x ++ y) < getPairGrandness grandestPairs = getGrandestPairs' xs (x:xs) grandestPairs
  | greaterThanGrandestPair x y grandestPairs = getGrandestPairs' xs (x:xs) [(x,y)]
  | equalToGrandestPair     x y grandestPairs = getGrandestPairs' xs (x:xs) (grandestPairs ++ [(x,y)])
  | otherwise                                 = getGrandestPairs' xs xs grandestPairs

lessThanGrandestPair :: String -> String -> [(String, String)] -> Bool
lessThanGrandestPair word1 word2 grandestPairs = (getGrandness $ word1 ++ word2) < getPairGrandness grandestPairs

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

