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
getGrandestPairs contents = getGrandestPairs' (sortBy compareGrandness wordsWithGrandness) [("", "")]
  where wordsWithGrandness = map (\word -> (word, (getGrandness word))) (words contents)

getGrandestPairs' :: [(String, Int)] -> [(String, String)] -> [(String, String)]
getGrandestPairs' [] grandestPairs = grandestPairs
getGrandestPairs' (x:xs) grandestPairs
  | sumLessThanGrandestPair x x grandestPairs = grandestPairs
  | otherwise = getGrandestPairs' xs (getGrandestPairsForAWord x xs grandestPairs)

getGrandestPairsForAWord :: (String, Int) -> [(String, Int)] -> [(String, String)] -> [(String, String)]
getGrandestPairsForAWord word [] grandestPairs = grandestPairs
getGrandestPairsForAWord word (x:xs) grandestPairs
  | sumLessThanGrandestPair word x grandestPairs = grandestPairs
  | lessThanGrandestPair    word x grandestPairs = getGrandestPairsForAWord word xs grandestPairs
  | greaterThanGrandestPair word x grandestPairs = getGrandestPairsForAWord word xs [((fst word),(fst x))]
  | equalToGrandestPair     word x grandestPairs = getGrandestPairsForAWord word xs (grandestPairs ++ [((fst word),(fst x))])
  | otherwise = getGrandestPairsForAWord word xs grandestPairs

sumLessThanGrandestPair :: (String, Int) -> (String, Int) -> [(String, String)] -> Bool
sumLessThanGrandestPair word1 word2 grandestPairs = ((snd word1) + (snd word2)) < getPairGrandness grandestPairs

lessThanGrandestPair :: (String, Int) -> (String, Int) -> [(String, String)] -> Bool
lessThanGrandestPair word1 word2 grandestPairs = ((getGrandness $ (fst word1) ++ (fst word2))) < getPairGrandness grandestPairs

greaterThanGrandestPair :: (String, Int) -> (String, Int) -> [(String, String)] -> Bool
greaterThanGrandestPair word1 word2 grandestPairs = (getGrandness $ (fst word1) ++ (fst word2)) > getPairGrandness grandestPairs

equalToGrandestPair :: (String, Int) -> (String, Int) -> [(String, String)] -> Bool
equalToGrandestPair word1 word2 grandestPairs = (getGrandness $ (fst word1) ++ (fst word2)) == getPairGrandness grandestPairs

getPairGrandness :: [(String, String)] -> Int
getPairGrandness grandestPairs = (getGrandness $ (fst $ head grandestPairs) ++ (snd $ head grandestPairs))

getGrandness :: String -> Int
getGrandness word =
  length $ nub $ map toLower $ filter isLetter word

compareGrandness :: (String, Int) -> (String, Int) -> Ordering
compareGrandness a b
  | snd a < snd b = GT
  | (snd a) > (snd b) = LT
  | otherwise = EQ

