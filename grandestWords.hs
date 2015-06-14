module GrandestWords (getGrandestPairs, main) where

import Data.Char (isLetter, toLower)
import Data.List (nub, sortBy)
import System.IO (hGetContents, openFile, IOMode(ReadMode))
import System.Environment (getArgs)

main = do
  args <- getArgs
  handle <- openFile (head args) ReadMode
  contents <- hGetContents handle
  print $ nub $ fst $ getGrandestPairs contents

getGrandestPairs :: String -> ([(String, String)], Int)
getGrandestPairs contents = getGrandestPairs' (sortBy compareGrandness wordsWithGrandness) ([("", "")], 0)
  where wordsWithGrandness = map (\word -> (word, (getGrandness word))) (words contents)

getGrandestPairs' :: [(String, Int)] -> ([(String, String)], Int) -> ([(String, String)], Int)
getGrandestPairs' [] grandestPairs = grandestPairs
getGrandestPairs' (x:xs) grandestPairs
  | sumLessThanGrandestPair x x grandestPairs = grandestPairs
  | otherwise = getGrandestPairs' xs (getGrandestPairsForAWord x xs grandestPairs)

getGrandestPairsForAWord :: (String, Int) -> [(String, Int)] -> ([(String, String)], Int) -> ([(String, String)], Int)
getGrandestPairsForAWord word [] grandestPairs = grandestPairs
getGrandestPairsForAWord word (x:xs) grandestPairs
  | sumLessThanGrandestPair word x grandestPairs = grandestPairs
  | lessThanGrandestPair    word x grandestPairs = getGrandestPairsForAWord word xs grandestPairs
  | greaterThanGrandestPair word x grandestPairs = getGrandestPairsForAWord word xs ([((fst word),(fst x))], (getGrandness $ (fst word) ++ (fst x)))
  | equalToGrandestPair     word x grandestPairs = getGrandestPairsForAWord word xs (((fst grandestPairs) ++ [((fst word),(fst x))]), (snd grandestPairs))
  | otherwise = getGrandestPairsForAWord word xs grandestPairs

sumLessThanGrandestPair :: (String, Int) -> (String, Int) -> ([(String, String)], Int) -> Bool
sumLessThanGrandestPair word1 word2 grandestPairs = ((snd word1) + (snd word2)) < snd grandestPairs

lessThanGrandestPair :: (String, Int) -> (String, Int) -> ([(String, String)], Int) -> Bool
lessThanGrandestPair word1 word2 grandestPairs = ((getGrandness $ (fst word1) ++ (fst word2))) < snd grandestPairs

greaterThanGrandestPair :: (String, Int) -> (String, Int) -> ([(String, String)], Int) -> Bool
greaterThanGrandestPair word1 word2 grandestPairs = (getGrandness $ (fst word1) ++ (fst word2)) > snd grandestPairs

equalToGrandestPair :: (String, Int) -> (String, Int) -> ([(String, String)], Int) -> Bool
equalToGrandestPair word1 word2 grandestPairs = (getGrandness $ (fst word1) ++ (fst word2)) == snd grandestPairs

getGrandness :: String -> Int
getGrandness word =
  length $ nub $ map toLower $ filter isLetter word

compareGrandness :: (String, Int) -> (String, Int) -> Ordering
compareGrandness a b
  | snd a < snd b = GT
  | snd a > snd b = LT
  | otherwise = EQ

