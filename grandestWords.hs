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
  | grandnessSum < snd grandestPairs = grandestPairs
  | otherwise = getGrandestPairs' xs (getGrandestPairsForAWord x xs grandestPairs)
  where grandnessSum = (snd x) + (snd x)

getGrandestPairsForAWord :: (String, Int) -> [(String, Int)] -> ([(String, String)], Int) -> ([(String, String)], Int)
getGrandestPairsForAWord word [] grandestPairs = grandestPairs
getGrandestPairsForAWord word (x:xs) grandestPairs
  | grandnessSum  <  snd grandestPairs = grandestPairs
  | pairGrandness <  snd grandestPairs = getGrandestPairsForAWord word xs grandestPairs
  | pairGrandness >  snd grandestPairs = getGrandestPairsForAWord word xs ([((fst word),(fst x))], (getGrandness $ (fst word) ++ (fst x)))
  | pairGrandness == snd grandestPairs = getGrandestPairsForAWord word xs (((fst grandestPairs) ++ [((fst word),(fst x))]), (snd grandestPairs))
  where pairGrandness = getGrandness $ (fst word) ++ (fst x)
        grandnessSum = (snd word) + (snd x)

getGrandness :: String -> Int
getGrandness word =
  length $ nub $ map toLower $ filter isLetter word

compareGrandness :: (String, Int) -> (String, Int) -> Ordering
compareGrandness a b
  | snd a < snd b = GT
  | snd a > snd b = LT
  | otherwise = EQ

