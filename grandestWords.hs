module GrandestWords (getGrandestWords) where

import Data.Char (isLetter, toLower)
import Data.List (nub)

main = do
  contents <- getContents
  print $ getGrandestWords (words contents)

getGrandestWords words = getGrandestWords' [""] [""] words

getGrandestWords' :: [String] -> [String] -> [String] -> ([String], [String])
getGrandestWords' grandest secondGrandest [] = (grandest, secondGrandest)
getGrandestWords' grandest secondGrandest [x]
  | getGrandnessValue x > getGrandnessValue (head grandest) = getGrandestWords' [x] grandest []
  | getGrandnessValue x == getGrandnessValue (head grandest) = getGrandestWords' (x:grandest) secondGrandest [] 
  | getGrandnessValue x > getGrandnessValue (head secondGrandest) = getGrandestWords' grandest [x] [] 
  | getGrandnessValue x == getGrandnessValue (head secondGrandest) = getGrandestWords' grandest (x:secondGrandest) [] 
  | otherwise = getGrandestWords' grandest secondGrandest []
getGrandestWords' grandest secondGrandest (x:xs)
  | getGrandnessValue x > getGrandnessValue (head grandest) = getGrandestWords' [x] grandest xs
  | getGrandnessValue x == getGrandnessValue (head grandest) = getGrandestWords' (x:grandest) secondGrandest xs 
  | getGrandnessValue x > getGrandnessValue (head secondGrandest) = getGrandestWords' grandest [x] [] 
  | getGrandnessValue x == getGrandnessValue (head secondGrandest) = getGrandestWords' grandest (x:secondGrandest) xs 
  | otherwise = getGrandestWords' grandest secondGrandest xs

getGrandnessValue :: String -> Int
getGrandnessValue word =
  length $ nub $ map toLower $ filter isLetter word

