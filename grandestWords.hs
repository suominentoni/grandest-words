module GrandestWords (getGrandestWords) where

import Data.Char (isLetter, toLower)
import Data.List (nub)

main = do
  contents <- getContents
  print $ getGrandestWords (words contents)

getGrandestWords words = getGrandestWords' [""] [""] words

getGrandestWords' :: [String] -> [String] -> [String] -> ([String], [String])
getGrandestWords' mostGrand secondGrand [] = (mostGrand, secondGrand)
getGrandestWords' mostGrand secondGrand [x]
  | getGrandnessValue x > getGrandnessValue (head mostGrand) = getGrandestWords' [x] mostGrand []
  | getGrandnessValue x > getGrandnessValue (head secondGrand) = getGrandestWords' mostGrand [x] [] 
  | getGrandnessValue x == getGrandnessValue (head mostGrand) = getGrandestWords' (x:mostGrand) secondGrand [] 
  | getGrandnessValue x == getGrandnessValue (head secondGrand) = getGrandestWords' mostGrand (x:secondGrand) [] 
  | otherwise = getGrandestWords' mostGrand secondGrand []
getGrandestWords' mostGrand secondGrand (x:xs)
  | getGrandnessValue x > getGrandnessValue (head mostGrand) = getGrandestWords' [x] mostGrand xs
  | getGrandnessValue x > getGrandnessValue (head secondGrand) = getGrandestWords' mostGrand [x] [] 
  | getGrandnessValue x == getGrandnessValue (head mostGrand) = getGrandestWords' (x:mostGrand) secondGrand xs 
  | getGrandnessValue x == getGrandnessValue (head secondGrand) = getGrandestWords' mostGrand (x:secondGrand) xs 
  | otherwise = getGrandestWords' mostGrand secondGrand xs

getGrandnessValue :: String -> Int
getGrandnessValue word =
  length $ nub $ map toLower $ filter isLetter word

