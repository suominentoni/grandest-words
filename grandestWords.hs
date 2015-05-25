module GrandestWords where

import Data.Char (isLetter, toLower)
import Data.List (nub)

main = do
  contents <- getContents
  print $ getGrandestWords (words contents) [""] [""]

getGrandestWords :: [String] -> [String] -> [String] -> [[String]]
getGrandestWords [] mostGrand secondGrand = [mostGrand, secondGrand]
getGrandestWords [x] mostGrand secondGrand 
  | getGrandnessValue x > getGrandnessValue (head mostGrand) = getGrandestWords [] [x] mostGrand
  | getGrandnessValue x == getGrandnessValue (head mostGrand) = getGrandestWords [] (x:mostGrand) secondGrand 
  | getGrandnessValue x == getGrandnessValue (head secondGrand) = getGrandestWords [] mostGrand (x:secondGrand) 
  | otherwise = getGrandestWords [] mostGrand secondGrand
getGrandestWords (x:xs) mostGrand secondGrand
  | getGrandnessValue x > getGrandnessValue (head mostGrand) = getGrandestWords xs [x] mostGrand
  | getGrandnessValue x == getGrandnessValue (head mostGrand) = getGrandestWords xs (x:mostGrand) secondGrand 
  | getGrandnessValue x == getGrandnessValue (head secondGrand) = getGrandestWords xs mostGrand (x:secondGrand) 
  | otherwise = getGrandestWords xs mostGrand secondGrand

getGrandnessValue :: String -> Int
getGrandnessValue word =
  length $ nub $ map toLower $ filter isLetter word

