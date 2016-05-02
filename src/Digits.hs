module Digits where
  import Prelude
  import Data.Char(chr)

  numbers = ["ZERO", "ONE", "TWO", "THREE", "FOUR", "FIVE",
             "SIX", "SEVEN", "EIGHT", "NINE"]

  contains :: String -> Char -> Bool
  contains [] _ = False
  contains (h: tail) c
    | h == c = True
    | otherwise = contains tail c

  removeCharHelper:: String -> String -> Char -> String
  removeCharHelper acc (h: t) c
    | h == c = reverse acc ++ t
    | otherwise =  removeCharHelper (h:acc) t c
  removeChar:: String -> Char -> String
  removeChar = removeCharHelper []

  removeChars:: String -> String -> String
  removeChars = foldl removeChar

  containsWord:: String -> String -> Bool
  containsWord _ [] = True
  containsWord str [h] = contains str h
  containsWord str (h:t)
    | contains str h = containsWord (removeChar str h) t
    | otherwise = False

  digitsForWordHelper:: String -> [String] -> Int -> String -> String
  digitsForWordHelper [] _ _ acc = acc
  digitsForWordHelper _ [] _ acc = acc
  digitsForWordHelper word (w: t) pos acc
    | containsWord word w = digitsForWordHelper (removeChars word w) (w:t) pos (chr pos:acc)
    | otherwise = digitsForWordHelper word t (pos + 1) acc
  digitsForWord:: String -> String
  digitsForWord str = reverse $ digitsForWordHelper str numbers 48 []
