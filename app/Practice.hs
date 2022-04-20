module Practice where

import Data.List
import Data.Char

main :: IO ()
main =
  do
    putStrLn "Please enter a word to get L33ted"
    word <- getLine
    print (convertToL33t word)

function :: Integer -> Integer -> Integer
function x  y = if ( x > y ) then ( x + 10 ) else y

function2 :: Integer -> Integer -> Integer
function2 x y =
  case (x > y) of
    False -> y
    True -> x + 10

absVal :: (Num a, Ord a) => a -> a
absVal x =
  case ( x < 0 ) of
    False -> x
    True -> ( negate x )

validateUsernamePassword :: String -> String -> String
validateUsernamePassword username password =
  case (username, password) of
    ("" , "") -> "Empty username and password"
    ("", _) -> "Empty username"
    (_, "") -> "Empty password"
    (_, _) -> "Okay"

tail' :: [a] -> Maybe [a]
tail' []     = Nothing
tail' (x:xs) = Just xs

head' :: [a] -> Maybe [a]
head' []     = Nothing
head' (x:xs) = Just xs

isAnagram :: String -> String -> Bool
isAnagram word1 word2 = (sort word1) == (sort word2)

isWord :: String -> Maybe String
isWord word =
  case (null word) of
    True -> Nothing
    False ->
      case all isAlpha word of
        False -> Nothing
        True -> Just word

checkAnagram :: String -> String -> String
checkAnagram word1 word2 =
  case isWord word1 of
    Nothing -> "The first word is invalid"
    Just word1 ->
      case isWord word2 of
        Nothing -> "The second word is invalid"
        Just word2 ->
          case isAnagram word1 word2 of
            False -> "These words are not anagrams"
            True -> "These words are anagrams"

isPalindrome :: String -> String -> Bool
isPalindrome word1 word2 = word1 == reverse word2

convertToL33t :: String -> String
convertToL33t word =
  map replaceChar word

replaceChar :: Char -> Char
replaceChar c =
  case c of
    'e'       -> '3'
    'o'       -> '0'
    'l'       -> '7'
    's'       -> '5'
    nonLeeted -> nonLeeted
