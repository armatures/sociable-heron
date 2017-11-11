module Ch12 where
import Data.Maybe 
replaceThe:: String -> String
replaceThe = unwords . map replace . words
  where replace s =
          case notThe s of
            Just word -> word
            Nothing -> "a"

notThe:: String -> Maybe String
notThe s | s == "the" = Nothing
         | otherwise = Just s

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel = fst . (foldl countHelp (0,False)) . words
  where
    countHelp (count, isFollowingThe) word =
      if isFollowingThe && elem (head word) "aeiou" then
        (count + 1, False)
      else
        (count, isNothing $ notThe word)
