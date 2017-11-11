module Ch12 where
import Data.Maybe

replaceThe:: String -> String
replaceThe = unwords . map (fromMaybe "a" . notThe) . words

notThe:: String -> Maybe String
notThe s | s == "the" = Nothing
         | otherwise = Just s

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel = fst . (foldl countHelp (0, False)) . words
  where
    countHelp (count, isFollowingThe) word =
      if isFollowingThe && elem (head word) "aeiou" then
        (count + 1, False)
      else
        (count, isNothing $ notThe word)

countVowels :: String -> Integer
countVowels = fromIntegral . length . filter isVowel
  where
    isVowel = flip elem "aeiou"


newtype Word' =
  Word' String
  deriving (Eq, Show)

mkWord :: String -> Maybe Word'
mkWord word =
  if vowels > consonants then
    Nothing
  else
    Just $ Word' word
  where
    (vowels, consonants) = foldl countVowelsAndConsonants (0,0) word

countVowelsAndConsonants :: (Int, Int) -> Char -> (Int, Int)
countVowelsAndConsonants (vowels,consonants) x =
  if elem x "aeiou" then
    (vowels+1, consonants)
  else
    (vowels, consonants+1)
