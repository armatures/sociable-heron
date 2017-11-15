module Ch12 where
import Data.Maybe

replaceThe:: String -> String
replaceThe = unwords . replaceThe' . words

replaceThe':: [String] -> [String]
replaceThe' [] = []
replaceThe' (x:xs) =
    fromMaybe "a" (notThe x) : replaceThe' xs

notThe:: String -> Maybe String
notThe s | s == "the" = Nothing
         | otherwise = Just s

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel = countHelp . words

countHelp :: [String] -> Integer
countHelp (x:x':xs) =
  if isNothing (notThe x) && elem (head x') "aeiou" then
    1 + countHelp xs
  else
    countHelp xs
countHelp _ = 0

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

myIterate :: (a -> a) -> a -> [a]
myIterate f x =
  x:myIterate f (f x)

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f x = case f x of
  Nothing -> []
  Just (y,z) -> y:myUnfoldr f z

betterIterate :: (a -> a) -> a -> [a]
betterIterate f =
  myUnfoldr (\y -> Just (y,f y))

