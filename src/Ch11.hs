module Ch11 where
import Data.Char

cipherCaesar :: Char -> String -> String
cipherCaesar shift' =
  unwords . ((map . map) (shiftLetter shift')) . words

shiftLetter shift' = ( chr . ((+) aNum) . (flip mod 26) . ((+) shift) . ((+) (negate aNum)) . ord )
    where
      aNum = ord 'a'
      shift = (ord . toLower) shift' - aNum

cipherVigenere :: String -> String -> String
cipherVigenere keyword message =
  vigenereHelp (cycle keyword) message ""

vigenereHelp :: String -> String -> String -> String
vigenereHelp [] message _ = message
vigenereHelp (x:xs) message acc =
  case message of
    [] -> acc
    (m:ms) -> if m == ' ' then
        vigenereHelp (x:xs) ms (acc++[m])
      else
        vigenereHelp xs ms (acc ++ [shiftLetter x m])

