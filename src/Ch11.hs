module Ch11 where
import Data.Char

cipherCaesar :: Char -> String -> String
cipherCaesar shift' =
  unwords . ((map . map) (shiftLetter shift')) . words

shiftLetter shift' = ( chr . ((+) aNum) . (flip mod 25) . ((+) shift) . ((+) (negate aNum)) . ord )
    where
      aNum = ord 'a'
      shift = (ord . toLower) shift' - aNum

cipher :: String -> String -> String
cipher keyword message = undefined

