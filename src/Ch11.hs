module Ch11 where
import Data.Char

cipherCaesar :: Char -> String -> String
cipherCaesar shift' =
  unwords . ((map . map) letterCipher) . words
    where
      aNum = ord 'a'
      shift = (ord . toLower) shift' - aNum
      letterCipher = ( chr . ((+) aNum) . (flip mod 25) . ((+) shift) . ((+) (negate aNum)) . ord )

cipher :: String -> String -> String
cipher keyword message = undefined

