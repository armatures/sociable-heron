module Ch11 where
import Data.Char

cipherCaesar :: Char -> String -> String
cipherCaesar shift' message =
  map ( chr . ((+) aNum) . (flip mod 25) . ((+) shift) . ((+) (negate aNum)) . ord ) message
    where
      aNum = ord 'a'
      zNum = ord 'z'
      shift = (ord . toLower) shift' - aNum
      rankLetter = (((-) aNum) . ord )

cipher :: String -> String -> String
cipher keyword message = undefined

