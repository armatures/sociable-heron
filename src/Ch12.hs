module Ch12 where

replaceThe:: String -> String
replaceThe = unwords . map replace . words
  where replace s =
          case notThe s of
            Just word -> word
            Nothing -> "a"

notThe:: String -> Maybe String
notThe s | s == "the" = Nothing
         | otherwise = Just s


