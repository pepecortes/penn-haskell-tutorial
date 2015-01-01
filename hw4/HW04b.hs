import Data.List
import Data.Char

allCaps :: [String] -> Bool
allCaps [] = True
allCaps xs = and $ map isCapitalized xs
  where
    isCapitalized :: String -> Bool
    isCapitalized "" = False
    isCapitalized (first:rest) = isUpper first

allCaps2  :: [String] -> Bool
allCaps2 [] = True
allCaps2 ("":t) = False
allCaps2 ((f:r):t) = isUpper f && (allCaps t)

dropTrailingWhitespace :: String -> String
dropTrailingWhitespace s = dropWhileEnd isSpace s

firstLetters :: [String] -> [Char]
firstLetters [] = []
firstLetters ("":t) = firstLetters t
firstLetters ((f:r):t) = f : firstLetters t

-- etc
