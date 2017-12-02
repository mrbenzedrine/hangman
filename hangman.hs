import Data.List

displayWordSpaced :: String -> String
displayWordSpaced word = intersperse ' ' word

checkGuessedChar :: Char -> String -> Bool
checkGuessedChar guess word = guess `elem` word

showCharInWord :: Char -> String -> String
showCharInWord guess word = displayWordSpaced (map compareCharsPartiallyApplied word)
    where compareCharsPartiallyApplied = compareChars guess

compareChars :: Char -> Char -> Char
compareChars char1 char2
    |char1 == char2 = char1
    |otherwise      = '_'
