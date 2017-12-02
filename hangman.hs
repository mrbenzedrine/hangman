import Data.List

displayWordSpaced :: String -> String
displayWordSpaced word = intersperse ' ' word

checkGuessedChar :: Char -> String -> Bool
checkGuessedChar guess word = guess `elem` word
