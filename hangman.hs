
checkGuessedChar :: Char -> String -> Bool
checkGuessedChar guess word = guess `elem` word
