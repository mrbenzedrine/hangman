import Hangman

main = do
    putStrLn "Please enter the word to be guessed"
    word <- getLine
    let wordCharsReplacedWithUnderscores = displayWordSpaced (showCharInWord word '_')
    putStrLn ("Here is the chosen word with all of its characters hidden: ")
    putStrLn wordCharsReplacedWithUnderscores
    guessAChar word []

guessAChar :: String -> [Char] -> IO ()
guessAChar word listOfGuesses = do
    putStrLn ("Here is a list of ALL your guesses so far: " ++ listOfGuesses)
    let incorrectGuesses = getIncorrectGuesses word listOfGuesses
        maximumAllowedIncorrectGuesses = 5
        noOfIncorrectGuessesLeft = maximumAllowedIncorrectGuesses - (length incorrectGuesses)
    putStrLn ("You have made " ++ (show (length incorrectGuesses)) ++  " incorrect guess(es), so you are allowed to make only " ++ (show noOfIncorrectGuessesLeft) ++ " more incorrect guess(es)")
    putStrLn "Please enter your guess for a letter in the chosen word:"
    char <- getChar
    if(char == '\n')
        then
            guessAChar word listOfGuesses
        else do
            let isCharInWord = checkGuessedChar char word
                hasCharBeenGuessedBefore = checkIfGuessedBefore listOfGuesses char
            if(hasCharBeenGuessedBefore)
                then do
                    putStrLn "You have already guessed that letter before"
                    guessAChar word listOfGuesses
                else do
                    if(isCharInWord)
                        then
                            putStrLn "You have guessed a letter correctly!"
                        else
                            putStrLn "You have guessed incorrectly, sorry!"
                    let wordWithAllCorrectlyGuessedLettersRevealed = showAllCorrectCharsInWord word (getListOfWordsWithCorrectGuesses (char:listOfGuesses) word)
                    putStrLn "Here is the word with all the correctly guessed so far letters revealed:"
                    putStrLn wordWithAllCorrectlyGuessedLettersRevealed
                    let isWordCompletelyGuessed = wordWithAllCorrectlyGuessedLettersRevealed == (displayWordSpaced word)
                        newIncorrectGuesses = getIncorrectGuesses word (char:listOfGuesses)
                    putStrLn ("Incorrect guesses so far are:" ++ newIncorrectGuesses)
                    if(isWordCompletelyGuessed)
                        then
                            putStrLn "You have successfully guessed the word, congratulations!"
                        else
                            if(length newIncorrectGuesses < maximumAllowedIncorrectGuesses)
                                then
                                    guessAChar word (char:listOfGuesses)
                                else do
                                    putStrLn "You have run out of guesses, sorry, you've lost!"
                                    putStrLn "The word you were looking for was:"
                                    putStrLn (displayWordSpaced word)
