import Hangman
import Data.Char

main = do
    putStrLn "Please enter the word or phrase to be guessed"
    word <- getLine
    let wordCharsReplacedWithUnderscores = displayWordSpaced (showCharInWord word ' ')
    putStrLn ("Here is the chosen word or phrase with all of its characters hidden: ")
    putStrLn wordCharsReplacedWithUnderscores
    let lowercaseWord = convertToLowerCase word
    guessAChar lowercaseWord [' ']

guessAChar :: String -> [Char] -> IO ()
guessAChar word listOfGuesses = do
    putStrLn ("Here is a list of ALL your guesses so far: " ++ listOfGuesses)
    let incorrectGuesses = getIncorrectGuesses word listOfGuesses
        maximumAllowedIncorrectGuesses = 5
        noOfIncorrectGuessesLeft = maximumAllowedIncorrectGuesses - (length incorrectGuesses)
    putStrLn ("You have made " ++ (show (length incorrectGuesses)) ++  " incorrect guess(es), so you are allowed to make only " ++ (show noOfIncorrectGuessesLeft) ++ " more incorrect guess(es)")
    putStrLn "Please enter your guess for a letter in the chosen word or phrase:"
    char <- getChar
    if(char == '\n')
        then
            guessAChar word listOfGuesses
        else do
            let lowercaseChar = toLower char
                isCharInWord = checkGuessedChar lowercaseChar word
                hasCharBeenGuessedBefore = checkIfGuessedBefore listOfGuesses lowercaseChar
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
                    let wordWithAllCorrectlyGuessedLettersRevealed = showAllCorrectCharsInWord word (getListOfWordsWithCorrectGuesses (lowercaseChar:listOfGuesses) word)
                    putStrLn "Here is the word or phrase with all the correctly guessed so far letters revealed:"
                    putStrLn wordWithAllCorrectlyGuessedLettersRevealed
                    let isWordCompletelyGuessed = wordWithAllCorrectlyGuessedLettersRevealed == (displayWordSpaced word)
                        newIncorrectGuesses = getIncorrectGuesses word (lowercaseChar:listOfGuesses)
                    putStrLn ("Incorrect guesses so far are:" ++ newIncorrectGuesses)
                    if(isWordCompletelyGuessed)
                        then
                            putStrLn "You have successfully guessed the word or phrase, congratulations!"
                        else
                            if(length newIncorrectGuesses < maximumAllowedIncorrectGuesses)
                                then
                                    guessAChar word (lowercaseChar:listOfGuesses)
                                else do
                                    putStrLn "You have run out of guesses, sorry, you've lost!"
                                    putStrLn "The word or phrase you were looking for was:"
                                    putStrLn (displayWordSpaced word)
