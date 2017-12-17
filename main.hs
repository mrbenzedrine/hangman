import Hangman
import Data.Char

main = do
    putStrLn "Please enter the word or phrase to be guessed"
    word <- getLine
    if(not $ checkIfValidWord word)
        then do
            putStrLn "Please enter a valid word or phrase"
            main
        else do
            let wordCharsReplacedWithUnderscores = displayWordSpaced (showCharInWord word ' ')
            putStrLn ("Here is the chosen word or phrase with all of its characters hidden: ")
            putStrLn wordCharsReplacedWithUnderscores
            let lowercaseWord = convertToLowerCase word
                maximumAllowedIncorrectGuesses = 5
                doesInputHaveSpaces = ' ' `elem` lowercaseWord
            if(doesInputHaveSpaces)
                then
                    guessAChar lowercaseWord [' '] maximumAllowedIncorrectGuesses
                else
                    guessAChar lowercaseWord [] maximumAllowedIncorrectGuesses

guessAChar :: String -> [Char] -> Int -> IO ()
guessAChar word listOfGuesses maxIncorrectGuesses = do
    putStrLn ("Here is a list of ALL your guesses so far: " ++ listOfGuesses)
    let incorrectGuesses = getIncorrectGuesses word listOfGuesses
        noOfIncorrectGuessesLeft = maxIncorrectGuesses - (length incorrectGuesses)
    putStrLn ("You have made " ++ (show (length incorrectGuesses)) ++  " incorrect guess(es), so you are allowed to make only " ++ (show noOfIncorrectGuessesLeft) ++ " more incorrect guess(es)")
    putStrLn "Please enter your guess for a letter in the chosen word or phrase:"
    guess <- getLine
    if(null guess)
        then
            guessAChar word listOfGuesses maxIncorrectGuesses
        else do
            let char = head guess
                lowercaseChar = toLower char
                hasCharBeenGuessedBefore = checkIfGuessedBefore listOfGuesses lowercaseChar
            if(hasCharBeenGuessedBefore)
                then do
                    putStrLn "You have already guessed that letter before"
                    guessAChar word listOfGuesses maxIncorrectGuesses
                else
                    guessANewChar word (lowercaseChar:listOfGuesses) maxIncorrectGuesses

guessANewChar :: String -> [Char] -> Int -> IO ()
guessANewChar word newListOfGuesses@(guess:previousGuesses) maxIncorrectGuesses = do
    let isCharInWord = checkGuessedChar guess word
    if(isCharInWord)
        then
            putStrLn "You have guessed a letter correctly!"
        else
            putStrLn "You have guessed incorrectly, sorry!"
    let wordWithAllCorrectlyGuessedLettersRevealed = showAllCorrectCharsInWord word (getListOfWordsWithCorrectGuesses newListOfGuesses word)
    putStrLn "Here is the word or phrase with all the correctly guessed so far letters revealed:"
    putStrLn wordWithAllCorrectlyGuessedLettersRevealed
    let isWordCompletelyGuessed = wordWithAllCorrectlyGuessedLettersRevealed == (displayWordSpaced word)
        newIncorrectGuesses = getIncorrectGuesses word newListOfGuesses
    putStrLn ("Incorrect guesses so far are:" ++ newIncorrectGuesses)
    if(isWordCompletelyGuessed)
        then
            putStrLn "You have successfully guessed the word or phrase, congratulations!"
        else
            if(length newIncorrectGuesses < maxIncorrectGuesses)
                then
                    guessAChar word newListOfGuesses maxIncorrectGuesses
                else do
                    putStrLn "You have run out of guesses, sorry, you've lost!"
                    putStrLn "The word or phrase you were looking for was:"
                    putStrLn (displayWordSpaced word)
