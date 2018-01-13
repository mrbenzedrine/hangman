module Main where

import Hangman
import Data.Char
import System.Console.ANSI

main = do
    putStrLn "Please enter the word or phrase to be guessed"
    word <- getLine
    if(not $ checkIfValidWord word)
        then do
            putStrLn "Please enter a valid word or phrase"
            main
        else do
            clearScreen
            let wordWithExtraSpacesRemoved = removeExtraSpaces word
                wordCharsReplacedWithUnderscores = showCharInWord wordWithExtraSpacesRemoved ' '
            putStrLn ("Here is the chosen word or phrase with all of its characters hidden: ")
            putStrLn wordCharsReplacedWithUnderscores
            let lowercaseWord = convertToLowerCase wordWithExtraSpacesRemoved
                maximumAllowedIncorrectGuesses = 5
                doesInputHaveSpaces = ' ' `elem` lowercaseWord
            if(doesInputHaveSpaces)
                then
                    guessAChar lowercaseWord [' '] maximumAllowedIncorrectGuesses
                else
                    guessAChar lowercaseWord [] maximumAllowedIncorrectGuesses

guessAChar :: String -> [Char] -> Int -> IO ()
guessAChar word listOfGuesses maxIncorrectGuesses = do
    let incorrectGuesses = getIncorrectGuesses word listOfGuesses
        noOfIncorrectGuessesLeft = maxIncorrectGuesses - (length incorrectGuesses)
    putStrLn ("You have made " ++ (show (length incorrectGuesses)) ++  " incorrect guess(es), so you are allowed to make only " ++ (show noOfIncorrectGuessesLeft) ++ " more incorrect guess(es)")
    putStrLn ("Incorrect guesses so far are: " ++ incorrectGuesses)
    putStrLn "Please enter your guess for a letter in the chosen word or phrase:"
    guess <- getLine
    isGuessValid <- checkGuessIsValid guess listOfGuesses
    if(not isGuessValid)
        then
            guessAChar word listOfGuesses maxIncorrectGuesses
        else do
            let char = head guess
                lowercaseChar = toLower char
            guessANewChar word lowercaseChar
            let newListOfGuesses = lowercaseChar:listOfGuesses
                wordWithAllCorrectlyGuessedLettersRevealed = showAllCorrectCharsInWord word (getListOfWordsWithCorrectGuesses newListOfGuesses word)
            putStrLn "Here is the word or phrase with all the correctly guessed so far letters revealed:"
            putStrLn wordWithAllCorrectlyGuessedLettersRevealed
            checkIfGameIsFinished word wordWithAllCorrectlyGuessedLettersRevealed newListOfGuesses maxIncorrectGuesses

guessANewChar :: String -> Char -> IO ()
guessANewChar word guess = do
    let isCharInWord = checkGuessedChar guess word
    if(isCharInWord)
        then
            putStrLn "You have guessed a letter correctly!"
        else
            putStrLn "You have guessed incorrectly, sorry!"

checkIfGameIsFinished :: String -> String -> [Char] -> Int -> IO ()
checkIfGameIsFinished word wordWithCorrectLettersRevealed listOfGuesses maxIncorrectGuesses = do
    let isWordCompletelyGuessed = wordWithCorrectLettersRevealed == (displayWordSpaced word)
        newIncorrectGuesses = getIncorrectGuesses word listOfGuesses
    if(isWordCompletelyGuessed)
        then
            putStrLn "You have successfully guessed the word or phrase, congratulations!"
        else
            if(length newIncorrectGuesses < maxIncorrectGuesses)
                then
                    guessAChar word listOfGuesses maxIncorrectGuesses
                else do
                    putStrLn "You have run out of guesses, sorry, you've lost!"
                    putStrLn "The word or phrase you were looking for was:"
                    putStrLn (displayWordSpaced word)

checkGuessIsValid :: String -> [Char] -> IO Bool
checkGuessIsValid guess listOfGuesses = do
    if(null guess)
        then
            return False
        else do
            let isCharValid = checkIfValidChar (head guess)
            if(not isCharValid)
                then do
                    putStrLn "You have entered an invalid character, please enter a valid character"
                    return False
                else do
                    let char = head guess
                        lowercaseChar = toLower char
                        hasCharBeenGuessedBefore = checkIfGuessedBefore listOfGuesses lowercaseChar
                    if(hasCharBeenGuessedBefore)
                        then do
                            putStrLn "You have already guessed that letter before"
                            return False
                        else
                            return True
