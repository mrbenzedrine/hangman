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
    print listOfGuesses
    putStrLn "Please enter your guess for a letter in the chosen word:"
    char <- getChar
    print char
    let isCharInWord = checkGuessedChar char word
    if(isCharInWord)
        then do
            putStrLn "You have guessed a letter correctly!"
            putStrLn "Here is the word with the correctly guessed letter(s) revealed:"
            let wordWithRevealedLetters = showCharInWord word char
            putStrLn wordWithRevealedLetters
        else
            putStrLn "You have guessed incorrectly, sorry!"
    let wordWithAllCorrectlyGuessedLettersRevealed = showAllCorrectCharsInWord word (getListOfWordsWithCorrectGuesses (char:listOfGuesses) word)
    putStrLn "Here is the word with all the correctly guessed so far letters revealed:"
    putStrLn wordWithAllCorrectlyGuessedLettersRevealed
    let isWordCompletelyGuessed = wordWithAllCorrectlyGuessedLettersRevealed == (displayWordSpaced word)
    if(isWordCompletelyGuessed)
        then do
            putStrLn "You have successfully guessed the word, congratulations!"
        else
            guessAChar word (char:listOfGuesses)
