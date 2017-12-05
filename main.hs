import Hangman

main = do
    putStrLn "Please enter the word to be guessed"
    word <- getLine
    let wordCharsReplacedWithUnderscores = displayWordSpaced (showCharInWord word '_')
    putStrLn ("Here is the chosen word with all of its characters hidden: ")
    putStrLn wordCharsReplacedWithUnderscores
    let guessAChar = (do
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
        return char)
    guesses <- sequence (replicate 4 guessAChar)
    print guesses
