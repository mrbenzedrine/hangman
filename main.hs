import Hangman

main = do
    putStrLn "Please enter the word to be guessed"
    word <- getLine
    putStrLn ("Your chosen word is: " ++ word)
