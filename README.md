# hangman
Command line hangman game

## Building and running
Cabal is used for handling building and packaging in this project. If you don't
have Cabal installed you can download and install it via their 
[downloads page][Cabal download], or as part of the [Haskell Platform].

In the project directory, use `cabal build` to build the project and then 
`cabal run` to run it.

## Tests
Only hand-coded testing has been implemented so far, with the HUnit package.

To run a test in `tests.hs` load it into ghci with either `ghci tests.hs` if ghci
is not open, or `:l tests.hs` if ghci is already open, and use `runTestTT test_name`.

[Cabal download]: https://www.haskell.org/cabal/download.html
[Haskell Platform]: https://www.haskell.org/platform/
