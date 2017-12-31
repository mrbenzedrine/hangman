module Tests where

import Test.HUnit
import Hangman

test_checkIfValidWord = TestList [ "test checkIfValidWord: normal word"     ~: True     ~=? (checkIfValidWord "hello")
                                 , "test checkIfValidWord: invalid chars"   ~: False    ~=? (checkIfValidWord "#[]{}")
                                 , "test checkIfValidWord: blanks"          ~: False    ~=? (checkIfValidWord "   ")
                                 ]
