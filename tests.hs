module Tests where

import Test.HUnit
import Hangman

allTests = TestList [ test_checkIfValidWord
                    , test_checkIfValidChar
                    , test_removeExtraSpaces
                    ]

test_checkIfValidWord = TestList [ "test checkIfValidWord: normal word"     ~: True     ~=? (checkIfValidWord "hello")
                                 , "test checkIfValidWord: invalid chars"   ~: False    ~=? (checkIfValidWord "#[]{}")
                                 , "test checkIfValidWord: blanks"          ~: False    ~=? (checkIfValidWord "   ")
                                 ]

test_checkIfValidChar = TestList [ "test checkIfValidChar: normal letter"   ~: True     ~=? (checkIfValidChar 'a')
                                 , "test checkIfValidChar: invalid char"    ~: False    ~=? (checkIfValidChar '#')
                                 , "test checkIfValidChar: newline char"    ~: False    ~=? (checkIfValidChar '\n')
                                 ]

test_removeExtraSpaces = TestList [ "test removeExtraSpaces: no blanks"             ~: "hello"          ~=? (removeExtraSpaces "hello")
                                  , "test removeExtraSpaces: blanks between words"  ~: "hello world"    ~=? (removeExtraSpaces "hello   world")
                                  , "test removeExtraSpaces: blanks after words"    ~: "hello world"    ~=? (removeExtraSpaces "hello   world   ")
                                  ]
