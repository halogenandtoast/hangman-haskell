module Hangman.Game where

type Guess = Char
data GameState = GameState { gameWord :: String
                           , gameGuesses :: [Guess]
                           , gameMaxWrongGuesses :: Int
                           }

initialState :: String -> GameState
initialState word = GameState word [] 6


update :: Guess -> GameState -> GameState
update guess game = if guess `elem` guesses
                       then game
                       else game { gameGuesses = guess : guesses }
  where
    guesses = gameGuesses game


isFinished :: GameState -> Bool
isFinished game = guessedWord game || outOfGuesses game


guessedWord :: GameState -> Bool
guessedWord game = null remainingLetters
  where
    guesses = gameGuesses game
    word = gameWord game
    remainingLetters = filter (`notElem` guesses) word


outOfGuesses :: GameState -> Bool
outOfGuesses game = numberWrongGuesses game >= gameMaxWrongGuesses game


wrongGuesses :: GameState -> [Guess]
wrongGuesses game = filter (`notElem` word) guesses
  where
    guesses = gameGuesses game
    word = gameWord game


numberWrongGuesses :: GameState -> Int
numberWrongGuesses = length . wrongGuesses
