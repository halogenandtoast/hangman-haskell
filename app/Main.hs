module Main where

import System.Environment ( getArgs )
import System.Random ( randomRIO )
import Data.Char ( isAlpha
                 , toLower
                 )
import Hangman.Game

usage :: IO ()
usage = putStrLn "USAGE: stack exec hangman DICTIONARY_FILE"


loadDictionary :: FilePath -> IO [String]
loadDictionary filename = lines <$> readFile filename


showResult :: GameState -> IO ()
showResult game = if guessedWord game
                     then putStrLn $ "Correct, the word was " ++ word
                     else putStrLn $ "Sorry, the word was " ++ word
  where
    word = gameWord game


ask :: String -> IO String
ask msg = putStrLn msg >> getLine


displayHiddenWord :: GameState -> IO ()
displayHiddenWord game = putStrLn $ map (filterChar guesses) word
  where
    word = gameWord game
    guesses = gameGuesses game
    filterChar guesses c = if c `elem` guesses then c else '_'


displayWrongGuesses :: GameState -> IO ()
displayWrongGuesses game = putStrLn $ "Incorrect Guesses: " ++ show (wrongGuesses game)



getGuess :: IO Char
getGuess = do
    letter <- ask "Guess a letter > "
    case letter of
         [c] -> if isAlpha c
                   then return $ toLower c
                   else error
         _ -> error
  where
    error = putStrLn "Invalid letter" >> getGuess


mainLoop :: GameState -> IO ()
mainLoop game = do
    displayHiddenWord game
    displayWrongGuesses game
    guess <- getGuess
    let game' = update guess game
    if isFinished game'
       then showResult game'
       else mainLoop game'


run :: [String] -> IO ()
run [filename] =  do
    mword <- chooseRandomWordInDictionary filename
    maybe emptyDictionary (mainLoop . initialState) mword
run _ = usage


sample :: [a] -> IO (Maybe a)
sample [] = return Nothing
sample xs = do
    randomIndex <- randomRIO (0, length xs - 1)
    return . Just $ xs !! randomIndex


chooseRandomWordInDictionary :: FilePath -> IO (Maybe String)
chooseRandomWordInDictionary filename = loadDictionary filename >>= sample


emptyDictionary :: IO ()
emptyDictionary = putStrLn "The dictionary you supplied is empty"


main :: IO ()
main = getArgs >>= run
