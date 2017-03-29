module Hangman.GameSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Hangman.Game

spec :: Spec
spec = do
  describe "update" $ do
    it "returns a new state with updated guesses" $ do
      let state = GameState "food" "od" 6
      let state' = update 'f' state
      gameGuesses state' `shouldBe` "fod"

    it "returns a the same state if guess already made" $ do
      let state = GameState "food" "od" 6
      let state' = update 'o' state
      gameGuesses state `shouldBe` "od"

  describe "isFinished" $ do
    it "returns true if word is guessed" $ do
      let state = GameState "food" "fod" 6
      isFinished state `shouldBe` True

    it "returns true if the number of wrong guesses exceeds the limit" $ do
      let state = GameState "h" "abcdefg" 6
      isFinished state `shouldBe` True

    it "returns false if the word is not guess and guesses remain" $ do
      let state = GameState "h" "a" 6
      isFinished state `shouldBe` False

  describe "guessedWord" $ do
    it "returns true if the guesses include all letters" $ do
      let state = GameState "food" "fod" 6
      guessedWord state `shouldBe` True

    it "returns false if the guesses do not include all letters" $ do
      let state = GameState "food" "fd" 6
      guessedWord state `shouldBe` False

  describe "outOfGuesses" $ do
    it "returns true if the number of wrong guesses exceeds the limit" $ do
      let state = GameState "h" "abcdefg" 6
      outOfGuesses state `shouldBe` True

    it "returns false if the number of wrong guesses is below the limit" $ do
      let state = GameState "abcdefg" "abcdefg" 6
      outOfGuesses state `shouldBe` False

  describe "wrongGuesses" $ do
    it "returns the guesses not in the word" $ do
      let state = GameState "abc" "abcdef" 6
      wrongGuesses state `shouldBe` "def"

  describe "numberWrongGuesses" $ do
    it "returns the number of gueeses not in the word" $ do
      let state = GameState "abc" "abcdef" 6
      numberWrongGuesses state `shouldBe` 3
