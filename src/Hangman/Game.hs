module Hangman.Game where

import Control.Monad (forever, when)
import Data.Char (toLower)
import Data.Maybe (isJust) 
import System.Exit (exitSuccess) 
import System.IO (hSetBuffering, stdout, BufferMode(NoBuffering))

import Hangman.Words (getRandomWord)
import Hangman.Puzzle ( Puzzle(..)
                      , fillInCharacter
                      , freshPuzzle
                      , charInPuzzleWord
                      , alreadyGuessed
                      , maxAttempts)


handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do 
    putStrLn $ "Your guess was: " <> [guess]
    let currentState = (charInPuzzleWord puzzle guess, alreadyGuessed puzzle guess)
    case currentState of
        (_, True)  -> putStrLn "You already guessed that character!" >> pure puzzle
        (True, _)  -> putStrLn "This character was in the word, congrats!" >> correctGuess
        (False, _) -> putStrLn "This character wasn't in the word!" >> inCorrectGuess
    where gameGuess = fillInCharacter puzzle guess
          correctGuess = pure $ gameGuess 0
          inCorrectGuess = pure $ gameGuess 1

gameOver :: Puzzle -> IO ()
gameOver (Puzzle wordToGuess _ _ attempts) =
    when isGameOver $ do
        putStrLn "You lose!"
        putStrLn ("The word was: " <> wordToGuess) >> exitSuccess
    where isGameOver = attempts > maxAttempts

gameWin :: Puzzle -> IO ()
gameWin (Puzzle _ filledInSoFar _ _) =
    when isGameWon $ putStrLn "You win!" >> exitSuccess
    where isGameWon = all isJust filledInSoFar

clear :: IO ()
clear = putStr "\ESC[2J"

runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
    gameOver puzzle
    gameWin puzzle
    clear
    putStrLn $ "Current: " <> show puzzle
    putStr "\nGuess a letter: "
    guess <- getLine
    case guess of
        [c] -> handleGuess puzzle c >>= runGame
        _ -> putStrLn "You may only guess a sinle character"


startGame :: IO ()
startGame = do
    hSetBuffering stdout NoBuffering
    putStrLn "Getting the random word..."
    word <- getRandomWord
    runGame (freshPuzzle (fmap toLower word))