module Hangman.Game where

import Control.Monad (forever, when)
import Data.Char (toLower)
import Data.Maybe (isJust) 
import System.Exit (exitSuccess) 
import System.IO (hSetBuffering, stdout, BufferMode(NoBuffering))

import Hangman.Puzzle (Puzzle(..), fillInCharacter, freshPuzzle, charInPuzzleWord, alreadyGuessed)
import Hangman.Words (getRandomWord)

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do 
    putStrLn $ "Your guess was: " <> [guess]
    let currentState = (charInPuzzleWord puzzle guess, alreadyGuessed puzzle guess)
    case currentState of
        (_, True)  -> putStrLn "You already guessed that character!" >> pure puzzle
        (True, _)  -> putStrLn "This character was in the word, congrats!" >> pure newPuzzle
        (False, _) -> putStrLn "This character wasn't in the word!" >> pure newPuzzle
    where newPuzzle = fillInCharacter puzzle guess

gameOver :: Puzzle -> IO ()
gameOver (Puzzle wordToGuess _ guessed) =
    when isGameOver $ do
        putStrLn "You lose!"
        putStrLn ("The word was: " <> wordToGuess) >> exitSuccess
    where isGameOver = (length guessed) > 7

gameWin :: Puzzle -> IO ()
gameWin (Puzzle _ filledInSoFar _) =
    when isGameWon $ putStrLn "You win!" >> exitSuccess
    where isGameWon = all isJust filledInSoFar

runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
    gameOver puzzle
    gameWin puzzle
    putStrLn $ "Current: " <> show puzzle
    putStr "Guess a letter: "
    guess <- getLine
    case guess of
        [c] -> handleGuess puzzle c >>= runGame
        _ -> putStrLn "You may only guess a sinle character"

startGame :: IO ()
startGame = do
    hSetBuffering stdout NoBuffering
    word <- getRandomWord
    runGame (freshPuzzle (fmap toLower word))