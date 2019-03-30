module Main where

import Control.Monad (forever, when)
import Data.Char (toLower)
import Data.Maybe (isJust) 
import Data.List (intersperse)
import System.Exit (exitSuccess) 
import System.Random (randomRIO)
import System.IO (hSetBuffering, stdout, BufferMode(NoBuffering))

type WordList = [String]

minWordLength = 5 :: Int
maxWordLength = 9 :: Int

allWords :: IO WordList
allWords = readFile "data/dict.txt" >>= pure . lines

gameWords :: IO WordList
gameWords = allWords >>= pure . filter haveAppropriateLength 

haveAppropriateLength :: String -> Bool
haveAppropriateLength w = l >= minWordLength && l < maxWordLength
    where l = length w

randomWord :: WordList -> IO String
randomWord wl = randomRIO (minIndex, maxIndex) >>= pure . (!!) wl
    where minIndex = 0
          maxIndex = (length wl) - 1

randomWord' :: IO String
randomWord' = gameWords >>= randomWord

---

data Puzzle = Puzzle String [Maybe Char] [Char]

instance Show Puzzle where
    show (Puzzle _ discovered guessed) = 
        (intersperse ' ' $ fmap renderPuzzleChar discovered)
        <> " Guessed so far: " <> guessed

freshPuzzle :: String -> Puzzle
freshPuzzle word = Puzzle word (fmap (const Nothing) word) []

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle word _ _) c = c `elem` word

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ guessed _) c = foldr check False guessed
    where check Nothing acc = acc
          check (Just a) acc = if c == a then True else acc

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar Nothing = '_'
renderPuzzleChar (Just a) = a

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word filledSoFar s) c =
    Puzzle word currentFilledSoFar (c : s)
    where
         zipper :: Char -> Char -> Maybe Char -> Maybe Char
         zipper guessed wordChar guessChar
            | wordChar == guessed = Just wordChar
            | otherwise = guessChar
        
         currentFilledSoFar :: [Maybe Char]
         currentFilledSoFar = zipWith (zipper c) word filledSoFar


handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do 
    putStrLn $ "Your guess was: " <> [guess]
    let currentState = (charInWord puzzle guess, alreadyGuessed puzzle guess)
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

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    word <- randomWord'
    runGame (freshPuzzle (fmap toLower word))
