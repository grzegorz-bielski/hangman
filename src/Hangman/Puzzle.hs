module Hangman.Puzzle 
( Puzzle(..)
, fillInCharacter
, freshPuzzle
, charInPuzzleWord
, alreadyGuessed
, maxAttempts
) where

import Data.List (intersperse)

type PuzzleWord = String
type Discovered = [Maybe Char]
type Guessed = [Char]
type Attempts = Int

maxAttempts = 6 :: Int

data Puzzle = Puzzle PuzzleWord Discovered Guessed Attempts

instance Show Puzzle where
    show (Puzzle _ discovered guessed attempts) = 
        (addSpace $ fmap renderPuzzleChar discovered)
        <> "\n"
        <> "\nGuessed so far: " <> addSpace guessed
        <> "\nLives left: " <> show (maxAttempts - attempts)

addSpace :: String -> String
addSpace = intersperse ' '

freshPuzzle :: String -> Puzzle
freshPuzzle word = Puzzle word (fmap (const Nothing) word) [] 0

charInPuzzleWord :: Puzzle -> Char -> Bool
charInPuzzleWord (Puzzle word _ _ _) c = c `elem` word

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ guessed _ _ ) c = foldr check False guessed
    where check Nothing acc = acc
          check (Just a) acc = if c == a then True else acc

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar Nothing = '_'
renderPuzzleChar (Just a) = a

fillInCharacter :: Puzzle -> Char -> Int -> Puzzle
fillInCharacter (Puzzle word filledSoFar s a) c attempt =
    Puzzle word currentFilledSoFar (c : s) (a + attempt)
    where
        zipper :: Char -> Char -> Maybe Char -> Maybe Char
        zipper guessed wordChar guessChar
            | wordChar == guessed = Just wordChar
            | otherwise = guessChar
        
        currentFilledSoFar :: [Maybe Char]
        currentFilledSoFar = zipWith (zipper c) word filledSoFar