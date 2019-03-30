module Hangman.Puzzle 
( Puzzle(..)
, fillInCharacter
, freshPuzzle
, charInPuzzleWord
, alreadyGuessed
) where

import Data.List (intersperse)

data Puzzle = Puzzle String [Maybe Char] [Char]

instance Show Puzzle where
    show (Puzzle _ discovered guessed) = 
        (intersperse ' ' $ fmap renderPuzzleChar discovered)
        <> " Guessed so far: " <> guessed

freshPuzzle :: String -> Puzzle
freshPuzzle word = Puzzle word (fmap (const Nothing) word) []

charInPuzzleWord :: Puzzle -> Char -> Bool
charInPuzzleWord (Puzzle word _ _) c = c `elem` word

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