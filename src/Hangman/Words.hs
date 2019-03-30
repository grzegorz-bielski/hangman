module Hangman.Words (getRandomWord) where

import System.Random (randomRIO)

newtype WordList = WordList [String]

minWordLength = 5 :: Int
maxWordLength = 9 :: Int

allWords :: IO WordList
allWords = readFile "data/dict.txt" >>= pure . WordList . lines

gameWords :: IO WordList
gameWords = allWords >>= (\(WordList aw) -> pure $ WordList $ filter haveAppropriateLength aw)

haveAppropriateLength :: String -> Bool
haveAppropriateLength w = l >= minWordLength && l < maxWordLength
    where l = length w

randomWord :: WordList -> IO String
randomWord (WordList wl) = randomRIO (minIndex, maxIndex) >>= pure . (!!) wl
    where minIndex = 0
          maxIndex = (length wl) - 1

getRandomWord :: IO String
getRandomWord = gameWords >>= randomWord
