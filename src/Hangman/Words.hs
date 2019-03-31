{-# LANGUAGE OverloadedStrings #-}

module Hangman.Words (getRandomWord) where

import System.Random (randomRIO)
import qualified Text.HTML.Scalpel as S


getRandomWord :: IO String
getRandomWord = getRemoteWord >>= extractWord
    where extractWord (Just a) = pure a
          extractWord Nothing = getLocalWord

-- local words

newtype WordList = WordList [String]

minWordLength = 5 :: Int
maxWordLength = 9 :: Int

localWordSource = "data/dict.txt"

allWords :: IO WordList
allWords = readFile localWordSource >>= pure . WordList . lines

gameWords :: IO WordList
gameWords = allWords >>= (\(WordList aw) -> pure $ WordList $ filter haveAppropriateLength aw)

haveAppropriateLength :: String -> Bool
haveAppropriateLength w = l >= minWordLength && l < maxWordLength
    where l = length w

randomWord :: WordList -> IO String
randomWord (WordList wl) = randomRIO (minIndex, maxIndex) >>= pure . (!!) wl
    where minIndex = 0
          maxIndex = (length wl) - 1

getLocalWord :: IO String
getLocalWord = gameWords >>= randomWord

-- remote words

remoteWordSource = "https://randomword.com/"

getRemoteWord :: IO (Maybe String)
getRemoteWord = S.scrapeURL remoteWordSource pageWord
    where pageWord = S.chroot selector $ S.text S.anySelector
          selector = "div" S.@: ["id" S.@= "random_word"]
