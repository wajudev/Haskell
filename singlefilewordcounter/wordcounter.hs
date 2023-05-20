module WordCounter (
  Word,
  Count,
  WordCount,
  countWords,
  main
) where

import Control.Applicative ((<$>))
import Data.Char (toLower)
import Data.List (group, sort)
import System.Environment (getArgs)

import Prelude hiding (Word)

type Word = String
type Count = Int

data WordCount = WordCount Word Count deriving (Eq, Show)
instance Ord WordCount where
  compare (WordCount wrd1 cnt1) (WordCount wrd2 cnt2)
    | cnt1 /= cnt2  = compare cnt2 cnt1
    | otherwise     = compare wrd1 wrd2

toWordCount :: [Word] -> WordCount
toWordCount ws = WordCount (head ws) (length ws)

isWordElem :: Char -> Bool
isWordElem = (`notElem` ",.;:?!/'\"()[]")

wordList :: String -> [Word]
wordList = words . map toLower . filter isWordElem

countWords :: String -> [WordCount]
countWords = sort . map toWordCount . group . sort . wordList



main :: IO ()
main = do
  args <- getArgs
  if length args /= 1
    then putStrLn "Please specify an input file."
    else do
      let (f:_) = args
      countWords <$> readFile f >>= print