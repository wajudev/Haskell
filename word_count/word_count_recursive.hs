-- This defines the WordCounter module, which exports several types and functions.
module WordCounter (
  Word,
  Count,
  WordCount,
  countWords,
  countWordsInFile,
  mergeCounts,
  main
) where

-- These lines import various modules that the code depends on.
import Control.Applicative ((<$>))
import Data.Char (toLower)
import Data.List (group, sort)
import System.Environment (getArgs)
import System.Directory (doesDirectoryExist, getDirectoryContents)

-- This line hides the Word type from the Prelude module, so that it doesn't clash with the Word type defined in this module.
import Prelude hiding (Word)

-- Defines two type aliases: Word for a string representing a word, and Count for an integer representing the count of a word.
type Word = String
type Count = Int

-- Defines a new data type WordCount, which represents a word and its count.
data WordCount = WordCount Word Count deriving (Eq, Show)

-- This block of code defines an instance of the Ord typeclass for WordCount, which specifies how WordCount values should be ordered.
instance Ord WordCount where
  compare (WordCount wrd1 cnt1) (WordCount wrd2 cnt2)
    | cnt1 /= cnt2  = compare cnt2 cnt1
    | otherwise     = compare wrd1 wrd2

-- This function takes a list of words and returns a WordCount value representing the first word in the list and the count of the entire list.
toWordCount :: [Word] -> WordCount
toWordCount ws = WordCount (head ws) (length ws)

-- This function takes a character and returns True if it should be considered part of a word, and False otherwise.
isWordElem :: Char -> Bool
isWordElem = (`notElem` ",.;:?!/'\"()[]--*")

-- This function takes a string and returns a list of the words in the string. It does this by first converting the string to lowercase, then filtering out any characters that shouldn't be considered part of a word, and finally using the words function to split the resulting string into a list of words
wordList :: String -> [Word]
wordList = words . map toLower . filter isWordElem

-- This function takes a string and returns a list of WordCount values representing the counts of each word in the string. It does this by first converting the string to a list of words using the wordList function, then grouping the words together and counting them using the toWordCount function, and finally sorting the resulting list by the count and the word using the sort function.
countWords :: String -> [WordCount]
countWords = sort . map toWordCount . group . sort . wordList

-- Reads content of a file, and uses a list comprehension to extract the word and count from each WordCount data constructor in the wordCounts list and return a list of word-count pairs.
countWordsInFile :: FilePath -> IO [(Word, Count)]
countWordsInFile file = do
  content <- readFile file
  let wordCounts = countWords content
  return [(w, c) | WordCount w c <- wordCounts]

-- This a function that takes two lists of word-count pairs and returns their merged version as a single list, where each word has the sum of the counts from both input lists.
mergeCounts :: [(Word, Count)] -> [(Word, Count)] -> [(Word, Count)]
mergeCounts [] ys = ys
mergeCounts xs [] = xs
mergeCounts ((x,w1):xs) ((y,w2):ys)
  | w1 > w2   = (x,w1) : mergeCounts xs ((y,w2):ys)
  | otherwise = (y,w2) : mergeCounts ((x,w1):xs) ys

-- This  function  takes a directory path and a file extension and returns a list of word-count pairs for all files in the directory with that extension.
countWordsInDirectory :: FilePath -> String -> IO [(Word, Count)]
countWordsInDirectory path ext = do
  isDir <- doesDirectoryExist path
  if not isDir
    then countWordsInFile path
    else do
      let isTxtFile = (== ext) . reverse . take 3 . reverse
      files <- filter isTxtFile <$> getDirectoryContents path
      let paths = map ((path ++ "/") ++) files
      wordCounts <- mapM countWordsInFile paths
      return $ foldr mergeCounts [] wordCounts

main :: IO ()
main = do
  args <- getArgs
  if length args /= 2
    then putStrLn "Please specify a directory and a file extension."
    else do
      let [path, ext] = args
      wordCounts <- countWordsInDirectory path ext
      mapM_ print wordCounts
