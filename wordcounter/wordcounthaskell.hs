module WordCounter (
  Word,
  Count,
  WordCount,
  countWords,
  countWordsInFile,
  mergeCounts,
  main
) where

import Control.Applicative ((<$>))
import Data.Char (toLower)
import Data.List (group, sort)
import System.Environment (getArgs)
import System.Directory (doesDirectoryExist, getDirectoryContents)

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
isWordElem = (`notElem` ",.;:?!/'\"()[]--*")

wordList :: String -> [Word]
wordList = words . map toLower . filter isWordElem

countWords :: String -> [WordCount]
countWords = sort . map toWordCount . group . sort . wordList

countWordsInFile :: FilePath -> IO [(Word, Count)]
countWordsInFile file = do
  content <- readFile file
  let wordCounts = countWords content
  return [(w, c) | WordCount w c <- wordCounts]

mergeCounts :: [(Word, Count)] -> [(Word, Count)] -> [(Word, Count)]
mergeCounts [] ys = ys
mergeCounts xs [] = xs
mergeCounts ((x,w1):xs) ((y,w2):ys)
  | w1 > w2   = (x,w1) : mergeCounts xs ((y,w2):ys)
  | otherwise = (y,w2) : mergeCounts ((x,w1):xs) ys

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

--main :: IO ()
--main = do
  --args <- getArgs
  --if length args /= 2
    --then putStrLn "Please specify a directory and a file extension."
    --else do
      --let [path, ext] = args
      --wordCounts <- countWordsInDirectory path ext
      --mapM_ print wordCounts


 -- Without console
main :: IO ()
main = do
    args <- getArgs
    let (path, ext) = case args of
                     [p, e] -> (p, e)
                     _      -> ("/Users/waju/Desktop/Haskell/wordcounter", ".txt")
    wordCounts <- countWordsInDirectory path ext
    mapM_ print wordCounts