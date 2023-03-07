import System.Directory
import System.FilePath
import Data.List

type WordCount = (String, Int)

countLines :: FilePath -> IO Int
countLines =  fmap (length . lines) . readFile


count :: Eq a => a -> [a] -> Int
count x xs = length [x' | x' <- xs, x==x']


countChars :: Char -> FilePath -> IO Int
countChars c f = count c <$> readFile f


main :: IO ()
main = do
    cnt <- countChars 'g' "sample-3mb-text-file.txt"
    print cnt
