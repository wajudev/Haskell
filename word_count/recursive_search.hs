import System.Directory
import System.FilePath
import Control.Monad (filterM)

findFilesWithExtension :: FilePath -> String -> IO [FilePath]
findFilesWithExtension dir ext = do
  contents <- listDirectory dir
  let paths = map (dir </>) contents
  files <- filterM doesFileExist paths
  let matchingFiles = filter (\file -> takeExtension file == ext) files
  subdirs <- filterM doesDirectoryExist paths
  matchingSubdirFiles <- mapM (\subdir -> findFilesWithExtension subdir ext) subdirs
  return $ matchingFiles ++ concat matchingSubdirFiles



main :: IO ()
main = do
  let dir = "/Users/waju/Desktop"
      ext = ".txt"
  matchingFiles <- findFilesWithExtension dir ext
  mapM_ putStrLn matchingFiles
