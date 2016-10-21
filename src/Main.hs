module Main where

import Control.Monad                 (filterM)
import Control.Applicative           ((<$>))
import System.Environment            (getArgs)
import System.FilePath               ((</>))
import System.Directory              (getDirectoryContents, doesFileExist)
import Data.ByteString               (readFile)
import Codec.Archive.Zip             (ZipArchive, mkEntrySelector, addEntry, createArchive, CompressionMethod(..))
import Path                          (parseRelFile)
import Prelude                       hiding (readFile)

prepareFiles :: FilePath -> IO (ZipArchive ())
prepareFiles file = do
    fn <- parseRelFile (file) >>= mkEntrySelector
    cn <- readFile (file)
    return $ addEntry Deflate cn fn

main :: IO ()
main = do
  (dir:_)   <- getArgs
  files     <- map (dir </>) <$> getDirectoryContents dir
  filesOnly <- filterM doesFileExist files
  zipPath   <- parseRelFile "archive.zip"
  prepared  <- sequence $ map prepareFiles filesOnly
  createArchive zipPath $ do
    foldr1 (>>) prepared
  putStrLn "Done."