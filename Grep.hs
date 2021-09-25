{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

module Grep where

import Turtle
import qualified Control.Foldl as Fold
import "Glob" System.FilePath.Glob (glob)
import qualified Data.ByteString.Lazy as BS
import qualified Codec.Compression.GZip as GZip

-- Find the first and last lines of each gzipped file
-- So that we know where to direct our searches
-- generateBounds = do
--   let zipFiles = fold (findtree (ends ".gz") (ls ".")) Fold.list

  -- heads <- fmap zhead zipFiles
  -- print heads
  -- where
  -- zhead fn = fold (inshell ("zcat " <> format fn) empty) Fold.head
  -- ztail fn = fold (inshell ("zcat " <> fn) empty) Fold.last

-- Gets the first or last line, if you give it Fold.head or Fold.last
-- getLine :: Fold a (Maybe a) -> Turtle.FilePath -> Line
-- getLine foldfunction filename = do
--   let shellCommand = inshell ("zcat " <> (format fn)) empty


wordBounds fn = do
  content <- fmap GZip.decompress (BS.readFile fn)
  let lines = BS.split 10 content
  let firstWord = head $ BS.split 9 $ head lines
  let lastWord = head $ BS.split 9 $ last lines
  print (fn, firstWord, lastWord)

main :: IO ()
main = do
  files <- glob "*.gz"
  bounds <- mapM_ wordBounds files
  print bounds


  -- let zipFiles = findtree (ends ".gz") (ls ".")
  -- view $ select [1, 2, 3]
  -- view $ generateBounds
  -- view $ grep (contains "Indonesia") $ inshell "zcat 2-00007-of-00047.gz" empty
