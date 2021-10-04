{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Data.List
import qualified Control.Foldl as Fold
import "Glob" System.FilePath.Glob (glob)
import qualified Codec.Compression.GZip as GZip
import Text.JSON.Generic
import GHC.Generics (Generic)
import Turtle
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.UTF8 as UTF
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as Char8

-- Find the first and last lines of each gzipped file
-- So that we know where to direct our searches

data WordBounds = WordBounds { zipFile :: Prelude.FilePath
                             , start :: BS.ByteString
                             , end :: BS.ByteString
                             } deriving (Show, Generic, Data)

-- Gets the first words of the first and last lines.
wordBounds :: Prelude.FilePath -> IO WordBounds
wordBounds fn = do
  content <- fmap GZip.decompress (BS.readFile fn)
  let lines = BS.split 10 content
  let firstWord = head $ BS.split 9 $ head lines
  -- Handle ending lines with no tabs in them (blank lines?)
  let noTab line = null (BS.split 9 line)
  let cleanLast = dropWhileEnd noTab lines
  let lastWord = head $ BS.split 9 $ last cleanLast
  return WordBounds { zipFile = fn, start = firstWord, end = lastWord }

boundsFile = "data/bounds.json"
boundsFileP = decodeString boundsFile

writeWordBounds = do
  files <- glob "data/*.gz"
  bounds <- mapM wordBounds files
  let jsondata = encodeJSON bounds
  BS.writeFile boundsFile (UTF.fromString jsondata)

-- | Select the file we need, based on the query.
-- selectFile query =

-- To select the file for our given query, we use two filters:
-- The first one selects the first number (the n- value).
-- The second one looks up of first word of the query.

-- | Find the ngram n- value for a WordBounds filename
boundsToN :: WordBounds -- ^ the WordBounds data structure to be extracted from
  -> Int -- ^ the n-value of that filename
boundsToN f = read $ take 1 $ drop 5 $ zipFile f

-- | Is our query in bounds for the n-value of the target filename?
-- That is, if our query is a 1-gram, does our filename start with "1-"?
inBoundsForN :: UTF.ByteString -- ^ The query
  -> WordBounds                -- ^ The bounds to compare to
  -> Bool                      -- ^ Does it start with the right number?
inBoundsForN q wb = length (Char8.words q) == boundsToN wb

-- | Compare first query word to starts of remaining files
withinBounds :: UTF.ByteString -- ^ The query string
  -> WordBounds                 -- ^ The bounds to compare to
  -> Bool                       -- ^ Whether it is in bounds for the file
withinBounds q wb = case length (Char8.words q) of
  0 -> error "You must supply a query."
  1 -> True -- One-grams are all in one file
  _ -> q > start wb && q < end wb

selectFile :: UTF.ByteString -- ^ The query string
  -> [WordBounds]            -- ^ The bounds to compare to
  -> Prelude.FilePath
selectFile q wbs = case filter (withinBounds q) (filter (inBoundsForN q) wbs) of
  [] -> error "Can't find an appropriate file. Maybe n-value is too big?"
  xs -> zipFile $ head xs


parser :: Parser Text
parser = optText "query" 'q' "Word(s) you want to look up, with optional _POSs."

greeting :: String
greeting = concat [ "This program searches Google Books ngram data for advanced queries."
                  , "\nExamples: " ,  "\n -q Dracula "
                  , "\n -q bank_NOUN " ,  "\n -q \"river bank_NOUN\""]
main :: IO ()
main = do
  -- Test whether we have WordBounds
  exists <- testfile boundsFileP
  unless exists writeWordBounds
  boundsJSON <- readFile boundsFile
  let bounds = decodeJSON boundsJSON :: [WordBounds]
  query <- options (fromString greeting) parser
  -- let selectedFile = selectFile bounds query
  let queryUTF = UTF.fromString (T.unpack query)
  let filename = selectFile queryUTF bounds
  print filename
  let grepped = inproc "rg" ["-z", "-e", query, T.pack filename] Turtle.empty
                          -- T.pack query, format fp filename] empty
  view grepped
