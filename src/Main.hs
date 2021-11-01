{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import qualified Codec.Compression.GZip as GZip
import qualified Control.Foldl as Fold
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as Char8
import qualified Data.ByteString.Lazy.UTF8 as UTF
import           Data.List
import qualified Data.Text as T
import           GHC.Generics (Generic)
import "Glob"    System.FilePath.Glob (glob)
import           Text.JSON.Generic
import           Text.Regex.TDFA.ByteString.Lazy
import           Text.Regex.TDFA.Common
import           Turtle
import Text.Regex.TDFA
import Data.Either (fromRight)
import GHC.Arr ((!))
import GHC.Int (Int64)
import Data.Word8 (_lf)



-- Find the first and last lines of each gzipped file
-- So that we know where to direct our searches

data WordBounds = WordBounds { zipFile :: Prelude.FilePath
                             , start :: BS.ByteString
                             , end :: BS.ByteString
                             } deriving (Show, Generic, Data)

boundsDir = "data/"
boundsFile = boundsDir ++ "bounds.json"
boundsFileP = decodeString boundsFile

-- To select the file for our given query, we use two filters:
-- The first one selects the first number (the n- value).
-- The second one looks up of first word of the query.

-- | Find the ngram n- value for a WordBounds filename
boundsToN :: WordBounds -- ^ the WordBounds data structure to be extracted from
  -> Int -- ^ the n-value of that filename
boundsToN f = read $ take 1 $ encodeString $ basename $ decodeString $ zipFile f


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

-- | We have a URL filepath like "http://storage.googleapis.com/books/ngrams/books/20200217/eng-fiction/1-00000-of-00001.gz"
-- But we want a filepath like "data/1-00000-of-00001.gz"
remoteToLocal :: Prelude.FilePath -> Prelude.FilePath
remoteToLocal = (boundsDir ++) . encodeString . filename . decodeString

parser :: Parser Text
parser = optText "query" 'q' "Word(s) you want to look up, with optional _POSs."

greeting :: String
greeting = concat [ "This program searches Google Books ngram data for advanced queries."
                  , "\nExamples: " ,  "\n -q Dracula "
                  , "\n -q bank_NOUN " ,  "\n -q \"river bank_NOUN\""]
main :: IO ()
main = do
  -- Test whether we have WordBounds
  -- exists <- testfile boundsFileP
  -- unless exists writeWordBounds
  boundsJSON <- readFile boundsFile
  let bounds = decodeJSON boundsJSON :: [WordBounds]
  query <- options (fromString greeting) parser
  -- let selectedFile = selectFile bounds query
  let queryUTF = UTF.fromString (T.unpack query)
  let filename = selectFile queryUTF bounds
  print filename
  let localFilename = remoteToLocal filename
  print localFilename
  content <- fmap GZip.decompress (BS.readFile localFilename)
  -- stdout $ grep "Dracula" content
  let regexCompiled = compile defaultCompOpt defaultExecOpt "Dracula"
  let compiled = case regexCompiled of
                   Left err -> error "Can't compile expression"
                   Right compiled -> compiled
  let result = execute compiled content
  let location = case result of
        Left _ -> error "Can't find expression"
        Right (Just xs) -> fst $ xs ! 0 :: Int
  print $ BS.takeWhile (/= _lf) $ BS.drop (fromIntegral location :: Int64) content
  -- let grepped = inproc "rg" ["-z", "-e", query, T.pack localFilename] Turtle.empty
                          -- T.pack query, format fp filename] empty
  -- view grepped
