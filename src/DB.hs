{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE PackageImports #-}

-- |

module DB where

import           Control.Monad.IO.Class  (liftIO)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Codec.Compression.GZip as GZip
import           Data.Text as T hiding (tail, head, drop)
import           Database.Persist
import           Database.Persist.Sqlite
import           Database.Persist.TH
import           Prelude hiding (Word, words, unwords)
import           Data.Text as T hiding (tail, head, drop, drop, head, tail)
import           Data.Text.IO as TIO (readFile)
import "Glob"    System.FilePath.Glob (glob)
import           System.FilePath.Posix

import           Types
import Data.Text.Encoding (decodeUtf8)
import Data.ByteString.Lazy (toStrict)



share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Ngram
    n Int
    w1 Word Maybe
    w2 Word Maybe
    w3 Word Maybe
    w4 Word Maybe
    w5 Word Maybe
    years Text
    deriving Show
|]


-- '_. Skoal_NOUN 1913,2,2 1920,2,2 1929,1,1 1930,1,1 1931,1,1 1934,1,1 1942,2,2
-- 1946,1,1 1951,1,1 1955,2,2 1963,1,1 1970,2,2 1971,1,1 1975,1,1 1977,1,1
-- 1979,2,2 1983,1,1 1985,1,1 1986,1,1 1987,1,1 1989,1,1 1990,3,3 1991,1,1
-- 1992,1,1 1997,2,2 1998,1,1 1999,1,1 2001,8,1 2002,4,4 2003,5,4 2004,2,1
-- 2005,3,3 2006,2,2 2007,5,3 2008,5,5 2009,3,3 2010,2,2 2011,13,4 2012,6,6
-- 2013,6,5 2014,11,9 2015,6,4 2016,4,3 2017,7,4 2018,2,2 2019,5,3

-- parseYears = [1000..]

parseWord :: Text -> Maybe Word
parseWord w = case T.find (== '_') w of
  Nothing -> Just $ Word w Nothing -- No POS
  Just _ -> case w of
    "_END_" -> Nothing
    "__" -> Just $ Word "_" (Just Punct) -- Assuming "__" is an actual underscore
    _ -> case Prelude.filter (\s -> T.length s > 0) (splitOn "_" w) of
      [orth] -> Just $ Word orth Nothing -- Without POS
      [orth, pos] -> Just $ Word orth (parsePOS pos) -- with POS
      [orth, pos, xs] -> Nothing -- error $ "Got extra stuff! : " ++ T.unpack w
      _ -> Nothing -- error $ "Got unexpected input! : " ++ T.unpack w

parsePOS :: Text -> Maybe POS
parsePOS posRaw = case posRaw of
  "NOUN" -> Just Noun
  "VERB" -> Just Verb
  "ADJ" -> Just Adj
  "NUM" -> Just Num
  "PRT" -> Just Prt
  "ADP" -> Just Adp
  "." -> Just Punct
  "" -> Nothing
  _ -> Nothing


parseLine :: Int -> Text -> Ngram
parseLine n line =
  let lineWords = words line
      firstWord = head lineWords
  in case n of
    1 -> let w1 = parseWord firstWord
             yearsData = unwords (tail lineWords)
         in Ngram 1 w1 Nothing Nothing Nothing Nothing yearsData
    2 -> let w1 = parseWord (head lineWords)
             w2 = parseWord (head (tail lineWords))
             yearsData = unwords (drop 2 lineWords)
         in Ngram 2 w1 w2 Nothing Nothing Nothing yearsData

-- parseYears :: Text -> [YearData]
-- parseYears years = do
--   let yearList =  T.splitOn "," <$> words years :: [[Text]]
--   let yearsAsInts = fmap (fmap (read . T.unpack)) yearList :: [[Int]]
--   fmap parseYearDatum yearsAsInts

-- parseYearDatum :: [Int] -> YearData
-- parseYearDatum [y, a, b] = YearData y a b

-- processFile :: FilePath -> IO [Key Ngram]
processFile path = do
  -- Infer ngram n-value from the first character of the filename
  putStrLn $ "Now processing file: " ++ path
  let n = read $ Prelude.take 1 $ takeFileName path :: Int
  rawContent <- BSL.readFile path
  let decompressed = GZip.decompress rawContent
  let content = decodeUtf8 $ toStrict decompressed
  let parsedNgrams = fmap (parseLine n) (T.lines content) :: [Ngram]
  runSqlite "ngrams.db" $ do
    runMigration migrateAll
    insertMany parsedNgrams

makeDB :: IO ()
makeDB = do
  paths <- glob "../data/*.gz"
  print paths
  mapM_ processFile paths

testDB :: IO ()
testDB = runSqlite "ngrams.db" $ do
    let w1 = Just $ Word "," Nothing
    let w2 = Just $ Word "narratology" Nothing
    selected <- selectList [NgramW1 ==. w1, NgramW2 ==. w2] [LimitTo 1]
    liftIO $ print selected

main :: IO ()
main = do
  -- makeDB
  testDB
