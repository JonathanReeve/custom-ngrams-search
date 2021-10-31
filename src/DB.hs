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

-- |

module DB where

import           Control.Monad.IO.Class  (liftIO)
import           Database.Persist
import           Database.Persist.Sqlite
import           Database.Persist.TH
import           Prelude hiding (Word)
import           Data.Text

import           Types

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
NgramData
    gram Ngram
    deriving Show
|]


-- '_. Skoal_NOUN 1913,2,2 1920,2,2 1929,1,1 1930,1,1 1931,1,1 1934,1,1 1942,2,2
-- 1946,1,1 1951,1,1 1955,2,2 1963,1,1 1970,2,2 1971,1,1 1975,1,1 1977,1,1
-- 1979,2,2 1983,1,1 1985,1,1 1986,1,1 1987,1,1 1989,1,1 1990,3,3 1991,1,1
-- 1992,1,1 1997,2,2 1998,1,1 1999,1,1 2001,8,1 2002,4,4 2003,5,4 2004,2,1
-- 2005,3,3 2006,2,2 2007,5,3 2008,5,5 2009,3,3 2010,2,2 2011,13,4 2012,6,6
-- 2013,6,5 2014,11,9 2015,6,4 2016,4,3 2017,7,4 2018,2,2 2019,5,3

parseWord = span (/= '_')

parseLine :: String -> Int -> Ngram
parseLine line n = case n of
  1 -> let (w1, w1pos) = parseWord (head (words line))
           yearsData = unwords (tail (words line))
    in Ngram 1 w1 (Just w1pos) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing yearsData
  2 -> let (w1, w1pos) = parseWord (head (words line))
           (w2, w2pos) = parseWord (head (tail (words line)))
           yearsData = unwords (drop 2 (words line))
    in Ngram 2 w1 (Just w1pos) (Just w2) (Just w2pos) Nothing Nothing Nothing Nothing Nothing Nothing yearsData

main :: IO ()
main = runSqlite "test.db" $ do
    runMigration migrateAll
    let testItem = Ngram 2 "test" (Just "NOUN") (Just "testing") (Just "NOUN") Nothing Nothing Nothing Nothing Nothing Nothing ""
    insert testItem
    selected <- selectList [NgramFirstWord ==. "test"] [LimitTo 1]
    liftIO $ print selected
