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

module Types where

import           Database.Persist
import           Database.Persist.Sqlite
import           Database.Persist.TH
import           Data.Text
import           Prelude hiding (Word)

data POS = Noun | Verb | Adj | Conj | Prt | Adp | Punct | Num | End deriving (Show, Read, Eq)
derivePersistField "POS"

data Word = Word
  { orth :: Text
  , pos :: Maybe POS
  } deriving (Show, Read, Eq)
derivePersistField "Word"

-- data YearData = YearData
--   { year :: Int
--   , occurrencesA :: Int
--   , occurrencesB :: Int
--   } deriving (Show, Read, Eq)

type YearData = (Int, Int, Int)
type YearsData = [YearData]
derivePersistField "YearsData"
