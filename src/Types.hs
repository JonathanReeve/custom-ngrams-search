module Types where

import           Database.Persist
import           Database.Persist.Sqlite
import           Database.Persist.TH

data POS = Noun | Verb | Adj | Punct deriving (Show, Read, Eq)
derivePersistField "POS"

data Word = Word
  { orth :: String
  , pos :: Maybe POS
  } deriving (Show, Read, Eq)
derivePersistField "Word"

data Ngram = Ngram
  { nValue :: Int
  , w1 :: Word
  , w2 :: Maybe Word
  , w3 :: Maybe Word
  , w4 :: Maybe Word
  , w5 :: Maybe Word
  , years :: Text
  }
derivePersistField "Ngram"
