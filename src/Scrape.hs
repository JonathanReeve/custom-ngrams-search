{-# LANGUAGE OverloadedStrings #-}

-- Scrapes start and end bounds for Ngrams zip files.
-- E.g., in the 2-grams index, http://storage.googleapis.com/books/ngrams/books/20200217/eng/eng-2-ngrams_exports.html
-- the file at http://storage.googleapis.com/books/ngrams/books/20200217/eng/2-00039-of-00589.gz
-- starts "155_NUM counts_NOUN" and ends (implicitly) before the next one,
-- 163_NUM 2.90. So we get all those bounds.
-- Usage: runhaskell Scrape

module Scrape where

import Network.HTTP.Client.Conduit ( getUri, Request )
import Text.XML.HXT.Core
import Text.HandsomeSoup
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as C8
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.UTF8 as UTF
import Text.JSON.Generic

import Main hiding (main)

scrape url = runX $ fromUrl url >>> css "li a" >>> (getAttrValue "href" &&& (deep getText))

-- We have a pair of links, and we want to get the bounds from the second and add them to the first.
-- Let's make a WordBounds object of it.
findEnds :: ((String, String), (String, String)) -> WordBounds
findEnds ((url, start), (url', start')) = WordBounds { zipFile = url, start = C8.pack start, end = C8.pack start' }

main = do
  let urls = ["http://storage.googleapis.com/books/ngrams/books/20200217/eng-fiction/eng-fiction-"
              <> show n <> "-ngrams_exports.html" | n <- [1..3]]
  let url = urls !! 1
  links <- mapM scrape urls
  let links' = concat links -- flatten
  -- print links
  let zipped = zip links' (tail links')
  let bounds = map findEnds zipped
  let boundsEncoded = encodeJSON bounds
  BS.writeFile "bounds.json" (UTF.fromString boundsEncoded)
  -- print boundsEncoded
