{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import qualified Data.ByteString.Char8 as B8
import Network.HTTP.Simple
    ( getResponseBody, httpBS, parseRequest )
import Network.URI ( pathSegments )
import Text.Printf ( printf )
import Network.HTTP.Client.Conduit ( getUri, Request )
import Data.ByteString.Char8 (ByteString)
import Turtle hiding ( printf )
import Data.Text (pack)

{-

Version 20200217 of the English Fiction ngrams dataset.

URLs look like this:

http://storage.googleapis.com/books/ngrams/books/20200217/eng-fiction/1-00000-of-00001.gz
http://storage.googleapis.com/books/ngrams/books/20200217/eng-fiction/2-00001-of-00047.gz
http://storage.googleapis.com/books/ngrams/books/20200217/eng-fiction/3-00000-of-00549.gz
http://storage.googleapis.com/books/ngrams/books/20200217/eng-fiction/4-00000-of-00515.gz
http://storage.googleapis.com/books/ngrams/books/20200217/eng-fiction/5-00000-of-01449.gz

-}


formatUrl :: (Int, Int) -> [String]
formatUrl (a, b) = [ prefix ++ printf "%1d-%05d-of-%05d.gz" a n b | n <- [0..b-1] ] where
  prefix = "http://storage.googleapis.com/books/ngrams/books/20200217/eng-fiction/"


-- wget :: Request -> IO ()
-- wget url = do
--   resp <- httpBS url
--   let fileName = Prelude.last . pathSegments . getUri $ url
--   let body = getResponseBody resp :: ByteString
--   B8.writeFile fileName body
  -- print fileName

wget :: String -> IO ()
wget url = do
  req <- parseRequest url
  let downloadDir = "data/"
  let fileName = Prelude.last . pathSegments . getUri $ req
  let dest = downloadDir ++ fileName
  -- Does it exist?
  exists <- testfile $ decodeString dest
  if exists then putStrLn ("File " <> dest <> " already exists.")
    else view $ inshell ("wget -O " <> pack dest <> " " <> pack url) empty

main :: IO ()
main = do
  let patterns = [(1, 1), (2, 47), (3, 549), (4, 515), (5, 1449)]
  let urls :: [String] = concatMap formatUrl patterns

  mapM_ wget urls

  -- let url = "http://storage.googleapis.com/books/ngrams/books/datasetsv3.html"
  -- contents <- httpBS url
  -- print contents
  -- 2. Get list of article URLs
  -- let doc = fromUrl url
  -- urlList <- runX $ doc >>> css "h3" /> getText
  -- print urlList
  -- 3. For each article URL, get the HTML file
  -- B8.putStrLn . getResponseBody $ contents
  -- let doc = fromUrl "1921apr-1.html"
  -- text <- runX $ doc >>> css "div.sqs-block-html" -- //> getText
  -- title <- runX $ doc >>> css "h1" /> getText
  -- print text
