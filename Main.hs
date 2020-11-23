#! /usr/bin/env nix-shell
#! nix-shell -i runghc --packages "ghc.withPackages (pkgs: with pkgs; [ turtle lucid aeson ])"

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
 
import Turtle
import Control.Foldl as F (list, length)
import Prelude hiding (readFile, writeFile, FilePath)
import Data.List as L hiding (find)
import Data.Function (on)
import qualified Data.Text as T hiding (find)
import Data.Map as M hiding (fold)
import Lucid
import Data.Aeson
import GHC.Generics (Generic)
import qualified Data.ByteString.Lazy as B

data Counts = Counts { text :: Text
                     , size :: Int } deriving Generic
instance ToJSON Counts

main :: IO ()
main = do
  -- let doc = fromUrl "http://storage.googleapis.com/books/ngrams/books/datasetsv3.html"
  -- pres <- runX $ doc >>> css "pre" /> getText
  -- wordBlob <- readFile "wordList.txt"
  -- let wordList = splitOn "," wordBlob
  args <- arguments
  let pat = L.head args
      searchPat = Turtle.text $ T.take 2 pat <> ".gz"
      filesToSearch = find (suffix searchPat) "data"
  view filesToSearch
  -- stdout $ inproc "gunzip" filesToSearch empty
  -- view $ grepAll filesToSearch pat
  sh $ do
    files <- fold filesToSearch F.list
    fs <- select files
    lineList <- fold (do
      let grepped = inproc "rg" ["-z", "-e", pat, format fp fs] Turtle.empty
      let filtered = grep (greenPat pat) grepped
      filtered) F.list
    let instances = do [(item1 line,item3 line) | line <- lineList ] where
          item1 = (!! 0) . T.splitOn "_" . (!! 1) . T.splitOn " " . (!! 0) . cut tab . lineToText
          item3 :: Line -> Int
          item3 = read . T.unpack . (!! 3) . cut tab . lineToText
    let countsMap = fromListWith (+) instances
    let sortedList = L.take 100 $ L.sortBy (flip compare `on` snd) $ toList countsMap
    let minCount = minimum [snd pair | pair <- sortedList]
    let countsObjs = [Counts first (second `div` minCount) | (first, second) <- sortedList]
    -- liftIO $ print counts
    liftIO $ renderToFile "wordcloud.html" $ html $ countsObjs


greenPat pat = begins $ (Turtle.text pat <|> (Turtle.text pat <> "_ADJ")) <> spaces1 <> chars1 <> "_NOUN"

html :: [Counts] -> Html ()
html counts = html_ $ do
  head_ $ do
    mapM_ (\src -> script_ [src_ src, charset_ "utf-8"] T.empty)
      [ "d3-wordcloud/lib/d3/d3.js"
      , "d3-wordcloud/lib/d3/d3.layout.cloud.js"
      , "d3-wordcloud/d3.wordcloud.js"
      ]
    script_ [] $ B.concat [ "const words = ", encode counts]
  body_ $ do
    wordCloud "green"
    wordCloud "blue"

-- Obtained via, e.g., d3.schemeReds[9].slice(-4,-1)
getColors c = T.pack $ show colors where
  colors = case c of
    "green" -> ["#41ab5d", "#238b45", "#006d2c"]
    "blue" -> ["#4292c6", "#2171b5", "#08519c"]
    "purple" -> ["#807dba", "#6a51a3", "#54278f"]
    "orange" -> ["#f16913", "#d94801", "#a63603"]
    "red" -> ["#ef3b2c", "#cb181d", "#a50f15"]
    "black" -> ["#737373", "#525252", "#252525"]


wordCloud :: Text -> Html ()
wordCloud color = do
    div_ [id_ color ] ""
    footer_ $ do
      script_ [] $ T.concat ["d3.wordcloud().size([500, 300])"
                            , ".selector('#", color, "')"
                            , ".fill(d3.scale.ordinal().range(", getColors color ,"))"
                            , ".words(words).start()"
                            ]
