#! /usr/bin/env nix-shell
#! nix-shell -i runghc --packages "ghc.withPackages (pkgs: with pkgs; [ turtle lucid aeson ])"

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
 
import Turtle
import qualified Control.Foldl as F
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

type Color = Text

main :: IO ()
main = do
  -- let doc = fromUrl "http://storage.googleapis.com/books/ngrams/books/datasetsv3.html"
  -- pres <- runX $ doc >>> css "pre" /> getText
  -- wordBlob <- readFile "wordList.txt"
  -- let wordList = splitOn "," wordBlob
  -- args <- arguments
  let color = "green"
  stats <- fold (getStats color) F.list
  let wordClouds = wordCloud color (head stats)
  -- let colorList = ["green", "blue", "red"]
  -- let wordClouds = makeWordCloud <$> colorList
  renderToFile "wordcloud.html" $ html wordClouds
  print "heyo"

-- makeWordCloud :: Text -> Html ()
-- makeWordCloud color = do
--   let stats = fold (getStats color) mconcat
--   wordCloud color (head stats)

-- makeWordCloud :: Color -> Html WordCloud
-- makeWordCloud color = do
--   stats <- getStats color

getStats :: Text -> Shell [Counts]
getStats color = do
  echo "Getting stats"
  -- liftIO $ putStrLn "Getting color stats for: " <> color
  let searchPat = Turtle.text $ T.take 2 color <> ".gz"
      filesToSearch = find (suffix searchPat) "data"
  view filesToSearch
  -- stdout $ inproc "gunzip" filesToSearch empty
  -- view $ grepAll filesToSearch pat
  files <- fold filesToSearch F.list
  fs <- select files
  lineList <- fold (do
    let grepped = inproc "rg" ["-z", "-e", color, format fp fs] Turtle.empty
    let filtered = grep (greenPat color) grepped
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
  return countsObjs


greenPat pat = begins $ (Turtle.text pat <|> (Turtle.text pat <> "_ADJ")) <> spaces1 <> chars1 <> "_NOUN"

html :: Html () -> Html ()
html wordclouds = html_ $ do
  head_ $ do
    mapM_ (\src -> script_ [src_ src, charset_ "utf-8"] T.empty)
      [ "d3-wordcloud/lib/d3/d3.js"
      , "d3-wordcloud/lib/d3/d3.layout.cloud.js"
      , "d3-wordcloud/d3.wordcloud.js"
      ]
  body_ $ wordclouds

-- Obtained via, e.g., d3.schemeReds[9].slice(-4,-1)
getColors c = T.pack $ show colors where
  colors = case c of
    "green" -> ["#41ab5d", "#238b45", "#006d2c"]
    "blue" -> ["#4292c6", "#2171b5", "#08519c"]
    "purple" -> ["#807dba", "#6a51a3", "#54278f"]
    "orange" -> ["#f16913", "#d94801", "#a63603"]
    "red" -> ["#ef3b2c", "#cb181d", "#a50f15"]
    "black" -> ["#737373", "#525252", "#252525"]

wordCloud :: Text -> [Counts] -> Html ()
wordCloud color counts = do
    div_ [id_ color ] ""
    footer_ $ do
      script_ [] $ B.concat [ "const words = ", encode counts]
      script_ [] $ T.concat ["d3.wordcloud().size([500, 300])"
                            , ".selector('#", color, "')"
                            , ".fill(d3.scale.ordinal().range(", getColors color ,"))"
                            , ".words(words).start()"
                            ]
