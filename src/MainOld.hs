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
import Data.Text.Encoding (encodeUtf8)
import Data.Map as M hiding (fold)
import Data.Maybe
import Lucid
import Data.Aeson
import GHC.Generics (Generic)
import qualified Data.ByteString.Lazy as B

data Counts = Counts { text :: Text
                     , size :: Int } deriving (Show, Generic)
instance ToJSON Counts

type Color = Text

main :: IO ()
main = do
  let colorList = ["white", "black", "red", "green", "yellow", "blue", "brown", "purple", "violet", "pink", "orange", "gray", "greenish", "reddish", "bluish"]
  wordClouds <- mapM makeWordCloud colorList
  -- let wordClouds = makeWordCloud <$> colorList
  renderToFile "wordcloud.html" $ html $ mconcat wordClouds

makeWordCloud :: Text -> IO (Html ())
makeWordCloud color = do
  stats <- fold (getStats color) F.head
  let wordClouds = wordCloud color (fromJust stats)
  return wordClouds
  

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

-- Obtained via, e.g., d3.schemeReds[9].slice(-4,-1)
getColors c = T.pack $ show colors where
  purples = ["#807dba", "#6a51a3", "#54278f"]
  reds = ["#ef3b2c", "#cb181d", "#a50f15"]
  greens = ["#41ab5d", "#238b45", "#006d2c"]
  blues = ["#4292c6", "#2171b5", "#08519c"]
  colors = case c of
    "green" -> greens
    "greenish" -> greens
    "blue" -> blues
    "bluish" -> blues
    "purple" -> purples
    "violet" -> purples
    "orange" -> ["#f16913", "#d94801", "#a63603"]
    "red" -> reds
    "reddish" -> reds
    "black" -> ["#737373", "#525252", "#252525"]
    "white" -> ["#c1c1c1", "#d8d6d6", "#ede8e8"]
    "gray" -> ["#737373", "#525252", "#252525"]
    "pink" -> ["#ed2dbd", "#ea75cd", "#efb1e0"]
    "brown" -> ["#493e32", "#8c663d", "#bc7121"]
    "yellow" -> ["#efef0b", "#efef6e", "#f7f7be"]

html :: Html () -> Html ()
html wordclouds = html_ $ do
  head_ $ do
    mapM_ (\src -> script_ [src_ src, charset_ "utf-8"] T.empty)
      [ "/04-colors/includes/d3.js"
      , "/04-colors/includes/d3.layout.cloud.js"
      , "/04-colors/includes/d3.wordcloud.js"
      ]
  body_ $ main_ [class_ "cloudsContainer", style_ "display: flex; flex-wrap: wrap;"] wordclouds


wordCloud :: Text -> [Counts] -> Html ()
wordCloud color counts = do
  div_ [class_ "cloudContainer", style_ "display: flex; flex-direction: column;"] $ do
    let colorStyle = style_ ("color:" <> color <> "; text-align: center; background: none;")
    h2_ [colorStyle] $ toHtml color
    a_ [href_ ("/04-colors/custom-ngrams-search/categorize-words/" <> color <> ".html"), colorStyle] $
      div_ [id_ color ] ""
    footer_ $ do
      script_ [] $ B.concat [ "const ", B.fromStrict (encodeUtf8 color), " = ", encode counts]
      script_ [] $ T.concat ["d3.wordcloud().size([500, 300])"
                            , ".selector('#", color, "')"
                            , ".fill(d3.scale.ordinal().range(", getColors color ,"))"
                            , ".words(", color, ").start()"
                            ]
