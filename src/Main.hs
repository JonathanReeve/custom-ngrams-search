-- |

{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Scotty

import Data.Monoid (mconcat)

main = scotty 3000 $
    get "/" $ do
        html "<h1>Heyo!</h1>"

    get "/:word" $ do
        beam <- param "word"
        html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]
