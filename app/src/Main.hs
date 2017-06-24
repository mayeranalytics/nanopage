{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Nanopage

main :: IO ()
main = do
    opts <- parseCliOpts
    runNanopage opts
