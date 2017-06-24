#!/usr/bin/env stack runhaskell
{-# LANGUAGE TemplateHaskell #-}
module Main where

import           Control.Monad     (forM_)
import           Data.List         (intercalate)
import           Data.Time.Clock   (getCurrentTime)
import           Data.Time.ISO8601 (formatISO8601Millis)
import           Nanopage

-- This executable generates the src/Partials.hs file. The exe should be run
-- in the src directory.

main :: IO ()
main = do
    timeStr <- formatISO8601Millis <$> getCurrentTime
    let ps = map ("Partials."++)$(getNamesOfPartials)
    putStrLn ("-- Automatically generated by mkPartialsHs on " ++ timeStr)
    putStrLn "module Partials ("
    putStrLn $ intercalate ",\n" (map ("    module "++) ps)
    putStrLn ") where"
    putStrLn ""
    putStrLn $ intercalate "\n" (map ("import "++) ps)
