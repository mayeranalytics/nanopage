{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module CliOpts (
    parseCliOpts
) where

import           Data.Monoid         ((<>))
import           Options.Applicative
-- nanopage imports
import           Config
import           FileDB              (Mode (..))

helpHeader :: String
helpHeader = "This is nanoPage, a minimalistic flat-file CMS written in Haskell. For more information see http://nanopage.li/."

helpDescription :: String
helpDescription = "Run the nanoPage webserver."

-- | Server config info
defaultPortNumber = 3000 :: Int
defaultServerName = "localhost" :: String

parseOpts :: Parser NanopageConfig
parseOpts = NanopageConfig
    <$> option auto (
        long "port" <>
        short 'p' <>
        help "port number" <>
        showDefault <>
        value defaultPortNumber <>
        metavar "INT"
    ) <*> strOption (
        long "server" <>
        short 'n' <>
        help "Server name" <>
        showDefault <>
        value defaultServerName <>
        metavar "STRING"
    ) <*> option auto (
        long "mode" <>
        short 'm' <>
        help "Server mode can be PROD or ADMIN. The admin pages are only shown in ADMIN mode." <>
        showDefault <>
        value PROD <>
        metavar "MODE"
    ) <*> strOption (
        long "working-dir" <>
        short 'C' <>
        help "Working directory" <>
        showDefault <>
        value "." <>
        metavar "STRING"
    )

parseCliOpts :: IO NanopageConfig
parseCliOpts = execParser $ info (parseOpts <**> helper) (fullDesc <> progDesc helpDescription <> header helpHeader)
