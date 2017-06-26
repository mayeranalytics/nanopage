{-# LANGUAGE OverloadedStrings #-}
module Config (
    NanopageConfig(..), isAdmin
)
where

import           Data.String.ToString
import qualified Data.Text            as T
-- nanopage imports
import           FileDB               (Mode (..))

helpHeader :: String
helpHeader = "This is nanoPage, a minimalistic flat-file CMS written in Haskell. For more information see http://nanopage.li/."

helpDescription :: String
helpDescription = "Run the nanoPage webserver."

-- | The application options
data NanopageConfig = NanopageConfig {
    portNumber :: Int,
    serverName :: String,
    mode       :: Mode,
    contentDir :: FilePath
}
instance ToString NanopageConfig where
    toString opts = "Configuration:"
        ++ "\n  port   \t" ++ show (portNumber opts)
        ++ "\n  server \t" ++ show (serverName opts)
        ++ "\n  mode   \t" ++ show (mode opts)
        ++ "\n  content\t" ++ show (contentDir opts)

-- | Server config info
defaultPortNumber = 3000 :: Int
defaultServerName = "localhost" :: String

isAdmin :: NanopageConfig -> Bool
isAdmin = (ADMIN ==) . mode
