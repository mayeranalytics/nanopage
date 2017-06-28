{- LANGUAGE TemplateHaskell #-}
module Internal.EmbedDir (embedDir)
where

-- The idea to use an environment variable to set the content dir
-- during compile time comes from this stackoverflow question:
-- https://stackoverflow.com/questions/19679024/how-to-properly-communicate-compile-time-information-to-template-haskell-functio

import           Control.Monad
import qualified Data.FileEmbed             as E (embedDir)
import qualified Data.Text                  as T
import qualified Data.Text.IO               as T
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax (Lift (..))
import           System.Directory           (makeAbsolute)
import           System.Environment         (getEnvironment)
import           System.FilePath.Posix      (joinPath)

-- | Look up a compile-time environment variable.
lookupCompileEnv :: String -> Q (Maybe String)
lookupCompileEnv key = lookup key <$> runIO getEnvironment

-- | Look up an compile-time environment variable and fail if it's not present.
getCompileEnv :: String -> Q String
getCompileEnv key =
  lookupCompileEnv key >>=
  maybe (fail $ "Environment variable " ++ key ++ " not defined") return

-- | Look up a compile-time environment variable and fail if it's not present.
-- The result is a TH expression of type @String@.
getCompileEnvExp :: String -> Q Exp
getCompileEnvExp = lift <=< getCompileEnv

-- | Embed directory by prepending the path found in the environment variable APPDIR.
-- An error is thrown when APPDIR is not defined or the resulting directory cannot be found or read.
-- The result is a TH expression of type @[(String, String)]@.
embedDir :: FilePath -> Q Exp
embedDir p = do
    appDir <- getCompileEnv "APPDIR"
    E.embedDir $ joinPath [appDir, p]
