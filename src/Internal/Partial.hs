{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TemplateHaskell           #-}
module Internal.Partial (
    Partial_(..), Partial(..),
    partialRoutes, partialRender, partialName,
    getPartials
) where

import           Data.Either           (either)
import           Data.Either.Extra     (fromRight)
import qualified Data.Text             as T
import           Language.Haskell.TH
import           System.Directory      (doesDirectoryExist, getCurrentDirectory)
import           System.Environment    (getEnvironment)
import           System.FilePath.Find
import           System.FilePath.Posix (joinPath, takeBaseName)
import qualified Text.Blaze.Html5      as H
import qualified Web.Spock             as Sp
-- nanopage imports
import           Internal.FileDB       (FileDB, Page, Params)


import           Debug.Trace           (trace)
tr' xs a = trace (xs ++ show a) a

type RouteType = [Sp.SpockM FileDB () () ()]

class Partial_ a where

    -- | A list of extra routes that should be added
    partialRoutes_ :: a -> [Sp.SpockM FileDB () () ()]

    -- | The definition of the partial
    partialRender_ :: a -> FileDB -> Page -> Params -> H.Html

    -- | The name of the partial, i.e. also the string that identifies the
    -- partial in mustache patterns {{}}, {{{}}}
    partialName_ :: a -> T.Text

data Partial = forall a. Partial_ a => Partial a

partialRoutes :: Partial -> [Sp.SpockM FileDB () () ()]
partialRoutes (Partial p) = partialRoutes_ p

partialName :: Partial -> T.Text
partialName (Partial p) = partialName_ p

partialRender :: Partial -> FileDB -> Page -> Params -> H.Html
partialRender (Partial p) = partialRender_ p

-- | Return a list of .hs files in ./Partials and ./src/Partials.
-- Missing directories are silently ignored.
partialList :: IO [String]
partialList = do
     cwd <- getCurrentDirectory
     fs <- fromRight <$> return [] <*> findHsFiles (joinPath [cwd, "src", "Partials"])
     return $ takeBaseName <$> fs

-- | Get the list of files in directory @fp@. When the directory doesn't exist return Left errormsg.
findHsFiles :: FilePath -> IO (Either String [FilePath])
findHsFiles fp = do
    fp_ex <- doesDirectoryExist fp
    if fp_ex
    then Right <$> find notHidden regularHsFile fp
    else return (Left $ "Directory " ++ fp ++ " does not exist!!") where
        regularHsFile = fileType ==? RegularFile &&? extension ==? ".hs"
        notHidden = (('.'/=).head) <$> fileName

-- | Return a list of Partial.
-- The return type is [Partial]
getPartials :: Q Exp
getPartials = SigE <$> psE <*> [t| [Partial] |] where
    psE = ListE <$> (runIO partialList >>= mapM mkPartial)

-- | Return an instance of Partial <name>
-- The return type is Partial
mkPartial :: String -> Q Exp
mkPartial name = AppE <$> [e| Partial |] <*> conE (mkName name)

-- @Todo: qAddDependentFile
