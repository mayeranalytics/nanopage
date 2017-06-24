{-# LANGUAGE TemplateHaskell #-}
module Internal.Partial (
    Partial, extraRoutes, partial, partialName,
    getNamesOfPartials, getRoutesOfPartials, getPartials
) where

import           Control.Monad         (liftM)
import qualified Data.Text             as T
import           Language.Haskell.TH
import           System.Directory      (doesDirectoryExist, getCurrentDirectory)
import           System.FilePath.Find
import           System.FilePath.Posix (joinPath, takeBaseName)
import qualified Text.Blaze.Html5      as H
import qualified Web.Spock             as Sp
-- nanopage imports
import           Internal.FileDB       (FileDB, Page, Params)

class Partial a where

    -- | A list of extra routes that should be added
    extraRoutes :: a -> [Sp.SpockM FileDB () () ()]

    -- | The definition of the partial
    partial     :: a -> FileDB -> Page -> Params -> H.Html

    -- | The name of the partial, i.e. also the string that identifies the
    -- partial in mustache patterns {{}}, {{{}}}
    partialName :: a -> T.Text


-- | Returns a list of .hs files in ./Partials and ./src/Partials.
-- Missing directories are ignored.
partialList :: IO [String]
partialList = do
     cwd <- getCurrentDirectory
     fs1 <- findFiles' (joinPath [cwd, "Partials"])
     fs2 <- findFiles' (joinPath [cwd, "src", "Partials"] )
     return $ takeBaseName <$> (fs1 ++ fs2) where
         findFiles' fp = do
             fp_ex <- doesDirectoryExist fp
             if fp_ex then find notHidden regularHsFile fp else return [] where
                 regularHsFile = fileType ==? RegularFile &&? extension ==? ".hs"
                 notHidden = (\n->head n /= '.') `liftM` fileName

getNamesOfPartials :: Q Exp
getNamesOfPartials = fmap (ListE . map (LitE . StringL)) fs
    where fs = runIO partialList

getRouteOfPartial :: String -> Q Exp
getRouteOfPartial name = AppE <$> [e| extraRoutes |] <*> (conE $ mkName name)

getRoutesOfPartials :: Q Exp
getRoutesOfPartials = AppE <$> [e| concat |] <*> rss where
    rss = ListE <$> ((runIO partialList) >>= mapM getRouteOfPartial)

-- | Return a list of Partial.partial
getPartials :: Q Exp
getPartials = ListE <$> ((runIO partialList) >>= mapM getTuple) where
    getPartial name = AppE <$> [e| partial |] <*> (conE $ mkName name)
    getPartialName name = AppE <$> [e| partialName |] <*> (conE $ mkName name)
    getTuple name = tupE [getPartialName name, getPartial name]
