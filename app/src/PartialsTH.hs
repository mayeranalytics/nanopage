{-# LANGUAGE TemplateHaskell #-}
module PartialsTH (
    getNamesOfPartials,
    getRoutesOfPartials
) where

import           Internal.FileDB       (FileDB)
import qualified Internal.Partial      as Partial

import           Control.Monad         (liftM)
import           Language.Haskell.TH
import           System.Directory      (doesDirectoryExist, getCurrentDirectory)
import           System.FilePath.Find
import           System.FilePath.Posix (joinPath, takeBaseName)
import qualified Web.Spock             as Sp

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
getRouteOfPartial name = AppE <$> [e| Partial.extraRoutes |] <*> (conE $ mkName name)

getRoutesOfPartials :: Q Exp
getRoutesOfPartials = AppE <$> [e| concat |] <*> rss where
    rss = ListE <$> ((runIO partialList) >>= mapM getRouteOfPartial)
