{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TemplateHaskell           #-}
module Internal.Partial (
    Partial_, Partial(..),
    extraRoutes, extraRoutes', partial, partialName, partialName',
    getPartials
) where

import           Control.Monad         (liftM)
import           Data.Maybe            (fromMaybe)
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

type RouteType = [Sp.SpockM FileDB () () ()]

class Partial_ a where

    -- | A list of extra routes that should be added
    extraRoutes :: a -> [Sp.SpockM FileDB () () ()]

    -- | The definition of the partial
    partial     :: a -> FileDB -> Page -> Params -> H.Html

    -- | The name of the partial, i.e. also the string that identifies the
    -- partial in mustache patterns {{}}, {{{}}}
    partialName :: a -> T.Text

data Partial = forall a. Partial_ a => Partial a

--extraRoutes' :: Partial a -> [Sp.SpockM FileDB () () ()]
extraRoutes' (Partial p) = extraRoutes p

partialName' (Partial p) = partialName p

-- | Return a list of .hs files in ./Partials and ./src/Partials.
-- Missing directories are silently ignored.
partialList :: IO [String]
partialList = do
     cwd <- getCurrentDirectory
     partialsDir <- lookup "PARTIALSDIR" `liftM` getEnvironment
     fs1 <- case partialsDir of
         Just dir -> findFiles' dir
         Nothing  -> return []
     fs2 <- findFiles' (joinPath [cwd, "src", "Partials"] )
     return $ takeBaseName <$> (fs1 ++ fs2) where
         findFiles' fp = do
             fp_ex <- doesDirectoryExist fp
             if fp_ex then find notHidden regularHsFile fp else return [] where
                 regularHsFile = fileType ==? RegularFile &&? extension ==? ".hs"
                 notHidden = (\n->head n /= '.') `liftM` fileName

con name = AppE <$> [e| Partial |] <*> conE (mkName name)

-- | Return a list of Partial.
-- The return type is [Partial]
getPartials :: Q Exp
getPartials = ListE <$> (runIO partialList >>= mapM mkPartial)

mkPartial :: String -> Q Exp
mkPartial name = AppE <$> [e| Partial |] <*> conE (mkName name)

{-
-- | Get the list of partials as returned by partialList. The result is a TH expression of type @[String]@.
getNamesOfPartials :: Q Exp
getNamesOfPartials = fmap (ListE . map (LitE . StringL)) fs
    where fs = runIO partialList

-- | The result is a TH expression of type @RouteType@.
getRouteOfPartial :: String -> Q Exp
getRouteOfPartial name = AppE <$> [e| extraRoutes |] <*> conE (mkName name)

-- | The result is a TH expression of type @[RouteType]@.
getRoutesOfPartials :: Q Exp
getRoutesOfPartials = AppE <$> [e| concat |] <*> rss where
    rss = ListE <$> (runIO partialList >>= mapM getRouteOfPartial)

-- | Return a list of Partial.partial.
-- The result is a TH expression of type @[a -> FileDB -> Page -> Params -> H.Html]@.
getPartials :: Q Exp
getPartials = ListE <$> (runIO partialList >>= mapM getTuple) where
    getTuple name = tupE [getPartialName name, getPartial name]
    getPartial name = AppE <$> [e| partial |] <*> conE (mkName name)
    getPartialName name = AppE <$> [e| partialName |] <*> conE (mkName name)
-}
