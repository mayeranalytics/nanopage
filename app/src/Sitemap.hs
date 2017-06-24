{-# LANGUAGE OverloadedStrings #-}
module Sitemap(mkSitemap) where

import qualified Data.Map         as Map
import           Data.Monoid      ((<>))
import qualified Data.Text        as T (pack)
import qualified Data.Text.Lazy   as TL
import           Internal.Helpers (getISOTime)
import           Text.XML

-- | Make a sitemap. Provide the server name and a list of paths as arguments.
mkSitemap :: String -> [String] -> IO TL.Text
mkSitemap server_name pages = do
    time <- getISOTime
    let pages' = (\xs->server_name++"/"++xs) <$> pages
        root_attr = Map.fromList [(makeName "xmlns", "http://www.sitemaps.org/schemas/sitemap/0.9")]
        root = Element (makeName "urlset") root_attr (Prelude.map (packageToNode time) pages')
        xml = renderText def $ Document (Prologue [] Nothing []) root []
    return xml

packageToNode :: String -> String -> Node
packageToNode time pagename = makeNodeURL pagename time "weekly" "1.0"

-- | Make node
-- @loc@, @lastmod@, @changefreq@, @priority@
makeNodeURL :: String -> String -> String -> String -> Node
makeNodeURL loc lastmod changefreq priority =
    NodeElement $ Element (makeName "url") noAttr
    [ NodeElement $ Element (makeName "loc") noAttr
        (makeNodeValue loc)
    , NodeElement $ Element (makeName "lastmod") noAttr
        (makeNodeValue lastmod)
    , NodeElement $ Element (makeName "changefreq") noAttr
        (makeNodeValue changefreq)
    , NodeElement $ Element (makeName "priority") noAttr
        (makeNodeValue priority)
    ] where
        noAttr = Map.empty
        makeNodeValue :: String -> [Node]
        makeNodeValue val = [NodeContent (T.pack val)]

makeName :: String -> Name
makeName s = Name
  { nameLocalName = T.pack s
  , nameNamespace = Nothing
  , namePrefix = Nothing }
