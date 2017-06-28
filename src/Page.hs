{-# LANGUAGE OverloadedStrings #-}
module Page(pageHandler, transformHtml, markdownToHtmlString,
    routePage, renderPage, routePreview, renderPreview, PageInfo, mkPageInfo) where

import           Control.Monad.IO.Class (MonadIO)
import qualified Data.Text.Lazy         as TL
import qualified Web.Spock              as Sp
-- nanopage imports
import           FileDB                 (FileDB, Page, PageInfo, makeContent,
                                         makePreview, mkPageInfo, slug)
import           Internal.HtmlOps
import           Internal.Partial

import           Debug.Trace            (trace)
pPage :: FileDB.Page -> FileDB.Page
pPage p = trace (show $ FileDB.slug p) p

pageHandler :: MonadIO m => TL.Text -> Sp.ActionT m ()
pageHandler = Sp.html . TL.toStrict

renderPage :: [Partial] -> FileDB.Page -> Sp.ActionCtxT () (Sp.WebStateM FileDB.FileDB () ()) ()
renderPage partials p = do
    params <- Sp.params
    content <- Sp.runQuery (makeContent p params partials)
    Sp.html $ TL.toStrict content

routePage :: [Partial] -> FileDB.Page -> Sp.SpockM FileDB.FileDB () () ()
routePage partials p = Sp.get route action where
    action = renderPage partials p
    route | slug == "/" = Sp.root
          | otherwise   = Sp.static $ TL.unpack slug
          where slug = FileDB.slug p

renderPreview :: FileDB.Page -> Sp.ActionCtxT () (Sp.WebStateM FileDB.FileDB () ()) ()
renderPreview p = do
    params <- Sp.params
    content <- Sp.runQuery (makePreview p params)
    Sp.html $ TL.toStrict content

routePreview :: FileDB.Page -> Sp.SpockM FileDB.FileDB () () ()
routePreview p = Sp.get route action where
    action = renderPreview p
    slug = FileDB.slug p
    route = Sp.static (TL.unpack slug ++ "/preview")
