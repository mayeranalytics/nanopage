{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module FileDB (FileDB(..), Page(..), Mode(..), TemplateName, Params, listPages,
    makePage, makePreview, getPagesNoContent, makePages, getStaticDirRoutes,
    defaultFileDB, getTemplate, title, slug, tags, keywords, categories,
    description, author, makeContent, isHiddenPage, PageInfo, mkPageInfo, renderPreviewWith
) where

import qualified Control.Exception                    as Exception
import           Control.Monad                        (filterM, when)
import           Data.Aeson                           ((.=))
import qualified Data.Aeson                           as Y (object, pairs)
import qualified Data.Aeson.Types                     as Y (Pair)
import qualified Data.ByteString                      as BS
import qualified Data.ByteString.Char8                as BS8
import qualified Data.ByteString.Lazy                 as BL
import           Data.Maybe                           (fromMaybe)
import           Data.Monoid                          ((<>))
import qualified Data.Text                            as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy                       as TL
import           Data.Yaml                            ((.:), (.:?))
import qualified Data.Yaml.Aeson                      as Y
import           GHC.Exts                             (fromString)
import           Network.HTTP.Types.Status            (Status)
import           Network.URI                          (isAbsoluteURI)
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import           Network.Wai.Middleware.Static
import           System.FilePath.Posix                (joinPath, makeRelative,
                                                       replaceExtension,
                                                       splitFileName)
import qualified System.IO.Error                      as Error
import           Text.Blaze.Html.Renderer.Text        (renderHtml)
import qualified Text.Mustache                        as M
import qualified Text.Mustache.Compile                as M
import           Text.Parsec.Error                    (errorMessages,
                                                       messageString)
import           Text.XML.HXT.Core                    as HXT hiding (app, when)
import           Web.Spock                            ((<//>))
import qualified Web.Spock                            as Sp
import qualified Web.Spock.Config                     as Sp
-- nanoPage imports
import           Internal.FileDB
import qualified Internal.FileDB                      as FileDB (Mode (..),
                                                                 mode)
import           Internal.Helpers
import           Internal.HtmlOps                     (markdownToHtmlString,
                                                       transformHtml)
import           Internal.Partial
-- partials
import           Partials
import Partials.AdminBlock

makePreview :: Page -> Params -> FileDB -> IO TL.Text
makePreview p params db = return $ Internal.FileDB.mdPreview p

-- |Turn a page created by makePage into content.
makeContent :: Page -> Params -> [Partial]-> FileDB -> IO TL.Text
makeContent page params partials db = do
    -- Note this: The Mustache template plugging happens here. Mustache works with
    -- key/value pairs. These pairs are built up, here. The {{content}} pattern
    -- itself has to be Mustache interpolated, so there's a nested application
    -- of Mustache patterns.
    let t = fromMaybe (error "INTERNAL ERROR: No template found.") (template page)
    -- 5. get all the partials
    let allPartials = $(getPartials) ++ partials
    -- 6. Treat the htmlContent as a template and let  Mustache render it with the standard pairs
    let cfg = config page
    let config_pairs = [
            "title"       .= _title cfg,
            "keywords"    .= keywordsString cfg,
            "description" .= descriptionString cfg,
            "author"      .= authorString cfg
            ] :: [Y.Pair]
    let partials_pairs = map mkPair $ filter filtF allPartials where
        mkPair p = (partialName p) .= (TL.toStrict $ renderHtml $ (partialRender p) db page params)
        filtF p = FileDB.mode db == FileDB.ADMIN || partialName p /= "adminblock"
    let htmlContentsRendered = (config_pairs ++ partials_pairs) `renderWithTemplate` mdContent page
    -- 7. Now render the template, using all pairs. In particular, plug in the
    --    {{content}}
    let content_pair = ["content" .= htmlContentsRendered] :: [Y.Pair]
    let yObjs = Y.object (config_pairs ++ partials_pairs ++ content_pair)
    return (M.renderMustache t yObjs)
