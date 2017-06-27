{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Partials.TagCloud (TagCloud(..)) where

import           Control.Monad                 (forM_, when)
import           Data.List                     (intercalate, nub, sort)
import           Data.Monoid                   ((<>))
import           Data.String.QQ
import qualified Data.Text                     as T (Text, pack, unlines,
                                                     unpack)
import qualified Data.Text.Lazy                as TL (Text, toStrict, unpack)
import           GHC.Exts                      (fromString)
import qualified Text.Blaze                    as H (textValue)
import           Text.Blaze.Html.Renderer.Text (renderHtml)
import           Text.Blaze.Html5              ((!))
import qualified Text.Blaze.Html5              as H
import qualified Text.Blaze.Html5.Attributes   as A
import qualified Web.Spock                     as Sp
-- nanopage imports
import           Internal.FileDB               (FileDB (..), Page (..),
                                                PageInfo (..), Params,
                                                categories, getPagesNoContent,
                                                keywords, makePages, mkPageInfo,
                                                renderPreviewWith, slug, tags,
                                                title)
import           Internal.Partial

-- | Partial "TagCloud" displays
data TagCloud = TagCloud

instance Partial TagCloud where
    extraRoutes _ = [route]
    partial       = _partial
    partialName _ = "tagcloud"

route :: Sp.SpockM FileDB () () ()
route = Sp.get "pages" $ do
    params <- Sp.params
    pages <- Sp.runQuery makePages
    Sp.json $ map (renderPreviewWith renderPreview . mkPageInfo) pages

-- This function is used with renderPreviewWith
renderPreview :: (H.Html -> PageInfo -> H.Html)
renderPreview input p = do    -- render a card
    let title = ti p
    let image = im p
    let slug = TL.toStrict $ sl p
    let fn = "location.href='" <> slug <> "';"
    H.div ! A.class_ "card hoverable grid-item" ! A.onclick (H.textValue fn) $ do
        H.div ! A.class_ "card-image" $ do
            H.img ! A.src (H.textValue $ TL.toStrict image)
            H.a ! A.class_ "btn-floating btn-large halfway-fab waves-effect waves-light red" ! A.href (H.textValue slug) $
                H.i ! A.class_ "material-icons" $ "add"
        H.div ! A.class_ "card-content" $ do
            H.h2 ! A.class_ "card-title" $ H.toHtml title
            H.div ! A.class_ "preview-content" $ input

_partial :: TagCloud -> FileDB -> Page -> Params -> H.Html
_partial _ db p _ = do
    let pages = getPagesNoContent db
    H.div ! A.class_ "tagcloud" $
        H.ul ! A.class_ "hlist" $ allTagButtons pages
    H.div ! A.id "pages" ! A.class_ "grid row" $ H.p "empty"
    H.script ! A.type_ "text/javascript" $ H.preEscapedText javascript
    H.style $ H.text style

unique :: Eq a => [a] -> [a]
unique = Prelude.reverse . nub . Prelude.reverse

mkButton :: TL.Text -> TL.Text -> H.Html
mkButton typ name = H.a ! A.class_ "waves-effect waves-light btn btn-small" !
    A.href (fromString js) $ nameEl where
        nameEl | name == "*" = H.i ! A.class_ "material-icons make-wide" $ "list"
               | otherwise = H.toHtml name
        js = "javascript:showPreviews(\""++name'++"\", \"#pages\")"
        typ' = TL.unpack typ
        name' = TL.unpack name

allTagButtons :: [Page] -> H.Html
allTagButtons ps = do
    mkButton "tags" "*"
    forM_ ts (mkButton "tags") where
    ts' = unique $ concat $ map tags ps :: [TL.Text]
    ts = sort ts' :: [TL.Text]

javascript :: T.Text
javascript = [s|
var pageObjs = [];
tags = function() { return [].concat.apply([], pageObjs.map(function(p) {return p.ts;})) }
categories = function() { return [].concat.apply([], pageObjs.map(function(p) {return p.cs;})) }
filterPageObjs = function(tag) { return pageObjs.filter(function(p) {return p.ts.indexOf(tag)>=0;}) }
window.onload = function(){
    if(!pageObjs.length) {
        $.ajax({
            url: "pages",
            success: function(result) { pageObjs=result; showPreviews("*", "#pages"); }
        });
    }
}
previewPage = function(p, target) {$(target).html(p.pr);}
showPreviews = function(tag, target) {
    var ps = tag==="*" ? pageObjs : filterPageObjs(tag);
    $(target).html(ps.map(function(p) {return p.pr;}).join(""));
}
|]

style :: T.Text
style = [s|
ul.hlist > a.btn {
    margin-right: 3px;
    margin-bottom: 3px;
}
.btn-small {
    height: 24px;
    line-height: 24px;
    padding: 0 0.5rem;
    border-radius: 9px;
}
.card {
    width: 320px;
}
.btn-floating.btn-large.halfway-fab {
    position: absolute;
    bottom: -28px;
    right: 24px;
}
.card .card-content .card-title {
    line-height: 30px;
    margin-bottom: 5px;
}
.make-wide {
    width: 42px;
}
.grid-item {
  float: left;
  width: 320px;
  height: 440px;
  margin-right: 20px;
}
div.preview-content {
    height: 440px;
    overflow: hidden;
    text-overflow: ellipsis;
    white-space: normal;
}
|]
