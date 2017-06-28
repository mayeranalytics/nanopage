{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Partials.AdminBlock (AdminBlock(..)) where

import           Control.Monad                 (forM_, when)
import           Data.List                     (intercalate, nub, sort)
import           Data.Monoid                   ((<>))
import           Data.String.QQ
import qualified Data.Text                     as T (Text, pack, unlines,
                                                     unpack)
import qualified Data.Text.Lazy                as TL (Text, toStrict, unpack)
import           GHC.Exts                      (fromString)
import           Text.Blaze.Html.Renderer.Text (renderHtml)
import           Text.Blaze.Html5              ((!))
import qualified Text.Blaze.Html5              as H
import qualified Text.Blaze.Html5.Attributes   as A
import qualified Web.Spock                     as Sp
-- nanopage imports
import           Internal.FileDB               (FileDB (..), Page (..), Params,
                                                categories, getPagesNoContent,
                                                keywords, slug, tags, title)
import           Internal.Partial

-- | Partial "AdminBlock"
data AdminBlock = AdminBlock

instance Partial_ AdminBlock where
    partialRoutes_ _ = [route]
    partialRender_   = _partial
    partialName_ _   = "adminblock"

route :: Sp.SpockM FileDB () () ()
route = Sp.get "admin/getpages" $ do
    params <- Sp.params
    renderAdminTags params

isPageMatch :: (T.Text, T.Text) -> Page -> Bool
isPageMatch ("tag",v) p      = v `elem` map TL.toStrict (tags p)
isPageMatch ("keyword",v) p  = v `elem` map TL.toStrict (keywords p)
isPageMatch ("category",v) p = v `elem` map TL.toStrict (categories p)
isPageMatch (_,_) _          = False

renderPagesList :: [Page] -> H.Html
renderPagesList ps = H.ul ! A.class_ "collection" $
    forM_ ps $ \p -> H.a ! A.class_ "collection-item" ! (A.href . fromString) (TL.unpack $ slug p) $ H.toHtml (title p)

renderAdminTags :: [(T.Text, T.Text)] -> Sp.ActionCtxT () (Sp.WebStateM FileDB () ()) ()
renderAdminTags params = do
    pages <- Sp.runQuery (return . getPagesNoContent)
    when (null params) (Sp.text "")
    let param = head params
    let pages' = filter (isPageMatch param) pages
    let pagesList = renderPagesList pages'
    (Sp.html . TL.toStrict . renderHtml) $ do
        H.p $ do
            H.toHtml ("Pages with " <> fst param <> " ")
            H.b $ H.i $ H.toHtml $ snd param
            H.html ":"
        pagesList

_partial :: AdminBlock -> FileDB -> Page -> Params -> H.Html
_partial _ db p _ = do
    let pages = getPagesNoContent db
    H.style $ H.text style
    H.div ! A.class_ (fromString "adminblock") $ do
        H.h2 $ H.toHtml ("Pages" :: TL.Text)
        renderPagesList pages

        H.h2 $ H.toHtml ("Tags" :: TL.Text)
        H.ul ! hClass $ allTags pages
        H.div ! A.id "tag" $ ""

        H.h2 $ H.toHtml ("Categories" :: TL.Text)
        H.ul ! hClass $ allCategories pages
        H.div ! A.id "category" $ ""

        H.h2 $ H.toHtml ("Keywords" :: TL.Text)
        H.ul ! hClass $ allKeywords pages
        H.div ! A.id "keyword" $ ""
    H.script ! A.type_ "text/javascript" $ H.text javascript

javascript :: T.Text
javascript = [s|
function getPages(query, target) {
$.ajax({
    url: "admin/getpages?" + query,
    success: function(result) { $(target).html(result); },
    cache:true
    }
    );
}
|]

style :: T.Text
style = [s|
ul.hlist > a.btn {
margin-right: 2px;
margin-bottom: 2px;
}
.btn-small {
height: 24px;
line-height: 24px;
padding: 0 0.5rem;
border-radius: 9px;
}
|]

hClass = A.class_ (fromString "hlist")
liClass = A.class_ (fromString "chip")


unique :: Eq a => [a] -> [a]
unique = Prelude.reverse . nub . Prelude.reverse

mkButton :: TL.Text -> TL.Text -> H.Html
mkButton typ name = H.a ! A.class_ (fromString "waves-effect waves-light btn btn-small") !
    A.href (fromString js) $ H.toHtml name where
        js = "javascript:getPages(\"" ++ typ' ++ "=" ++ name' ++ "\", " ++ ("\"#" <> typ') ++ "\")"
        typ' = TL.unpack typ
        name' = TL.unpack name

listAll :: TL.Text -> (Page -> [TL.Text]) -> [Page] -> H.Html
listAll name f ps = forM_ ts (mkButton name) where
    ts' = unique $ concat $ map f ps :: [TL.Text]
    ts = sort ts' :: [TL.Text]


allTags       = listAll "tag" tags
allKeywords   = listAll "keyword" keywords
allCategories = listAll "category" categories
