{-# LANGUAGE OverloadedStrings #-}
module Partials.CategoryList (CategoryList(..)) where

import           Control.Monad               (forM_)
import           GHC.Exts                    (fromString)
import           Text.Blaze.Html5            ((!))
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A
-- nanopage imports
import           Internal.FileDB
import           Internal.Partial

-- | Partial "CategoryList" displays a sequence of <div class="chip">, one for each category.
data CategoryList = CategoryList

instance Partial_ CategoryList where
    partialRoutes_ _ = []
    partialRender_   = _partial
    partialName_ _   = "categorylist"

_partial :: CategoryList -> FileDB -> Page -> Params -> H.Html
_partial _ _ p _ = H.div ! A.class_ (fromString "categorylist") $
    forM_ (categories p) ((H.div ! categoryClass) . H.toHtml)

categoryClass :: H.Attribute
categoryClass = A.class_ (fromString "chip")
