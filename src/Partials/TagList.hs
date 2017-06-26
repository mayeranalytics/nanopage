{-# LANGUAGE OverloadedStrings #-}
module Partials.TagList (TagList(..)) where

import           Control.Monad               (forM_)
import           GHC.Exts                    (fromString)
import           Internal.FileDB
import           Text.Blaze.Html5            ((!))
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A

import           Internal.Partial

-- | Partial "TagList" displays a sequence of <div class="chip">, one for each tag.
data TagList = TagList

instance Partial TagList where
    extraRoutes _ = []
    partial       = _partial
    partialName _ = "taglist"

_partial :: TagList -> FileDB -> Page -> Params -> H.Html
_partial _ _ p _ = H.div ! A.class_ (fromString "taglist") $
    forM_ (tags p) ((H.div ! tagClass) . H.toHtml)

tagClass :: H.Attribute
tagClass = A.class_ (fromString "chip")
