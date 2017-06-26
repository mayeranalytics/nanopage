{-# LANGUAGE OverloadedStrings #-}
module Partials.KeywordList (KeywordList(..)) where

import           Control.Monad               (forM_)
import           GHC.Exts                    (fromString)
import           Internal.FileDB
import           Internal.Partial
import           Text.Blaze.Html5            ((!))
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A

-- | Partial "KeywordList" displays a sequence of <div class="chip">, one for each keyword.
data KeywordList = KeywordList

instance Partial KeywordList where
    extraRoutes _ = []
    partial       = _partial
    partialName _ = "keywordlist"

_partial :: KeywordList -> FileDB -> Page -> Params -> H.Html
_partial _ _ p _ = H.div ! A.class_ (fromString "keywordlist") $
    forM_ (keywords p) ((H.div ! keywordClass) . H.toHtml)

keywordClass :: H.Attribute
keywordClass = A.class_ (fromString "chip")
