{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
module Internal.HtmlOps (
    transformHtml, markdownToHtmlString, getFirstImage, removeImages
    ) where

import qualified Control.Monad         as M (when)
import qualified Data.ByteString.Char8 as BS8
import           Data.List             (intercalate)
import qualified Data.Text             as T (unpack)
import qualified Data.Text.Encoding    as T (decodeUtf8)
import qualified Data.Text.Lazy        as TL
import           Internal.Helpers
import           Network.URI           (isAbsoluteURI)
import           System.FilePath.Posix (joinPath)
import           Text.Pandoc
import           Text.XML.HXT.Core     as HXT hiding (app)
-- trace
import qualified Debug.Trace           as Tr (trace, traceShowId)
tr :: Show a => a -> a
tr = Tr.traceShowId
tr' :: Show a => String -> a -> a
tr' s a = Tr.trace (s ++ " " ++ show a) a

-- a good intro to using HXT arrows is http://adit.io/posts/2012-04-14-working_with_HTML_in_haskell.html

-- |Prefix the relative paths with <base>
transformHrefs :: ArrowXml a => String -> a XmlTree XmlTree
transformHrefs base
    = processTopDown (editHref >>> removeDocWhiteSpace) where
        editHref = processAttrl
            (changeAttrValue changeHref `when` hasName "src")
            `when` ( isElem >>> hasName "img" )
            `when` ( getAttrValue "src") where
                changeHref :: String -> String
                changeHref uri | isAbsoluteURI uri = uri
                               | otherwise = joinPath ["/", base, uri]

transformHtml :: HTML -> HTML -> IO TL.Text
transformHtml base !html = do
    out <- runX $ readString [withParseHTML yes, withWarnings no] html
        >>> transformHrefs base
        >>> writeDocumentToString [withOutputEncoding utf8, withOutputHTML]
    return $ TL.pack $ (intercalate "\n" out)

-- |Convert markdown string to html string
markdownToHtmlString :: BS8.ByteString -> HTML
markdownToHtmlString !input = t input where
    rOpts = def
    wOpts = def {
        writerReferenceLinks = True,
        writerHTMLMathMethod = MathJax "https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.1/MathJax.js?config=TeX-MML-AM_CHTML"
        }
    t = writeHtmlString wOpts . handleError . readMarkdown rOpts . unpack
    unpack = T.unpack . T.decodeUtf8

-- |Extract the src of the first image occuring in html
getFirstImage :: TL.Text -> IO TL.Text
getFirstImage html = do
    let dflt = "assets/img/cog.svg" :: TL.Text
    let doc = readString [withParseHTML yes, withWarnings no] $ TL.unpack html
    imgSrcs <- runX $ doc //> hasName "img" >>> getAttrValue "src"
    let src | null imgSrcs = dflt
            | otherwise    = TL.pack $ head imgSrcs
    return src

-- |Remove all the images in some Html
removeImages :: TL.Text -> IO TL.Text
removeImages html = do
    out <- runX $ readString [withParseHTML yes, withWarnings no] (TL.unpack html)
        >>> processTopDown (ifA (hasName "img") (none) (this))
        >>> writeDocumentToString [withOutputEncoding utf8, withOutputHTML]
    return $ TL.pack $ (intercalate "\n" out)
