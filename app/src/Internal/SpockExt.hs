{-# LANGUAGE OverloadedStrings #-}
module Internal.SpockExt (serveFile, xml) where

import           Control.Monad.IO.Class (MonadIO)
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Char8  as BS8
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as T
import           Network.Mime
import           Web.Spock

-- | Serve a file, set the content-type in accordance with the file name.
serveFile :: MonadIO m => FileName -> BS8.ByteString -> ActionCtxT ctx m a
serveFile fileName val = do
    let mimeType = defaultMimeLookup fileName
    setHeader "Content-Type" (T.decodeUtf8 mimeType)
    bytes val

-- | Send xml as response. Content-Type will be "application/xml"
xml :: MonadIO m => BS8.ByteString -> ActionCtxT ctx m a
xml val = do
    setHeader "Content-Type" "application/xml; charset=utf-8"
    bytes val
{-# INLINE xml #-}
