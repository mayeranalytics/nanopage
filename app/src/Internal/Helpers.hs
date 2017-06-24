module Internal.Helpers (
    MarkDown, HTML, TemplateName, ErrorMessage,
    makeSlug,
    getISOTime
) where

import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Lazy       as TL
import           Data.Time.Clock      (getCurrentTime)
import           Data.Time.ISO8601    (formatISO8601)
import qualified System.IO.Error      as Error
import           Web.Slug             (mkSlug, unSlug)

type MarkDown = String
type HTML = String
type TemplateName = String
type ErrorMessage = String

makeSlug :: TL.Text -> Maybe TL.Text
makeSlug t = do
    let t' =  TL.toStrict t
    s <- mkSlug t'
    Just $ TL.fromStrict $ unSlug s

getISOTime :: IO String
getISOTime = formatISO8601 <$> getCurrentTime
