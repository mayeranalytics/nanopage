module Internal.Helpers (
    MarkDown, HTML, TemplateName, ErrorMessage,
    makeSlug
) where

import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Lazy       as TL
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
