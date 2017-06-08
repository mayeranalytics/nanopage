module Internal.Partial (Partial, extraRoutes, partial) where

import qualified Text.Blaze.Html5 as H
import qualified Web.Spock        as Sp
-- nanopage imports
import           Internal.FileDB  (FileDB, Page, Params)

class Partial a where

    -- | A list of extra routes that should be added
    extraRoutes :: a -> [Sp.SpockM FileDB () () ()]

    -- | The definition of the partial
    partial     :: a -> FileDB -> Page -> Params -> H.Html
