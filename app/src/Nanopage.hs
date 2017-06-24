{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Nanopage (
    -- Internal.Partial
    Partial, extraRoutes, partial, partialName,
    getNamesOfPartials, getRoutesOfPartials, getPartials,
    -- Page
    renderPage, renderPreview, routePage, routePreview,
    -- FileDB
    FileDB, Mode (..), defaultFileDB, getStaticDirRoutes, isHiddenPage, makePages,
    slug,
    -- Config
    NanopageConfig, parseCliOpts,
    -- nanopage
    runNanopage
) where

import           Control.Monad                        (forM_, unless, when)
import           Control.Monad.IO.Class               (liftIO)
import           Data.Monoid                          ((<>))
import           Data.String.ToString
import qualified Data.Text                            as T
import qualified Data.Text.Encoding                   as T
import qualified Data.Text.Lazy                       as TL
import qualified Data.Text.Lazy.IO                    as TL (putStrLn)
import           Language.Haskell.TH
import           Network.HTTP.Types.Status            (Status)
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import           Network.Wai.Middleware.Static
import           System.Directory                     (setCurrentDirectory)
import           Web.Browser                          (openBrowser)
import           Web.Spock                            ((<//>))
import qualified Web.Spock                            as Sp
import qualified Web.Spock.Config                     as Sp
-- nanopage imports
import           Internal.Partial
import           Internal.SpockExt                    as Sp (xml)

import           CliOpts
import           Config
import           FileDB                               (FileDB, Mode (..),
                                                       defaultFileDB,
                                                       getStaticDirRoutes,
                                                       isHiddenPage, makePages,
                                                       slug)
import           Page                                 (renderPage,
                                                       renderPreview, routePage,
                                                       routePreview)
import           Partials
import           Sitemap

runNanopage :: NanopageConfig -> IO ()
runNanopage opts = do
    putStrLn (toString opts)
    setCurrentDirectory (contentDir opts)
    if contentDir opts /= "."
    then putStrLn ("Working directory changed to \"" ++ contentDir opts ++ "\"")
    else putStrLn "Working directory is \".\""
    config <- spockConfig opts
    when (mode opts == ADMIN) $ do
        success <- openBrowser $ "http://localhost:" ++ show (portNumber opts)
        unless success (putStrLn "Failed to open browser.")
    Sp.runSpock (portNumber opts) (Sp.spock config $ appMiddleware opts >> app opts)

-- | Todo: Make a nice error page.
errHandler :: Status -> Sp.ActionCtxT () IO ()
errHandler _ = Sp.text "Error: Resource does not exist."

appMiddleware :: NanopageConfig -> Sp.SpockM FileDB () () ()
appMiddleware o = do
   Sp.middleware logStdoutDev
   when (isAdmin o) $ do
       -- Only in ADMIN mode do we serve files from file system directly.
       -- The directories added here must mirror the ones in Internal.FileDB.embedAllFiles
       Sp.middleware $ staticPolicy $ noDots >-> addBase "./static"
       Sp.middleware $ staticPolicy $ noDots >-> hasPrefix "pages" >-> addBase "."
       liftIO $ putStrLn "Serving files from ./static and ./pages"

spockConfig :: NanopageConfig -> IO (Sp.SpockCfg FileDB () ())
spockConfig opts = do
   fileDb <- defaultFileDB (mode opts)
   sessionCfg <- Sp.defaultSessionCfg ()
   let conn = Sp.PCConn Sp.ConnBuilder {
       Sp.cb_createConn = return fileDb ,
       Sp.cb_destroyConn = \_->return (),
       Sp.cb_poolConfiguration = Sp.PoolCfg {
           Sp.pc_stripes = 1,
           Sp.pc_resPerStripe = 1,
           Sp.pc_keepOpenTime = 9999
       }
   }
   return Sp.SpockCfg {
       Sp.spc_initialState = (),
       Sp.spc_database = conn,
       Sp.spc_sessionCfg = sessionCfg,
       Sp.spc_maxRequestSize = Just 5,
       Sp.spc_errorHandler = errHandler,
       Sp.spc_csrfProtection = False,
       Sp.spc_csrfHeaderName = "",
       Sp.spc_csrfPostName = ""
   }

app :: NanopageConfig -> Sp.SpockM FileDB () () ()
app opts = do
   pages <- Sp.runQuery makePages
   let pages' | mode opts == ADMIN = pages
              | otherwise = filter (not.isHiddenPage) pages
   liftIO $ putStrLn "Pages:"
   liftIO $ forM_ pages' (TL.putStrLn . ("> " <>) . slug)
   -- now add all the routes
   sitemap <- liftIO $ mkSitemap (serverName opts) (map (TL.unpack . slug) pages')
   Sp.get "sitemap.xml" $ (Sp.xml . T.encodeUtf8 . TL.toStrict) sitemap
   forM_ pages' routePage
   forM_ pages' routePreview
   sequence_ $(getRoutesOfPartials)
   unless (isAdmin opts) $ do
       staticDirRoutes <- Sp.runQuery getStaticDirRoutes
       liftIO $ putStrLn "Files:"
       sequence_ staticDirRoutes
       liftIO $ putStrLn "Serving content from exe."
