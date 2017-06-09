{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad                        (filterM, forM_, liftM,
                                                       unless, when)
import           Control.Monad.IO.Class               (MonadIO, liftIO)
import qualified Data.ByteString.Lazy                 as BL
import           Data.Maybe                           (fromMaybe)
import           Data.Monoid
import qualified Data.Text                            as T
import qualified Data.Text.Encoding                   as T
import qualified Data.Text.Lazy                       as TL
import qualified Data.Text.Lazy.IO                    as TL (putStrLn)
import           GHC.Exts                             (fromString)
import qualified Internal.SpockExt                    as Sp
import           Network.HTTP.Types.Status            (Status)
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import           Network.Wai.Middleware.Static
import           Options.Applicative
import qualified Page                                 (routePage)
import           System.Directory                     (setCurrentDirectory)
import           System.Environment                   (lookupEnv)
import           System.FilePath.Posix                (joinPath)
import           Text.Blaze.Html.Renderer.Text        (renderHtml)
import           Text.Blaze.Html5                     ((!))
import qualified Text.Blaze.Html5                     as H
import qualified Text.Blaze.Html5.Attributes          as A
import           Web.Spock                            ((<//>))
import qualified Web.Spock                            as Sp
import qualified Web.Spock.Config                     as Sp
-- nanopage imports
import           FileDB                               (FileDB, Mode (..),
                                                       defaultFileDB,
                                                       getStaticDirRoutes,
                                                       isHiddenPage, makePages)
import qualified FileDB
import qualified Internal.Partial                     as Partial
import           Page                                 (renderPage,
                                                       renderPreview, routePage,
                                                       routePreview)
import           Partials.AdminBlock
import           Partials.CategoryList
import           Partials.KeywordList
import           Partials.TagCloud
import           Partials.TagList
import           Sitemap

helpHeader :: String
helpHeader = "This is nanoPage, a minimalistic flat-file CMS written in Haskell. For more information see http://nanopage.li/."

helpDescription :: String
helpDescription = "Run the nanoPage webserver."

-- | The application options
data Opts = Opts {
    portNumber :: Int,
    serverName :: String,
    mode       :: Mode,
    contentDir :: FilePath
}
instance Show Opts where
    show opts = "Configuration:"
        ++ "\n  port   \t" ++ show (portNumber opts)
        ++ "\n  server \t" ++ show (serverName opts)
        ++ "\n  mode   \t" ++ show (mode opts)
        ++ "\n  content\t" ++ show (contentDir opts)

-- | Server config info
extraRoutes :: [Sp.SpockM FileDB () () ()]
extraRoutes = Partial.extraRoutes AdminBlock
           ++ Partial.extraRoutes CategoryList
           ++ Partial.extraRoutes KeywordList
           ++ Partial.extraRoutes TagCloud
           ++ Partial.extraRoutes TagList
defaultPortNumber = 3000 :: Int
defaultServerName = "localhost" :: String

isAdmin :: Opts -> Bool
isAdmin o = mode o == ADMIN

parseOpts :: Parser Opts
parseOpts = Opts
    <$> option auto (
        long "port" <>
        short 'p' <>
        help "port number" <>
        showDefault <>
        value defaultPortNumber <>
        metavar "INT"
    ) <*> strOption (
        long "server" <>
        short 'n' <>
        help "Server name" <>
        showDefault <>
        value defaultServerName <>
        metavar "STRING"
    ) <*> option auto (
        long "mode" <>
        short 'm' <>
        help "Server mode can be PROD or ADMIN. The admin pages are only shown in ADMIN mode." <>
        showDefault <>
        value PROD <>
        metavar "MODE"
    ) <*> strOption (
        long "working-dir" <>
        short 'C' <>
        help "Working directory" <>
        showDefault <>
        value "." <>
        metavar "STRING"
    )

-- | Todo: Make a nice error page.
errHandler :: Status -> Sp.ActionCtxT () IO ()
errHandler _ = Sp.text "Error: Resource does not exist."

appMiddleware :: Opts -> Sp.SpockM FileDB () () ()
appMiddleware o = do
    Sp.middleware logStdoutDev
    when (isAdmin o) $ do
        -- Only in ADMIN mode do we serve files from file system directly.
        -- The directories added here must mirror the ones in Internal.FileDB.embedAllFiles
        Sp.middleware $ staticPolicy $ noDots >-> addBase "./static"
        Sp.middleware $ staticPolicy $ noDots >-> addBase "./pages"
        liftIO $ putStrLn ("Serving files from ./static and ./pages")

spockConfig :: Opts -> IO (Sp.SpockCfg FileDB () ())
spockConfig opts = do
    let fileDb = defaultFileDB { FileDB.mode = mode opts }
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

app :: Opts -> Sp.SpockM FileDB () () ()
app opts = do
    pages <- Sp.runQuery makePages
    let pages' = filter (not.isHiddenPage) pages
    liftIO $ putStrLn "Pages:"
    liftIO $ forM_ pages' (TL.putStrLn . ("> " <>) . FileDB.slug)
    -- now add all the routes
    sitemap <- liftIO $ mkSitemap (serverName opts) (map (TL.unpack . FileDB.slug) pages')
    Sp.get "sitemap.xml" (Sp.xml $ T.encodeUtf8 $ TL.toStrict sitemap)
    forM_ pages' Page.routePage
    forM_ pages' Page.routePreview
    sequence_ extraRoutes
    unless (isAdmin opts) $ do
        staticDirRoutes <- Sp.runQuery getStaticDirRoutes
        liftIO $ putStrLn "Files:"
        sequence_ staticDirRoutes
        liftIO $ putStrLn "Serving content from exe."

main :: IO ()
main = do
    opts <- execParser $ info (parseOpts <**> helper) (fullDesc <> progDesc helpDescription <> header helpHeader)
    print opts
    setCurrentDirectory (contentDir opts)
    if contentDir opts /= "."
        then (putStrLn $ "Working directory changed to \"" ++ contentDir opts ++ "\"")
        else (putStrLn "Working directory is \".\"")
    config <- spockConfig opts
    Sp.runSpock (portNumber opts) (Sp.spock config $ appMiddleware opts >> (app opts))
