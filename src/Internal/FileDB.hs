{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Internal.FileDB where

import           Control.Monad                 (filterM, when)
import           Control.Monad.IO.Class        (MonadIO, liftIO)
import           Data.Aeson                    (FromJSON, ToJSON, fromJSON,
                                                toJSON)
import qualified Data.Aeson                    as A
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Char8         as BS8
import qualified Data.ByteString.Lazy          as BL
import           Data.List                     (find, nub)
import           Data.Maybe                    (fromMaybe)
import           Data.Monoid                   ((<>))
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as T (decodeUtf8)
import qualified Data.Text.Lazy                as TL
import           Data.Yaml                     ((.:), (.:?), (.=))
import qualified Data.Yaml.Aeson               as Y
import           GHC.Generics
import           Prelude                       hiding (readFile)
import           System.FilePath.Find          ((==?))
import qualified System.FilePath.Find          as Find (FileType (RegularFile),
                                                        always, fileName,
                                                        fileType, find)
import           System.FilePath.Posix         (joinPath, makeRelative,
                                                replaceExtension,
                                                splitDirectories,
                                                splitExtension, splitFileName)
import qualified System.IO.Error               as Error
import           Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Text.Blaze.Html5              as H
import qualified Text.Mustache                 as M
import qualified Web.Spock                     as Sp
-- nanpage imports
import           Internal.EmbedDir             (embedDir)
import           Internal.Helpers
import           Internal.HtmlOps              (getFirstImage,
                                                markdownToHtmlString,
                                                removeImages, transformHtml)
import qualified Internal.SpockExt             as Sp

import qualified Debug.Trace                   as Tr (trace, traceShowId)
--tr :: Show a => a -> a
--tr = Tr.traceShowId
tr' :: Show a => String -> a -> a
tr' s a = Tr.trace (s ++ " " ++ show a) a

{------------------------------ FileDB ----------------------------------------}

data FileDB = FileDB {
    pagesDir     :: String,
    templatesDir :: String,
    files        :: [(FilePath, BS.ByteString)],
    mode         :: Mode
}

-- | Mode can be PROD or ADMIN, representing deployment environments.
data Mode = PROD | ADMIN deriving (Read, Show, Eq)

-- | Return a FileDB record with default parameters. This must be in the IO
-- monad because the readAllFiles necessesarily is in the IO monad
-- (whereas embedAllFiles doesn't really have to be).
defaultFileDB :: Mode -> IO FileDB
defaultFileDB m = do
    allFiles <- if m == ADMIN then readAllFiles else embedAllFiles
    return FileDB {
        pagesDir = "pages",
        templatesDir = "templates",
        files = allFiles,
        mode = m
    }

readAllFiles :: IO [(FilePath, BS.ByteString)]
readAllFiles = do
    pages     <- fileList "pages"
    templates <- fileList "templates"
    mapM get (pages ++ templates) where
        get p = do
            c <- BS.readFile (tr' "readFile:" p)
            return (p, c)
        fileList :: FilePath -> IO [FilePath]
        fileList = Find.find notHidden regularFile where
            regularFile = Find.fileType ==? Find.RegularFile
            notHidden = (('.'/=).head) <$> Find.fileName

-- | Embed all required files. Note that the paths are hardcoded relative paths
-- which has strong implications on project file structure.
-- embedAllFiles doesn't really have to be in the IO monad, but in should
-- match the signature of readAllFiles.
embedAllFiles :: IO [(FilePath, BS.ByteString)]
embedAllFiles = return $
    prefixWith "pages" pages
    ++ prefixWith "templates" templates
    ++ static where
        prefixWith' p (p',c) = (joinPath [p,p'], c)
        prefixWith p = map (prefixWith' p)
        pages     = $(embedDir "content/pages")
        templates = $(embedDir "content/templates")
        static    = $(embedDir "content/static")

-- | Normalize a directory name, i.e. replace double // by / etc.
normalizePath :: FilePath -> FilePath
normalizePath = joinPath . splitDirectories

-- | In files of FileDB @db@ find filenames with extension @ext@ under
-- directory @dir@
findFiles :: FileDB -> String -> FilePath -> [FilePath]
findFiles db ext dir = filter f ps where
    ps = fst <$> files db
    f p = n dir == n dir' && ext == ext' where
        n = normalizePath
        (dir', file) = splitFileName p
        ext' = (snd.splitExtension) file

-- | Read file with path @p@ from FileDB @db@, throw an error if it doesn't exist.
readFile :: FileDB -> FilePath -> BS.ByteString
readFile db p = case e_c of
    Nothing    -> error (p ++ " could not be found")
    Just (_,c) -> c
    where
        e_c = find (\(p',_)->normalizePath p == normalizePath p') (files db)

-- | In files of FileDB find all the directories underneath the given path.
-- Full paths are returned.
getDirectories :: FileDB -> FilePath -> [FilePath]
getDirectories db = getDirectories' fileNames where
    fileNames = map fst (files db)

-- | In a list of paths find all the directories underneath the given path
-- (This is a helper function for getDirectories.)
getDirectories' :: [FilePath] -> FilePath -> [FilePath]
getDirectories' ps p' = map joinPath $ filter (`isSubDir` p) dirs where
    dirs = unique $ filter (not.null) $ map (init.splitDirectories) ps
    p = splitDirectories p'

-- | Helper function that returns the unique elements in a list. Not efficient,
-- but ok for small lists.
unique :: Eq a => [a] -> [a]
unique = reverse . nub . reverse

-- | Test whether p1 is subdir of p2. The arguments are arranged such that
-- p1 `isSubDir` p2 tests naturally
isSubDir :: [FilePath] -> [FilePath] -> Bool
isSubDir p1 p2
    | l2 > l1 = False
    | take l2 p1 == p2 = True
    | otherwise = False
    where
        l1 = length p1
        l2 = length p2


{------------------------------ Template --------------------------------------}

-- | Load, compile and return the template with the given name.
getTemplate :: TemplateName -> FileDB -> M.Template
getTemplate tname db = case e_m of
        Left err -> error "getTemplate: compileMustacheText error"
        Right m  -> m
        where
            e_m = M.compileMustacheText (M.PName $ T.pack tname) content
            content = TL.fromStrict $ T.decodeUtf8 content'
            content' = readFile db $ joinPath [templatesDir db, tname]

-- | Render text @t@ using key/value @pairs@
renderWithTemplate :: [(T.Text, A.Value)] -> TL.Text -> TL.Text
renderWithTemplate pairs t = case e_m of
    Left err -> error ("renderWithTemplate: compileMustacheText error: " ++ show err)
    Right m  -> M.renderMustache m (A.object pairs)
    where
        e_m = M.compileMustacheText (M.PName "content") t

{-------------------------------- PageConfig ----------------------------------}

data PageConfig = PageConfig {
    _title       :: TL.Text,        -- | Title of the page
    _slug        :: Maybe TL.Text,  -- | Slug of the page. If Nothing it will be deduced from the title
    _keywords    :: Maybe [TL.Text],  -- | Keywords of the page
    _tags        :: Maybe [TL.Text],  -- | Tags of the page
    _categories  :: Maybe [TL.Text],   -- | Categories of the page
    _description :: Maybe TL.Text,     -- | Description of the page
    _author      :: Maybe TL.Text
} deriving Show

instance FromJSON PageConfig where
    parseJSON (A.Object o) = PageConfig
        <$> o .:  "title"
        <*> o .:? "slug"    -- optional
        <*> o .:? "keywords"
        <*> o .:? "tags"
        <*> o .:? "categories"
        <*> o .:? "description"
        <*> o .:? "author"

instance ToJSON PageConfig where
      toJSON (PageConfig t s ks ts cs d a) = A.object
        ["title" .= t, "slug" .= s, "keywords" .= ks, "tags" .= ts,
         "categories" .= cs, "description" .= d, "author" .= a]
      toEncoding (PageConfig t s ks ts cs d a) = A.pairs
        ("title" .= t <> "slug" .= s <> "keywords" .= ks <> "tags" .= ts
         <> "categories" .= cs <> "description" .= d <> "author" .= a)

readPageConfig :: FileDB -> FilePath -> PageConfig
readPageConfig db fname = case Y.decodeEither content of
        Left err -> error err
        Right p  -> p
        where
            content = readFile db fname

keywordsString :: PageConfig -> TL.Text
keywordsString c = case _keywords c of
    Just ks -> TL.intercalate "," ks
    Nothing -> "nanoPage,Website,CMS,Content management system,Haskell"

descriptionString :: PageConfig -> TL.Text
descriptionString c = fromMaybe "This is a nanoPage page" (_description c)

authorString :: PageConfig -> TL.Text
authorString c = fromMaybe "nanoPage" (_author c)


{-------------------------------- Page ----------------------------------------}

data Page = Page {
    config       :: PageConfig,
    mdContent    :: TL.Text,           -- the html'ized content of the md file
    mdPreview    :: TL.Text,           -- the html'ized preview of the md file
    previewImage :: TL.Text,           -- path to the preview image
    template     :: Maybe M.Template,  -- parsed content of the template file
    dirName      :: TL.Text            -- directory name of the page
}

-- | Parameters passed to partials, a list of key-value pairs.
type Params = [(T.Text, T.Text)]

-- | Return the title of a page.
title :: Page -> TL.Text
title p = _title (config p)

-- | Return the slug of a page. Throw an error when slug cannot be created.
slug :: Page -> TL.Text
slug p = case _slug (config p) of
    Just s  -> s
    Nothing -> fromMaybe (error "Cannot create slug!") (makeSlug (title p))

-- | Return the list of keywords of a page.
keywords :: Page -> [TL.Text]
keywords p = fromMaybe [] (_keywords $ config p)

-- | Return the list of tags of a page.
tags :: Page -> [TL.Text]
tags p = fromMaybe [] (_tags $ config p)

-- | Return the list of categories of a page.
categories :: Page -> [TL.Text]
categories p = fromMaybe [] (_categories $ config p)

-- | Return the description of a page.
description :: Page -> TL.Text
description = descriptionString . config

-- | Return the author name of a page.
author :: Page -> TL.Text
author = authorString . config

-- | Returns True when the page is a hidden page (has a dirName beginning with '_' or '.')
isHiddenPage :: Page -> Bool
isHiddenPage p = h == '_' || h == '.' where
    h = (TL.head . dirName) p

-- | Get the page identified by pageDir from FileDB.
getPageNoContent :: FilePath -> FileDB -> Page
getPageNoContent pageDir db = p where
    l = length mdFiles
    mdFiles = findFiles db ".md" pageDir
    cfg = readPageConfig db $ joinPath [pageDir, "config.yaml"]
    p | l == 0 = error $ "No .md files found in " ++ pageDir
      | l > 1  = error $ "There are multiple .md files in " ++ pageDir
      | otherwise = Page {
        config = cfg,
        mdContent = "",
        mdPreview = "",
        template = Nothing,
        previewImage = "assets/img/placeholder-320x160.png",
        dirName = (TL.pack . makeRelative "pages") pageDir
    }

-- | Return a list of all available pages
getAllPagesNoContent :: FileDB -> [Page]
getAllPagesNoContent db = map (`getPageNoContent` db) ps where
    ps = listPages db

-- | Return a list of all visible pages, i.e. all pages that don't have a leading
-- '.' or '_'
getPagesNoContent :: FileDB -> [Page]
getPagesNoContent db =
    if mode db == ADMIN
    then getAllPagesNoContent db
    else filter (not.isHiddenPage) (getAllPagesNoContent db)

-- | Returns the list of pages, i.e. the names of directories in pagesDir.
-- The pagesDir is prefixed, e.g. this is ["pages/_admin", "pages/documentation", ...]
listPages :: FileDB -> [String]
listPages db = filter f dirs where
    f d = (head d /= '.') && (d /= pagesDir db)
    dirs = getDirectories db (pagesDir db)

-- | Splits a file content into preview and content. It splits on the first
-- occurence of "---"
splitPagePreviewContent :: BS8.ByteString -> (BS8.ByteString, BS8.ByteString)
splitPagePreviewContent bs = (p, c) where
   brk = "---" :: BS8.ByteString
   (p', c') = BS8.breakSubstring brk bs
   (p,c) | c' == "" = ("", p')
         | otherwise = (p', BS8.drop 3 c')

-- | Create the page identified by @pageDir@ from FileDB @db@.
makePage :: FilePath -> FileDB -> IO Page
makePage pageDir db = do
    -- 1. Get a list of mdfiles in pageDir, raise an error if there is not exactly one
    let mdFiles = Internal.FileDB.findFiles db ".md" pageDir
    when (null mdFiles) (error $ "No .md files found in " ++ pageDir)
    when (length mdFiles > 1)  (error $ "There are multiple .md files in " ++ pageDir)
    let mdFileName = (snd . splitFileName . head) mdFiles
    -- 2. Extract the template filename from the md filename, load the template
    let templateFileName = replaceExtension mdFileName "html"    -- the filename of the page makes the templatename
    let template = getTemplate templateFileName db
    -- 3. read the mdfile, render the markdown to HTML, transform the HTML (fix paths, etc.)
    let mdFileContent = Internal.FileDB.readFile db $ joinPath [pageDir, mdFileName]
    let (mdPreview, mdContent) = splitPagePreviewContent mdFileContent
    let htmlContent' = markdownToHtmlString mdContent
    let htmlPreview' = markdownToHtmlString mdPreview
    htmlContent <- transformHtml pageDir htmlContent'
    htmlPreview'' <- transformHtml pageDir htmlPreview'
    previewImage <- getFirstImage htmlPreview''
    htmlPreview <- removeImages htmlPreview''
    -- 4. Read the config.yaml file, make the Page with empty content
    let cfg = readPageConfig db $ joinPath [pageDir, "config.yaml"]
    return Page {
        config = cfg,
        template = Just template,
        mdContent = htmlContent,
        mdPreview = htmlPreview,
        previewImage = previewImage,
        dirName = (TL.pack . makeRelative "pages") pageDir
    }

{-  not used
-- | 'Safe' version of makePage that catches IOErrors and returns error messages in an Either.
makePage' :: String -> FileDB -> IO (Either ErrorMessage Page)
makePage' f db = do
  p' <- Error.tryIOError (makePage f db)
  return $ case p' of
      Left err -> Left (Error.ioeGetErrorString err)
      Right p  -> Right p
-}

-- | Return a list of all available pages
makeAllPages :: FileDB -> IO [Page]
makeAllPages db = mapM (`makePage` db) (listPages db)

-- | Return a list of all visible pages, with content. Depending on mode pages with
-- leading '.' or '_' are shown (ADMIN mode) or not (otherwise)
makePages :: FileDB -> IO [Page]
makePages db = filter f <$> makeAllPages db where
   f p = (not . isHiddenPage) p || (mode db == ADMIN)

-- | Generate a list of routes for all files in FileDB @db@.
getStaticDirRoutes :: FileDB -> IO [Sp.SpockM FileDB () () ()]
getStaticDirRoutes db = return $ map routeFile (files db) where
    routeFile :: (FilePath, BS8.ByteString) -> Sp.SpockM FileDB () () ()
    routeFile (path, content) = do
        liftIO $ putStrLn ("> " ++ path)
        Sp.get (Sp.static path) $ Sp.serveFile (T.pack path) content


{-------------------------------- PageInfo ------------------------------------}

-- | A sanitized, short version of Page that can be returned as a JSON object.
-- PageInfo is used for showing preview cards, for example.
data PageInfo = PageInfo {
   ti :: TL.Text,
   sl :: TL.Text,
   au :: TL.Text,
   pr :: TL.Text,       -- the html'ized preview of the md file
   ts :: [TL.Text],     -- tags
   cs :: [TL.Text],     -- categories
   im :: TL.Text        -- image link
   } deriving (Generic, ToJSON)

-- | Create the sanitized PageInfo form of a Page
mkPageInfo :: Page -> PageInfo
mkPageInfo p = PageInfo (title p) (slug p) (author p) (mdPreview p) (tags p) (categories p) (previewImage p)

-- | Fill the PageInfo's pr field with rendered html by applying the function @f@.
renderPreviewWith :: (H.Html -> PageInfo -> H.Html) -> PageInfo -> PageInfo
renderPreviewWith f p = PageInfo (ti p) (sl p) (au p) preview' (ts p) (cs p) (im p) where
    preview' = renderHtml html'
    html' = f (H.preEscapedText $ TL.toStrict $ pr p) p
