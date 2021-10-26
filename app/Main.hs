module Main where

import           Prelude

import qualified Control.Exception          as Exception
import           Control.Monad              (filterM)
import           Data.Aeson                 (ToJSON,
                                             Value (Array, Bool, Object, String),
                                             encode, toJSON)
import           Data.ByteString.Builder    (stringUtf8, toLazyByteString)
import qualified Data.ByteString.Lazy       as BL
import           Data.Char                  (ord)
import           Data.Csv                   (DecodeOptions (decDelimiter),
                                             FromNamedRecord (parseNamedRecord),
                                             decodeByNameWith,
                                             defaultDecodeOptions, (.:))
import           Data.Functor               ((<&>))
import           Data.HashMap.Strict        ((!?))
import qualified Data.HashMap.Strict        as HM
import qualified Data.Set                   as Set
import qualified Data.Text                  as T
import           Data.Time                  (Day)
import           Data.Time.Clock            (getCurrentTime, utctDay)
import           Data.Vector                (Vector, fromList, toList)
import           Development.Shake          (Action, ShakeOptions (shakeFiles),
                                             cmd_, copyFileChanged, forP,
                                             getDirectoryFiles, liftIO, need,
                                             phony, putInfo, removeFilesAfter,
                                             shakeArgs, shakeOptions, want,
                                             writeFile', (%>))
import           Development.Shake.FilePath (takeBaseName, takeFileName, (</>))
import           GHC.Generics               (Generic)
import           Slick.Mustache             (compileTemplate')
import           System.Directory           (doesDirectoryExist, listDirectory)
import           Text.Mustache.Render       (substitute)

-- Config ---------------------------------------------------------------------

data SiteMeta =
  SiteMeta { baseUrl         :: String
           , imageUrl        :: String
           , siteTitle       :: String
           , siteDescription :: String
           , lastUpdate      :: Day
           }
  deriving (Generic, Eq, Ord, Show, ToJSON)

siteMeta :: Day -> SiteMeta
siteMeta day =
  SiteMeta { baseUrl = "/acinetobase-static"
           --, baseUrl = "https://acinetobase.vib.be"
           -- baseUrl = "https://vibbits.github.io/acinetobase-static/"
           , imageUrl = "/images"
           , siteTitle = "Acinetobase"
           , siteDescription = "Compendium of Experiments in the Lab"
           , lastUpdate = day
           }

outputDir :: FilePath
outputDir = "_site/"

sourceDir :: FilePath
sourceDir = "data/"

-- Business Data models -------------------------------------------------------

data Isolate =
  Isolate { density    :: !Bool
          , colony     :: !Bool
          , microscope :: !Bool
          , model      :: !Bool
          , name       :: !T.Text
          , kl         :: !T.Text
          , ocl        :: !T.Text
          , st         :: !T.Text
          , genbank    :: Maybe T.Text
          }
  deriving (Generic, Eq, Show, ToJSON)

instance FromNamedRecord Isolate where
  parseNamedRecord record =
    Isolate False False False False
       <$> record .: "Isolate"
       <*> record .: "KL"
       <*> record .: "OCL"
       <*> record .: "ST"
       <*> record .: "GenBank"

decodeIsolates :: BL.ByteString -> Either String (Vector Isolate)
decodeIsolates = fmap snd . decodeByNameWith decodeOptions
  where
    decodeOptions :: DecodeOptions
    decodeOptions = defaultDecodeOptions
      {
        decDelimiter = fromIntegral (ord ';')
      }

decodeIsolatesFromFile :: FilePath -> IO (Either String (Vector Isolate))
decodeIsolatesFromFile filePath =
  catchShowIO (dropBOM <$> BL.readFile filePath) <&> either Left decodeIsolates
  where
    -- Remove the byte-order mark if it's there.
    dropBOM :: BL.ByteString -> BL.ByteString
    dropBOM bs | BL.take 3 bs == BL.pack [0xEF, 0xBB, 0xBF] = BL.drop 3 bs
               | otherwise = bs

catchShowIO :: IO a -> IO (Either String a)
catchShowIO action =
  Exception.catch (Right <$> action) handleIOException
  where
    handleIOException :: Exception.IOException -> IO (Either String a)
    handleIOException = return . Left . show

withMeta :: (ToJSON a) => a -> Value -> Value
withMeta meta (Object obj) = Object $ HM.union obj metaObj
  where
    Object metaObj = toJSON meta
withMeta _ _               = error "Invalid metadata"

withUrl :: String -> Value
withUrl url =
  Object $ HM.singleton "url" (String $ T.pack url)

withIsolatesIndex :: Vector Isolate -> Value
withIsolatesIndex isolates =
  Object $ HM.singleton "isolates" (Array $ toJSON <$> isolates)

withAbout :: Value
withAbout =
  Object $ HM.singleton "about" $ Bool True

--- Build helpers -------------------------------------------------------------

isolateFile :: Isolate -> FilePath
isolateFile isolate =
  "isolates" </> T.unpack (name isolate) ++ ".html"

filesFromIsolates :: Vector Isolate -> Vector FilePath
filesFromIsolates isolates =
  let
    iname :: Isolate -> String
    iname iso = T.unpack (name iso)

    html :: Vector FilePath
    html
      = (\i -> outputDir </> isolateFile i) <$> isolates

    images :: Vector FilePath
    images
      = (\i -> fromList $ map snd $ filter fst $ zip [density i, colony i, microscope i, model i]
          [ outputDir </> "images" </> iname i ++ "_density.png"
                       , outputDir </> "images" </> iname i ++ "_colony.jpg"
                       , outputDir </> "images" </> iname i ++ "_TEM.png"
                       , outputDir </> "images" </> iname i ++ ".png"]
               ) =<< isolates
  in
    html <> images

--- Build Actions -------------------------------------------------------------

buildIsolatePage :: Day -> Isolate -> Action ()
buildIsolatePage day isolate = do
  let url = isolateFile isolate
  template <- compileTemplate' "templates/isolate.html"
  let pageData = withMeta (siteMeta day) . withMeta isolate $ withUrl url
  writeFile' (outputDir </> url) . T.unpack $ substitute template pageData

buildIndex :: Day -> Vector Isolate -> Action ()
buildIndex day isolates = do
  indexT <- compileTemplate' "templates/index.html"
  let indexData = withMeta (siteMeta day) $ withIsolatesIndex isolates
  writeFile' (outputDir </> "index.html") . T.unpack $ substitute indexT indexData


-- Top Level ------------------------------------------------------------------

buildRules :: Day -> Vector Isolate -> IO ()
buildRules day isolates = do
  shakeArgs shakeOptions{shakeFiles="_build/"} $ do
    let outputFiles = filesFromIsolates isolates
    let base = toLazyByteString $ stringUtf8 $ baseUrl $ siteMeta day

    want $ toList outputFiles ++ [outputDir </> "index.html", outputDir </> "about.html", outputDir </> "index.js"]

    phony "clean" $ do
      putInfo "Cleaning..."
      removeFilesAfter "_build" ["//*"]
      removeFilesAfter "_site" ["//*"]

    outputDir </> "index.html" %> \_ -> do
      static <- getDirectoryFiles "static" [ "*.webp"
                                          , "*.svg"
                                          , "*.jpg"
                                          , "*.png"
                                          , "vib.css"
                                          , "acinetobase.css"
                                          , "Dense-Regular.otf"
                                          , "Dense-Bold.otf"
                                          ]
      need ((sourceDir </> "metadata.csv") : ("templates" </> "index.html") : ((outputDir </>) <$> static))
      buildIndex day isolates

    outputDir </> "about.html" %> \_ -> do
      need ["templates" </> "about.html"]
      template <- compileTemplate' "templates/about.html"
      let pageData = withMeta (siteMeta day) withAbout
      writeFile' (outputDir </> "about.html") . T.unpack $ substitute template pageData

    outputDir </> "*.webp" %> \output -> do
      need ["static" </> takeFileName output]
      copyFileChanged ("static" </> takeFileName output) output

    outputDir </> "*.svg" %> \output -> do
      need ["static" </> takeFileName output]
      copyFileChanged ("static" </> takeFileName output) output

    outputDir </> "*.jpg" %> \output -> do
      need ["static" </> takeFileName output]
      copyFileChanged ("static" </> takeFileName output) output

    outputDir </> "*.png" %> \output -> do
      need ["static" </> takeFileName output]
      copyFileChanged ("static" </> takeFileName output) output

    outputDir </> "*.css" %> \css -> do
      need ["static" </> takeFileName css]
      cssTemplate <- compileTemplate' $ "static" </> takeFileName css
      let cssData = withMeta (siteMeta day) $ Object HM.empty
      writeFile' css . T.unpack $ substitute cssTemplate cssData

    outputDir </> "*.otf" %> \font -> do
      need ["static" </> takeFileName font]
      copyFileChanged ("static" </> takeFileName font) font

    outputDir </> "images" </> "*" %> \output -> do
      let dir = T.unpack $ head $ T.split (== '_') $ T.pack . takeBaseName $ output
      need ["data" </> dir </> takeFileName output]
      copyFileChanged ("data" </> dir </> takeFileName output) output

    outputDir </> "isolates" </> "*.html" %> \_ -> do
      need [sourceDir </> "metadata.csv"]
      _ <- forP (toList isolates) $ buildIsolatePage day
      pure ()

    outputDir </> "index.js" %> \js -> do
      need [ "src" </> "Site.js"
           , "src" </> "index.js"
           , "src" </> "Main.purs"
           , "src" </> "Site.purs"
           ]
      cmd_ ("npm run build" :: String)
      copyFileChanged ("dist" </> "index.js") js

    "src" </> "Site.js" %> \output -> do
      need [sourceDir </> "metadata.csv"]
      liftIO $ BL.writeFile output
        $ "\"use strict;\"\n\nexports.baseURL = \""
        <> base <> "\";\n\n"
        <> "exports.isolatesImpl = "
        <> encode isolates <> ";\n"

updateIsolates :: Vector Isolate -> HM.HashMap FilePath (Set.Set FilePath) -> Vector Isolate
updateIsolates isolates files =
  updateIsolate <$> isolates
  where
    updateIsolate :: Isolate -> Isolate
    updateIsolate isolate =
      let
        ident = T.unpack $ name isolate
      in
      case files !? ident of
        Nothing -> isolate
        Just fs -> isolate { density = Set.member (ident ++ "_density.png") fs
                           , colony = Set.member (ident ++ "_colony.jpg") fs
                           , microscope = Set.member (ident ++ "_TEM.png") fs
                           , model = Set.member (ident ++ ".png") fs }

main :: IO ()
main = do
  day <- utctDay <$> getCurrentTime
  isos <- decodeIsolatesFromFile $ sourceDir </> "metadata.csv"

  sourceDirContents <- HM.fromList <$> (
    listDirectory sourceDir
    >>= filterM (\file -> doesDirectoryExist $ sourceDir </> file)
    >>= mapM (\file -> (\files -> (file, Set.fromList files)) <$> listDirectory (sourceDir </> file))
    )

  case isos of
    Right isolates -> buildRules day $ updateIsolates isolates sourceDirContents
    Left err       -> putStrLn err

