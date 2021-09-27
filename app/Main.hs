{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Prelude                    hiding (filter)

import qualified Control.Exception          as Exception
import           Control.Monad              (filterM)
import           Data.Aeson                 (ToJSON,
                                             Value (Array, Object, String),
                                             toJSON)
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
import           Data.Vector                (Vector, filter, toList)
import           Development.Shake          (Action, ShakeOptions (shakeFiles),
                                             copyFileChanged, forP,
                                             getDirectoryFiles, need, phony,
                                             putInfo, removeFilesAfter,
                                             shakeArgs, shakeOptions, want,
                                             writeFile', (%>))
import           Development.Shake.FilePath (takeFileName)
import           GHC.Generics               (Generic)
import           Slick.Mustache             (compileTemplate')
import           System.Directory           (doesDirectoryExist, listDirectory)
import           System.FilePath.Posix      ((</>))
import           Text.Mustache.Render       (substitute)

-- Config ---------------------------------------------------------------------

data SiteMeta =
  SiteMeta { siteAuthor      :: String
           , baseUrl         :: String
           , dataUrl         :: String
           , siteTitle       :: String
           , siteDescription :: String
           }
  deriving (Generic, Eq, Ord, Show, ToJSON)

siteMeta :: SiteMeta
siteMeta =
  SiteMeta { siteAuthor = "James Collier"
           , baseUrl = "https://acinetobase.vib.be"
           , dataUrl = "/data"
           , siteTitle = "Acinetobase"
           , siteDescription = "Compendium of Experiments in the Lab"
           }

outputDir :: FilePath
outputDir = "_site/"

dataDir :: FilePath
dataDir = "data/"

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
          }
  deriving (Generic, Eq, Show, ToJSON)

instance FromNamedRecord Isolate where
  parseNamedRecord record =
    Isolate False False False False
       <$> record .: "Isolate"
       <*> record .: "KL"
       <*> record .: "OCL"
       <*> record .: "ST"

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
withMeta _ _ = error "Invalid metadata"

withUrl :: String -> Value
withUrl url =
  Object $ HM.singleton (T.pack "url") (String $ T.pack url)

withIsolatesIndex :: Vector Isolate -> Value
withIsolatesIndex isolates =
  Object $ HM.singleton (T.pack "isolates") (Array $ toJSON <$> isolates)

--- Build helpers -------------------------------------------------------------

isolateFile :: Isolate -> FilePath
isolateFile isolate =
  "isolates" </> T.unpack (name isolate) ++ ".html"

filesFromIsolates :: Vector Isolate -> Vector FilePath
filesFromIsolates isolates =
  (\i -> outputDir </> isolateFile i) <$> isolates

--- Build Actions -------------------------------------------------------------

buildIsolatePage :: Isolate -> Action ()
buildIsolatePage isolate = do
  let url = isolateFile isolate
  template <- compileTemplate' "templates/isolate.html"
  let pageData = withMeta siteMeta . withMeta isolate $ withUrl url
  writeFile' (outputDir </> url) . T.unpack $ substitute template pageData

buildIndex :: Vector Isolate -> Action ()
buildIndex isolates = do
  indexT <- compileTemplate' "templates/index.html"
  let indexData = withMeta siteMeta $ withIsolatesIndex isolates
  writeFile' (outputDir </> "index.html") . T.unpack $ substitute indexT indexData


-- Top Level ------------------------------------------------------------------

runBuild :: Vector Isolate -> IO ()
runBuild isolates = do
  shakeArgs shakeOptions{shakeFiles="_build/"} $ do
    let outputFiles = filesFromIsolates isolates

    want $ toList outputFiles ++ [outputDir </> "index.html"]

    phony "clean" $ do
      putInfo "Cleaning..."
      removeFilesAfter "_build" ["//*"]
      removeFilesAfter "_site" ["//*"]

    outputDir </> "index.html" %> \_ -> do
      static <- getDirectoryFiles "static" ["*.webp"]
      need ((dataDir </> "metadata.csv") : ((outputDir </>) <$> static))
      buildIndex isolates

    outputDir </> "*.webp" %> \output -> do
      need ["static" </> takeFileName output]
      copyFileChanged ("static" </> takeFileName output) output

    outputDir </> "isolates" </> "*.html" %> \_ -> do
      need [dataDir </> "metadata.csv"]
      _ <- forP (toList isolates) buildIsolatePage
      pure ()

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
  isos <- decodeIsolatesFromFile $ dataDir </> "metadata.csv"

  dataDirContents <- HM.fromList <$> (
    listDirectory dataDir
    >>= filterM (\file -> doesDirectoryExist $ dataDir </> file)
    >>= mapM (\file -> (\files -> (file, Set.fromList files)) <$> listDirectory (dataDir </> file))
    )
  print dataDirContents

  case isos of
    Right isolates -> runBuild $ updateIsolates isolates dataDirContents
    Left err       -> putStrLn err

