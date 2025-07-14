{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}

module Options.ConfFile where

import qualified Control.Exception as Cexc
import qualified Data.Aeson as Aes
import Data.Text (Text)
import GHC.Generics
import qualified System.Directory as Sdir
import qualified System.Environment as Senv
import qualified System.FilePath.Posix as Spsx
import qualified System.IO.Error as Serr

import qualified Data.Yaml as Yaml


data ServerOpts = ServerOpts {
    port :: Maybe Int
  , host :: Maybe Text
  , cache :: Maybe FilePath
  }
  deriving stock (Show, Generic)

data JwtOpts = JwtOpts {
    jEnabled :: Maybe Bool
  , keyFile :: Maybe FilePath
  }
  deriving stock (Show, Generic)

data CorsOpts = CorsOpts {
    oEnabled :: Maybe Bool
  , allowed :: Maybe [String]
  }
  deriving stock (Show, Generic)


data PgDbOpts = DbOpts {
  port :: Maybe Int
  , host :: Maybe String
  , user :: Maybe String
  , passwd :: Maybe String
  , dbase :: Maybe String
  , poolSize :: Maybe Int
  , poolTimeOut :: Maybe Int
}
  deriving stock (Show, Generic)


data MqlDbOpts = MqlDbOpts {
  port :: Maybe Int
  , host :: Maybe String
  , user :: Maybe String
  , passwd :: Maybe String
  , dbase :: Maybe String
}
  deriving stock (Show, Generic)


data WpOptions = WpOptions {
  rootPath :: Maybe FilePath
  , db :: Maybe MqlDbOpts
  }
  deriving stock (Show, Generic)

newtype ZbOptions = ZbOptions {
    zbRoot :: Maybe FilePath
  }
  deriving stock (Show, Generic)

data WappOptions = WappOptions {
  waDef :: Maybe FilePath
  , waContent :: Maybe FilePath
  }
  deriving stock (Show, Generic)


data OpenAiOptions = OpenAiOptions {
  apiKey :: Maybe Text
  , model :: Maybe Text
  }
  deriving stock (Show, Generic)


newtype TavusOptions = TavusOptions {
  apiKey :: Maybe Text
  }
  deriving stock (Show, Generic)

data S3Options = S3Options {
  accessKey :: Maybe Text
  , secretKey :: Maybe Text
  , host :: Maybe Text
  , region :: Maybe Text
  , bucket :: Maybe Text
  }
  deriving stock (Show, Generic)

data FileOptions = FileOptions {
  debug :: Maybe Int
  , primaryLocale :: Maybe String
  , db :: Maybe PgDbOpts
  , server :: Maybe ServerOpts
  , jwt :: Maybe JwtOpts
  , cors :: Maybe CorsOpts
  , wordpress :: Maybe WpOptions
  , zhopness :: Maybe ZbOptions
  , wapp :: Maybe WappOptions
  , openai :: Maybe OpenAiOptions
  , tavus :: Maybe TavusOptions
  , s3store :: Maybe S3Options
 }
 deriving stock (Show, Generic)

defaultConfName :: FilePath
defaultConfName = ".fudd/easywordy/config.yaml"


defaultConfigFilePath :: IO (Either String FilePath)
defaultConfigFilePath = do
  eiHomeDir <- Cexc.try Sdir.getHomeDirectory :: IO (Either Serr.IOError FilePath)
  case eiHomeDir of
    Left err -> pure . Left $ "@[defaultConfigFilePath] err: " <> show err
    Right aPath -> pure . Right $ Spsx.joinPath [aPath, defaultConfName]


-- YAML support:
instance Aes.FromJSON FileOptions
instance Aes.FromJSON PgDbOpts
instance Aes.FromJSON MqlDbOpts
instance Aes.FromJSON ServerOpts
instance Aes.FromJSON JwtOpts
instance Aes.FromJSON CorsOpts
instance Aes.FromJSON WpOptions
instance Aes.FromJSON ZbOptions
instance Aes.FromJSON OpenAiOptions
instance Aes.FromJSON WappOptions
instance Aes.FromJSON TavusOptions
instance Aes.FromJSON S3Options

parseFileOptions :: FilePath -> IO (Either String FileOptions)
parseFileOptions filePath =
  let
    fileExt = Spsx.takeExtension filePath
  in case fileExt of
    ".yaml" -> do
      -- putStrLn $ "@[parseFileOptions] filePath: " <> filePath
      eiRez <- Yaml.decodeFileEither filePath
      -- putStrLn $ "@[parseFileOptions] eiRez: " <> show eiRez
      case eiRez of
        Left err -> pure . Left $ "@[parseYaml] err: " <> show err
        Right aContent ->
          pure $ Right aContent
    _ -> pure . Left $ "@[parseFileOptions] unknown conf-file extension: " <> fileExt
