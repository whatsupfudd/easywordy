module Options.Runtime (
  defaultRun, RunOptions (..)
  , WpConfig (..), defaultWpConf
  , ZbConfig (..), defaultZbConf
  , OpenAiConfig (..), defaultOpenAiConf
  , WappConfig (..), defaultWappConf
  , TavusConfig (..), defaultTavusConf
  , module DB.Connect

) where
-- , DbConfig (..)

-- import Data.Int (Int)
import Data.Text (Text)

import HttpSup.CorsPolicy (CORSConfig, defaultCorsPolicy)
import DB.Connect (PgDbConfig (..), defaultPgDbConf, MqlDbConfig (..), defaultMqlDbConf)


-- TODO: create a proper Wordpress configuration set:
defaultWpConf :: FilePath -> WpConfig
defaultWpConf homeDir = WpConfig {
  rootPath = homeDir <> "/Wordpress"
  , mqlDbConf = defaultMqlDbConf
}


data WpConfig = WpConfig {
  rootPath :: FilePath
  , mqlDbConf :: MqlDbConfig
  }
  deriving (Show)


newtype ZbConfig = ZbConfig {
  zbRootPath :: FilePath
  }
  deriving (Show)

data OpenAiConfig = OpenAiConfig {
  apiKey :: Maybe Text
  , model :: Maybe Text
  }
  deriving (Show)


defaultZbConf :: FilePath -> ZbConfig
defaultZbConf homeDir = ZbConfig {
  zbRootPath = homeDir <> "/ZhopNess"
  }


defaultOpenAiConf :: OpenAiConfig
defaultOpenAiConf = OpenAiConfig {
  apiKey = Nothing
  , model = Nothing
  }


data WappConfig = WappConfig {
  waDefDir :: FilePath
  , waContentDir :: FilePath
  }
  deriving (Show)


defaultWappConf :: FilePath -> WappConfig
defaultWappConf homeDir = WappConfig {
  waDefDir = homeDir <> "/Wapp/Defs"
  , waContentDir = homeDir <> "/Wapp/content"
  }

newtype TavusConfig = TavusConfig {
  apiKeyTavus :: Text
  }
  deriving (Show)

defaultTavusConf :: TavusConfig
defaultTavusConf = TavusConfig {
  apiKeyTavus = "1234567890"
  }

data RunOptions = RunOptions {
    debug :: Int
    , corsPolicy :: Maybe CORSConfig
    , jwkConfFile :: Maybe FilePath
    , serverPort :: Int
    , serverHost :: Text
    , pgDbConf :: PgDbConfig
    , wp :: WpConfig
    , zb :: ZbConfig
    , wapp :: WappConfig
    , openai :: OpenAiConfig
    , tavus :: TavusConfig
  }
  deriving (Show)


defaultRun :: FilePath -> FilePath -> Text -> Int -> RunOptions
defaultRun homeDir homeConfig server port =
  RunOptions {
    debug = 0
    , corsPolicy = Just defaultCorsPolicy
    , jwkConfFile = Just (homeConfig <> "/jwkConf.json")
    , serverHost = server
    , serverPort = port
    , pgDbConf = defaultPgDbConf
    , wp = defaultWpConf homeDir
    , zb = defaultZbConf homeDir
    , wapp = defaultWappConf homeDir
    , openai = defaultOpenAiConf
    , tavus = defaultTavusConf
  }
