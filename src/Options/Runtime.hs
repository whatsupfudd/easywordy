module Options.Runtime (
  defaultRun, RunOptions (..)
  , WpConfig (..), defaultWpConf
  , ZbConfig (..), defaultZbConf
  , module DB.Connect

) where
-- , DbConfig (..)

-- import Data.Int (Int)
import Data.Text (Text)

import HttpSup.CorsPolicy (CORSConfig, defaultCorsPolicy)
import DB.Connect (PgDbConfig (..), defaultPgDbConf, MqlDbConfig (..), defaultMqlDbConf)


-- TODO: create a proper Wordpress configuration set:
defaultWpConf :: WpConfig
defaultWpConf = WpConfig {
  rootPath = "../Lib/wordpress"
  , mqlDbConf = defaultMqlDbConf
}


data WpConfig = WpConfig {
  rootPath :: FilePath
  , mqlDbConf :: MqlDbConfig
  }
  deriving (Show)


data ZbConfig = ZbConfig {
  zbRootPath :: FilePath
  }
  deriving (Show)

defaultZbConf :: ZbConfig
defaultZbConf = ZbConfig {
  zbRootPath = "../Lib/ZhopNess/wapp"
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
  }
  deriving (Show)


defaultRun :: FilePath -> Text -> Int -> RunOptions
defaultRun homeDir server port =
  RunOptions {
    debug = 0
    , corsPolicy = Just defaultCorsPolicy
    , jwkConfFile = Just (homeDir <> "/jwkConf.json")
    , serverHost = server
    , serverPort = port
    , pgDbConf = defaultPgDbConf
    , wp = defaultWpConf
    , zb = defaultZbConf
  }
