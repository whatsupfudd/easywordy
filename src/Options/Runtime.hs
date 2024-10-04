module Options.Runtime (defaultRun, RunOptions (..)) where
-- , DbConfig (..)

-- import Data.Int (Int)
import Data.Text (Text)

import HttpSup.CorsPolicy (CORSConfig, defaultCorsPolicy)
-- import DB.Connect (DbConfig (..), defaultDbConf)


data RunOptions = RunOptions {
    debug :: Int
    , corsPolicy :: Maybe CORSConfig
    , jwkConfFile :: Maybe FilePath
    , serverPort :: Int
    , serverHost :: Text
  }
  deriving (Show)

defaultRun :: RunOptions
defaultRun =
  RunOptions {
    debug = 0
    , corsPolicy = Just defaultCorsPolicy
    , jwkConfFile = Just "/bwork/wrkspc/karlin/.fudd/daniell/jwkConf.json"
    , serverPort = 8885
    , serverHost = "localhost"
  }
