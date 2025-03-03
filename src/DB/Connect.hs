module DB.Connect where

import Control.Exception (bracket)
import Control.Monad.Cont (ContT (..), liftIO)

import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Time.Clock (DiffTime)

import GHC.Word (Word16)

import qualified Hasql.Connection as DbConn
import           Hasql.Pool (Pool, acquire, release)

import qualified Database.MySQL.Base as Msql


data PgDbConfig = PgDbConfig {
  port :: Word16
  , host :: ByteString
  , user :: ByteString
  , passwd :: ByteString
  , dbase :: ByteString
  , poolSize :: Int
  , acqTimeout :: DiffTime
  , poolTimeOut :: DiffTime
  , poolIdleTime :: DiffTime
  }
  deriving (Show)


data MqlDbConfig = MqlDbConfig {
  hostMq :: String
  , portMq :: Int
  , userMq :: ByteString
  , passwdMq :: ByteString
  , dbaseMq :: ByteString
  }
  deriving (Show)


defaultPgDbConf = PgDbConfig {
  port = 5432
  , host = "test"
  , user = "test"
  , passwd = "test"
  , dbase = "test"
  , poolSize = 5
  , acqTimeout = 5
  , poolTimeOut = 60
  , poolIdleTime = 300
  }

defaultMqlDbConf = MqlDbConfig {
  hostMq = "test"
  , portMq = 3306
  , userMq = "test"
  , passwdMq = "test"
  , dbaseMq = "test"
  }


startPg :: PgDbConfig -> ContT r IO Pool
startPg dbC =
  let
    dbSettings = DbConn.settings dbC.host dbC.port dbC.user dbC.passwd dbC.dbase
  in do
  liftIO . putStrLn $ "@[startPg] user: " <> show dbC.user <> " db: " <> show dbC.dbase <> "."
  ContT $ bracket (acquire dbC.poolSize dbC.acqTimeout dbC.poolTimeOut dbSettings) release

startMql :: MqlDbConfig -> ContT r IO Msql.MySQLConn
startMql dbC =
  let
    dbSettings = Msql.defaultConnectInfo {
          Msql.ciUser = dbC.userMq
          , Msql.ciPassword = dbC.passwdMq
          , Msql.ciDatabase = dbC.dbaseMq
          , Msql.ciHost = dbC.hostMq
          , Msql.ciPort = fromIntegral dbC.portMq
        }
  in do
  liftIO . putStrLn $ "@[startMql] user: " <> show dbC.userMq <> " db: " <> show dbC.dbaseMq <> "."
  rezA <- ContT $ bracket (Msql.connect dbSettings) Msql.close
  liftIO . putStrLn $ "@[startMql] done."
  pure rezA
