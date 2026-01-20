module Options  (
  module Cl
  , module Fo
  , module Rt
  , mergeOptions
 )
where

import Control.Monad.State ( MonadState (put), MonadIO, runStateT, State, StateT, modify, lift, liftIO )
import Control.Monad.Except ( ExceptT, MonadError (throwError) )
import Data.Functor.Identity ( Identity (..) )

import Data.Foldable (for_)
import Data.Either (lefts)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import qualified System.IO.Error as Serr
import qualified Control.Exception as Cexc
import qualified System.Posix.Env as Senv
import qualified System.Directory as Sdir

import qualified HttpSup.CorsPolicy as Hcrs

import qualified Options.Cli as Cl (CliOptions (..), EnvOptions (..))
import qualified Options.ConfFile as Fo (FileOptions (..), PgDbOpts (..), MqlDbOpts (..), CorsOpts (..), JwtOpts (..), ServerOpts (..)
                  , WpOptions (..), ZbOptions (..), OpenAiOptions (..), WappOptions (..), TavusOptions (..), S3Options (..), AiservOptions (..))
import qualified Assets.Types as At
import qualified Options.Runtime as Rt (RunOptions (..), defaultRun, WpConfig (..), defaultWpConf, PgDbConfig (..), defaultPgDbConf
                , MqlDbConfig (..), defaultMqlDbConf, ZbConfig (..), defaultZbConf, OpenAiConfig (..), defaultOpenAiConf
                , WappConfig (..), defaultWappConf, TavusConfig (..), defaultTavusConf, AiservConfig (..), defaultAiservConf)


type ConfError = Either String ()
type RunOptSt = State Rt.RunOptions ConfError
type RunOptIOSt = StateT Rt.RunOptions IO ConfError
type PgDbOptIOSt = StateT Rt.PgDbConfig (StateT Rt.RunOptions IO) ConfError
type WpOptIOSt = StateT Rt.WpConfig (StateT Rt.RunOptions IO) ConfError
type MqlDbOptIOSt = StateT Rt.MqlDbConfig (StateT Rt.WpConfig (StateT Rt.RunOptions IO)) ConfError
type ZbOptIOSt = StateT Rt.ZbConfig (StateT Rt.RunOptions IO) ConfError
type OpenAiOptIOSt = StateT Rt.OpenAiConfig (StateT Rt.RunOptions IO) ConfError
type WappOptIOSt = StateT Rt.WappConfig (StateT Rt.RunOptions IO) ConfError
type TavusOptIOSt = StateT Rt.TavusConfig (StateT Rt.RunOptions IO) ConfError
type S3OptIOSt = StateT At.S3Config (StateT Rt.RunOptions IO) ConfError
type AiservOptIOSt = StateT Rt.AiservConfig (StateT Rt.RunOptions IO) ConfError


mconf :: MonadState s m => Maybe t -> (t -> s -> s) -> m ()
mconf mbOpt setter =
  case mbOpt of
    Nothing -> pure ()
    Just opt -> modify $ setter opt


innerConf :: MonadState s f => (t1 -> s -> s) -> (t2 -> StateT t1 f (Either String b)) -> t1 -> Maybe t2 -> f ConfError
innerConf updState innerParser defaultVal mbOpt =
  case mbOpt of
    Nothing -> pure $ Right ()
    Just anOpt -> do
      (result, updConf) <- runStateT (innerParser anOpt) defaultVal
      case result of
        Left errMsg -> pure $ Left errMsg
        Right _ -> do
          modify $ updState updConf
          pure $ Right ()


mergeOptions :: Cl.CliOptions -> Fo.FileOptions -> Cl.EnvOptions -> IO Rt.RunOptions
mergeOptions cli file env = do
  appHome <- case env.appHome of
    Nothing -> do
      eiHomeDir <- Cexc.try Sdir.getHomeDirectory :: IO (Either Serr.IOError FilePath)
      case eiHomeDir of
        Left err -> pure "/tmp"
        Right aVal -> pure $ aVal <> "/EasyWordy"
    Just aVal -> pure aVal
  appConfig <- case env.appConfig of
    Nothing -> do
      eiConfigDir <- Cexc.try Sdir.getHomeDirectory :: IO (Either Serr.IOError FilePath)
      case eiConfigDir of
        Left err -> pure ".fudd/easywordy"
        Right aVal -> pure $ aVal <> "/.fudd/easywordy"
    Just aVal -> pure aVal
  -- putStrLn $ "@[mergeOptions] file: " <> show file
  (result, runtimeOpts) <- runStateT (parseOptions cli file appHome) (Rt.defaultRun appHome appConfig "http://localhost" 8885)
  case result of
    Left errMsg -> error errMsg
    Right _ -> pure runtimeOpts

  where
  parseOptions :: Cl.CliOptions -> Fo.FileOptions -> FilePath -> RunOptIOSt
  parseOptions cli file appHome = do
    mconf cli.debug $ \nVal s -> s { Rt.debug = nVal }
    for_ file.server parseServer
    innerConf (\nVal s -> s { Rt.pgDbConf = nVal }) parsePgDb Rt.defaultPgDbConf file.db
    for_ file.jwt parseJWT
    for_ file.cors parseCors
    innerConf (\nVal s -> s { Rt.wp = nVal }) parseWp (Rt.defaultWpConf appHome) file.wordpress
    innerConf (\nVal s -> s { Rt.zb = nVal }) parseZb (Rt.defaultZbConf appHome) file.zhopness
    innerConf (\nVal s -> s { Rt.wapp = nVal }) parseWapp (Rt.defaultWappConf appHome) file.wapp
    innerConf (\nVal s -> s { Rt.openai = nVal }) parseOpenAi Rt.defaultOpenAiConf file.openai
    innerConf (\nVal s -> s { Rt.tavus = nVal }) parseTavus Rt.defaultTavusConf file.tavus
    innerConf (\nVal s -> s { Rt.s3store = Just nVal }) parseS3 At.defaultS3Conf file.s3store
    innerConf (\nVal s -> s { Rt.aiserv = Just nVal }) parseAiserv Rt.defaultAiservConf file.aiserv
    -- pure $ Right ()


  parsePgDb :: Fo.PgDbOpts -> PgDbOptIOSt
  parsePgDb dbO = do
    mconf dbO.host $ \nVal s -> s { Rt.host = T.encodeUtf8 . T.pack $ nVal }
    mconf dbO.port $ \nVal s -> s { Rt.port = fromIntegral nVal }
    mconf dbO.user $ \nVal s -> s { Rt.user = T.encodeUtf8 . T.pack $ nVal }
    mconf dbO.passwd $ \nVal s -> s { Rt.passwd = T.encodeUtf8 . T.pack $ nVal }
    mconf dbO.dbase $ \nVal s -> s { Rt.dbase = T.encodeUtf8 . T.pack $ nVal }
    pure $ Right ()


  parseWp :: Fo.WpOptions -> WpOptIOSt
  parseWp wpO = do
    resolveValue wpO.rootPath $ \nVal s -> s { Rt.rootPath = nVal }
    innerConf (\nVal s -> s { Rt.mqlDbConf = nVal }) parseMqlDb Rt.defaultMqlDbConf wpO.db
    pure $ Right ()


  parseZb :: Fo.ZbOptions -> ZbOptIOSt
  parseZb zbO = do
    resolveValue zbO.zbRoot $ \nVal s -> s { Rt.zbRootPath = nVal }
  

  parseWapp :: Fo.WappOptions -> WappOptIOSt
  parseWapp wappO = do
    rezA <- resolveValue wappO.waDef $ \nVal s -> s { Rt.waDefDir = nVal }
    rezB <- resolveValue wappO.waContent $ \nVal s -> s { Rt.waContentDir = nVal }
    pure $ case lefts [rezA, rezB] of
      [] -> Right ()
      errs -> Left $ unlines errs


  parseOpenAi :: Fo.OpenAiOptions -> OpenAiOptIOSt
  parseOpenAi openAiO = do
    mconf openAiO.apiKey $ \nVal s -> s { Rt.apiKey = Just nVal }
    mconf openAiO.model $ \nVal s -> s { Rt.model = Just nVal }
    pure $ Right ()

  parseAiserv :: Fo.AiservOptions -> AiservOptIOSt
  parseAiserv aiservO = do
    mconf aiservO.token $ \nVal s -> s { Rt.token = T.unpack nVal }
    mconf aiservO.server $ \nVal s -> s { Rt.server = T.unpack nVal }
    pure $ Right ()


  parseMqlDb :: Fo.MqlDbOpts -> MqlDbOptIOSt
  parseMqlDb dbO = do
    mconf dbO.host $ \nVal s -> s { Rt.hostMq = nVal }
    mconf dbO.port $ \nVal s -> s { Rt.portMq = nVal }
    mconf dbO.user $ \nVal s -> s { Rt.userMq = T.encodeUtf8 . T.pack $ nVal }
    mconf dbO.passwd $ \nVal s -> s { Rt.passwdMq = T.encodeUtf8 . T.pack $ nVal }
    mconf dbO.dbase $ \nVal s -> s { Rt.dbaseMq = T.encodeUtf8 . T.pack $ nVal }
    pure $ Right ()



  parseServer :: Fo.ServerOpts -> RunOptIOSt
  parseServer so = do
    mconf so.port $ \nVal s -> s { Rt.serverPort = nVal }
    mconf so.host $ \nVal s -> s { Rt.serverHost = nVal }
    pure $ Right ()


  parseJWT :: Fo.JwtOpts -> RunOptIOSt
  parseJWT jo = do
    case jo.jEnabled of
      Just False -> do
        modify $ \s -> s { Rt.jwkConfFile = Nothing }
        pure $ Right ()
      _ ->
        resolveValue jo.keyFile $ \nVal s -> s { Rt.jwkConfFile = Just nVal }


  parseCors :: Fo.CorsOpts -> RunOptIOSt
  parseCors co = do
    case co.oEnabled of
      Just False ->
        modify $ \s -> s { Rt.corsPolicy = Nothing }
      _ -> do
        mconf co.allowed $ \nVal s -> s { Rt.corsPolicy = Just $ Hcrs.defaultCorsPolicy { Hcrs.allowedOrigins = map T.pack nVal } }
    pure $ Right ()


  parseTavus :: Fo.TavusOptions -> TavusOptIOSt
  parseTavus tavusO = do
    mconf tavusO.apiKey $ \nVal s -> s { Rt.apiKeyTavus = nVal }
    pure $ Right ()


  parseS3 :: Fo.S3Options -> S3OptIOSt
  parseS3 s3O = do
    mconf s3O.accessKey $ \nVal s -> s { At.user = nVal }
    mconf s3O.secretKey $ \nVal s -> s { At.passwd = nVal }
    mconf s3O.host $ \nVal s -> s { At.host = nVal }
    mconf s3O.region $ \nVal s -> s { At.region = nVal }
    mconf s3O.bucket $ \nVal s -> s { At.bucket = nVal }
    pure $ Right ()

-- | resolveEnvValue resolves an environment variable value.
resolveEnvValue :: FilePath -> IO (Maybe FilePath)
resolveEnvValue aVal =
  case head aVal of
      '$' ->
        let
          (envName, leftOver) = break ('/' ==) aVal
        in do
        mbEnvValue <- Senv.getEnv $ tail envName
        case mbEnvValue of
          Nothing -> pure Nothing
          Just aVal -> pure . Just $ aVal <> leftOver
      _ -> pure $ Just aVal


resolveValue :: (MonadIO f, MonadState s f) => Maybe String -> (String -> s -> s) -> f ConfError
resolveValue aVal setter = do
  case aVal of
      Nothing -> pure $ Right ()
      Just aVal -> do
        mbRezVal <- liftIO $ resolveEnvValue aVal
        case mbRezVal of
          Nothing -> pure . Left $ "Could not resolve value: " <> aVal
          Just aVal -> do
            modify $ setter aVal
            pure $ Right ()

