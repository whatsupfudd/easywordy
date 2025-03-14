module MainLogic
where

import Data.Text (pack)
import qualified System.Environment as Env

import qualified Options as Opt
import qualified Options.Cli as Opt (CliOptions (..), EnvOptions (..), Command (..))
import qualified Options.ConfFile as Opt (FileOptions (..))
import Commands as Cmd


runWithOptions :: Opt.CliOptions -> Opt.FileOptions -> IO ()
runWithOptions cliOptions fileOptions = do
  -- putStrLn $ "@[runWithOptions] cliOpts: " <> show cliOptions
  -- putStrLn $ "@[runWithOptions] fileOpts: " <> show fileOptions
  case cliOptions.job of
    Nothing -> do
      putStrLn "@[runWithOptions] start on nil command."
    Just aJob -> do
      -- Get environmental context in case it's required in the merge. Done here to keep the merge pure:
      mbHome <- Env.lookupEnv "EASYWORDY"
      mbConfig <- Env.lookupEnv "EASYWORDY_CONFIG"
      let
        envOptions = Opt.EnvOptions {
            appHome = mbHome
            , appConfig = mbConfig
          }
        cmdExecutor =
          case aJob of
            Opt.HelpCmd -> Cmd.helpCmd
            Opt.VersionCmd -> Cmd.versionCmd
            Opt.ServerCmd -> Cmd.serverCmd
            Opt.TestJS -> Cmd.testJsCmd 2 ""
      rtOptions <- Opt.mergeOptions cliOptions fileOptions envOptions
        -- switchboard to command executors:
      result <- cmdExecutor rtOptions
      -- TODO: return a properly kind of conclusion.
      pure ()
