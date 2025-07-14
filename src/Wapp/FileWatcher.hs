{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use forM_" #-}
module Wapp.FileWatcher where

import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (forkIO, threadDelay, killThread, ThreadId)
import Control.Concurrent.Chan (Chan, newChan, readChan)
import Control.Concurrent.MVar (MVar, newMVar, modifyMVar)
import qualified Control.Concurrent.STM as Cs
import qualified Control.Concurrent.STM.TMVar as Ct
import Control.Exception (bracket)
import Control.Monad (forever, void, when)

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Time.Clock (UTCTime, getCurrentTime, diffUTCTime)

import System.FilePath ((</>), takeDirectory, takeExtension, takeFileName)
import System.FSNotify (Event(..), WatchManager, EventIsDirectory (..), watchTreeChan, withManager, stopManager, startManager)


-- | Debouncer state for tracking file modification times
data DebouncerState = DebouncerState { 
    pathMap :: Map FilePath UTCTime
  , duration :: Int -- microseconds
  , threads :: Map FilePath ThreadId -- For tracking and killing threads
  }
 deriving (Show)

-- | Watcher control structure to manage the file watcher lifecycle
data WatcherControl = WatcherControl {
    stopWatcher :: IO ()
  , watcherThreadId :: ThreadId
  }

instance Show WatcherControl where
  show wCtrl = "WatcherControl { threadID: " <> show wCtrl.watcherThreadId <>  " }"

-- | Initialize a new debouncer state with the given duration in milliseconds
initDebouncerState :: Int -> DebouncerState
initDebouncerState durationMs = DebouncerState {
    pathMap = Map.empty
  , duration = durationMs * 1000 -- convert to microseconds
  , threads = Map.empty
  }

-- | Process events from the event channel with debouncing
-- Returns a control structure to stop the watcher
newWatcher :: FilePath -> Cs.TVar FilePath -> Cs.TMVar () -> IO WatcherControl
newWatcher watchDir commChannel updateSignal = do
  let
    changeHandler = fileChangeHandler commChannel updateSignal

  putStrLn $ "@[fileWatcher] watching: " <> watchDir
  -- Create an MVar to hold our debouncer state
  stateVar <- newMVar $ initDebouncerState 500 -- 500ms debounce time
  eventChan <- newChan
  mgr <- startManager 
  _ <- watchTreeChan mgr watchDir isTargetFile eventChan
      
  -- Start the event processor thread
  processorThreadId <- forkIO $ eventProcessor stateVar eventChan changeHandler

  -- Return the control structure
  pure $ WatcherControl { 
        stopWatcher = do
          killThread processorThreadId
          cleanupThreads stateVar
          stopManager mgr
      , watcherThreadId = processorThreadId
    }
  where
  isTargetFile event =
    case event of
      Modified fPath timeStamp dirMode ->
        case dirMode of
          IsFile ->
            (head (takeFileName fPath) /= '.')
            && (case takeExtension fPath of -- eventPath event == filePath
                ".js" -> True
                ".elm" -> True
                ".html" -> True
                ".yaml" -> True
                ".css" -> True
                _ -> False)
          IsDirectory -> False


fileChangeHandler :: Cs.TVar FilePath -> Ct.TMVar () -> FilePath -> IO ()
fileChangeHandler commChannel updateSignal filePath = do
  -- putStrLn $ "@[fileChangeHandler] Detected change in code file: " <> filePath

  -- Update the shared TVar with new bytecode and increment version
  Cs.atomically $ do
    oldState <- Cs.readTVar commChannel
    Cs.writeTVar commChannel filePath

  -- Signal all WebSocket handlers about the update
  Cs.atomically $ Ct.putTMVar updateSignal ()
  -- putStrLn "@[fileChangeHandler] ending."

-- | Process events from the channel
eventProcessor :: MVar DebouncerState -> Chan Event -> (FilePath -> IO ()) -> IO ()
eventProcessor stateVar eventChan deltaHandler =
  forever $ do
    event <- readChan eventChan
    case event of
      Modified path timeStamp dirMode -> handleModifiedEvent stateVar (path, timeStamp, dirMode) deltaHandler
      _ -> pure () -- Ignore other event types

-- | Clean up all debounce threads
cleanupThreads :: MVar DebouncerState -> IO ()
cleanupThreads stateVar = modifyMVar stateVar $ \state -> do
  -- Kill all the threads
  mapM_ killThread state.threads
  
  -- Return a clean state
  return (state { threads = Map.empty }, ())

-- | Handle a modified event with debouncing
handleModifiedEvent :: MVar DebouncerState -> (FilePath, UTCTime, EventIsDirectory) -> (FilePath -> IO ()) -> IO ()
handleModifiedEvent stateVar (path, timeStamp, dirMode) deltaHandler = do
  -- now <- getCurrentTime
  -- Kill any existing debounce thread for this path
  modifyMVar stateVar $ \state -> do
    -- Kill the existing thread if there is one
    case Map.lookup path state.threads of
      Just threadId -> killThread threadId
      Nothing -> return ()
    
    -- Update the file's modification time
    let newPathMap = Map.insert path timeStamp state.pathMap
    
    -- Fork a new debounce thread
    debounceThreadId <- forkIO $ do
      threadDelay state.duration
      quietPeriodCheck stateVar path deltaHandler timeStamp
    
    -- Update the state with the new thread
    let newThreads = Map.insert path debounceThreadId state.threads
    
    return (state { pathMap = newPathMap, threads = newThreads }, ())

-- | Check if there have been no updates to the file during the quiet period
quietPeriodCheck :: MVar DebouncerState -> FilePath -> (FilePath -> IO ()) -> UTCTime -> IO ()
quietPeriodCheck stateVar path deltaHandler checkTime = do
  putStrLn $ "@[quietPeriodCheck] path: " <> path <> ", checkTime: " <> show checkTime
  shouldTrigger <- modifyMVar stateVar $ \state -> do
    case Map.lookup path state.pathMap of
      -- If the file was removed from our map, don't trigger
      Nothing -> return (state, False)
      
      -- If the last modification time is the same as when we scheduled this check,
      -- it means no new modifications have occurred during the quiet period
      Just lastTime -> do
        let noNewModifications = lastTime == checkTime
        
        -- Clean up our state if we're triggering
        let newPathMap = if noNewModifications
                        then Map.delete path state.pathMap  -- Remove from tracking
                        else state.pathMap                  -- Keep tracking
        
        -- Remove this path from our threads map
        let newThreads = Map.delete path state.threads
        
        return (state { pathMap = newPathMap, threads = newThreads }, noNewModifications)
  
  -- If there were no new modifications during quiet period, trigger the callback
  when shouldTrigger $ deltaHandler path

  
  