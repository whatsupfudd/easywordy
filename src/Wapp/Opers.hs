{-# LANGUAGE QuasiQuotes #-}

module Wapp.Opers where


import qualified Data.ByteString as Bs
import Data.Int (Int16, Int32, Int64)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Vector (Vector)
import Data.Time.Clock (NominalDiffTime, nominalDiffTimeToSeconds)

import Hasql.Session (Session, statement)
import Hasql.Pool (Pool, use)
import qualified Hasql.TH as TH


getVersions :: Pool -> IO (Either String (Vector (Int32, Text)))
getVersions pgDb = do
  rezA <- use pgDb locateAllVersions
  case rezA of
    Left err -> pure $ Left $ show err
    Right versions -> pure $ Right versions


getFoldersForVersion :: Pool -> Int32 -> IO (Either String (Vector (Int32, Text)))
getFoldersForVersion pgDb versionID = do
  rezA <- use pgDb $ locateFoldersForVersion versionID
  case rezA of
    Left err -> pure $ Left $ show err
    Right folders -> pure $ Right folders

getFilesForFolder :: Pool -> Int32 -> IO (Either String (Vector (Int32, Text)))
getFilesForFolder pgDb folderID = do
  rezA <- use pgDb $ locateFilesForFolder folderID
  case rezA of
    Left err -> pure $ Left $ show err
    Right files -> pure $ Right files

getAstForFile :: Pool -> Int32 -> IO (Either String (Maybe Bs.ByteString))
getAstForFile pgDb fileID = do
  rezA <- use pgDb $ locateAstForFile fileID
  case rezA of
    Left err -> pure $ Left $ show err
    Right ast -> pure $ Right ast

getConstantsForFile :: Pool -> Int32 -> IO (Either String (Maybe Bs.ByteString))
getConstantsForFile pgDb fileID = do
  rezA <- use pgDb $ locateConstantsForFile fileID
  case rezA of
    Left err -> pure $ Left $ show err
    Right constants -> pure $ Right constants

-- *** SQL *** --

locateAllVersions :: Session (Vector (Int32, Text))
locateAllVersions =
  statement () [TH.vectorStatement|
    select uid::int4, label::text from wpversion
  |]


locateFoldersForVersion :: Int32 -> Session (Vector (Int32, Text))
locateFoldersForVersion versionID =
  statement versionID [TH.vectorStatement|
    select uid::int4, path::text from folder where versionRef = $1::int4
  |]


locateFilesForFolder :: Int32 -> Session (Vector (Int32, Text))
locateFilesForFolder folderID =
  statement folderID [TH.vectorStatement|
    select uid::int4, path::text from file where folderRef = $1::int4
  |]

locateAstForFile :: Int32 -> Session (Maybe Bs.ByteString)
locateAstForFile fileID =
  statement fileID [TH.maybeStatement|
    select value::bytea from ast where fileref = $1::int4
  |]

locateConstantsForFile :: Int32 -> Session (Maybe Bs.ByteString)
locateConstantsForFile fileID =
  statement fileID [TH.maybeStatement|
    select value::bytea from constant where fileref = $1::int4
  |]
