{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Wapp.Apps.KnowDocs.DbOps where

import qualified Data.ByteString.Lazy as Lbs
import Data.Int (Int32)
import qualified Data.Map as Mp
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time.Clock (UTCTime)
import Data.UUID (UUID, fromText)
import Data.Vector (Vector)
import qualified Data.Vector as V

import GHC.Generics (Generic)

import qualified Hasql.TH as H
import qualified Hasql.Session as H
import Hasql.Pool (Pool, use)

import Data.Aeson (encode, FromJSON (..), ToJSON, fromJSON, Result(..), withObject, (.:))

import qualified Wapp.Apps.KnowDocs.DbContainers as Dc
import Wapp.AppDef (NativeLibFunction)
import Hasql.Decoders (timestamptz)
 -- Pool -> IO (Either String (Vector Dc.TopLevelPrez))

getKnowDocs :: NativeLibFunction
getKnowDocs dbPool _ = do
  ieRezA <- use dbPool $ do
    H.statement () [H.vectorStatement|
        select
          id::int4, eid::uuid, title::text, locale::text, created_at::timestamptz
        from basicdoc
        order by created_at desc
      |]
  case ieRezA of
    Left err -> pure . Left . show $ err
    Right prez -> pure . Right . encode $ V.map dbToKnowDoc prez
  where
  dbToKnowDoc :: (Int32, UUID, Text, Text, UTCTime) -> Dc.KnowDoc
  dbToKnowDoc (id, eid, title, locale, createdAt) = Dc.KnowDoc id eid title locale createdAt


-- get_blocks_dfs_at_seq SQL procedure:
-- - params:
-- - return value:
--     TABLE(depth integer, block_id integer, parent_block_id integer, content text, seq_pos numeric, has_more boolean)

newtype DocEid = DocEid {
  docEid :: Text
}
  deriving (Show, Generic)

instance FromJSON DocEid where
  parseJSON = withObject "DocEid" $ \v -> DocEid <$> v .: "doceid"

getDocTree :: NativeLibFunction
getDocTree dbPool (value, mbLabel) = do
  case fromJSON value :: Result DocEid of
    Error err -> pure . Left $ "@[getDocTree] can't decode Value: " <> err
    Success aDocEid ->
      case fromText aDocEid.docEid of
        Nothing -> pure . Left $ "@[getDocTree] can't decode UUID: " <> T.unpack aDocEid.docEid
        Just anEID -> do
          ieRezA <- use dbPool $ do
            H.statement anEID [H.singletonStatement|
              select id::int4 from basicdoc where eid = $1::uuid
            |]
          case ieRezA of
            Left err -> pure . Left . show $ err
            Right anID -> do
              ieRezB <- use dbPool $ do
                H.statement anID [H.vectorStatement|
                  select
                    depth::int4, block_id::int4, parent_block_id::int4?
                    , content::text?, seq_pos::float8, has_more::boolean
                  from
                    get_blocks_dfs_at_seq($1::int4, 1000000)
                |]
              case ieRezB of
                Left err -> pure . Left . show $ err
                Right blockRows -> pure . Right . encode $ V.map dbToBlockNode blockRows
  where
  dbToBlockNode :: (Int32, Int32, Maybe Int32, Maybe Text, Double, Bool) -> Dc.BlockNode
  dbToBlockNode (depth, blockId, mbParentBlockId, mbContent, seqPos, hasMore) =
    Dc.BlockNode depth blockId mbParentBlockId mbContent seqPos hasMore


addNewDocument :: NativeLibFunction
addNewDocument dbPool (value, mbLabel) = do
  case fromJSON value :: Result Dc.NewDocInfo of
    Error err -> pure . Left $ "@[addNewDocument] can't decode Value: " <> err
    Success args -> do
      ieRezA <- use dbPool $ do
        H.statement (args.title, args.locale) [H.singletonStatement|
            insert into basicdoc (title, locale)
              values ($1::text, $2::text)
            returning eid::uuid, title::text, locale::text, created_at::timestamptz
          |]
      case ieRezA of
        Left err -> pure . Left . show $ err
        Right newDoc -> pure . Right . encode $ dbToNewDocResult newDoc
  where
  dbToNewDocResult :: (UUID, Text, Text, UTCTime) -> Dc.NewDocResult
  dbToNewDocResult (eid, title, locale, createdAt) = Dc.NewDocResult eid title locale createdAt