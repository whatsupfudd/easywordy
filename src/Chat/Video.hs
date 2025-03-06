{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Chat.Video where

import qualified Data.ByteString.Lazy as Lbs


import qualified Data.ByteString as Bs
import Data.Char (isUpper, toUpper, toLower)
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack)
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Lazy (fromStrict)
import Data.Time.Clock (getCurrentTime, diffUTCTime, UTCTime)
import Data.Time (formatTime, defaultTimeLocale, getZonedTime, ZonedTime)

import GHC.Generics (Generic)
import qualified Data.Aeson as Ae

import Hasql.Pool (Pool)

import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Client.TLS as TLS
import Network.HTTP.Types.Status (statusCode)

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Htmx as X
import qualified Text.Blaze.Htmx.WebSockets as X
import qualified Text.Blaze.Internal as Bli
import qualified Text.Blaze.Svg11 as S
import qualified Text.Blaze.Svg11.Attributes as Sa
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import qualified Text.Markdown as Md

import qualified Options.Runtime as Rt
import qualified Wapp.Types as Wt


startSession :: Wt.InternalFunction
startSession rtOpts pgDb (jsonParams, content) = do
  rezA <- fetchConvURL rtOpts
  case rezA of
    Left err -> do
      pure . Right . Wt.BasicFR $ renderHtml $
        H.h2 H.! A.class_ "text-2xl font-bold" $ H.text ("Avatar error: " <>  pack err)
    Right convURL -> do
      pure . Right . Wt.BasicFR $ renderHtml $
        showResult convURL

{-
The Tavus API uses the following format (provide as Javascript code):
const options = {
  method: 'POST',
  headers: {'x-api-key': '<api-key>', 'Content-Type': 'application/json'},
  body: '{
        "replica_id":"r79e1c033f"
      , "persona_id":"p5317866"
      , "callback_url":"https://yourwebsite.com/webhook"
      , "conversation_name":"A Meeting with Hassaan"
      , "conversational_context":"You are about to talk to Hassaan, one of the cofounders of Tavus. He loves to talk about AI, startups, and racing cars."
      , "custom_greeting":"Hey there Hassaan, long time no see!"
      , "properties":{
          "max_call_duration":3600
          ,"participant_left_timeout":60
          ,"participant_absent_timeout":300
          ,"enable_recording":true
          ,"enable_transcription":true
          ,"apply_greenscreen":true
          ,"language":"english"
          ,"recording_s3_bucket_name":"conversation-recordings"
          ,"recording_s3_bucket_region":"us-east-1"
          ,"aws_assume_role_arn":""
        }
      }'
};

fetch('https://tavusapi.com/v2/conversations', options)
  .then(response => response.json())
  .then(response => console.log(response))
  .catch(err => console.error(err));
-}


data TavusRequest = TavusRequest {
  replicaId :: Text
  -- , personaId :: Text
  -- , callbackUrl :: Maybe Text
  , conversationName :: Text
  , conversationalContext :: Text
  , customGreeting :: Text
  , properties :: TavusProperties
  }
  deriving (Show, Generic)


data TavusProperties = TavusProperties {
  maxCallDuration :: Int
  , participantLeftTimeout :: Int
  , participantAbsentTimeout :: Int
  -- , enableRecording :: Bool
  -- , enableTranscription :: Bool
  , applyGreenscreen :: Bool
  , language :: Text
  -- , recordingS3BucketName :: Text
  -- , recordingS3BucketRegion :: Text
  -- , awsAssumeRoleArn :: Text
  }
  deriving (Show, Generic)

{-
A successful response from the Tavus API looks like this:
{
  "conversation_id": "c123456",
  "conversation_name": "A Meeting with Hassaan",
  "status": "active",
  "conversation_url": "https://tavus.daily.co/c123456",
  "replica_id": "r79e1c033f",
  "persona_id": "p5317866",
  "created_at": "<string>"
}
-}

data TavusResponse = TavusResponse {
  conversationId :: Text
  , conversationName :: Text
  , conversationUrl :: Text
  , createdAt :: Text
  , status :: Text
  , callbackUrl :: Maybe Text
  }
  deriving (Show, Generic)

-- Convert camelCase to snake_case for JSON encoding
instance Ae.ToJSON TavusRequest where
  toJSON = Ae.genericToJSON Ae.defaultOptions {
    Ae.fieldLabelModifier = camelToSnake
  }

instance Ae.ToJSON TavusProperties where
  toJSON = Ae.genericToJSON Ae.defaultOptions {
    Ae.fieldLabelModifier = camelToSnake
  }

-- Convert snake_case to camelCase for JSON decoding  
instance Ae.FromJSON TavusResponse where
  parseJSON = Ae.withObject "TavusResponse" $ \o -> TavusResponse
    <$> o Ae..: "conversation_id"
    <*> o Ae..: "conversation_name"
    <*> o Ae..: "conversation_url"
    <*> o Ae..: "created_at"
    <*> o Ae..: "status"
    <*> o Ae..:? "callback_url"

{-
  Ae.genericParseJSON Ae.defaultOptions {
    Ae.fieldLabelModifier = snakeToCamel
  }
-}


fetchConvURL :: Rt.RunOptions -> IO (Either String Text)
fetchConvURL rtOpts =
  let
    tavusRequest = TavusRequest {
        replicaId = "r4c41453d2"
        -- , personaId = "p5317866"
        -- , callbackUrl = Nothing
        , conversationName = "Helping the class learn more about mental health"
        , conversationalContext = "The class is studying the effects of conventional education approach to mental health. "
           <> "Contrary to progressive education where students have a personalized and asynchronous learning experience, "
           <> "conventional education is a one-size-fits-all approach where students are expected to be present in a classroom at a specific time and date. "
           <> "This has been shown to be ineffective and even harmful to students' mental health. We investigate all aspects of these issues."
        , customGreeting = "Hi, I'm the digital twin in Z1, I am available when a real person isn't!"
        , properties = TavusProperties {
            maxCallDuration = 300
            , participantLeftTimeout = 30
            , participantAbsentTimeout = 30
            -- , enableRecording = True
            -- , enableTranscription = True
            , applyGreenscreen = False
            , language = "english"
            -- , recordingS3BucketName = "conversation-recordings"
            -- , recordingS3BucketRegion = "us-east-1"
            -- , awsAssumeRoleArn = ""
        }
      }
  in do
  manager <- Http.newManager TLS.tlsManagerSettings
  initialRequest <- Http.parseRequest "https://tavusapi.com/v2/conversations"
  let request = initialRequest { Http.method = "POST"
          , Http.requestHeaders = [("x-api-key", encodeUtf8 rtOpts.tavus.apiKeyTavus), ("Content-type", "application/json")]
          , Http.requestBody = Http.RequestBodyLBS $ Ae.encode tavusRequest
        }
  putStrLn $ "@[fetchConvURL] request: " <> show request
  response <- Http.httpLbs request manager
  putStrLn $ "@[fetchConvURL] response: " <> show response
  case statusCode response.responseStatus of
    200 -> do
      case Ae.eitherDecode response.responseBody :: Either String TavusResponse of
        Left err -> do
          pure . Left $ "@[fetchConvURL] failed to decode response body: " <> show err
        Right tavusResponse -> do
          pure . Right $ tavusResponse.conversationUrl
    _ -> do
      pure . Left $ "@[fetchConvURL] failed to create conversation: " <> show response.responseStatus


showResult :: Text -> H.Html
showResult convURL = do
  H.div H.! A.class_ "bg-gray-50 dark:bg-gray-900 p-4 md:ml-64 lg:mr-16 min-h-full pt-20" $
    H.iframe H.! A.src (Bli.textValue convURL) H.! allowAttr "camera; microphone" H.! A.height "950" H.! A.width "1100" H.! A.title "Demo #2" $ mempty


allowAttr :: Text -> H.Attribute
allowAttr values = Bli.attribute "allow" " allow=\"" (Bli.textValue values)


-- Helper function to convert camelCase to snake_case
camelToSnake :: String -> String
camelToSnake = foldr (\c acc -> if isUpper c 
                                then '_' : toLower c : acc
                                else c : acc) []

-- Helper function to convert snake_case to camelCase
snakeToCamel :: String -> String
snakeToCamel [] = []
snakeToCamel ('_':c:cs) = toUpper c : snakeToCamel cs 
snakeToCamel (c:cs) = c : snakeToCamel cs
