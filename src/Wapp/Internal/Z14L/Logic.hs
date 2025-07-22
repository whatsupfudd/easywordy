{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Wapp.Internal.Z14L.Logic where

import Control.Concurrent (threadDelay)

import qualified Data.ByteString as Bs
import qualified Data.ByteString.Lazy as Lbs
import Data.Int (Int32)
import Data.Maybe (fromMaybe, isNothing)
import Data.Text (Text, pack, unpack)
import qualified Data.Text.Encoding as T
import Data.Text.Lazy (fromStrict)
import Data.Time.Clock (getCurrentTime, diffUTCTime, UTCTime)
import Data.Time (formatTime, defaultTimeLocale, getZonedTime, ZonedTime)
import Data.UUID (UUID)
import qualified Data.UUID as Uu

import GHC.Generics (Generic)

import qualified Network.HTTP.Client as Hc
import qualified Network.HTTP.Types.Status as Hs
import qualified Network.HTTP.Client.TLS as Ht
import qualified Data.Aeson as Ae
import qualified Data.Aeson.Key as Aek

import Hasql.Pool (Pool)

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
import qualified Wapp.AppDef as Wd

data UserProfile = UserProfile {
  avatar :: Text
  , altAvatar :: Text
  , name :: Text
  }


data ChatMessage =
  FromCM ChatMessageContent
  | ToCM ChatMessageContent


data ChatMessageContent = ChatMessageContent {
  userProfile :: UserProfile
  , content :: [ [H.Html]]
  , timestamp :: String
  , messageID :: Text
  }


-- Recover from AI Server package, client versions:
data InvokeRequest = InvokeRequest { 
    function :: UUID
  , context :: Maybe UUID
  , parameters :: Ae.Value
  , content :: Text
  , files :: [UUID]
  , references :: [UUID]
  } deriving (Show, Eq, Generic)

instance Ae.ToJSON InvokeRequest where
  toEncoding = Ae.genericToEncoding Ae.defaultOptions { Ae.omitNothingFields = True }

data InvokeResponse = InvokeResponse { 
    requestEId :: UUID
  , contextEId :: UUID
  , status :: Text
  , result :: Maybe Text
  }
  deriving (Show, Eq, Generic, Ae.FromJSON)


data ResponseMsg = ResponseMsg {
  requestEId :: UUID
  , contextEId :: UUID
  , status :: Text
  , result :: ResponseInfo
  }
  deriving (Show, Eq, Generic, Ae.FromJSON)

data ResponseInfo = ResponseInfo {
  responseEId :: UUID
  , result :: ResponseContent
  }
  deriving (Show, Eq, Generic, Ae.FromJSON)


data ResponseContent =
  TextBlockRK TextBlock
  | AssetRK AssetRef
  | NoResponseYetRK
  deriving (Show, Eq, Generic, Ae.FromJSON)

data TextBlock = TextBlock {
  content :: Text
  , format :: Text
  } deriving (Show, Eq, Generic, Ae.FromJSON)

data AssetRef = AssetRef {
  assetEId :: UUID
  , notes :: Maybe Text
  , size :: Int32
  } deriving (Show, Eq, Generic, Ae.FromJSON)


data ParameterValue = ParameterValue {
    name :: Text
  , value :: Text
  } deriving (Show, Eq, Generic)
instance Ae.ToJSON ParameterValue where
  toEncoding aP = Ae.pairs $ (Aek.fromString . unpack) aP.name Ae..= aP.value

-- Structures for the client side:
data ContentReply =
  TextCR Text
  | AssetCR AssetKind UUID
  | ErrorCR String
  deriving (Show, Eq)

data AssetKind =
  ImageAK
  | VideoAK
  | AudioAK
  | FileAK
  deriving (Show, Eq)


receiveMsg :: Wd.InternalFunction
receiveMsg rtOpts pgDb (jsonParams, content) =
  case rtOpts.aiserv of
    Nothing ->
      let
        response = renderHtml $
          H.div H.! A.class_ "text-gray-900 dark:text-gray-100" $
            H.toHtml ("No AI service token" :: Text)
      in
      pure . Right $ Wd.BasicFR (response, Nothing)
    Just aiConf ->
      case content of
        Nothing ->
          let
            response = renderHtml $
              H.div H.! A.class_ "text-gray-900 dark:text-gray-100" $
                H.toHtml ("Need some content to send to AI" :: Text)
          in
          pure . Right $ Wd.BasicFR (response, Nothing)
        Just aText -> do
          startTime <- getZonedTime
          putStrLn $ "Model: " <> show rtOpts.openai.model
          putStrLn $ "Prompt: " <> show aText
          rezA <- aiServiceCall aiConf "message" (T.encodeUtf8 aText)
          endTime <- getZonedTime
          case rezA of
            Left errMsg ->
              let
                response = renderHtml $
                  H.div H.! A.class_ "text-gray-900 dark:text-gray-100" $
                    H.toHtml errMsg
              in do
              putStrLn $ "@[receiveMsg] err: " <> show errMsg
              pure . Right $ Wd.AppendChildFR (response, Nothing)
            Right (TextCR reply) ->
              let
                response = renderHtml $
                  mapM_ showMessageR (buildMessages (aText, startTime) (reply, endTime))
              in do
              putStrLn $ "@[receiveMsg] reply: " <> show reply
              pure . Right $ Wd.AppendChildFR (response, Nothing)


aiServiceCall :: Rt.AiservConfig -> Text -> Bs.ByteString -> IO (Either String ContentReply)
aiServiceCall  aisConf action aText =
  case getUuidForFunction action of
    Nothing ->
      pure . Left $ "Invalid function: " <> unpack action
    Just functionUUID -> do
      manager <- Hc.newManager Ht.tlsManagerSettings
      baseRequest <- Hc.parseRequest $ aisConf.server <> "/invoke"
      let
        invokeRequest = InvokeRequest {
          function = functionUUID
          , context = Nothing
          , parameters = Ae.toJSON $ ParameterValue { name = "model", value = "gemini-2.5-flash" }
          , content = T.decodeUtf8 aText
          , files = []
          , references = []
        }
        request = baseRequest {
          Hc.method = "POST"
          , Hc.requestHeaders = [
                ("Authorization", "Bearer " <> (T.encodeUtf8 . pack) aisConf.token)
              , ("Content-Type", "application/json")
              ]
          , Hc.requestBody = Hc.RequestBodyLBS (Ae.encode invokeRequest)
          }
      -- putStrLn $ "@[aiServiceCall] request: " <> show request
      -- putStrLn $ "@[aiServiceCall] request body: " <> show (Ae.encode invokeRequest)
      -- putStrLn $ "@[aiServiceCall] token: " <> aisConf.token
      response <- Hc.httpLbs request manager
      case Hs.statusCode response.responseStatus of
        200 ->
          case Ae.eitherDecode response.responseBody :: Either String InvokeResponse of
            Left errMsg -> do
              putStrLn $ "@[aiServiceCall] error decoding request reply: " <> show response.responseBody
              pure $ Left errMsg
            Right invResp ->
              case invResp.status of
                "OK" ->
                  waitForResponse manager aisConf invResp.requestEId 0
                _ -> pure . Left $ "@[aiServiceCall] ai server api err: " <> unpack invResp.status
        _ -> do
          putStrLn $ "@[aiServiceCall] http body: " <> show response.responseBody
          pure . Left $ "@[aiServiceCall] http err: " <> show (Hs.statusCode response.responseStatus) <> ", body: " <> show response.responseBody


waitForResponse :: Hc.Manager -> Rt.AiservConfig -> UUID -> Int32 -> IO (Either String ContentReply)
waitForResponse manager aisConf requestEId iterCount= do
  baseRequest <- Hc.parseRequest $ aisConf.server <> "/invoke/response?tid=" <> show requestEId
  let
    request = baseRequest {
      Hc.method = "GET"
      , Hc.requestHeaders = [("Authorization", "Bearer " <> (T.encodeUtf8 . pack) aisConf.token)]
    }
  response <- Hc.httpLbs request manager
  case Hs.statusCode response.responseStatus of
    200 ->
      case Ae.eitherDecode response.responseBody :: Either String ResponseMsg of
        Left errMsg -> do
          putStrLn $ "@[waitForResponse] error decoding request reply: " <> show response.responseBody
          pure $ Left errMsg
        Right respMsg ->
          case respMsg.status of
            "OK" ->
              case respMsg.result.result of
                NoResponseYetRK ->
                  if iterCount > 30 then
                    pure . Left $ "@[waitForResponse] ai server api err: " <> unpack respMsg.status
                  else do
                    threadDelay 1000000
                    waitForResponse manager aisConf requestEId (iterCount + 1)
                TextBlockRK aTextBlock ->
                  pure . Right $ TextCR aTextBlock.content
                AssetRK aAssetRef ->
                  pure . Right $ AssetCR ImageAK aAssetRef.assetEId
                -- Keep this as an inverse warning: if it's redundant, it means all possible case values are covered.
                _ -> pure . Left $ "@[waitForResponse] ai server api err: " <> unpack respMsg.status
    _ ->
      pure . Left $ "@[waitForResponse] http err: " <> show (Hs.statusCode response.responseStatus)

getUuidForFunction :: Text -> Maybe UUID
getUuidForFunction aText =
  {- hard-coded for now, using Google:
  # text_to_speech   | 2eaacb33-471b-4d89-8509-bc242ae3e00e
  # text_to_video    | b4a85bf1-3efd-4b9b-8f08-1d67fe8a83b7
  # text_to_image    | 30083aad-5897-4881-83ba-f26ddbb925a0
  # message          | 0de56430-c761-4be6-a6c9-7ea965ea56d6
  # chain_of_thought | 3057bc2b-a12b-4378-a7f7-4f570255fb8b
  # edit_image       | da1f6658-e07f-4464-9cfd-20c73c4651c9
  -}
  case aText of
    "text_to_speech" -> Uu.fromString "2eaacb33-471b-4d89-8509-bc242ae3e00e"
    "text_to_video" -> Uu.fromString "b4a85bf1-3efd-4b9b-8f08-1d67fe8a83b7"
    "text_to_image" -> Uu.fromString "30083aad-5897-4881-83ba-f26ddbb925a0"
    "message" -> Uu.fromString "0de56430-c761-4be6-a6c9-7ea965ea56d6"
    "chain_of_thought" -> Uu.fromString "3057bc2b-a12b-4378-a7f7-4f570255fb8b"
    "edit_image" -> Uu.fromString "da1f6658-e07f-4464-9cfd-20c73c4651c9"
    _ -> Nothing


showMessageR :: ChatMessage -> H.Html
showMessageR cMsg =
  case cMsg of
    FromCM aMsg ->
      messageFromR aMsg
    ToCM aMsg ->
      messageToR aMsg


messageToR :: ChatMessageContent -> H.Html
messageToR cMsg =
  H.div H.! A.class_ "group flex max-w-[404px] items-start gap-2.5" $ do
    H.img H.!
      A.class_ "h-8 w-8 rounded-full"
      H.! A.src (Bli.textValue cMsg.userProfile.avatar)
      H.! A.alt (Bli.textValue cMsg.userProfile.altAvatar)
    H.div H.! A.class_ "flex flex-col gap-1" $ do
      H.div H.! A.class_ "flex items-center space-x-2 rtl:space-x-reverse" $ do
        H.a H.! A.href "#" H.! A.class_ "text-sm font-semibold text-gray-900 hover:cursor-pointer hover:underline dark:text-white" $
          H.toHtml cMsg.userProfile.name
        H.span H.! A.class_ "text-sm font-normal text-gray-500 dark:text-gray-400" $
          H.toHtml cMsg.timestamp
      H.div H.! A.class_ "space-y-1 text-start" $ do
        mapM_ (\ctnt ->
            H.div H.! A.class_ "leading-1.5 ms-auto inline-flex flex-col rounded-e-xl rounded-es-xl border-gray-200 bg-gray-100 p-4 text-start dark:bg-gray-700" $
              H.toHtml ctnt
          ) cMsg.content
    popUpMenuR cMsg.messageID


messageFromR :: ChatMessageContent -> H.Html
messageFromR cMsg =
  H.div H.! A.class_ "group ms-auto flex max-w-[404px] items-start justify-end gap-2.5" $ do
    popUpMenuR cMsg.messageID
    H.div H.! A.class_ "flex flex-col gap-1" $ do
      H.div H.! A.class_ "flex items-center justify-end space-x-2 rtl:space-x-reverse" $ do
        H.a
          H.! A.href "#"
          H.! A.class_ "text-sm font-semibold text-gray-900 hover:cursor-pointer hover:underline dark:text-white" $
            H.toHtml cMsg.userProfile.name
        H.span H.! A.class_ "text-sm font-normal text-gray-500 dark:text-gray-400" $
          H.toHtml cMsg.timestamp
      H.div H.! A.class_ "space-y-1 text-end" $ do
        mapM_ (\ctnt ->
            H.div H.! A.class_ "leading-1.5 ms-auto inline-flex flex-col rounded-s-xl rounded-ee-xl border-gray-200 bg-gray-100 p-4 text-start dark:bg-gray-700" $
              H.toHtml ctnt
          ) cMsg.content
    H.img
      H.! A.class_ "h-8 w-8 rounded-full"
      H.! A.src (Bli.textValue cMsg.userProfile.avatar)
      H.! A.alt (Bli.textValue cMsg.userProfile.altAvatar)

dropdownToggle :: Bli.AttributeValue -> Bli.Attribute
dropdownToggle =
    Bli.attribute "data-dropdown-toggle" " data-dropdown-toggle=\""

dropdownPlacement :: Bli.AttributeValue -> Bli.Attribute
dropdownPlacement =
    Bli.attribute "data-dropdown-placement" " data-dropdown-placement=\""

popperPlacement :: Bli.AttributeValue -> Bli.Attribute
popperPlacement =
    Bli.attribute "data-popper-placement" " data-popper-placement=\""

popUpMenuR :: Text -> H.Html
popUpMenuR messageID = do
    H.button
      H.! A.id (Bli.textValue ("dropdownMenuIconButton_" <> messageID))
      H.! dropdownToggle (Bli.textValue ("dropdownDots_" <> messageID))
      H.! dropdownPlacement "bottom-start"
      H.! A.class_ "inline-flex items-center self-center rounded-lg bg-gray-50 p-2 text-center text-sm font-medium text-gray-900 opacity-0 transition-opacity hover:bg-gray-100 focus:outline-none focus:ring-4 focus:ring-gray-50 group-hover:opacity-100 dark:bg-gray-900 dark:text-white dark:hover:bg-gray-800 dark:focus:ring-gray-600"
      H.! A.type_ "button"
      $ do
        S.svg
          H.! Sa.class_ "h-4 w-4 text-gray-500 dark:text-gray-400"
          H.! Sa.fill "currentColor"
          H.! Sa.viewbox "0 0 4 15"
          $ do
            S.path
              H.! Sa.d "M3.5 1.5a1.5 1.5 0 1 1-3 0 1.5 1.5 0 0 1 3 0Zm0 6.041a1.5 1.5 0 1 1-3 0 1.5 1.5 0 0 1 3 0Zm0 5.959a1.5 1.5 0 1 1-3 0 1.5 1.5 0 0 1 3 0Z"
    H.div
      H.! A.id (Bli.textValue ("dropdownDots_" <> messageID))
      H.! A.class_ "z-10 hidden w-40 divide-y divide-gray-100 rounded-lg bg-white shadow dark:divide-gray-600 dark:bg-gray-700"
      H.! popperPlacement "bottom-start"
      H.! A.style "position=absolute; inset=0px auto auto 0px; margin=0px; transform=translate3d(1316px, 547px, 0px)"
      $ do
        H.ul
          H.! A.class_ "p-2 text-sm font-medium text-gray-500 dark:text-gray-400"
          H.! Bli.attribute "aria-labelledby" " aria-labelledby=\"" (Bli.textValue ("dropdownMenuIconButton_" <> messageID))
          $ do
            menuItemR "Reply" False
            menuItemR "Forward" False
            menuItemR "Copy" False
            menuItemR "Report" False
            menuItemR "Delete" True
  where
  menuItemR :: Text -> Bool -> H.Html
  menuItemR aText isRed =
    let
      anchorClass = if isRed then
            "inline-flex w-full items-center rounded-md px-3 py-2 text-red-600 hover:bg-gray-100 dark:hover:bg-gray-600"
          else
            "inline-flex w-full items-center rounded-md px-3 py-2 hover:bg-gray-100 hover:text-gray-900 dark:hover:bg-gray-600 dark:hover:text-white"
    in
    H.li $ do
      H.a
        H.! A.href "#"
        H.! A.class_ anchorClass
        $ do
          S.svg
            H.! Sa.class_ "me-1.5 h-4 w-4"
            H.! Sa.width "24"
            H.! Sa.height "24"
            H.! Sa.fill "currentColor"
            H.! Sa.viewbox "0 0 24 24"
            $ do
              S.path
                H.! Sa.d "M14.502 7.046h-2.5v-.928a2.122 2.122 0 0 0-1.199-1.954 1.827 1.827 0 0 0-1.984.311L3.71 8.965a2.2 2.2 0 0 0 0 3.24L8.82 16.7a1.829 1.829 0 0 0 1.985.31 2.121 2.121 0 0 0 1.199-1.959v-.928h1a2.025 2.025 0 0 1 1.999 2.047V19a1 1 0 0 0 1.275.961 6.59 6.59 0 0 0 4.662-7.22 6.593 6.593 0 0 0-6.437-5.695Z"
          H.text aText


robertaCasas :: UserProfile
robertaCasas = UserProfile {
  avatar = "/imgs/users/roberta-casas.png"
  , altAvatar = "Roberta image"
  , name = "Roberta Casas"
  }

josephMcFall :: UserProfile
josephMcFall = UserProfile {
  avatar = "/imgs/users/joseph-mcfall.png"
  , altAvatar = "Joseph image"
  , name = "Joseph McFall"
  }

buildMessages :: (Text, ZonedTime) -> (Text, ZonedTime) -> [ChatMessage]
buildMessages (aText, queryTime) (reply, replyTime) =
  let
    timeZone = defaultTimeLocale
    startID = pack $ formatTime timeZone "%s%Q" queryTime
    endID = pack $ formatTime timeZone "%s%Q" replyTime
  in [
    FromCM $ ChatMessageContent {
        userProfile = josephMcFall
        , content = [[
            H.div H.! A.class_ "text-sm font-normal text-gray-900 dark:text-white" $
              Md.markdown Md.def (fromStrict aText)
          ]]
        , timestamp = formatTime timeZone "%H:%M" queryTime
        , messageID = startID
    }
    , ToCM $ ChatMessageContent {
        userProfile = robertaCasas
        , content = [[
            H.div H.! A.class_ "text-sm font-normal text-gray-900 dark:text-white" $
              Md.markdown Md.def (fromStrict reply)
          ]]
        , timestamp = formatTime timeZone "%H:%M" replyTime
        , messageID = endID
    }
  ]