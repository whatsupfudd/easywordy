{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda" #-}
module Chat.Logic where

import qualified Data.ByteString as Bs
import Data.Text (Text)

import qualified Data.Aeson as Ae

import Hasql.Pool (Pool)

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Htmx as X
import qualified Text.Blaze.Htmx.WebSockets as X
import qualified Text.Blaze.Internal as Bli
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)

import OpenAI (think)
import Context (Context(..), Result (..))
import Action (Action(..))
import Actions.Chat (CompleteParams(..), ChatVerb(..))

import qualified Options.Runtime as Rt
import qualified Wapp.Types as Wt
import Data.Maybe (fromMaybe)


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
  , messageID :: String
  }


receiveMsg :: Rt.RunOptions -> Pool -> Wt.InternalArgs -> IO (Either String Bs.ByteString)
receiveMsg rtOpts pgDb (jsonParams, content) = do
  case rtOpts.openai.apiKey of
    Nothing ->
      pure . Right $ Bs.toStrict . renderHtml $
        H.div H.! A.class_ "text-gray-900 dark:text-gray-100" $
          H.toHtml ("No OPENAI api key" :: Text)
    Just aKey ->
      {-
      pure . Right $ Bs.toStrict . renderHtml $
        showMessageR (head (fakeMessages "Hello"))
      -}
      case content of
        Nothing ->
          pure . Right $ Bs.toStrict . renderHtml $
            H.div H.! A.class_ "text-gray-900 dark:text-gray-100" $
              H.toHtml ("Need some content to send to AI" :: Text)
        Just aText ->
          let
            oaiContext = Simple aKey (fromMaybe "gpt-4o-mini" rtOpts.openai.model)
            params = SimplePrompt aText
            action = Complete params
          in do
          putStrLn $ "Model: " <> show rtOpts.openai.model
          putStrLn $ "Prompt: " <> show aText
          rezA <- think $ Chat action oaiContext
          case rezA of
            Left errMsg -> do
              putStrLn $ "@[receiveMsg] err: " <> show errMsg
              pure . Right $ Bs.toStrict . renderHtml $
                H.div H.! A.class_ "text-gray-900 dark:text-gray-100" $
                  H.toHtml errMsg
            Right (TextResult reply) -> do
              putStrLn $ "@[receiveMsg] reply: " <> show reply
              pure . Right $ Bs.toStrict . renderHtml $
                showMessageR (head (fakeMessages reply))



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


messageFromR :: ChatMessageContent -> H.Html
messageFromR cMsg =
  H.div H.! A.class_ "group ms-auto flex max-w-[404px] items-start justify-end gap-2.5" $ do
    H.div H.! A.class_ "flex flex-col gap-1" $ do
      H.div H.! A.class_ "flex items-center justify-end space-x-2 rtl:space-x-reverse" $ do
        H.a H.! A.href "#" H.! A.class_ "text-sm font-semibold text-gray-900 hover:cursor-pointer hover:underline dark:text-white" $
          H.toHtml cMsg.userProfile.name
        H.span H.! A.class_ "text-sm font-normal text-gray-500 dark:text-gray-400" $
          H.toHtml cMsg.timestamp
      H.div H.! A.class_ "space-y-1 text-end" $ do
        mapM_ (\ctnt ->
            H.div H.! A.class_ "leading-1.5 ms-auto inline-flex flex-col rounded-s-xl rounded-ee-xl border-gray-200 bg-gray-100 p-4 text-start dark:bg-gray-700" $
              H.toHtml ctnt
          ) cMsg.content
        H.img H.! A.class_ "h-8 w-8 rounded-full" H.! A.src (Bli.textValue cMsg.userProfile.avatar) H.! A.alt (Bli.textValue cMsg.userProfile.altAvatar)
        H.span H.! A.class_ "text-sm font-normal text-gray-500 dark:text-gray-400" $
          H.toHtml cMsg.timestamp



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

fakeMessages :: Text -> [ChatMessage]
fakeMessages aText = [
    ToCM $ ChatMessageContent {
        userProfile = robertaCasas
        , content = [[
            H.p H.! A.class_ "text-sm font-normal text-gray-900 dark:text-white"
              $ H.toHtml aText
          ]]
        , timestamp = "11:46"
        , messageID = "1"
    }
  ]