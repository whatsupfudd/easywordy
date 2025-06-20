{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module WordPress.Functions where

import qualified Data.ByteString as Bs
import qualified Data.ByteString.Lazy as Lbs
import Data.Int (Int32)
import qualified Data.Map as Mp
import Data.Text (Text, pack)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Binary.Get as Bg
import qualified Data.Vector as V
import Data.Word (Word8)
import qualified Numeric as Nm

import GHC.Generics (Generic)
import qualified Data.Aeson as Ae

import Hasql.Pool (Pool)

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Htmx as X
import qualified Text.Blaze.Htmx.WebSockets as X
import qualified Text.Blaze.Internal as Bli
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)

import qualified Options.Runtime as Rt
import qualified Wapp.AppDef as Wd

import WordPress.Opers (getVersions, getFoldersForVersion, getFilesForFolder
          , getConstantsForFile, getAstForFile, getFileDetailsForID
          , getErrorForFile, getFolderDetailsForID)


newtype VersionParams = VersionParams {
    versionID :: Int32
  }
  deriving (Generic, Ae.FromJSON, Ae.ToJSON)

newtype FolderParams = FolderParams {
    folderID :: Int32
  }
  deriving (Generic, Ae.FromJSON, Ae.ToJSON)

newtype FileParams = FileParams {
    fileID :: Int32
  }
  deriving (Generic, Ae.FromJSON, Ae.ToJSON)


-- type InternalFunction = Rt.RunOptions -> Hp.Pool -> InternalArgs -> IO (Either String FunctionReply)

fetchVersions :: Wd.InternalFunction
fetchVersions rtOpts pgDb (jsonParams, _) = do
  rezA <- getVersions pgDb
  case rezA of
    Left err -> pure . Right . Wd.BasicFR $ renderHtml $ H.div
        H.! A.id "mainContainer"
        H.! A.class_ "text-gray-900 dark:text-gray-100" $ H.toHtml err
    Right versions ->
      let
        response =  H.div H.! A.id "mainContainer" $
          H.table H.! A.class_ "p-4 w-full text-sm text-left rtl:text-right text-gray-500 dark:text-gray-400" $ do
            H.thead H.! A.class_ "text-xs text-gray-700 uppercase bg-gray-50 dark:bg-gray-700 dark:text-gray-400"$ do
              H.tr $ do
                H.th H.! A.scope "col" H.! A.class_ "px-6 py-3" $ "Label"
                H.th H.! A.scope "col" H.! A.class_ "px-6 py-3" $ "ID"
            H.tbody $ do
              mapM_ (\(uid, label) ->
                let
                  folderArgs = VersionParams { versionID = uid }
                  versionParams = T.decodeUtf8 . Bs.toStrict . Ae.encode $ folderArgs
                in
                H.tr H.! A.class_ "bg-white border-b dark:bg-gray-800 dark:border-gray-700" $ do
                  -- hx-target="#mainContainer" hx-swap="outerHtml" hx-headers='{"mid":"wp_versions_1"}'
                  H.td H.! A.scope "row" H.! A.class_ "px-6 py-4 font-medium text-gray-900 whitespace-nowrap dark:text-white"
                        H.! X.wsSend "" H.! X.hxTarget "#mainContainer" H.! X.hxSwap "outerHtml"
                        H.! X.hxHeaders (Bli.textValue $ "{\"mid\":\"wp_folders_1\", \"params\":" <> versionParams <> "}") $ H.toHtml label
                  H.td H.! A.class_ "px-6 py-4" $ H.toHtml uid
                ) versions
      in
      pure . Right . Wd.BasicFR $ renderHtml response


fetchFolders :: Wd.InternalFunction
fetchFolders rtOpts pgDb (jsonParams, _) =
  let
    eiVersionParams = case Ae.fromJSON jsonParams :: Ae.Result VersionParams of
      Ae.Success aValue -> Right aValue
      Ae.Error errMsg -> Left errMsg
  in
  case eiVersionParams of
    Left errMsg -> pure . Right . Wd.BasicFR $ renderHtml $ H.div
        H.! A.id "mainContainer"
        H.! A.class_ "text-gray-900 dark:text-gray-100" $ H.toHtml errMsg
    Right folderArgs -> do
      rezA <- getFoldersForVersion pgDb folderArgs.versionID
      case rezA of
        Left err -> pure . Right . Wd.BasicFR . renderHtml $ H.div
          H.! A.id "mainContainer"
          H.! A.class_ "text-gray-900 dark:text-gray-100" $ H.toHtml err
        Right versions ->
          let
            response =  H.div H.! A.id "mainContainer" $
              H.table H.! A.class_ "p-4 w-full text-sm text-left rtl:text-right text-gray-500 dark:text-gray-400" $ do
                H.thead H.! A.class_ "text-xs text-gray-700 uppercase bg-gray-50 dark:bg-gray-700 dark:text-gray-400"$ do
                  H.tr $ do
                    H.th H.! A.scope "col" H.! A.class_ "px-6 py-3" $ "Label"
                    H.th H.! A.scope "col" H.! A.class_ "px-6 py-3" $ "ID"
                H.tbody $ do
                  mapM_ (\(uid, label) ->
                    let
                      folderParams = FolderParams { folderID = uid }
                      folderParamsStr = T.decodeUtf8 . Bs.toStrict . Ae.encode $ folderParams
                    in
                    H.tr H.! A.class_ "bg-white border-b dark:bg-gray-800 dark:border-gray-700" $ do
                      H.td H.! A.class_ "px-6 py-4 font-medium text-gray-900 whitespace-nowrap dark:text-white"
                          H.! X.wsSend "" H.! X.hxTarget "#mainContainer"
                          H.! X.hxSwap "outerHtml" H.! X.hxHeaders (Bli.textValue $ "{\"mid\":\"wp_files_1\", \"params\":" <> folderParamsStr <> "}") $
                            H.toHtml (if label == "" then "<wp_root>" else label)
                      H.td H.! A.class_ "px-6 py-4" $ H.toHtml uid
                    ) versions
          in
          pure . Right . Wd.BasicFR $ renderHtml response


fetchFiles :: Wd.InternalFunction
fetchFiles rtOpts pgDb (jsonParams, _) =
  let
    eiFolderParams = case Ae.fromJSON jsonParams :: Ae.Result FolderParams of
      Ae.Success aValue -> Right aValue
      Ae.Error errMsg -> Left errMsg
  in
  case eiFolderParams of
    Left errMsg -> pure . Right . Wd.BasicFR . renderHtml $ H.div
        H.! A.id "mainContainer"
        H.! A.class_ "text-gray-900 dark:text-gray-100" $ H.toHtml errMsg
    Right folderParams -> do
      rezA <- getFilesForFolder pgDb folderParams.folderID
      rezB <- getFolderDetailsForID pgDb folderParams.folderID
      case rezA of
        Left err -> pure . Right . Wd.BasicFR . renderHtml $ H.div
            H.! A.id "mainContainer"
            H.! A.class_ "text-gray-900 dark:text-gray-100" $ H.toHtml err
        Right files ->
          let
            folderPointer = case rezB of
              Right (Just (versionID, folderPath)) -> do
                H.a H.! A.class_ "text-blue-900 dark:text-blue-300"
                    H.! X.wsSend "" H.! X.hxTarget "#mainContainer" H.! X.hxSwap "outerHtml"
                    H.! X.hxHeaders (Bli.textValue $ "{\"mid\":\"wp_folders_1\", \"params\":{\"versionID\":" <> T.pack (show versionID) <> "}}") $ H.toHtml ("UP" :: Text)
                H.div H.! A.class_ "mx-4text-sm text-gray-900 dark:text-gray-300" $ H.toHtml (if folderPath == "" then "wp_root" else "> wp_root/" <> folderPath)
              _ -> H.toHtml ("<i>No folder</i>" :: Text)
            response =  H.div H.! A.id "mainContainer" $ do
              folderPointer
              H.table H.! A.class_ "p-4 w-full text-sm text-left rtl:text-right text-gray-500 dark:text-gray-400" $ do
                H.thead H.! A.class_ "text-xs text-gray-700 uppercase bg-gray-50 dark:bg-gray-700 dark:text-gray-400"$ do
                  H.tr $ do
                    H.th H.! A.scope "col" H.! A.class_ "px-6 py-3" $ "Label"
                    H.th H.! A.scope "col" H.! A.class_ "px-6 py-3" $ "ID"
                H.tbody $ do
                  mapM_ (\(uid, label) -> 
                    let
                      fileParams = FileParams { fileID = uid }
                      fileParamsStr = T.decodeUtf8 . Bs.toStrict . Ae.encode $ fileParams
                    in
                    H.tr H.! A.class_ "bg-white border-b dark:bg-gray-800 dark:border-gray-700" $ do
                      H.td H.! A.class_ "px-6 py-4 font-medium text-gray-900 whitespace-nowrap dark:text-white"
                          H.! X.wsSend "" H.! X.hxTarget "#mainContainer" H.! X.hxSwap "outerHtml"
                          H.! X.hxHeaders (Bli.textValue $ "{\"mid\":\"wp_fileDetails_1\", \"params\":" <> fileParamsStr <> "}") $ H.toHtml label
                      H.td H.! A.class_ "px-6 py-4" $ H.toHtml uid
                    ) files
          in
          pure . Right . Wd.BasicFR $ renderHtml response


fetchFileDetails :: Wd.InternalFunction
fetchFileDetails rtOpts pgDb (jsonParams, _) =
  let
    eiFileParams = case Ae.fromJSON jsonParams :: Ae.Result FileParams of
      Ae.Success aValue -> Right aValue
      Ae.Error errMsg -> Left errMsg
  in
  case eiFileParams of
    Left errMsg -> pure . Right . Wd.BasicFR . renderHtml $ H.div
        H.! A.id "mainContainer"
        H.! A.class_ "text-gray-900 dark:text-gray-100" $ H.toHtml errMsg
    Right fileParams -> do
      rezA <- getAstForFile pgDb fileParams.fileID
      rezB <- getConstantsForFile pgDb fileParams.fileID
      rezC <- getFileDetailsForID pgDb fileParams.fileID
      let
        folderPointer = case rezC of
          Right (Just (folderID, fileName, folderPath)) -> do
            H.a H.! A.class_ "text-blue-900 dark:text-blue-300"
                H.! X.wsSend "" H.! X.hxTarget "#mainContainer" H.! X.hxSwap "outerHtml"
                H.! X.hxHeaders (Bli.textValue $ "{\"mid\":\"wp_files_1\", \"params\":{\"folderID\":" <> T.pack (show folderID) <> "}}") $ H.toHtml ("UP" :: Text)
            H.div H.! A.class_ "mx-4text-sm text-gray-900 dark:text-gray-300" $ H.toHtml ("> wp_root/" <> folderPath <> "/" <> fileName)
          _ -> H.toHtml ("<i>No folder</i>" :: Text)

      case (rezA, rezB) of
        (Left err, _) -> pure . Right . Wd.BasicFR . renderHtml $
            H.div H.! A.id "mainContainer" H.! A.class_ "text-gray-900 dark:text-gray-300" $ H.toHtml err
        (_, Left err) -> pure . Right . Wd.BasicFR . renderHtml $
            H.div H.! A.id "mainContainer" H.! A.class_ "text-gray-900 dark:text-gray-300" $ H.toHtml err
        (Right (Just ast), Right (Just constants)) ->
          let
            derefedAst = printAst ast constants
          in
          pure . Right . Wd.BasicFR . renderHtml $ H.div H.! A.id "mainContainer" $ do
            folderPointer
            derefedAst
        _ -> do
          rezErr <- getErrorForFile pgDb fileParams.fileID
          case rezErr of
            Left err -> pure . Right . Wd.BasicFR . renderHtml $
              H.div H.! A.id "mainContainer" H.! A.class_ "mx-4 text-gray-900 dark:text-gray-300" $ H.toHtml err
            Right (Just (errMsg, procTime)) ->
              pure . Right . Wd.BasicFR . renderHtml $ do
                H.div H.! A.id "mainContainer" H.! A.class_ "p-4 text-gray-900 dark:text-gray-300" $ do
                  folderPointer
                  H.br
                  H.toHtml ("Error: " <> errMsg)
                  H.br
                  H.toHtml ("Proc time: " <> T.pack (show procTime) <> "s" :: Text)


printAst :: Bs.ByteString -> Bs.ByteString -> H.Html
printAst astBs constantsBs =
  let
    (constantVect, nbrConstants, offsets) = buildConstants constantsBs
    htmlActions = case Bg.runGetOrFail (parseActionList constantVect) (Bs.fromStrict astBs) of
      Left (consumed, rest, err) -> [ H.div $ H.toHtml $ show err <> ", rest: " <> show rest ]
      Right (consumed, rest, actions) -> actions
    opCodes = V.fromList $ map (
            T.intercalate " " . map (\w8 -> T.pack $ Nm.showInt w8 "")
          ) (groupBy (Bs.unpack astBs))
    numberedOps = V.map (\(opCode, idx) -> do
            H.br
            H.toHtml $ show (4 * idx) <> ": " <> T.unpack opCode) 
        (V.zip opCodes (V.enumFromN 0 (V.length opCodes)))
  in
  H.div H.! A.class_ "p-4 text-gray-900 dark:text-gray-300" $ do
    sequence_ htmlActions
    H.br
    doHtml "AST"
    sequence_ numberedOps
    H.br
    H.toHtml ("Nbr constants:" <> T.pack (show nbrConstants) <> ", length: " <> T.pack (show offsets))
    H.br
    H.toHtml ("\nConstants: " :: Text)
    mapM_ (\(constant, idx) -> do
        H.br
        H.toHtml $ T.pack (show idx) <> ": " <> T.pack (show constant)
      ) (V.zip constantVect (V.enumFromN 0 (fromIntegral nbrConstants)))


groupBy :: [Word8] -> [[Word8]]
groupBy aList =
  if length aList < 4 then
    [ aList ]
  else
    let (firstChunk, rest) = splitAt 4 aList
    in firstChunk : groupBy rest


buildConstants :: Bs.ByteString -> (V.Vector Bs.ByteString, Int32, [Int32])
buildConstants byteString =
  let
    (nbrConstants, offsets) = Bg.runGet parseConstantHeader (Bs.fromStrict byteString)
    (constants, _) = foldl (\(accum, position) offset ->
          (
            V.snoc accum (Bs.take offset (Bs.drop position byteString))
            , position + offset
          )
        ) (V.empty, 8 + 4 * fromIntegral nbrConstants) (map fromIntegral offsets)
  in
  (constants, nbrConstants, offsets)


parseConstantHeader :: Bg.Get (Int32, [Int32])
parseConstantHeader = do
  nbrConstants <- Bg.getInt32be
  totalLength <- Bg.getInt32be
  offsets <- mapM (const Bg.getInt32be) [1..nbrConstants]
  pure (nbrConstants, offsets)


parseActionList :: V.Vector Bs.ByteString -> Bg.Get [ H.Html ]
parseActionList constantVec = do
  isDone <- Bg.isEmpty
  if isDone then
    pure []
  else do
    anAction <- parseAction constantVec
    -- t2Action <- parseAction constantVec
    -- t3Action <- parseAction constantVec
    {-- 
    t2Action <- do
      firstNbr <- Bg.getInt32be
      sndNbr <- Bg.getInt32be
      pure $ H.div $ do
        doHtmlStr $ show firstNbr <> ", " <> show sndNbr
    --}

    rest <- parseActionList constantVec
    pure (anAction : rest)  -- [ t2Action, t3Action ]


parseAction :: V.Vector Bs.ByteString -> Bg.Get H.Html
parseAction constantVec = do
  ranEmpty <- Bg.isEmpty
  if ranEmpty then
    pure $ doHtmlStr "stmt: ran out of bytes!"
  else do
    startPos <- Bg.bytesRead
    stmt <- Bg.getInt32be
    block <- case stmt of
      -- Actions:
      1 -> do
        cteID <- Bg.getInt32be
        pure $ H.div $ do
          showStatement "Verbatim "
          showConstant $ cteID `derefCte` constantVec
      2 -> do
        cteID <- Bg.getInt32be
        pure $ H.div $ do
          showStatement "Comment "
          H.pre $ H.code H.! A.class_ "p-1 text-pink-700 dark:text-pink-400" $ H.toHtml (T.decodeUtf8 $ cteID `derefCte` constantVec)
      3 -> do
        startRow <- Bg.getInt32be
        startCol <- Bg.getInt32be
        endRow <- Bg.getInt32be
        endCol <- Bg.getInt32be
        pure $ doHtmlStr $ "MiscST " <> show startRow <> ":" <> show startCol <> "-" <> show endRow <> ":" <> show endCol
      -- Statements:
      4 -> pure . showStatement $ "NamedLabel"
      5 -> do
        expression <- parseExpression constantVec
        pure $ H.div $ do
          showStatement "ExprST"
          expression
      6 -> do
        condExpr <- parseExpression constantVec
        thenBranch <- parseAction constantVec
        pure . H.div $ do
          showStatement "IfSt"
          condExpr
          doHtml "Then"
          thenBranch
          doHtml "No else."
      7 -> do
        condExpr <- parseExpression constantVec
        thenBranch <- parseAction constantVec
        elseBranch <- parseAction constantVec
        pure . H.div $ do
          showStatement "IfSt"
          condExpr
          doHtml "Then"
          thenBranch
          doHtml "Else"
          elseBranch
      8 -> pure . showStatement $ "SwitchST"
      9 -> pure . showStatement $ "WhileST"
      10 -> pure . showStatement $ "DoST"
      11 -> pure . showStatement $ "ForST"
      12 -> do
        -- TODO: refVarSpec, mbIndex
        condExpr <- parseExpression constantVec
        loopStmt <- parseAction constantVec
        pure . H.div $ do
          showStatement "ForEachST"
          condExpr
          doHtml "Loop"
          loopStmt
      13 -> pure . showStatement $ "GotoST"
      14 -> pure . showStatement $ "ContinueST"
      15 -> pure . showStatement $ "BreakST"
      16 -> pure . H.div $ do
              showStatement "ReturnST"
      85 -> do
        mbExpr <- parseExpression constantVec
        pure . H.div $ do
          showStatement "ReturnST"
          mbExpr
      17 -> pure . showStatement $ "TryST"
      18 -> pure . showStatement $ "DeclareST"
      19 -> do
        nbrExpr <- Bg.getInt32be
        exprs <- mapM (\_ -> parseExpression constantVec) [1..nbrExpr]
        pure . H.div $ do
          showStatement "EchoST"
          sequence_ exprs
      20 -> pure . showStatement $ "ExitST"
      21 -> do
        expr <- parseExpression constantVec
        pure . H.div $ do
          showStatement "ExitST"
          expr
      22 -> pure . showStatement $ "UnsetST"
      23 -> pure . showStatement $ "ConstDeclST"
      24 -> do
        qualName <- parseQualName constantVec
        action <- parseAction constantVec
        pure . H.div $ do
          showStatement "FunctionDefST"
          qualName
          action
      25 -> do
        nbrMembers <- Bg.getInt32be
        nameID <- Bg.getInt32be
        extendID <- Bg.getInt32be
        nbrImplIDs <- Bg.getInt32be
        implIDs <- mapM (const Bg.getInt32be) [1..nbrImplIDs]
        members <- mapM (\_ -> parseClassMember constantVec) [1..nbrMembers]
        pure . H.div $ do
          showStatement "ClassDefST"
          doHtml $ "Name: "
          showConstant $ derefCte nameID constantVec
          doHtml "Extend: "
          showConstant $ derefCte extendID constantVec
          doHtmlStr $ "Impl: " <> show implIDs
          sequence_ members
      26 -> pure . showStatement $ "InterfaceDefST"
      27 -> pure . showStatement $ "TraitDefST"
      28 -> pure . showStatement $ "EnumDefST"
      29 -> pure . showStatement $ "NamespaceDefST"
      30 -> pure . showStatement $ "NamespaceUseST"
      31 -> pure . showStatement $ "GlobalDeclST"
      32 -> pure . showStatement $ "FunctionStaticST"
      33 -> do
        action <- parseAction constantVec
        pure . H.div $ do
          showStatement "DanglingST"
          action
      34 -> pure . showStatement $ "EndDeclareDC"
      35 -> pure . showStatement $ "EndForDC"
      36 -> pure . showStatement $ "EndForEachDC"
      37 -> pure . showStatement $ "EndIfDC"
      38 -> pure . showStatement $ "EndSwitchDC"
      39 -> pure . showStatement $ "EndWhileDC"
      86 -> do
        nbrActions <- Bg.getInt32be
        actions <- mapM (\_ -> parseAction constantVec) [1..nbrActions]
        pure . H.div $ do
          showStatement "BlockST"
          sequence_ actions
      87 -> do
        nbrSubActions <- Bg.getInt32be
        subActions <- mapM (\_ -> do
            parseAction constantVec
          ) [1..nbrSubActions]
        pure . H.div $ do
          showStatement "Interpolation"
          -- doHtmlStr $ "nbr subActions: " <> show nbrSubActions
          sequence_ subActions
      _ -> pure $ H.div $ H.toHtml ("Unknown statement type " <> T.pack (show stmt))

    endPos <- Bg.bytesRead
    pure $ do
      H.div H.! A.class_ "mx-2" H.! A.style "border-left: 1px solid gray;" $ do
        block
        H.div H.! A.class_ "text-sm text-gray-500" $ H.i $ H.toHtml $ "stmt pos: " <> show startPos <> "-" <> show endPos

  

parseExpression :: V.Vector Bs.ByteString -> Bg.Get H.Html
parseExpression constantVec = do
  ranEmpty <- Bg.isEmpty
  if ranEmpty then
    pure $ doHtmlStr "Expr: ran out of bytes!"
  else do
    startPos <- Bg.bytesRead
    expr <- Bg.getInt32be
    block <- case expr of
      40 -> do
        varSpec <- parseVariableSpec constantVec
        pure . H.div $ do
          showExpression "Variable"
          varSpec
      41 -> do
        anID <- Bg.getInt32be
        pure . H.div $ do
          showExpression "Symbol"
          showConstant $ derefCte anID constantVec
      42 -> do
        op <- Bg.getInt32be
        left <- parseExpression constantVec
        right <- parseExpression constantVec
        pure . H.div $ do
          showExpression "BinaryOp"
          left
          doHtmlStr $ "op: " <> showBinOp op
          right
      43 -> do
        op <- Bg.getInt32be
        expr <- parseExpression constantVec
        pure . H.div $ do
          showExpression "UnaryOp"
          doHtmlStr $ showUnaryOp op
          expr
      44 -> do
        condExpr <- parseExpression constantVec
        thenExpr <- parseExpression constantVec
        elseExpr <- parseExpression constantVec
        pure . H.div $ do
          showExpression "TernaryOp"
          condExpr
          doHtml "Then"
          thenExpr
          doHtml "Else"
          elseExpr
      45 -> do
        nbrArgs <- Bg.getInt32be
        expr <- parseCallerSpec constantVec
        args <- mapM (\_ -> parseExpression constantVec) [1..nbrArgs]
        pure . H.div $ do
          showExpression "FunctionCall"
          expr
          doHtmlStr "Args: <todo>"
          sequence_ args
      46 -> do
        baseExpr <- parseExpression constantVec
        indexExpr <- parseExpression constantVec
        pure . H.div $ do
          showExpression "ArrayAccess"
          baseExpr
          doHtml "Index"
          indexExpr
      47 -> do
        nbrExprs <- Bg.getInt32be
        exprs <- mapM (\_ -> parseExpression constantVec) [1..nbrExprs]
        pure . H.div $ do
          showExpression "ArrayLiteral"
          sequence_ exprs
      48 -> do
        nbrExprs <- Bg.getInt32be
        exprs <- mapM (\_ -> parseExpression constantVec) [1..nbrExprs]
        pure . H.div $ do
          showExpression "Parenthized"
          sequence_ exprs
      49 -> do
        posFlag <- Bg.getInt32be
        leftSide <- parseExpression constantVec
        rightSide <- parseExpression constantVec
        pure . H.div $ do
          showExpression "AssignOp"
          doHtmlStr $ "flag: " <> show posFlag
          leftSide
          doHtml "="
          rightSide
      50 -> do
        cteID <- Bg.getInt32be
        pure . H.div $ do
          showExpression "CommentX"
          showConstant $ derefCte cteID constantVec
      51 -> do
        startRow <- Bg.getInt32be
        startCol <- Bg.getInt32be
        endRow <- Bg.getInt32be
        endCol <- Bg.getInt32be
        pure . H.div $ do
          showExpression "MiscX"
          doHtmlStr $ show startRow <> ":" <> show startCol <> "-" <> show endRow <> ":" <> show endCol
      52 -> do
        expr <- parseExpression constantVec
        pure . H.div $ do
          showExpression "Subscript"
          expr
      84 -> do
        expr <- parseExpression constantVec
        subExpr <- parseExpression constantVec
        pure . H.div $ do
          showExpression "Subscript"
          expr
          doHtml "Index:"
          subExpr
      53 -> do
        expr <- parseExpression constantVec
        acessMode <- parseAccessMode constantVec
        pure . H.div $ do
          showExpression "MemberAccess"
          expr
          doHtml "Mode"
          acessMode
      54 -> do
        nbrArgs <- Bg.getInt32be
        callerExpr <- parseExpression constantVec
        accessMode <- parseAccessMode constantVec
        args <- mapM (\_ -> parseExpression constantVec) [1..nbrArgs]
        pure . H.div $ do
          showExpression "MemberCall"
          callerExpr
          doHtmlStr "Mode"
          accessMode
          doHtmlStr "Args: "
          sequence_ args
      55 -> do
        condExpr <- parseExpression constantVec
        thenExpr <- parseExpression constantVec
        elseExpr <- parseExpression constantVec
        pure . H.div $ do
          showExpression "CondExpr"
          condExpr
          doHtml "Then"
          thenExpr
          doHtml "Else"
          elseExpr
      56 -> do
        cteID <- Bg.getInt32be
        expr <- parseExpression constantVec
        pure . H.div $ do
          showExpression "Casting"
          showConstant $ derefCte cteID constantVec
          expr
      57 -> do
        nbrExprs <- Bg.getInt32be
        accessMode <- parseAccessMode constantVec
        exprs <- mapM (\_ -> parseExpression constantVec) [1..nbrExprs]
        pure . H.div $ do
          showExpression "ObjectCreation"
          doHtmlStr "Mode"
          accessMode
          doHtmlStr "Exprs: "
          sequence_ exprs
      58 -> do
        incMode <- Bg.getInt32be
        expr <- parseExpression constantVec
        pure . H.div $ do
          showExpression "Include"
          doHtmlStr $ showInclMode incMode
          expr
      59 -> do
        leftExpr <- parseExpression constantVec
        binOp <- Bg.getInt32be
        rightExpr <- parseExpression constantVec
        pure . H.div $ do
          showExpression "AugmentedAssign"
          leftExpr
          doHtmlStr $ showBinOp binOp
          rightExpr
      60 -> do
        nbrModes <- Bg.getInt32be
        nbrArgs <- Bg.getInt32be
        modes <- mapM (\_ -> parseScopeMode constantVec) [1..nbrModes]
        args <- mapM (\_ -> parseExpression constantVec) [1..nbrArgs]
        pure . H.div $ do
          showExpression "ScopeCall"
          doHtmlStr "Modes: "
          sequence_ modes
          doHtmlStr "Args: <todo>"
          sequence_ args
      61 -> do
        mode <- parseScopeMode constantVec
        expr <- parseExpression constantVec
        pure . H.div $ do
          showExpression "ScopedPropertyAccess"
          mode
          doHtml "Expr"
          expr
      62 -> do
        expr <- parseExpression constantVec
        pure . H.div $ do
          showExpression "ErrorSuppression"
          expr
      63 -> do
        nbrExprs <- Bg.getInt32be
        exprs <- mapM (\_ -> parseExpression constantVec) [1..nbrExprs]
        pure . H.div $ do
          showExpression "ListLiteral"
          sequence_ exprs
      64 -> do
        startRow <- Bg.getInt32be
        startCol <- Bg.getInt32be
        endRow <- Bg.getInt32be
        pure . H.div $ do
          showExpression "HereDoc"
          doHtmlStr $ show startRow <> ", " <> show startCol <> " - " <> show endRow <> "."
      65 -> do
        scopeMode <- parseScopeMode constantVec
        cteID <- Bg.getInt32be
        pure . H.div $ do
          showExpression "ClassConstantAccess"
          scopeMode
          showConstant $ derefCte cteID constantVec
      66 -> do
        expr <- parseExpression constantVec
        pure . H.div $ do
          showExpression "ShellCommand"
          expr
      67 -> do
        expr <- parseExpression constantVec
        pure . H.div $ do
          showExpression "ThrowExpr"
          expr
      68 -> do
        posFlag <- Bg.getInt32be
        updOp <- Bg.getInt32be
        expr <- parseExpression constantVec
        pure . H.div $ do
          showExpression "UpdateExpr"
          doHtmlStr $ "flag: " <> show posFlag
          doHtmlStr $ showUpdateOp updOp
          expr
      69 -> do
        expr <- parseExpression constantVec
        pure . H.div $ do
          showExpression "CloneExpr"
          expr
      70 -> do
        literal <- parseLiteral constantVec
        pure . H.div $ do
          showExpression "Literal"
          literal
      _ -> pure $ doHtmlStr ("Unknown expression type " <> show expr)

    endPos <- Bg.bytesRead
    pure $ do
      H.div H.! A.class_ "mx-2" H.! A.style "border-left: 1px solid green;" $ do
        block
        H.div H.! A.class_ "text-sm text-gray-500" $ H.i $ H.toHtml $ "expr pos: " <> show startPos <> "-" <> show endPos


parseLiteral :: V.Vector Bs.ByteString -> Bg.Get H.Html
parseLiteral constantVec = do
  ranEmpty <- Bg.isEmpty
  if ranEmpty then
    pure $ doHtmlStr "literal: ran out of bytes!"
  else do
    literal <- Bg.getInt32be
    case literal of
      80 -> do
        anID <- Bg.getInt32be
        pure . H.div $ do
          doHtml "BoolLiteral"
          showConstant $ derefCte anID constantVec
      81 -> do
        anID <- Bg.getInt32be
        pure . H.div $ do
          doHtml "IntLiteral"
          showConstant $ derefCte anID constantVec
      82 -> do
        anID <- Bg.getInt32be
        pure . H.div $ do
          doHtml "FloatLiteral"
          showConstant $ derefCte anID constantVec
      83 -> do
        flag <- Bg.getInt32be
        strDetails <- parseStringDetails constantVec
        pure . H.div $ do
          doHtml "StringLiteral"
          doHtmlStr $ "flag: " <> show flag
          strDetails
      84 -> do
        pure . H.div $ do
          doHtml "NullLiteral"
      _ -> pure $ doHtmlStr ("Unknown literal type " <> show literal)
    

parseStringDetails :: V.Vector Bs.ByteString -> Bg.Get H.Html
parseStringDetails constantVec = do
  ranEmpty <- Bg.isEmpty
  if ranEmpty then
    pure $ doHtmlStr "literal: ran out of bytes!"
  else do
    strDetails <- Bg.getInt32be
    let
      nameStrDetails = case strDetails of
        1 -> "SimpleString"
        2 -> "EncapsedString"
        _ -> "Unknown"
    nbrEncModes <- Bg.getInt32be
    encModes <- mapM (\_ -> parseEncapsedMode constantVec) [1..nbrEncModes]
    pure . H.div $ do
      doHtml nameStrDetails
      sequence_ encModes

parseEncapsedMode :: V.Vector Bs.ByteString -> Bg.Get H.Html
parseEncapsedMode constantVec = do
  ranEmpty <- Bg.isEmpty
  if ranEmpty then
    pure $ doHtmlStr "literal: ran out of bytes!"
  else do
    encMode <- Bg.getInt32be
    case encMode of
      1 -> do
        strID <- Bg.getInt32be
        pure . H.div $ do
          doHtml "ContentEM"
          showConstant $ derefCte strID constantVec
      2 -> do
        strID <- Bg.getInt32be
        pure . H.div $ do
          doHtml "EscapeEM"
          showConstant $ derefCte strID constantVec
      3 -> do
        expr <- parseExpression constantVec
        pure . H.div $ do
          doHtml "VariableEM"
          expr
      4 -> do
        flag <- Bg.getInt32be
        pure . H.div $ do
          doHtml "CurlyEM"
          doHtmlStr $ show flag
      _ -> pure $ doHtmlStr ("Unknown encapsed mode " <> show encMode)

parseClassMember :: V.Vector Bs.ByteString -> Bg.Get H.Html
parseClassMember constantVec = do
  ranEmpty <- Bg.isEmpty
  if ranEmpty then
    pure $ doHtmlStr "literal: ran out of bytes!"
  else do
    mbrDecl <- Bg.getInt32be
    case mbrDecl of
      66 -> do
        cteID <- Bg.getInt32be
        pure . H.div $ do
          doHtml "CommentCDecl"
          showConstant $ derefCte cteID constantVec
      67 -> do
        nbrExprs <- Bg.getInt32be
        constDecls <- mapM (\_ -> do
            cteID <- Bg.getInt32be
            expr <- parseExpression constantVec
            pure . H.div $ do
              showConstant $ derefCte cteID constantVec
              expr
          ) [1..nbrExprs]
        pure . H.div $ do
          doHtml "ConstantCDecl"
          sequence_ constDecls
      68 -> do
        -- TODO: finish
        pure . H.div $ do
          doHtml "PropertyCDecl"
      69 -> do
        pure . H.div $ do
          doHtml "MethodCDecl"
      70 -> do
        pure . H.div $ do
          doHtml "ConstructorCDecl"
      71 -> do
        pure . H.div $ do
          doHtml "DestructorCDecl"
      72 -> do
        nbrImpls <- Bg.getInt32be
        impls <- mapM (\_ -> do
            cteID <- Bg.getInt32be
            pure . H.div $ do
              showConstant $ derefCte cteID constantVec
          ) [1..nbrImpls]
        nbrList <- Bg.getInt32be
        -- TODO: finish the reading of the list
        pure . H.div $ do
          doHtml "TraitUseCDecl"
          sequence_ impls
      _ -> pure $ doHtmlStr ("Unknown class member declaration " <> show mbrDecl)


parseVariableSpec :: V.Vector Bs.ByteString -> Bg.Get H.Html
parseVariableSpec constantVec = do
  ranEmpty <- Bg.isEmpty
  if ranEmpty then
    pure $ doHtmlStr "literal: ran out of bytes!"
  else do
    varSpec <- Bg.getInt32be
    case varSpec of
      72 -> do
        anID <- Bg.getInt32be
        pure . H.div $ do
          doHtml "SimpleVS"
          showConstant $ derefCte anID constantVec
      73 -> do
        aID <- Bg.getInt32be
        bID <- Bg.getInt32be
        pure . H.div $ do
          doHtml "DynamicVS"
          showConstant $ derefCte aID constantVec
          showConstant $ derefCte bID constantVec
      74 -> do
        anExpr <- parseExpression constantVec
        pure . H.div $ do
          doHtml "ComplexVS"
          anExpr
      _ -> pure $ doHtmlStr ("Unknown variable spec type " <> show varSpec)

parseCallerSpec :: V.Vector Bs.ByteString -> Bg.Get H.Html
parseCallerSpec constantVec = do
  ranEmpty <- Bg.isEmpty
  if ranEmpty then
    pure $ doHtmlStr "literal: ran out of bytes!"
  else do
    spec <- Bg.getInt32be
    case spec of
      75 -> do
        qualName <- parseQualName constantVec
        pure . H.div $ do
          doHtml "QualNameCS"
          qualName
      76 -> do
        varSpec <- parseVariableSpec constantVec
        pure . H.div $ do
          doHtml "VariableCS"
          varSpec
      77 -> do
        expr <- parseExpression constantVec
        pure . H.div $ do
          doHtml "SubscriptCS"
          expr
      _ -> pure $ doHtmlStr ("Unknown caller spec type " <> show spec)


parseQualName :: V.Vector Bs.ByteString -> Bg.Get H.Html
parseQualName constantVec = do
  ranEmpty <- Bg.isEmpty
  if ranEmpty then
    pure $ doHtmlStr "literal: ran out of bytes!"
  else do
    qName <- Bg.getInt32be
    case qName of
      78 -> do
        cteID <- Bg.getInt32be
        pure . H.div $ do
          doHtml "SimpleNameQN"
          showConstant $ derefCte cteID constantVec
      79 -> do
        nbrIDs <- Bg.getInt32be
        qualIDs <- mapM (\_ -> Bg.getInt32be) [1..nbrIDs]
        pure . H.div $ do
          doHtml "QualifiedNameQN"
          doHtmlStr $ show qualIDs
      _ -> pure $ doHtmlStr ("Unknown qual name type " <> show qName)


parseAccessMode :: V.Vector Bs.ByteString -> Bg.Get H.Html
parseAccessMode constantVec = do
  ranEmpty <- Bg.isEmpty
  if ranEmpty then
    pure $ doHtmlStr "literal: ran out of bytes!"
  else do
    accessMode <- Bg.getInt32be
    case accessMode of
      0 -> do
        cteID <- Bg.getInt32be
        pure . H.div $ do
          doHtml "NameMT"
          showConstant $ derefCte cteID constantVec
      1 -> pure . H.div $ do
        doHtml "ParentMT"
      2 -> pure . H.div $ do
        doHtml "SelfMT"
      3 -> do
        expr <- parseExpression constantVec
        pure . H.div $ do
          doHtml "VarExprMT"
          expr
      4 -> do
        expr <- parseExpression constantVec
        pure . H.div $ do
          doHtml "VarExprMT"
          expr
      _ -> pure . H.div $ do
        doHtml "Unknown access mode."


parseScopeMode :: V.Vector Bs.ByteString -> Bg.Get H.Html
parseScopeMode constantVec = do
  ranEmpty <- Bg.isEmpty
  if ranEmpty then
    pure $ doHtmlStr "literal: ran out of bytes!"
  else do
    scopeMode <- Bg.getInt32be
    case scopeMode of
      0 -> pure . H.div $ do
        doHtml "RelativeSelfSM"
      1 -> pure . H.div $ do
        doHtml "RelativeStaticSM"
      2 -> pure . H.div $ do
        doHtml "RelativeParentSM"
      3 -> do
        cteID <- Bg.getInt32be
        pure . H.div $ do
          doHtml "NamedSM"
          showConstant $ derefCte cteID constantVec
      4 -> do
        varSpec <- parseVariableSpec constantVec
        pure . H.div $ do
          doHtml "VariableSM"
          varSpec
      _ -> pure . H.div $ do
        doHtml "Unknown scope mode."


showBinOp :: Int32 -> String
showBinOp op = case op of
  0 -> "."
  1 -> "+"
  2 -> "-"
  3 -> "*"
  4 -> "/"
  5 -> "%"
  6 -> "^"
  7 -> "&"
  8 -> "|"
  9 -> "^"
  10 -> "=="
  11 -> "==="
  12 -> "!="
  13 -> "!=="
  14 -> ">"
  15 -> "<"
  16 -> ">="
  17 -> "<="
  18 -> "&&"
  19 -> "||"
  20 -> "???"
  21 -> "<<"
  22 -> ">>"
  _ -> "Unknown"

showUnaryOp :: Int32 -> String
showUnaryOp op = case op of
  0 -> "-"
  1 -> "!"
  _ -> "Unknown"

showUpdateOp :: Int32 -> String
showUpdateOp op = case op of
  0 -> "++"
  1 -> "--"
  _ -> "Unknown"

showInclMode :: Int32 -> String
showInclMode op = case op of
  0 -> "IncludeOnce"
  1 -> "RequireOnce"
  2 -> "Include"
  3 -> "Require"
  _ -> "Unknown"

derefCte :: Int32 -> V.Vector Bs.ByteString -> Bs.ByteString
derefCte cteID constantVec = constantVec V.! fromIntegral cteID

showStatement :: Bs.ByteString -> H.Html
showStatement bsText = H.div H.! A.class_ "p-1 text-yellow-700 dark:text-yellow-400" $ H.toHtml (T.decodeUtf8 $ bsText <> " ")

showExpression :: Bs.ByteString -> H.Html
showExpression bsText = H.div H.! A.class_ "p-1 text-blue-700 dark:text-blue-400" $ H.toHtml (T.decodeUtf8 $ bsText <> " ")

showConstant :: Bs.ByteString -> H.Html
showConstant bsText = H.div H.! A.class_ "p-1 text-pink-700 dark:text-pink-400" $ H.toHtml (T.decodeUtf8 $ bsText <> " ")

doHtml :: Bs.ByteString -> H.Html
doHtml bsText = H.div H.! A.class_ "p-1 hover:bg-blue-200 dark:hover:bg-blue-800" $ H.toHtml (T.decodeUtf8 $ bsText <> " ")


doHtmlStr :: String -> H.Html
doHtmlStr str = H.div H.! A.class_ "p-1 hover:bg-blue-200 dark:hover:bg-blue-800" $ H.toHtml (str <> " ")
