module Wapp.InternalLib where

import qualified Data.Map as Mp
import Data.Text (Text)

import qualified Wapp.AppDef as Wd

import qualified WordPress.Functions as Wpf
import qualified Wapp.Apps.Z14L.Logic as Cl
import qualified Wapp.Apps.Z14L.Video as Cv
import qualified Wapp.Apps.Scenario.Prez as Sc

type LibraryMap = Mp.Map Text (Mp.Map Text Wd.InternalFunction)


buildInternalLibrary :: LibraryMap
buildInternalLibrary = Mp.fromList [  
    ("internal.wordpress", Mp.fromList [
      ("fetchVersions", Wpf.fetchVersions)
      , ("fetchFolders", Wpf.fetchFolders)
      , ("fetchFiles", Wpf.fetchFiles)
      , ("fetchFileDetails", Wpf.fetchFileDetails)
    ])
    , ("internal.chat", Mp.fromList [
      ("receiveMsg", Cl.receiveMsg)
      , ("startVideoSession", Cv.startSession)
    ])
    , ("internal.scenario", Mp.fromList [
      ("browsePrez", Sc.browsePrez)
    ])
  ]
