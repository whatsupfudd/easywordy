module Wapp.InternalLib where

import qualified Data.Map as Mp
import Data.Text (Text)

import qualified Wapp.AppDef as Wd

import qualified WordPress.Functions as Wpf
import qualified Wapp.Apps.Z14L.Logic as Cl
import qualified Wapp.Apps.Z14L.Video as Cv
import qualified Wapp.Apps.Scenario.Prez as Sc
import qualified Wapp.Apps.Scenario.Presentation.DbOps as Sc
import qualified Wapp.Apps.Aox.Logic as Aop
import qualified Wapp.Apps.Aox.Test as Aot
import qualified Wapp.Apps.GnuHealth.Logic as Ghlt
-- Logic defined within the EasyWordy app:
buildInternalLibrary :: Wd.LibraryMap
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
    , ("internal.aox", Mp.fromList [
      ("getUserMailboxes", Aot.getUserMailboxes)
    ])
  ]


-- Logic added to the EW by dynamic libraries, on a per-app requirement basis.
-- TODO: How to initialize this from an external file?
buildNativeLibrary :: Wd.NativeLibMap
buildNativeLibrary = Mp.fromList [
  ("scenario.prez", Mp.fromList [
    ("getTopLevelPrez", Sc.getTopLevelPrez)
    , ("getActsForPrez", Sc.getActsForPrez)
    , ("getFullPrezTree", Sc.getFullPrezTree)
  ])
  , ("aox.mb", Mp.fromList [
    ("getUserMailboxes", Aop.getUserMailboxes)
  ])
  , ("gnuhealth.dbops", Mp.fromList [
    ("gnuhealth_conf_commands", Ghlt.gnuhealth_conf_commands)
  ])
  ]
