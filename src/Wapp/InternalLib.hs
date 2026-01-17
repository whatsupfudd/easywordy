module Wapp.InternalLib where

import qualified Data.Map as Mp
import Data.Text (Text)

import qualified Wapp.AppDef as Wd

import qualified Wapp.Internal.WordPress.Functions as Wpf
import qualified Wapp.Internal.Z14L.Logic as Cl
import qualified Wapp.Internal.Z14L.Video as Cv
-- import qualified Wapp.Apps.Scenario.Prez as Sc
import qualified Wapp.Apps.Scenario.Presentation.DbOps as Sc
import qualified Wapp.Apps.Aox.Logic as Aop
import qualified Wapp.Apps.Aox.Test as Aot
import qualified Wapp.Apps.GnuHealth.FctDispatcher as Gdb
import qualified Wapp.Apps.KnowDocs.DbOps as Kd
import qualified Wapp.Apps.Complexor.DbOps as Cplx
import qualified Wapp.Apps.GoldFann.Ops as Gfo
import qualified Wapp.Apps.KdofA.Ops as Kdo


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
        -- ("gnuhealth_conf_commands", Ghlt.gnuhealth_conf_commands)
        ("general_tree_loader", Gdb.dispatch)
    ])
    , ("knowdocs.dbops", Mp.fromList [
      ("getKnowDocs", Kd.getKnowDocs)
      , ("getDocTree", Kd.getDocTree)
      , ("addNewDocument", Kd.addNewDocument)
    ])
    , ("complexor.dbops", Mp.fromList [
      ("getAllHistory", Cplx.getReposAndCommitLogs)
    ])
    , ("goldfann.ops", Mp.fromList [
      ("getCategoryTree", Gfo.getCategoryTree)
      , ("getDocs", Gfo.getDocs)
      , ("getDocContent", Gfo.getDocContent)
    ])
    , ("kdofa.ops", Mp.fromList [
      ("startImageBrowse", Kdo.startImageBrowse)
      , ("fetchImageSet", Kdo.fetchImageSet)
    ])
  ]
