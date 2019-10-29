module Statusbar where

import NixConfig (NixConfig, selection, colorTheme)
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified GHC.IO.Handle.Types as HandleTypes
import qualified Workspaces as Workspaces
import qualified XMonad as XMonad
import qualified XMonad.Actions.WorkspaceNames as WorkspaceNames
import qualified XMonad.Hooks.DynamicLog as DynamicLog
import qualified XMonad.Util.Run as Run

-- Set up status bar output
statusbar :: NixConfig -> HandleTypes.Handle -> XMonad.X ()
statusbar nixConfig pipe = statusbarPlugins options >>= DynamicLog.dynamicLogWithPP
  where
    options = DynamicLog.xmobarPP
      { DynamicLog.ppOutput = Run.hPutStrLn pipe
      , DynamicLog.ppSep = "    "
      , DynamicLog.ppLayout = showLayout
      , DynamicLog.ppCurrent = showCurrentWorkspace nixConfig
      , DynamicLog.ppHidden = showHiddenWorkspace
      , DynamicLog.ppTitle = showTitle nixConfig
      }

-- Plugins that apply to status bar output
statusbarPlugins :: DynamicLog.PP -> XMonad.X DynamicLog.PP
statusbarPlugins = WorkspaceNames.workspaceNamesPP

-- Make layout indicator clickable
showLayout :: String -> String
showLayout = DynamicLog.wrap "<action=xdotool key super+Tab>" "</action>"

-- Highlight visible workspace
showCurrentWorkspace :: NixConfig -> XMonad.WorkspaceId -> String
showCurrentWorkspace nixConfig =
  DynamicLog.wrap ("<fc=" ++ (selection $ colorTheme nixConfig) ++ ">[") "]</fc>"

-- Make workspace clickable
showHiddenWorkspace :: XMonad.WorkspaceId -> String
showHiddenWorkspace workspace =
  DynamicLog.wrap ("<action=`xdotool key 'super+" ++ key ++ "'`> ") " </action>" workspace
  where
    key = Maybe.fromMaybe "" $
      XMonad.keysymToString <$> snd <$> List.find ((== workspace) . fst) Workspaces.workspaces

-- Color and ellipsize the title
showTitle :: NixConfig -> String -> String
showTitle nixConfig
  = DynamicLog.xmobarColor (selection $ colorTheme nixConfig) ""
  . DynamicLog.shorten 100
