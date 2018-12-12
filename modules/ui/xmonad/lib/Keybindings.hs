module Keybindings where

import XMonad ((.|.))

import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Graphics.X11.ExtraTypes.XF86 as XF86
import qualified Layouts.Tabbed as Tabbed
import qualified Paths as Paths
import qualified Workspaces as Workspaces
import qualified XMonad as XMonad
import qualified XMonad.Actions.Navigation2D as Navigation2D
import qualified XMonad.Layout.Maximize as Maximize
import qualified XMonad.Layout.MultiToggle as MultiToggle
import qualified XMonad.Layout.MultiToggle.Instances as MultiToggleInstances
import qualified XMonad.Layout.ResizableTile as ResizableTile
import qualified XMonad.StackSet as StackSet

-- Use mod4 (OS key) as the primary modifier
modMask :: XMonad.KeyMask
modMask = XMonad.mod4Mask

-- Vim-style mapping from keybinding to direction.
directions :: [(Navigation2D.Direction2D, XMonad.KeySym)]
directions =
  [ (Navigation2D.D, XMonad.xK_j)
  , (Navigation2D.U, XMonad.xK_k)
  , (Navigation2D.L, XMonad.xK_h)
  , (Navigation2D.R, XMonad.xK_l)
  ]

-- List bound keys.
keybindings ::
  Paths.Paths ->
  XMonad.XConfig XMonad.Layout ->
  Map.Map (XMonad.ButtonMask, XMonad.KeySym) (XMonad.X ())
keybindings paths _ = Map.fromList $

  -- Switch layouts
  [ ((modMask, XMonad.xK_Tab), XMonad.sendMessage XMonad.NextLayout)
  , ((modMask .|. XMonad.shiftMask, XMonad.xK_Tab), goPreviousLayout)

  -- Resize
  , ((modMask .|. XMonad.controlMask, XMonad.xK_j), XMonad.sendMessage ResizableTile.MirrorShrink)
  , ((modMask .|. XMonad.controlMask, XMonad.xK_k), XMonad.sendMessage ResizableTile.MirrorExpand)
  , ((modMask .|. XMonad.controlMask, XMonad.xK_h), XMonad.sendMessage XMonad.Shrink)
  , ((modMask .|. XMonad.controlMask, XMonad.xK_l), XMonad.sendMessage XMonad.Expand)

  -- Floating/fullscreen toggle
  , ((modMask, XMonad.xK_space), Navigation2D.switchLayer)
  , ((modMask .|. XMonad.shiftMask, XMonad.xK_space), XMonad.withFocused toggleFloating)
  , ((modMask, XMonad.xK_f), XMonad.withFocused (XMonad.sendMessage . Maximize.maximizeRestore))
  , ((modMask .|. XMonad.shiftMask, XMonad.xK_f), XMonad.sendMessage $ MultiToggle.Toggle MultiToggleInstances.FULL)

  -- Number of windows in master
  , ((modMask, XMonad.xK_comma), XMonad.sendMessage (XMonad.IncMasterN 1))
  , ((modMask, XMonad.xK_period), XMonad.sendMessage (XMonad.IncMasterN (-1)))

  -- Close window
  , ((modMask .|. XMonad.shiftMask, XMonad.xK_q), XMonad.kill)

  -- Reset stuff
  , ((modMask .|. XMonad.shiftMask, XMonad.xK_Delete), XMonad.spawn $ Paths.setupMonitors paths)

  -- Launcher
  , ((modMask, XMonad.xK_d), XMonad.spawn $ Paths.launch paths)
  , ((modMask .|. XMonad.shiftMask, XMonad.xK_d), XMonad.spawn $ Paths.launch paths ++ " $(" ++ Paths.xclip paths ++ " -o)")
  , ((modMask, XMonad.xK_Return), XMonad.spawn "$TERMINAL")

  -- Screen Locker
  , ((modMask .|. XMonad.shiftMask, XMonad.xK_Return), XMonad.spawn $ Paths.i3lock paths ++ " -c 000000")

  -- Volume
  , ((modMask, XMonad.xK_End), XMonad.spawn $ Paths.volume paths ++ " toggle")
  , ((XMonad.noModMask, XF86.xF86XK_AudioMute), XMonad.spawn $ Paths.volume paths ++ " toggle")
  , ((modMask, XMonad.xK_Page_Up), XMonad.spawn $ Paths.volume paths ++ " + 1")
  , ((XMonad.noModMask, XF86.xF86XK_AudioRaiseVolume), XMonad.spawn $ Paths.volume paths ++ " + 1")
  , ((modMask, XMonad.xK_Page_Down), XMonad.spawn $ Paths.volume paths ++ " - 1")
  , ((XMonad.noModMask, XF86.xF86XK_AudioLowerVolume), XMonad.spawn $ Paths.volume paths ++ " - 1")

  -- Backlight
  , ((XMonad.noModMask, XF86.xF86XK_MonBrightnessUp), XMonad.spawn $ Paths.xbacklight paths ++ " + 10")
  , ((XMonad.noModMask, XF86.xF86XK_Launch6), XMonad.spawn $ Paths.xbacklight paths ++ " + 10")
  , ((XMonad.noModMask, XF86.xF86XK_MonBrightnessDown), XMonad.spawn $ Paths.xbacklight paths ++ " - 10")
  , ((XMonad.noModMask, XF86.xF86XK_Launch5), XMonad.spawn $ Paths.xbacklight paths ++ " - 10")

  ] ++

  -- Move focus with mod + direction
  (directionalKeys modMask Navigation2D.windowGo
    [ ((XMonad.description Tabbed.layout, XMonad.xK_h), XMonad.windows StackSet.focusUp)
    , ((XMonad.description Tabbed.layout, XMonad.xK_l), XMonad.windows StackSet.focusDown)
    ]
  ) ++

  -- Swap windows with mod + shift + direction
  (directionalKeys (modMask .|. XMonad.shiftMask) Navigation2D.windowSwap
    [ ((XMonad.description Tabbed.layout, XMonad.xK_h), XMonad.windows StackSet.swapUp)
    , ((XMonad.description Tabbed.layout, XMonad.xK_l), XMonad.windows StackSet.swapDown)
    ]
  ) ++

  -- Move screens with mod + ctrl + shift + direction
  (directionalKeys (modMask .|. XMonad.shiftMask .|. XMonad.controlMask) Navigation2D.screenSwap []) ++

  -- Switch workspace with mod + workspace key
  (workspaceKeys modMask StackSet.view) ++

  -- Move windows to other workspaces with mod + shift + workspace key
  (workspaceKeys (modMask .|. XMonad.shiftMask) StackSet.shift)

-- Map directional keys
directionalKeys ::
  XMonad.ButtonMask ->
  (Navigation2D.Direction2D -> Bool -> XMonad.X ()) ->
  [((String, XMonad.KeySym), XMonad.X ())] ->
  [((XMonad.ButtonMask, XMonad.KeySym), (XMonad.X ()))]
directionalKeys keymod action overrides = mappedKeys directions keymod overrides $ \arg ->
  action arg True

-- Map workspace keys
workspaceKeys ::
  XMonad.ButtonMask ->
  (String -> XMonad.WindowSet -> XMonad.WindowSet) ->
  [((XMonad.ButtonMask, XMonad.KeySym), (XMonad.X ()))]
workspaceKeys keymod action = mappedKeys Workspaces.workspaces keymod [] $ \arg ->
  XMonad.windows $ action arg

-- Build keybindings out of a map of keybindings to arguments (for directional
-- keys or workspace keys)
mappedKeys ::
  [(b, XMonad.KeySym)] ->
  XMonad.ButtonMask ->
  [((String, XMonad.KeySym), XMonad.X ())] ->
  (b -> (XMonad.X ())) ->
  [((XMonad.ButtonMask, XMonad.KeySym), (XMonad.X ()))]
mappedKeys keys keymod overrides makeAction = flip map keys $ \(arg, key) ->
  ((keymod, key), withCurrentLayout $ \layout ->
      Maybe.fromMaybe (makeAction arg) $
        snd <$> List.find (\(condition, _) -> fst condition == layout && snd condition == key) overrides
  )

withCurrentLayout cb =
  XMonad.withWindowSet $ cb . XMonad.description . StackSet.layout . StackSet.workspace . StackSet.current

-- Sink a window if it's currently floated, float it otherwise
toggleFloating window =
  XMonad.windows $ \stackset ->
    if Map.member window (StackSet.floating stackset)
      then StackSet.sink window stackset
      else StackSet.float window (StackSet.RationalRect 0.25 0.25 0.5 0.5) stackset

--
goPreviousLayout = do
  XMonad.sendMessage XMonad.NextLayout
  XMonad.sendMessage XMonad.NextLayout
