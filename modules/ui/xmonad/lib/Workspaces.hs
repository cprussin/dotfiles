module Workspaces where

import qualified XMonad as XMonad

-- Map from the keybinding to the workspace name.
workspaces :: [(String, XMonad.KeySym)]
workspaces =
  [ ("1", XMonad.xK_parenleft)
  , ("2", XMonad.xK_parenright)
  , ("3", XMonad.xK_braceright)
  , ("4", XMonad.xK_plus)
  , ("5", XMonad.xK_braceleft)
  , ("6", XMonad.xK_bracketright)
  , ("7", XMonad.xK_bracketleft)
  , ("8", XMonad.xK_exclam)
  , ("9", XMonad.xK_equal)
  , ("0", XMonad.xK_asterisk)
  ]

workspaceNames :: [String]
workspaceNames = map fst workspaces
