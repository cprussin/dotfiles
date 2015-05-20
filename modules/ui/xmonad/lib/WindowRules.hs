module WindowRules where

import XMonad ((=?), (-->))

import qualified XMonad as XMonad
import qualified XMonad.Hooks.ManageDocks as ManageDocks
import qualified XMonad.Hooks.Place as Place

windowRules :: XMonad.ManageHook
windowRules = XMonad.composeAll
  [ placeNewFloatsUnderMouse
  , floats
  , ManageDocks.manageDocks
  ]

floats :: XMonad.ManageHook
floats = XMonad.composeAll $ map (--> XMonad.doFloat)
  [ XMonad.className =? "Mixer"
  , XMonad.className =? "Xtensoftphone"
  , XMonad.resource =? "wifi-menu"
  , XMonad.resource =? "bluetoothctl"
  , XMonad.title =? "pinentry"
  , XMonad.title =? "Auto-Type - KeePassX"
  ]

-- Place new floating windows under the middle of the mouse
placeNewFloatsUnderMouse :: XMonad.ManageHook
placeNewFloatsUnderMouse =
  Place.placeHook $ Place.inBounds $ Place.underMouse (0.5, 0.5)
