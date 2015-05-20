module Layouts
  ( layoutHook
  ) where

import XMonad ((|||))

import qualified Layouts.HTiled as HTiled
import qualified Layouts.Tabbed as Tabbed
import qualified Layouts.VTiled as VTiled
import qualified XMonad.Hooks.ManageDocks as ManageDocks
import qualified XMonad.Layout.MultiToggle as MultiToggle
import qualified XMonad.Layout.MultiToggle.Instances as MultiToggleInstances

layoutHook = layoutPlugins availableLayouts

-- Layout plugins that apply to all layouts
layoutPlugins
  = ManageDocks.avoidStruts -- Add space for the status bar
  . MultiToggle.mkToggle (MultiToggle.single MultiToggleInstances.FULL)

-- Available layouts
availableLayouts =
  VTiled.layout ||| HTiled.layout ||| Tabbed.layout
