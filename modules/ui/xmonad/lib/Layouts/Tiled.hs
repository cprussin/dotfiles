module Layouts.Tiled
  ( layout
  ) where

import qualified XMonad.Layout.Maximize as Maximize
import qualified XMonad.Layout.ResizableTile as ResizableTile
import qualified XMonad.Layout.Spacing as Spacing

-- Any old tiling layout
layout
  = Maximize.maximizeWithPadding 40 -- Allow mod+f to toggle maximized
  . Spacing.spacingRaw True (Spacing.Border 0 10 10 10) True (Spacing.Border 10 10 10 10) True -- Add 5px between windows
  $ ResizableTile.ResizableTall 1 (3/100) (3/5) []
