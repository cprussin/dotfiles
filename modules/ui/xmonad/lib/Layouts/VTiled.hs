module Layouts.VTiled
  ( layout
  ) where

import qualified Layouts.Tiled as Tiled
import qualified XMonad.Layout.NoBorders as NoBorders
import qualified XMonad.Layout.Renamed as Renamed

-- Name the layout.  Use box-drawing characters as a pseudo-icon.
name = "┃ ├─┨"

-- A vertically tiling layout
layout =
  NoBorders.smartBorders $ Renamed.renamed [Renamed.Replace name] Tiled.layout
