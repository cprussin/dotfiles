module Layouts.HTiled
  ( layout
  ) where

import qualified Layouts.VTiled as VTiled
import qualified XMonad as XMonad
import qualified XMonad.Layout.Renamed as Renamed

-- Name the layout.  Use box-drawing characters as a pseudo-icon.
name = "┠─┬─┨"

-- A vertically tiling layout
layout =
  Renamed.renamed [Renamed.Replace name] $ XMonad.Mirror VTiled.layout
