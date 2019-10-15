module Layouts.Tabbed
  ( layout
  ) where

import NixConfig (NixConfig, primaryFont, face, size)
import qualified Styles as Styles
import qualified XMonad.Layout.NoBorders as NoBorders
import qualified XMonad.Layout.Renamed as Renamed
import qualified XMonad.Layout.Tabbed as Tabbed

-- Name the layout.  Use box-drawing characters as a pseudo-icon.
name = "┃   ┃"

fontName :: NixConfig -> String
fontName nixConfig = "xft:" <> fontFace <> ":size=" <> fontSize
  where
    fontFace = face theme
    fontSize = show $ size theme
    theme = primaryFont nixConfig

-- A tabbed layout
layout nixConfig = NoBorders.noBorders $ Renamed.renamed [Renamed.Replace name] $
  Tabbed.tabbed Tabbed.shrinkText Tabbed.def
    { Tabbed.activeColor = Styles.focusedBorderColor
    , Tabbed.activeBorderColor = Styles.focusedBorderColor
    , Tabbed.activeTextColor = Styles.activeTextColor
    , Tabbed.inactiveColor = Styles.normalBorderColor
    , Tabbed.inactiveBorderColor = Styles.inactiveTextColor
    , Tabbed.inactiveTextColor = Styles.inactiveTextColor
    , Tabbed.urgentColor = Styles.urgentColor
    , Tabbed.urgentBorderColor = Styles.urgentColor
    , Tabbed.urgentTextColor = Styles.activeTextColor
    , Tabbed.fontName = fontName nixConfig
    }
