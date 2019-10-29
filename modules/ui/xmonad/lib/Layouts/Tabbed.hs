module Layouts.Tabbed
  ( layout
  ) where

import NixConfig (NixConfig, primaryFont, colorTheme, face, size, selection, background, urgent, highlightForeground, highlightBackground)
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
    { Tabbed.activeColor = selection $ colorTheme nixConfig
    , Tabbed.activeBorderColor = selection $ colorTheme nixConfig
    , Tabbed.activeTextColor = background $ colorTheme nixConfig
    , Tabbed.inactiveColor = highlightBackground $ colorTheme nixConfig
    , Tabbed.inactiveBorderColor = highlightForeground $ colorTheme nixConfig
    , Tabbed.inactiveTextColor = highlightForeground $ colorTheme nixConfig
    , Tabbed.urgentColor = urgent $ colorTheme nixConfig
    , Tabbed.urgentBorderColor = urgent $ colorTheme nixConfig
    , Tabbed.urgentTextColor = background $ colorTheme nixConfig
    , Tabbed.fontName = fontName nixConfig
    }
