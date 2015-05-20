module Styles where

import qualified XMonad as XMonad

--
font :: String
font = "xft:DejaVu Sans Mono-10"

--
normalBorderColor :: String
normalBorderColor = "#073642"

--
focusedBorderColor :: String
focusedBorderColor = "#859900"

--
activeTextColor :: String
activeTextColor = "#fdf6e3"

--
inactiveTextColor :: String
inactiveTextColor = "#268bd2"

--
urgentColor :: String
urgentColor = "#d33682"

-- Add 3px border around windows
borderWidth :: XMonad.Dimension
borderWidth = 3
