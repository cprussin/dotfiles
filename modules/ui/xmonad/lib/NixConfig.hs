module NixConfig where

data NixConfig = NixConfig
  { paths :: Paths
  , primaryFont :: Font
  , colorTheme :: ColorTheme
  }

data Paths = Paths
  { xmobar :: String
  , xsetroot :: String
  , launch :: String
  , xclip :: String
  , lockScreen :: String
  , backlight :: String
  , volume :: String
  , terminal :: String
  , rofiPass :: String
  }

data Font = Font
  { face :: String
  , size :: Integer
  }

data ColorTheme = ColorTheme
  { background :: String
  , foreground :: String
  , highlightBackground :: String
  , highlightForeground :: String
  , secondaryContent :: String
  , selection :: String
  , bright :: String
  , urgent :: String
  , warn :: String
  , black :: String
  , white :: String
  , grey :: String
  , lightGrey :: String
  , red :: String
  , lightRed :: String
  , green :: String
  , lightGreen :: String
  , yellow :: String
  , lightYellow :: String
  , blue :: String
  , lightBlue :: String
  , purple :: String
  , lightPurple :: String
  , cyan :: String
  , lightCyan :: String
  }
