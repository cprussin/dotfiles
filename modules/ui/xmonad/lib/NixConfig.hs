module NixConfig where

data NixConfig = NixConfig
  { paths :: Paths
  , primaryFont :: Font
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
