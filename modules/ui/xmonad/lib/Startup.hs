module Startup where

import qualified Data.Foldable as Foldable
import qualified Paths as Paths
import qualified XMonad as XMonad

startup :: Paths.Paths -> XMonad.X ()
startup paths = Foldable.traverse_ XMonad.spawn
  -- UI stuff
  [ Paths.setupMonitors paths
  , Paths.xsetroot paths ++ " -cursor_name left_ptr"
  ]
