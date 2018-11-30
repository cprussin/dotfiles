module Startup where

import qualified Data.Foldable as Foldable
import qualified XMonad as XMonad

startup :: XMonad.X ()
startup = Foldable.traverse_ XMonad.spawn
  -- UI stuff
  [ "setup-monitors"
  , "xsetroot -cursor_name left_ptr"
  ]
