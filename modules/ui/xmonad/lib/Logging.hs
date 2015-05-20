module Logging where

import qualified Statusbar as Statusbar
import qualified XMonad.Actions.UpdatePointer as UpdatePointer

logHook xmobarPipe =
  Statusbar.statusbar xmobarPipe >> UpdatePointer.updatePointer (0.5, 0.5) (0, 0)
