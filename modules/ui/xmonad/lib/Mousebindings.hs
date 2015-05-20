module Mousebindings where

import qualified Data.Map as Map
import qualified Keybindings as Keybindings
import qualified XMonad as XMonad
import qualified XMonad.StackSet as StackSet

-- List bound mouse actions.
mousebindings ::
  XMonad.XConfig XMonad.Layout ->
  Map.Map (XMonad.ButtonMask, XMonad.Button) (XMonad.Window -> XMonad.X ())
mousebindings _ = Map.fromList $

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((Keybindings.modMask, XMonad.button1), (\w ->
        XMonad.focus w >>
        XMonad.mouseMoveWindow w >>
        XMonad.windows StackSet.shiftMaster
      ))

    -- mod-button2, Raise the window to the top of the stack
    , ((Keybindings.modMask, XMonad.button2), (\w ->
        XMonad.focus w >> XMonad.windows StackSet.shiftMaster
      ))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((Keybindings.modMask, XMonad.button3), (\w ->
        XMonad.focus w >>
        XMonad.mouseResizeWindow w >>
        XMonad.windows StackSet.shiftMaster
      ))

    ]
