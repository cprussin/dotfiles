{ writeShellScriptBin, bash, coreutils, emacs }:

writeShellScriptBin "toggle-colors" ''
  test=${coreutils}/bin/test
  emacsclient=${emacs}/bin/emacsclient

  # Figure out the color to set to and the opposite of that ('light' and
  # 'dark').
  color="$1"
  if $test $color == "dark"
  then
      opposite="light"
  else
      opposite="dark"
  fi

  # FIXME fix xrdb color setting
  # Set up Xresources colorscheme.
  # sed -i "s/.Xresources-$opposite/.Xresources-$color/" ~/.Xresources
  # xrdb ~/.Xresources

  # Set up emacs theme
  $emacsclient --eval "(load-theme 'solarized-$color t)"

  # TODO set up urxvtc theme
  # TODO set up xmonad theme
''
