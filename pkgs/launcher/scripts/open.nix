{ writeShellScriptBin
, callPackage
, coreutils
, file
, gnused
, zathura
, imv
, mpv
, prusa-slicer
, freecad
, emacs
}:
let
  browse = callPackage ./browse.nix { };
in
writeShellScriptBin "open" ''
  test=${coreutils}/bin/test
  echo=${coreutils}/bin/echo
  file=${file}/bin/file
  sed=${gnused}/bin/sed
  zathura=${zathura}/bin/zathura
  browse=${browse}/bin/browse
  imv=${imv}/bin/imv
  mpv=${mpv}/bin/mpv
  prusaSlicer=${prusa-slicer}/bin/prusa-slicer
  freecad=${freecad}/bin/freecad
  emacsclient=${emacs}/bin/emacsclient

  if $test ! -e "$(eval echo \"$1\")" -a "$1" != "*scratch*"
  then
    $echo "$1 is not a file"
    exit 1
  fi

  case "$1" in
    *.stl) exec $prusaSlicer "$1" ;;
    *.3mf) exec $prusaSlicer "$1" ;;
    *.stp) exec $freecad "$1" ;;
    *)
      case $($file --brief --mime-type "$1") in
        application/pdf) exec $zathura "$1" ;;
        text/html) exec $browse "$1" ;;
        image/*) exec $imv "$1" ;;
        video/*) exec $mpv "$1" ;;
        audio/*) exec $mpv --force-window=yes "$1" ;;
        *) exec $emacsclient -c "$1" ;;
      esac
  esac
''
