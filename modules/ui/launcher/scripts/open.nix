{ writeScript
, bash
, coreutils
, file
, gnused
, zathura
, libreoffice
, sxiv
, feh
, mplayer
, emacs
}:

writeScript "open" ''
  #! ${bash}/bin/sh

  test=${coreutils}/bin/test
  echo=${coreutils}/bin/echo
  file=${file}/bin/file
  sed=${gnused}/bin/sed
  zathura=${zathura}/bin/zathura
  libreoffice=${libreoffice}/bin/libreoffice
  browse=@out@/bin/browse
  sxiv=${sxiv}/bin/sxiv
  feh=${feh}/bin/feh
  mplayer=${mplayer}/bin/mplayer
  emacsclient=${emacs}/bin/emacsclient

  if $test ! -e "$(eval echo $1)" -a "$1" != "*scratch*"
  then
    $echo "$1 is not a file"
    exit 1
  fi

  case $($file --mime-type "$1" | $sed 's/.*: //') in
    application/pdf) exec $zathura "$1" ;;
    application/msword) exec $libreoffice "$1" ;;
    application/*officedocument*) exec $libreoffice "$1" ;;
    application/vnd.ms-excel) exec $libreoffice "$1" ;;
    text/html) exec $browse "$1" ;;
    image/gif) exec $sxiv -a "$1" ;;
    image/*) exec $feh -. "$1" ;;
    video/*) exec $mplayer "$1" ;;
    *) exec $emacsclient -c "$1" ;;
  esac
''
