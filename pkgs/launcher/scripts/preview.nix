{ writeShellScriptBin
, bat
, coreutils
, figlet
, file
, gnused
, gnutar
, python3Packages
, poppler_utils
, kitty
, imagemagick
, ghostscript
, ffmpeg
}:

writeShellScriptBin "preview" ''
  bat=${bat}/bin/bat
  head=${coreutils}/bin/head
  ls=${coreutils}/bin/ls
  figlet=${figlet}/bin/figlet
  file=${file}/bin/file
  sed=${gnused}/bin/sed
  tar=${gnutar}/bin/tar
  eyeD3=${python3Packages.eyeD3}/bin/eyeD3
  pdftotext=${poppler_utils}/bin/pdftotext
  kitty=${kitty}/bin/kitty
  convert=${imagemagick}/bin/convert
  ffmpeg=${ffmpeg}/bin/ffmpeg

  lineLimit=100

  limit() {
    $head -n $lineLimit
  }

  showimg="$kitty +kitten icat --transfer-mode file --place 79x53@85x1"

  $kitty +kitten icat --transfer-mode file --clear

  if [ -d "$1" ]
  then
    exec $ls --color=always -ahl "$1" | limit
  elif [ -f "$1" ]
  then
    case $($file --mime-type "$1" | $sed 's/.*: //') in
      application/gzip) $tar ztvf "$1" | limit ;;
      application/pdf) PATH=$PATH:${ghostscript}/bin $convert -quiet -density 100 "$1"[0] jpeg:- | $showimg ;;
      audio/*) $eyeD3 "$1" ;;
      image/*) $showimg "$1" ;;
      video/*) $ffmpeg -v 0 -ss 00:00:15 -i "$1" -vframes 1 -q:v 2 -f singlejpeg - | $showimg ;;
      *) exec $bat --style=numbers --color=always "$1" | limit ;;
    esac
  else
    exec $figlet -c "$1"
  fi
''
