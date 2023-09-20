{
  writeShellScriptBin,
  bat,
  coreutils,
  figlet,
  file,
  gnused,
  gnutar,
  python3Packages,
  poppler_utils,
  kitty,
  ghostscript,
  ffmpeg,
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
  gs=${ghostscript}/bin/gs
  ffmpeg=${ffmpeg}/bin/ffmpeg

  lineLimit=100

  limit() {
    $head -n $lineLimit
  }

  # Kitty kittens in fzf are broken, see https://github.com/junegunn/fzf/issues/3228
  # showimg="$kitty +kitten icat --place 79x53@85x1"
  # $kitty +kitten icat --clear

  homefile=$HOME/"$1"

  if [ -d "$homefile" ]
  then
    $ls --color=always -ahl "$homefile" | limit
  elif [ -f "$homefile" ]
  then
    case $($file --mime-type "$homefile" | $sed 's/.*: //') in
      application/gzip) $tar ztvf "$homefile" | limit ;;
      # application/pdf) $gs -q -sOutputFile=- -sDEVICE=jpeg -dNOPAUSE -dBATCH -dFirstPage=1 -dLastPage=1 -dJPEG=90 -r300 "$homefile" | $showimg ;;
      audio/*) $eyeD3 "$homefile" ;;
      # image/*) $showimg "$homefile" ;;
      # video/*) $ffmpeg -v 0 -ss 00:00:15 -i "$homefile" -vframes 1 -q:v 2 -f singlejpeg - | $showimg ;;
      *) $bat --style=numbers --color=always "$homefile" | limit ;;
    esac
  else
    $figlet -c "$1"
  fi
''
