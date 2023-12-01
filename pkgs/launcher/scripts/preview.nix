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
  kitten=${kitty}/bin/kitten
  gs=${ghostscript}/bin/gs
  ffmpeg=${ffmpeg}/bin/ffmpeg

  lineLimit=100

  limit() {
    $head -n $lineLimit
  }

  showimg() {
    if [ "$1" ]; then
      stdin=no
    else
      stdin=yes
    fi
    $kitten icat --clear --transfer-mode=memory --place="$COLUMNS"x"$LINES"@"$((COLUMNS + 5))"x0 --align center --stdin $stdin "$1" > /dev/tty
  }
  kitten icat --clear --transfer-mode=memory --stdin=no > /dev/tty

  homefile=$HOME/"$1"

  if [ -d "$homefile" ]
  then
    $ls --color=always -ahl "$homefile" | limit
  elif [ -f "$homefile" ]
  then
    case $($file --mime-type "$homefile" | $sed 's/.*: //') in
      application/gzip) $tar ztvf "$homefile" | limit ;;
      application/pdf) $gs -q -sOutputFile=- -sDEVICE=jpeg -dNOPAUSE -dBATCH -dFirstPage=1 -dLastPage=1 -dJPEG=90 -r300 "$homefile" | showimg ;;
      audio/*) $eyeD3 "$homefile" ;;
      image/*) showimg "$homefile" ;;
      video/*) $ffmpeg -v 0 -ss 00:00:15 -i "$homefile" -vframes 1 -q:v 2 -f image2pipe - | showimg ;;
      *) $bat --style=numbers --color=always "$homefile" | limit ;;
    esac
  else
    $figlet -c "$1"
  fi
''
