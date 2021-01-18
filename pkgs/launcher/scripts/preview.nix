{ writeShellScriptBin
, bat
, coreutils
, figlet
, file
, gnused
, gnutar
, python3Packages
, poppler_utils
, mediainfo
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
  mediainfo=${mediainfo}/bin/mediainfo

  lineLimit=100

  limit() {
    $head -n $lineLimit
  }

  if [ -d "$1" ]
  then
    exec $ls --color=always -ahl "$1" | limit
  elif [ -f "$1" ]
  then
    case $($file --mime-type "$1" | $sed 's/.*: //') in
      application/gzip) $tar ztvf "$1" | limit ;;
      application/pdf) $pdftotext "$1" - | limit ;;
      audio/*) $eyeD3 "$1" ;;
      # TODO revisit these when termite supports sixel images
      image/*) $mediainfo "$1" ;;
      video/*) $mediainfo "$1" ;;
      *) exec $bat --style=numbers --color=always "$1" | limit ;;
    esac
  else
    exec $figlet -c "$1"
  fi
''
