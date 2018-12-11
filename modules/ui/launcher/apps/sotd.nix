{ writeScript, bash }:

writeScript "sotd" ''
  #! ${bash}/bin/sh

  open=@out@/bin/open
  browse=@out@/bin/browse

  case "$1" in
    list) exec $open "$HOME/Projects/songoftheday.net/Sources/sotdomizer/List" ;;
    *) exec $browse "https://docs.google.com/spreadsheets/d/11yYp4Ma5t7wJxSBZQYyVcO7FlWuG6cEOJrPXcQCv3AI" ;;
  esac
''
