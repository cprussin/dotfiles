{
  writeShellScriptBin,
  callPackage,
  coreutils,
  gnugrep,
  gnused,
}: let
  browse = callPackage ./browse.nix {};
in
  writeShellScriptBin "search" ''
    printf=${coreutils}/bin/printf
    echo=${coreutils}/bin/echo
    grep=${gnugrep}/bin/grep
    sed=${gnused}/bin/sed
    browse=${browse}/bin/browse

    amazon="https://www.amazon.com/s/ref=nb_sb_noss?url=search-alias%3Daps&field-keywords=%QUERY%"
    archWiki="https://wiki.archlinux.org/index.php?title=Special%3ASearch&search=%QUERY%"
    google="https://google.com/search?q=%QUERY%"
    googleMaps="https://www.google.com/maps/search/%QUERY%"
    googleImages="https://www.google.com/search?q=%QUERY%&tbm=isch"
    github="https://github.com/%QUERY_NOESCAPE%"
    wikipedia="https://en.wikipedia.org/wiki/Special:Search?search=%QUERY%"
    youtube="https://www.youtube.com/results?search_query=%QUERY%"

    declare -A engines=(
        ["!g"]=$google
        ["!gh"]=$github
        ["!a"]=$amazon
        ["!amazon"]=$amazon
        ["!maps"]=$googleMaps
        ["!wiki"]=$wikipedia
        ["!w"]=$wikipedia
        ["!yt"]=$youtube
        ["!im"]=$googleImages
        ["!i"]=$googleImages
        ["!aw"]=$archWiki
    )
    default_engine=$google

    urlencode() {
      local length="''${#1}"
      for (( i = 0; i < length; i++ )); do
        local c="''${1:i:1}"
        case $c in
          [a-zA-Z0-9.~_-]) $printf "$c" ;;
          *) $printf '%%%02X' "'$c"
        esac
      done
    }

    query="$*"
    engine="$default_engine"

    for matcher in "''${!engines[@]}"; do
      if $echo "$query" | $grep -qE '(^|\s)'"$matcher"'($|\s)'
      then
        query="''${query/$matcher/}"
        engine="''${engines[$matcher]}"
      fi
    done

    query="$($echo "$query" | $sed 's/^[[:space:]]*//;s/[[:space:]]*$//')"
    url="''${engine/\%QUERY_NOESCAPE\%/$query}"

    exec $browse "''${url/\%QUERY\%/$(urlencode "$query")}"
  ''
