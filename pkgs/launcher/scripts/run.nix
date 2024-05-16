{
  writeShellScriptBin,
  callPackage,
  coreutils,
  gnugrep,
}: let
  open = callPackage ./open.nix {};
  browse = callPackage ./browse.nix {};
  search = callPackage ./search.nix {};
in
  writeShellScriptBin "run" ''
    open=${open}/bin/open
    browse=${browse}/bin/browse
    search=${search}/bin/search
    echo=${coreutils}/bin/echo
    test=${coreutils}/bin/test
    grep=${gnugrep}/bin/grep

    tlds='com|net|org|gov|edu|co|io|do|me'
    browsePrefixes='http://|go/|localhost|chrome://'
    browseRegex='([0-9]{1,3}\.){3}[0-9]{1,3}'

    resolvedPath="$(eval $echo \"$@\" 2>/dev/null)"

    if $test -e $HOME/.launcher-apps/''${1/ */}
    then
      exec $HOME/.launcher-apps/$@
    elif $test "$resolvedPath" -a -e "$resolvedPath"
    then
      exec $open "$resolvedPath"
    elif $test "$resolvedPath" -a -e "$HOME/$resolvedPath"
    then
      exec $open "$HOME/$resolvedPath"
    elif $echo "$1" | $grep -oqE "(\.($TLDS)|^($browsePrefixes)|^($browseRegex)$)"
    then
      exec $browse "$1"
    elif $test "$*"
    then
      exec $search "$*"
    fi
  ''
