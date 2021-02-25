{ writeShellScript, callPackage, coreutils }:

name: url:
let
  mkWebApp = callPackage ./mkWebApp.nix { };
  webappForUser = user:
    mkWebApp "${name}-webapp-${user}" (
      builtins.replaceStrings [ "@user@" ] [ user ] url
    );
in
writeShellScript name ''
  test=${coreutils}/bin/test

  if $test "$1" == "netflix"
  then
    exec ${webappForUser "cprussin@netflix.com"}
  else
    exec ${webappForUser "connor@prussin.net"}
  fi
''
