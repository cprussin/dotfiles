{ writeShellScript, pass, coreutils, gnugrep, gnused }:

writeShellScript "get-aws-field" ''
  ${pass}/bin/pass show "Computer Services/AWS/root" |\
    ${gnugrep}/bin/grep "$1:" |\
    ${coreutils}/bin/cut -d ':' -f 2 |\
    ${gnused}/bin/sed 's/ *//'
''
