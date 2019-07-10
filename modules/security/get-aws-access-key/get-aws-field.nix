{ writeScript, pass, gnugrep, gnused, coreutils }:

writeScript "get-aws-field" ''
  pass=${pass}/bin/pass
  grep=${gnugrep}/bin/grep
  sed=${gnused}/bin/sed
  cut=${coreutils}/bin/cut

  $pass show "Computer Services/AWS" |\
    $grep "$1:" |\
    $cut -d ':' -f 2 |\
    $sed 's/ *//'
''
