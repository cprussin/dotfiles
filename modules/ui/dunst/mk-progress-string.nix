{ writeShellScriptBin, gnused, coreutils, bc }:

writeShellScriptBin "mkProgressString" ''
  bc=${bc}/bin/bc
  printf=${coreutils}/bin/printf
  echo=${coreutils}/bin/echo
  sed=${gnused}/bin/sed

  items="25"
  filled="█"
  empty="░"

  numFilled=$($echo "(($items * $1)/100 + 0.5) / 1" | $bc)
  numEmpty=$($echo "$items - $numFilled" | $bc)

  echo -n "  "
  $printf "%''${numFilled}s" | $sed "s| |$filled|g"
  $printf "%''${numEmpty}s" | $sed "s| |$empty|g"
''
