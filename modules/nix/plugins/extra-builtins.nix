{ exec, ... }:

let
  stringify = pkgs: "${pkgs.gnused}/bin/sed '1s/^/\"/;$s/$/\"/'";
  runCmd = pkgs: cmd: exec ["sh" "-c" "${cmd} | ${stringify pkgs}"];
in

{
  pass = pkgs: name: runCmd pkgs "${pkgs.pass}/bin/pass show '${name}'";
}
