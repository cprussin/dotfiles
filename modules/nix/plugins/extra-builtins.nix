{ exec, ... }:

let
  stringify = pkgs: "${pkgs.gnused}/bin/sed '1s/^/\"/;$s/$/\"/'";
  pass = pkgs: name: "${pkgs.pass}/bin/pass show '${name}' | ${stringify pkgs}";
in

{
  pass = pkgs: name: exec ["sh" "-c" (pass pkgs name)];
}
