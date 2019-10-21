{ exec, ... }:

let
  stringify = pkgs: "${pkgs.gnused}/bin/sed '1s/^/\"/;$s/$/\"/'";
  runCmd = pkgs: cmd: exec [ "sh" "-c" "${cmd} | ${stringify pkgs}" ];
in

{
  pass = pkgs: config: name:
    runCmd pkgs "sudo -u ${config.primaryUserName} ${pkgs.pass}/bin/pass show '${name}'";
}
