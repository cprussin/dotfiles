{ exec, ... }:

let
  stringify = pkgs: "${pkgs.gnused}/bin/sed '1s/^/\"/;$s/$/\"/'";
  runCmd = pkgs: cmd: exec [ "sh" "-c" "${cmd} | ${stringify pkgs}" ];
in

{
  pass = pkgs: config: name:
    runCmd pkgs (
      pkgs.writeShellScript "pass" ''
        sudo -u ${config.primaryUserName} \
          PASSWORD_STORE_DIR=${config.secure.passwords} \
          GNUPGHOME=${config.secure.gnupg} \
          ${pkgs.pass}/bin/pass show "${name}"
      ''
    );
}
