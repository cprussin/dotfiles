{ exec, ... }:

let
  stringify = pkgs: "${pkgs.gnused}/bin/sed '1s/^/\"/;$s/$/\"/'";

  runCmd = pkgs: cmd: exec [ "sh" "-c" "${cmd} | ${stringify pkgs}" ];

  pass = pkgs: config: name:
    runCmd pkgs (
      pkgs.writeShellScript "pass" ''
        sudo -u ${config.primary-user.name} \
          PASSWORD_STORE_DIR=${config.primary-user.secure.passwords} \
          GNUPGHOME=${config.primary-user.secure.gnupg} \
          ${pkgs.pass}/bin/pass show "${name}"
      ''
    );

  passSplit = pkgs: config: name:
    pkgs.lib.splitString "\n" (pass pkgs config name);

  passFileName = name: "pass-${builtins.replaceStrings [ "/" ] [ "-" ] name}";
in

{
  password = pkgs: config: name:
    builtins.elemAt (passSplit pkgs config name) 0;

  passwordFile = pkgs: config: name: pkgs.writeText (passFileName name) (
    pass pkgs config name
  );

  passwordField = pkgs: config: name: field:
    pkgs.lib.replaceStrings [ "${field}: " ] [ "" ] (
      pkgs.lib.findFirst (pkgs.lib.hasPrefix "${field}: ") "" (
        passSplit pkgs config name
      )
    );

  publicSshKey = pkgs: config:
    runCmd pkgs (
      pkgs.writeShellScript "get-public-ssh-key" ''
        sudo -u ${config.primary-user.name} \
          GNUPGHOME=${config.primary-user.secure.gnupg} \
          ${pkgs.gnupg}/bin/gpg --export-ssh-key $(cat ${config.primary-user.secure.passwords}/.gpg-id)
      ''
    );
}
