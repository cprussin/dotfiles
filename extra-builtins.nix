{ exec, ... }:

let
  stringify = pkgs: "${pkgs.gnused}/bin/sed '1s/^/\"/;$s/$/\"/'";
  runCmd = pkgs: cmd: exec [ "sh" "-c" "${cmd} | ${stringify pkgs}" ];
in

{
  password = pkgs: name:
    runCmd pkgs "${pkgs.pass}/bin/pass show \"${name}\"";

  base64Password = pkgs: name:
    runCmd pkgs "${pkgs.pass}/bin/pass show \"${name}\" | ${pkgs.coreutils}/bin/base64 -w 0 -";

  hashedUserPassword = pkgs: name:
    runCmd pkgs "${pkgs.pass}/bin/pass show \"Infrastructure/login/${name}\" | ${pkgs.mkpasswd}/bin/mkpasswd -m sha-512 -s";

  publicSshKey = pkgs: id:
    runCmd pkgs "${pkgs.gnupg}/bin/gpg --export-ssh-key \"${id}\"";
}
