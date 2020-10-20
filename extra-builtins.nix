{ exec, ... }:

let
  stringify = pkgs: "${pkgs.gnused}/bin/sed '1s/^/\"/;$s/$/\"/'";
  runCmd = pkgs: cmd: exec [ "sh" "-c" "${cmd} | ${stringify pkgs}" ];
  hashMosquittoPassword = pkgs: pkgs.writeShellScript "hash-mosquitto-password" ''
    tmp=$(mktemp)
    password="$(${pkgs.pass}/bin/pass show "Infrastructure/IoT/mqtt/$1")"

    ${pkgs.mosquitto}/bin/mosquitto_passwd -b $tmp foo "$password"
    ${pkgs.coreutils}/bin/cat $tmp | ${pkgs.gnused}/bin/sed 's/foo://'
    ${pkgs.coreutils}/bin/rm $tmp
  '';
in

{
  password = pkgs: name:
    runCmd pkgs "${pkgs.pass}/bin/pass show \"${name}\"";

  base64Password = pkgs: name:
    runCmd pkgs "${pkgs.pass}/bin/pass show \"${name}\" | ${pkgs.coreutils}/bin/base64 -w 0 -";

  hashedUserPassword = pkgs: name:
    runCmd pkgs "${pkgs.pass}/bin/pass show \"Infrastructure/login/${name}\" | ${pkgs.mkpasswd}/bin/mkpasswd -m sha-512 -s";

  hashedMosquittoPassword = pkgs: name:
    runCmd pkgs "${hashMosquittoPassword pkgs} ${name}";

  publicSshKey = pkgs: id:
    runCmd pkgs "${pkgs.gnupg}/bin/gpg --export-ssh-key \"${id}\"";
}
