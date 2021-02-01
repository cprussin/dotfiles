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

  hashedPassword = pkgs: prefix: method: name:
    runCmd pkgs "${pkgs.pass}/bin/pass show \"${prefix}/${name}\" | ${pkgs.mkpasswd}/bin/mkpasswd -m ${method} -s";

  publicSshKey = pkgs: id:
    runCmd pkgs "${pkgs.gnupg}/bin/gpg --export-ssh-key \"${id}\"";

  wpaPassphrase = pkgs:
    let
      wpa_passphrase = "${pkgs.wpa_supplicant}/bin/wpa_passphrase";
      pass = "${pkgs.pass}/bin/pass";
      grep = "${pkgs.gnugrep}/bin/grep";
      sed = "${pkgs.gnused}/bin/sed";
      getWpaPassphrase = pkgs.writeShellScript "getWpaPassphrase" ''
        password="$(${pass} show "Wifi/$*")"
        ${wpa_passphrase} "$*" "$password" | ${grep} -P '^\tpsk=' | ${sed} 's/^\tpsk=//'
      '';
    in
    network: runCmd pkgs "${getWpaPassphrase} ${network}";
}
