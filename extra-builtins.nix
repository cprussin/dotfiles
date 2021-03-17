{ exec, ... }:
let
  stringify = pkgs: "${pkgs.gnused}/bin/sed '1s/^/\"/;$s/$/\"/'";
  runCmd = pkgs: cmd: exec [ "sh" "-c" "${cmd} | ${stringify pkgs}" ];
  passwords = pkgs: pkgs.callPackage ./lib/passwords.nix { };
  getPassword = pkgs: "${(passwords pkgs).passwordUtils}/bin/getPassword";
  getFullPassword = pkgs: "${(passwords pkgs).passwordUtils}/bin/getFullPassword";
  getPasswordField = pkgs: "${(passwords pkgs).passwordUtils}/bin/getPasswordField";
in
{
  publicSshKey = pkgs: id:
    runCmd pkgs "${pkgs.gnupg}/bin/gpg --export-ssh-key \"${id}\"";

  getPasswordValue = pkgs: name:
    runCmd pkgs "${getPassword pkgs} '${name}'";

  getFullPasswordValue = pkgs: name:
    runCmd pkgs "${getFullPassword pkgs} '${name}'";

  getPasswordFieldValue = pkgs: name: field:
    runCmd pkgs "${getPasswordField pkgs} '${name}' '${field}'";

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
