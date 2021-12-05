{ pkgs }:
let
  pass = "${pkgs.pass}/bin/pass";
  head = "${pkgs.coreutils}/bin/head";
  grep = "${pkgs.gnugrep}/bin/grep";
  sed = "${pkgs.gnused}/bin/sed";
  base64 = "${pkgs.coreutils}/bin/base64";
  mkpasswd = "${pkgs.mkpasswd}/bin/mkpasswd";
  wpa_passphrase = "${pkgs.wpa_supplicant}/bin/wpa_passphrase";

  getFullPassword = pkgs.writeShellScriptBin "getFullPassword" ''
    set -euo pipefail
    ${pass} show "$1"
  '';

  getPassword = pkgs.writeShellScriptBin "getPassword" ''
    set -euo pipefail
    ${pass} show "$1" | ${head} -n 1
  '';

  getPasswordField = pkgs.writeShellScriptBin "getPasswordField" ''
    set -euo pipefail
    ${pass} show "$1" | ${grep} "^$2: " | ${sed} "s/^$2: //"
  '';

  getBase64EncodedPassword = pkgs.writeShellScriptBin "getBase64EncodedPassword" ''
    set -euo pipefail
    ${pass} show "$1" | ${base64} -w 0 -
  '';

  getHashedUserPassword = pkgs.writeShellScriptBin "getHashedUserPassword" ''
    set -euo pipefail
    ${pass} show "$1" | ${mkpasswd} -m sha-512 -s
  '';

  getUsernamePasswordFile = pkgs.writeShellScriptBin "getUsernamePasswordFile" ''
    set -euo pipefail
    ${getPasswordField}/bin/getPasswordField "$1" "Username"
    ${getPassword}/bin/getPassword "$1"
  '';

  getMatrixSynapseDatabasePasswordFile = pkgs.writeShellScriptBin "getMatrixSynapseDatabasePasswordFile" ''
    set -euo pipefail
    echo "database:"
    echo "  name: \"psycopg2\""
    echo "  args:"
    echo "    password: \"$(${getPassword}/bin/getPassword "$1")\""
    echo "    database: \"matrix-synapse\""
    echo "    user: \"matrix-synapse\""
  '';

  getWpaPassphraseFile = pkgs.writeShellScriptBin "getWpaPassphraseFile" ''
    set -euo pipefail
    while [[ $# -gt 0 ]]
    do
      network="$1"
      shift
      psk_name="$1"
      shift
      password="$(${getPassword}/bin/getPassword "Wifi/$network")"
      psk="$(${wpa_passphrase} "$network" "$password" | ${grep} -P '^\tpsk=' | ${sed} 's/^\tpsk=//')"
      echo "$psk_name=\"$psk\""
    done
  '';
in
{
  passwordUtils = pkgs.symlinkJoin {
    name = "passwordUtils";
    paths = [
      getFullPassword
      getPassword
      getPasswordField
      getBase64EncodedPassword
      getHashedUserPassword
      getUsernamePasswordFile
      getMatrixSynapseDatabasePasswordFile
      getWpaPassphraseFile
    ];
  };
  getPassword = name: [ "getPassword" name ];
  getFullPassword = name: [ "getFullPassword" name ];
  getPasswordField = name: field: [ "getPasswordField" name field ];
  getBase64EncodedPassword = name: [ "getBase64EncodedPassword" name ];
  getHashedUserPassword = name: [ "getHashedUserPassword" name ];
  getUsernamePasswordFile = name: [ "getUsernamePasswordFile" name ];
  getMatrixSynapseDatabasePasswordFile = name: [ "getMatrixSynapseDatabasePasswordFile" name ];
  getWpaPassphraseFile = networks: [ "getWpaPassphraseFile" ] ++ networks;
}
