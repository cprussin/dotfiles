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

  getMatrixSynapseDatabaseConfigFile = pkgs.writeShellScriptBin "getMatrixSynapseDatabaseConfigFile" ''
    set -euo pipefail
    echo "database:"
    echo "  name: \"psycopg2\""
    echo "  args:"
    echo "    password: \"$(${getPassword}/bin/getPassword "$1")\""
    echo "    database: \"matrix-synapse\""
    echo "    user: \"matrix-synapse\""
  '';

  getMautrixTelegramEnvironmentFile = pkgs.writeShellScriptBin "getMautrixTelegramEnvironmentFile" ''
    set -euo pipefail
    getPasswordField=${getPasswordField}/bin/getPasswordField
    echo "MAUTRIX_TELEGRAM_TELEGRAM_API_ID=\"$($getPasswordField "$1" "Api Id")\""
    echo "MAUTRIX_TELEGRAM_TELEGRAM_API_HASH=\"$($getPasswordField "$1" "Api Hash")\""
    echo "MAUTRIX_TELEGRAM_APPSERVICE_DATABASE=\"postgres://mautrix-telegram:$($getPasswordField "$1" "Database")@localhost/mautrix-telegram\""
    echo "MAUTRIX_TELEGRAM_APPSERVICE_AS_TOKEN=\"$($getPasswordField "$1" "AS Token")\""
    echo "MAUTRIX_TELEGRAM_APPSERVICE_HS_TOKEN=\"$($getPasswordField "$1" "HS Token")\""
  '';

  getMautrixSignalEnvironmentFile = pkgs.writeShellScriptBin "getMautrixSignalEnvironmentFile" ''
    set -euo pipefail
    getPasswordField=${getPasswordField}/bin/getPasswordField
    echo "MAUTRIX_SIGNAL_APPSERVICE_DATABASE=\"postgres://mautrix-signal:$($getPasswordField "$1" "Database")@localhost/mautrix-signal\""
    echo "MAUTRIX_SIGNAL_APPSERVICE_AS_TOKEN=\"$($getPasswordField "$1" "AS Token")\""
    echo "MAUTRIX_SIGNAL_APPSERVICE_HS_TOKEN=\"$($getPasswordField "$1" "HS Token")\""
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
      getMatrixSynapseDatabaseConfigFile
      getMautrixTelegramEnvironmentFile
      getMautrixSignalEnvironmentFile
      getWpaPassphraseFile
    ];
  };
  getPassword = name: [ "getPassword" name ];
  getFullPassword = name: [ "getFullPassword" name ];
  getPasswordField = name: field: [ "getPasswordField" name field ];
  getBase64EncodedPassword = name: [ "getBase64EncodedPassword" name ];
  getHashedUserPassword = name: [ "getHashedUserPassword" name ];
  getUsernamePasswordFile = name: [ "getUsernamePasswordFile" name ];
  getMatrixSynapseDatabaseConfigFile = name: [ "getMatrixSynapseDatabaseConfigFile" name ];
  getMautrixTelegramEnvironmentFile = name: [ "getMautrixTelegramEnvironmentFile" name ];
  getMautrixSignalEnvironmentFile = name: [ "getMautrixSignalEnvironmentFile" name ];
  getWpaPassphraseFile = networks: [ "getWpaPassphraseFile" ] ++ networks;
}
