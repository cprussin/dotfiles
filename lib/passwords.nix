{pkgs}: let
  pass = "${pkgs.pass}/bin/pass";
  head = "${pkgs.coreutils}/bin/head";
  base64 = "${pkgs.coreutils}/bin/base64";
  grep = "${pkgs.gnugrep}/bin/grep";
  sed = "${pkgs.gnused}/bin/sed";
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

  getMatrixSynapseSharedSecretConfigFile = pkgs.writeShellScriptBin "getMatrixSynapseSharedSecretConfigFile" ''
    set -euo pipefail
    echo "modules:"
    echo "  - module: shared_secret_authenticator.SharedSecretAuthProvider"
    echo "    config:"
    echo "      shared_secret: \"$(${getPassword}/bin/getPassword "$1")\""
    echo "      m_login_password_support_enabled: true"
  '';

  getMautrixTelegramEnvironmentFile = pkgs.writeShellScriptBin "getMautrixTelegramEnvironmentFile" ''
    set -euo pipefail
    getPasswordField=${getPasswordField}/bin/getPasswordField
    echo "MAUTRIX_TELEGRAM_TELEGRAM_API_ID=\"$($getPasswordField "$1" "Api Id")\""
    echo "MAUTRIX_TELEGRAM_TELEGRAM_API_HASH=\"$($getPasswordField "$1" "Api Hash")\""
    echo "MAUTRIX_TELEGRAM_APPSERVICE_DATABASE=\"postgres://mautrix-telegram:$($getPasswordField "$1" "Database")@localhost/mautrix-telegram\""
    echo "MAUTRIX_TELEGRAM_APPSERVICE_AS_TOKEN=\"$($getPasswordField "$1" "AS Token")\""
    echo "MAUTRIX_TELEGRAM_APPSERVICE_HS_TOKEN=\"$($getPasswordField "$1" "HS Token")\""
    echo "SHARED_SECRET=\"$(${getPassword}/bin/getPassword "$2")\""
  '';

  getMautrixSignalEnvironmentFile = pkgs.writeShellScriptBin "getMautrixSignalEnvironmentFile" ''
    set -euo pipefail
    getPasswordField=${getPasswordField}/bin/getPasswordField
    echo "MAUTRIX_SIGNAL_APPSERVICE_DATABASE=\"postgres://mautrix-signal:$($getPasswordField "$1" "Database")@localhost/mautrix-signal\""
    echo "MAUTRIX_SIGNAL_APPSERVICE_AS_TOKEN=\"$($getPasswordField "$1" "AS Token")\""
    echo "MAUTRIX_SIGNAL_APPSERVICE_HS_TOKEN=\"$($getPasswordField "$1" "HS Token")\""
    echo "SHARED_SECRET=\"$(${getPassword}/bin/getPassword "$2")\""
  '';

  getMautrixRegistrationFile = pkgs.writeShellScriptBin "getMautrixRegistrationFile" ''
    set -euo pipefail
    getPasswordField=${getPasswordField}/bin/getPasswordField
    password="Connor/Infrastructure/matrix/bridges/$1"
    echo "id: $1"
    echo "as_token: $($getPasswordField "$password" "AS Token")"
    echo "hs_token: $($getPasswordField "$password" "HS Token")"
    echo "namespaces:"
    echo "  users:"
    echo "    - regex: '@''${1}_.+:prussin\.net'"
    echo "      exclusive: true"
    echo "    - regex: '@''${1}bot:prussin\.net'"
    echo "      exclusive: true"
    echo "url: http://localhost:$2"
    echo "sender_localpart: ''${1}_sender_localpart"
    echo "rate_limited: false"
  '';

  getWpaPassphraseFile = pkgs.writeShellScriptBin "getWpaPassphraseFile" ''
    set -euo pipefail
    while [[ $# -gt 0 ]]
    do
      network_type="$1"
      shift
      case $network_type in
        --wpa3)
          network="$1"
          shift
          password_var_name="$1"
          shift
          password="$(${getPassword}/bin/getPassword "Connor/Wifi/$network")"
          echo "$password_var_name=\"$password\""
          ;;
        --wpa)
          network="$1"
          shift
          psk_var_name="$1"
          shift
          password="$(${getPassword}/bin/getPassword "Connor/Wifi/$network")"
          psk="$(${wpa_passphrase} "$network" "$password" | ${grep} -P '^\tpsk=' | ${sed} 's/^\tpsk=//')"
          echo "$psk_var_name=\"$psk\""
          ;;
        --peap-mschap)
          network="$1"
          shift
          identity_var_name="$1"
          shift
          password_var_name="$1"
          shift
          identity="$(${getPasswordField}/bin/getPasswordField "Connor/Wifi/$network" "Identity")"
          echo "$identity_var_name=\"$identity\""
          password="$(${getPassword}/bin/getPassword "Connor/Wifi/$network")"
          echo "$password_var_name=\"$password\""
          ;;
        *)
          echo "ERROR: Unknown network type: $network_type" >&2
          exit 1
       esac
    done
  '';

  getNixAccessToken = pkgs.writeShellScriptBin "getNixAccessToken" ''
    set -euo pipefail
    token="$(${getPassword}/bin/getPassword "$1")"
    domain="$(${getPasswordField}/bin/getPasswordField "$1" "Host")"
    echo "extra-access-tokens = $domain=$token"
  '';

  getVaultwardenSecrets = pkgs.writeShellScriptBin "getVaultwardenSecrets" ''
    set -euo pipefail
    password="$(${getPassword}/bin/getPassword "$1")"
    username="$(${getPasswordField}/bin/getPasswordField "$1" "Username")"
    database="$(${getPasswordField}/bin/getPasswordField "$1" "Database")"
    pushId="$(${getPasswordField}/bin/getPasswordField "$2" "Installation ID")"
    pushKey="$(${getPassword}/bin/getPassword "$2")"
    echo "DATABASE_URL=postgresql://$username:$password@localhost/$database"
    echo "PUSH_INSTALLATION_ID=$pushId"
    echo "PUSH_INSTALLATION_KEY=$pushKey"
    if [ "''${3-}" ]; then
      echo "ADMIN_TOKEN=$(${getPassword}/bin/getPassword "$3")"
    fi
  '';
in {
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
      getMatrixSynapseSharedSecretConfigFile
      getMautrixTelegramEnvironmentFile
      getMautrixSignalEnvironmentFile
      getMautrixRegistrationFile
      getWpaPassphraseFile
      getNixAccessToken
      getVaultwardenSecrets
    ];
  };
  getPassword = name: ["getPassword" name];
  getFullPassword = name: ["getFullPassword" name];
  getPasswordField = name: field: ["getPasswordField" name field];
  getBase64EncodedPassword = name: ["getBase64EncodedPassword" name];
  getHashedUserPassword = name: ["getHashedUserPassword" name];
  getUsernamePasswordFile = name: ["getUsernamePasswordFile" name];
  getMatrixSynapseDatabaseConfigFile = name: ["getMatrixSynapseDatabaseConfigFile" name];
  getMatrixSynapseSharedSecretConfigFile = name: ["getMatrixSynapseSharedSecretConfigFile" name];
  getMautrixTelegramEnvironmentFile = name: sharedSecret: ["getMautrixTelegramEnvironmentFile" name sharedSecret];
  getMautrixSignalEnvironmentFile = name: sharedSecret: ["getMautrixSignalEnvironmentFile" name sharedSecret];
  getMautrixRegistrationFile = name: port: ["getMautrixRegistrationFile" name (toString port)];
  getWpaPassphraseFile = networks: ["getWpaPassphraseFile"] ++ networks;
  getNixAccessToken = name: ["getNixAccessToken" name];
  getVaultwardenSecrets = database: push: adminToken:
    ["getVaultwardenSecrets" database push]
    ++ (
      if adminToken == null
      then []
      else [adminToken]
    );
}
