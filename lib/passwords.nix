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

  getGmailNewMailCounterEnvFile = pkgs.writeShellScriptBin "getGmailNewMailCounterEnvFile" ''
    set -euo pipefail
    echo "CLIENT_SECRET=$(${getPassword}/bin/getPassword "$1")"
    echo "CLIENT_ID=$(${getPasswordField}/bin/getPasswordField "$1" "Client ID")"
    echo "PROJECT_ID=$(${getPasswordField}/bin/getPasswordField "$1" "Project ID")"
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
      getVaultwardenSecrets
      getGmailNewMailCounterEnvFile
    ];
  };
  getPassword = name: ["getPassword" name];
  getFullPassword = name: ["getFullPassword" name];
  getPasswordField = name: field: ["getPasswordField" name field];
  getBase64EncodedPassword = name: ["getBase64EncodedPassword" name];
  getHashedUserPassword = name: ["getHashedUserPassword" name];
  getVaultwardenSecrets = database: push: adminToken:
    ["getVaultwardenSecrets" database push]
    ++ (
      if adminToken == null
      then []
      else [adminToken]
    );
  getGmailNewMailCounterEnvFile = name: ["getGmailNewMailCounterEnvFile" name];
}
