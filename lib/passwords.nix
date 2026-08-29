{
  pkgs,
  lib,
}: let
  pass = "${pkgs.pass}/bin/pass";
  head = "${pkgs.coreutils}/bin/head";
  base64 = "${pkgs.coreutils}/bin/base64";
  grep = "${pkgs.gnugrep}/bin/grep";
  sed = "${pkgs.gnused}/bin/sed";
  mkpasswd = "${pkgs.mkpasswd}/bin/mkpasswd";
  jq = "${pkgs.jq}/bin/jq";

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

  getMatrixSynapseDatabaseConfigFile = pkgs.writeShellScriptBin "getMatrixSynapseDatabaseConfigFile" ''
    set -euo pipefail
    echo "database:"
    echo "  name: \"psycopg2\""
    echo "  args:"
    echo "    password: \"$(${getPassword}/bin/getPassword "$1")\""
    echo "    database: \"matrix-synapse\""
    echo "    user: \"matrix-synapse\""
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

  # The offer adder wants one JSON document holding every bank login, so this
  # walks `<account id> <pass path>` pairs and assembles it. Each entry's first
  # line is the password; `Username` and the optional `TOTP Secret` are fields.
  # Nothing secret is ever passed in argv -- `ps` is world-readable -- so the
  # values reach jq through the environment instead.
  getOfferAdderCredentials = pkgs.writeShellScriptBin "getOfferAdderCredentials" ''
    set -euo pipefail
    # A plain assignment, and ahead of the loop: `VAR="$(...)" prog` is the one
    # construct `set -e` cannot see, so a lookup that fails in an assignment
    # prefix would hand jq an empty token and exit 0.  Doing it first also
    # fails a bad ntfy entry before any bank password is read.
    ntfyToken="$(${getPasswordField}/bin/getPasswordField "$1" "Token")"
    shift
    # Assembled into a variable rather than straight to stdout: `jq -s` on the
    # right of the pipe would otherwise emit a partial document before the
    # non-zero status of a failed lookup could stop it.
    document="$(
      {
        while [ "$#" -gt 0 ]; do
          if [ "$#" -lt 2 ]; then
            echo "getOfferAdderCredentials: account \"$1\" has no pass path" >&2
            exit 1
          fi
          entry="$(${pass} show "$2")"
          # `TOTP Secret` is absent on every account today -- neither issuer
          # offers authenticator-app enrollment -- and the credentials schema
          # makes it optional, so it is emitted only when present.  An absent
          # `Username` is always a malformed entry, though, and `sed` would
          # otherwise hand over an empty string that deploys clean and fails
          # at the bank sign-in twelve hours later.
          username="$(printf '%s\n' "$entry" | ${sed} -n 's/^Username: //p')"
          if [ -z "$username" ]; then
            echo "getOfferAdderCredentials: no Username field in \"$2\"" >&2
            exit 1
          fi
          OFFER_ADDER_PASSWORD="$(printf '%s\n' "$entry" | ${head} -n 1)" \
          OFFER_ADDER_USERNAME="$username" \
          OFFER_ADDER_TOTP="$(printf '%s\n' "$entry" | ${sed} -n 's/^TOTP Secret: //p')" \
            ${jq} -cn --arg id "$1" '{
              key: $id,
              value: (
                {
                  username: $ENV.OFFER_ADDER_USERNAME,
                  password: $ENV.OFFER_ADDER_PASSWORD,
                }
                + (
                  if $ENV.OFFER_ADDER_TOTP == "" then {}
                  else {totpSecret: $ENV.OFFER_ADDER_TOTP}
                  end
                )
              ),
            }'
          shift 2
        done
      } | OFFER_ADDER_NTFY_TOKEN="$ntfyToken" \
        ${jq} -s '{ntfyToken: $ENV.OFFER_ADDER_NTFY_TOKEN, accounts: from_entries}'
    )"
    printf '%s\n' "$document"
  '';

  # ntfy provisions its users, their ACLs and their access tokens from the
  # environment at startup, so the whole auth database is derivable from pass.
  # Each entry's first line is the password and `Username` names the ntfy user;
  # the first path is the service account, whose `Token` field the offer adder
  # authenticates with. That field must be `ntfy token generate` output --
  # `tk_` and 29 more characters -- because ntfy validates provisioned tokens
  # at startup and refuses to serve at all when one is malformed.
  getNtfySecrets = pkgs.writeShellScriptBin "getNtfySecrets" ''
    set -euo pipefail
    topic="$1"
    servicePath="$2"
    # Drop only the topic: the service account is a user like any other, it
    # just also carries the token.
    shift
    users=""
    access=""
    for path in "$@"; do
      username="$(${getPasswordField}/bin/getPasswordField "$path" "Username")"
      hash="$(${pass} show "$path" | ${mkpasswd} -m bcrypt -R 10 -s)"
      users="''${users:+$users,}$username:$hash:user"
      access="''${access:+$access,}$username:$topic:rw"
    done
    # Single quotes: systemd reads them literally, and a bcrypt hash is full of
    # dollar signs.
    echo "NTFY_AUTH_USERS='$users'"
    echo "NTFY_AUTH_ACCESS='$access'"
    echo "NTFY_AUTH_TOKENS='$(${getPasswordField}/bin/getPasswordField "$servicePath" "Username"):$(${getPasswordField}/bin/getPasswordField "$servicePath" "Token")'"
  '';

  getImmichSecrets = pkgs.writeShellScriptBin "getImmichSecrets" ''
    set -euo pipefail
    echo "DB_PASSWORD=$(${getPassword}/bin/getPassword "$1")"
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
      getMatrixSynapseDatabaseConfigFile
      getVaultwardenSecrets
      getGmailNewMailCounterEnvFile
      getImmichSecrets
      getNtfySecrets
      getOfferAdderCredentials
    ];
  };
  getPassword = name: ["getPassword" name];
  getFullPassword = name: ["getFullPassword" name];
  getPasswordField = name: field: ["getPasswordField" name field];
  getBase64EncodedPassword = name: ["getBase64EncodedPassword" name];
  getHashedUserPassword = name: ["getHashedUserPassword" name];
  getMatrixSynapseDatabaseConfigFile = name: ["getMatrixSynapseDatabaseConfigFile" name];
  getVaultwardenSecrets = database: push: adminToken:
    ["getVaultwardenSecrets" database push]
    ++ (
      if adminToken == null
      then []
      else [adminToken]
    );
  getGmailNewMailCounterEnvFile = name: ["getGmailNewMailCounterEnvFile" name];
  getImmichSecrets = db: ["getImmichSecrets" db];
  getNtfySecrets = topic: servicePath: humanPaths:
    ["getNtfySecrets" topic servicePath] ++ humanPaths;
  getOfferAdderCredentials = ntfyPath: accounts:
    ["getOfferAdderCredentials" ntfyPath]
    ++ lib.concatLists (lib.mapAttrsToList (id: path: [id path]) accounts);
}
