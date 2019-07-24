{ writeScript, bash, pass, config }:

writeScript "pass" ''
  #! ${bash}/bin/sh

  export PASSWORD_STORE_DIR=${config.secure.passwords}
  exec ${pass}/bin/pass "$@"
''
