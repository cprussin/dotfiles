{ writeShellScript, swaylock, config }:
let
  launcher-apps = config.primary-user.home-manager.programs.launcher.apps;
in
writeShellScript "lock" ''
  ${launcher-apps.insecure}
  exec ${swaylock}/bin/swaylock "$@"
''
