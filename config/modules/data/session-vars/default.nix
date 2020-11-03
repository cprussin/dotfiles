{ pkgs, lib, config, ... }:
let
  hmCfg = config.primary-user.home-manager;

  hm-session-vars = pkgs.writeTextFile {
    name = "hm-session-vars.sh";
    destination = "/etc/profile.d/hm-session-vars.sh";
    text = ''
      # Only source this once.
      if [ -n "$__HM_SESS_VARS_SOURCED" ]; then return; fi
      export __HM_SESS_VARS_SOURCED=1
      ${hmCfg.lib.shell.exportAll hmCfg.home.sessionVariables}
    '';
  };
in
{
  # FIXME: This is done under the hood in home-manager to set sessionVariables.
  # We do still want this in the environment, even if we want to clear out the
  # other automatically added stuff.  Ideally there should be a simpler way to
  # clear the environment except this file.
  primary-user.home-manager.home.packages = lib.mkForce [ hm-session-vars ];
}
