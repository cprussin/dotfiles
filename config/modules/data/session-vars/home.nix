# This is done under the hood in home-manager to set sessionVariables.  It's
# pretty important to have in the environment, even if I want to clear the
# environment out of other auto-added stuff.
#
# Since the only real way to clear other stuff is to set
# `primary-user.home-manager.home.packages` to `lib.mkForce []`, here I simply
# copypasta the [implementation from
# home-manager](https://github.com/nix-community/home-manager/blob/8127799f79ee96129b295d78294f40a54078131f/modules/home-environment.nix#L448-L465)
# except adding a `lib.mkForce` when adding to `home.packages`.
#
# There probably isn't any better way of doing this unless home-manager does
# some refactoring -- e.g. if they expose a function to generate this derivation
# or change to sourcing this directly out of the nix store instead of out of the
# profile -- but they likely won't do that since this is pretty nonstandard
# stuff.
{ pkgs, lib, config, ... }:
let
  hm-session-vars = pkgs.writeTextFile {
    name = "hm-session-vars.sh";
    destination = "/etc/profile.d/hm-session-vars.sh";
    text = ''
      # Only source this once.
      if [ -n "$__HM_SESS_VARS_SOURCED" ]; then return; fi
      export __HM_SESS_VARS_SOURCED=1
      ${config.lib.shell.exportAll config.home.sessionVariables}
    '' + lib.optionalString (config.home.sessionPath != [ ]) ''
      export PATH="$PATH''${PATH:+:}${lib.concatStringsSep ":" config.home.sessionPath}"
    '' + config.home.sessionVariablesExtra;
  };
in
{
  home.packages = lib.mkForce [ hm-session-vars ];
}
